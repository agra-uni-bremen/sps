(import (scheme base)
        (scheme write)
        (scheme process-context)

        (srfi 1)
        (srfi 151)

        (sisl)
        (sps))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mqtt-msg-type bv)
  (bytevector->number
    (get-field
      bv
      (ipv6-packet 0 (udp-datagram mqtt-header))
      'mtype)))

(define (mqtt-msg-id bv)
  (bytevector->number
    (get-field
      bv
      (ipv6-packet 0 (udp-datagram (subscribe-fmt 0)))
      'msgid)))

(define (mqtt-will? bv)
  (define WILL_FLAG #x08)

  (let* ((fmt (ipv6-packet 0 (udp-datagram connect-fmt)))
         (flags (bytevector->number (get-field bv fmt 'flags))))
    (not (zero? (bitwise-and flags WILL_FLAG)))))

(define (make-response fmt)
  (ipv6-packet
    udp-next-header
    (udp-datagram
      fmt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; IPv6 constants
(define ipv6-version-value #x6)
(define ipv6-loopback #u8(#x00 #x00 #x00 #x00
                          #x00 #x00 #x00 #x00
                          #x00 #x00 #x00 #x00
                          #x00 #x00 #x00 #x01))
(define ipv6-srcaddr #u8(#xfe #x80 #x00 #x00
                         #x00 #x00 #x00 #x00
                         #xca #xfe #xca #xfe
                         #xca #xfe #x00 #x02))

;; UDP constants
(define udp-next-header 17)
(define udp-port 1883)     ;; CONFIG_EMCUTE_DEFAULT_PORT
(define udp-src-port 2389) ;; can essentially be anything

;; MQTT constants
(define mqtt-msg-connack #x05)
(define mqtt-code-accept #x00)

;; See https://datatracker.ietf.org/doc/html/rfc8200#section-3
(define-input-format (ipv6-packet next-hdr &encapsulate payload)
  (make-uint 'version 4 ipv6-version-value)
  (make-uint 'traffic-class 8 #x0)
  (make-uint 'flow-label 20 #x0)
  (make-uint 'payload-length 16 (input-format-bytesize payload))
  (make-uint 'next-header 8 next-hdr)
  (make-uint 'hop-limit 8 #x42)
  (make-concrete 'src-addr 128 ipv6-srcaddr)
  (make-concrete 'dst-addr 128 ipv6-loopback))

;; See https://datatracker.ietf.org/doc/html/rfc768
(define-input-format (udp-datagram &encapsulate payload)
  (make-uint 'src-port 16 udp-src-port)
  (make-uint 'dst-port 16 udp-port)
  (make-uint 'length 16 (+ 8 (input-format-bytesize payload)))
  (make-uint 'checksum 16 0)) ;; checksum checks disabled in RIOT

(define-input-format mqtt-header
  (make-uint 'length 8 3)
  (make-uint 'mtype  8 0))

;; MQTT-SN CONNACK message
(define-input-format (mqtt-connack code)
  (make-uint 'length 8 3)
  (make-uint 'mtype  8 mqtt-msg-connack)
  (make-uint 'return 16 code))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define CONNECT      #x04)
(define CONNACK      #x05)
(define DISCONNECT   #x18)
(define SUBSCRIBE    #x12)
(define SUBACK       #x13)
(define PUBLISH      #x0C)
(define PUBACK       #x0D)
(define WILLTOPICREQ #x06)
(define WILLTOPIC    #x07)
(define WILLMSGREQ   #x08)
(define WILLMSG      #x09)

(define code-accept #x00)
(define code-congestion #x01)
(define code-invalid-topic #x02)
(define code-unsupported #x03)

(define-input-format connect-fmt
  (make-uint 'length 8 7)
  (make-uint 'mtype  8 CONNECT)
  (make-uint 'flags  8 0)
  (make-uint 'pid    8 23)
  (make-uint 'duration 16 0)
  (make-uint 'clientid 8 1))

(define-input-format (connack-fmt code)
  (make-uint 'length 8 3)
  (make-uint 'mtype  8 CONNACK)
  (make-symbolic 'return 8))

(define-input-format will-topic-req-fmt
  (make-uint 'length 8 2)
  (make-uint 'mtype  8 WILLTOPICREQ))

(define-input-format will-msg-req-fmt
  (make-uint 'length 8 2)
  (make-uint 'mtype  8 WILLMSGREQ))

(define-input-format (subscribe-fmt id)
  (make-uint 'length  8 7)
  (make-uint 'mtype   8 SUBSCRIBE)
  (make-uint 'flags   8 0)
  (make-uint 'msgid   16 id)
  (make-uint 'topic   16 23))

(define-input-format (suback-fmt id)
  (make-uint 'length  8 8)
  (make-uint 'mtype   8 SUBACK)
  (make-uint 'flags   8 0)
  (make-uint 'topicid 16 23)
  (make-uint 'msgid   16 id)
  (make-uint 'return  8 code-accept))

(define-input-format (puback-fmt msg-id topic-id)
  (make-uint 'length  8 7)
  (make-uint 'mtype   8 PUBACK)
  (make-uint 'puback-topic 16 topic-id)
  (make-uint 'msgid  16 msg-id)
  (make-symbolic 'puback-return 8))

(define-input-format disconn-fmt
  (make-uint 'length 8 2)
  (make-uint 'type   8 DISCONNECT))

(define-state-machine mqtt-machine
  (start pre-connected)
  (end   disconnected)

  (define-state (pre-connected input)
    (switch (mqtt-msg-type input)
      ((CONNECT) (if (mqtt-will? input)
                   (-> (make-response will-topic-req-fmt) will-topic-req)
                   (-> (make-response (connack-fmt code-accept)) connected)))))

  (define-state (will-topic-req input)
    (switch (mqtt-msg-type input)
      ((WILLTOPIC) (-> (make-response will-msg-req-fmt) will-msg-req))))

  (define-state (will-msg-req input)
    (switch (mqtt-msg-type input)
      ((WILLMSG) (-> (make-response (connack-fmt code-accept)) connected))))

  (define-state (connected input)
    (switch (mqtt-msg-type input)
      ((SUBSCRIBE)  (-> (make-response (suback-fmt (mqtt-msg-id input))) subscribed))
      ((DISCONNECT) (-> disconn-fmt disconnected))))

  (define-state (subscribed input)
    (switch (mqtt-msg-type input)
      ((PUBLISH) (-> (make-response
                       (puback-fmt
                         (mqtt-msg-id input)
                         0)) subscribed))
      ((DISCONNECT) (-> disconn-fmt disconnected))))

  (define-state disconnected))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (main args)
  (if (eq? (length args) 3)
    (sm-server mqtt-machine (second args) (string->number (third args)))
    (error "Missing host and port argument")))

(cond-expand
  ((or chicken-script compiling) (main (command-line)))
  (else #t))
