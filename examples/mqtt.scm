(import (scheme base)
        (scheme write)
        (scheme process-context)

        (srfi 1)
        (sisl)
        (sps))

(define (get-field bv field-name)
  (let* ((fmt (ipv6-packet 0 (udp-datagram mqtt-header)))
         (alist (parse-message bv fmt))
         (value (assq field-name alist)))
    (if value
      (cdr value)
      (error (string-append "no field named: " (symbol->string field-name))))))

(define (mqtt-msg-type bv)
  (bytevector->number (get-field bv 'mtype)))

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

;; UDP constants
(define udp-next-header 17)
(define udp-port 1883) ;; CONFIG_EMCUTE_DEFAULT_PORT

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
  (make-symbolic 'src-addr 128)
  (make-concrete 'dst-addr 128 ipv6-loopback))

;; See https://datatracker.ietf.org/doc/html/rfc768
(define-input-format (udp-datagram &encapsulate payload)
  (make-symbolic 'src-port 16)
  (make-uint 'dst-port 16 udp-port)
  (make-uint 'length 16 (+ 8 (input-format-bytesize payload)))
  (make-symbolic 'checksum 16))

(define-input-format mqtt-header
  (make-uint 'length 8 3)
  (make-uint 'mtype  8 0))

;; MQTT-SN CONNACK message
(define-input-format (mqtt-connack code)
  (make-uint 'length 8 3)
  (make-uint 'mtype  8 mqtt-msg-connack)
  (make-uint 'return 16 code))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define CONNECT    #x04)
(define CONNACK    #x05)
(define DISCONNECT #x18)
(define SUBSCRIBE  #x12)

(define code-accept #x00)
(define code-congestion #x01)
(define code-invalid-topic #x02)
(define code-unsupported #x03)

(define-input-format (connack-fmt code)
  (make-uint 'length 8 3)
  (make-uint 'mtype  8 CONNACK)
  (make-uint 'return 8 code))

(define-input-format disconn-fmt
  (make-uint 'length 8 2)
  (make-uint 'type   8 DISCONNECT))

(define-state-machine mqtt-machine
  (start pre-connected)
  (end   disconnected)

  (define-state (pre-connected input)
    (switch (mqtt-msg-type input)
      ((CONNECT) (-> (make-response (connack-fmt code-accept)) connected))))

  (define-state (connected input)
    (switch (mqtt-msg-type input)
      ((SUBSCRIBE)  (-> (error "subscribe not implemented") connected))
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
