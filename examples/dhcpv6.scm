(import (scheme base)
        (scheme write)
        (scheme process-context)

        (srfi 1)
        (srfi 151)

        (sisl)
        (sps))

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
                         #x00 #x00 #x00 #x00
                         #x00 #x00 #x00 #x01))

;; UDP constants
(define udp-next-header 17)
(define udp-port 546)      ;; DHCPV6_CLIENT_PORT
(define udp-src-port 547)  ;; DHCPV6_SERVER_PORT

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Extract DHCP message type from a DHCPv6 message.
(define (dhcp-type bv)
  (bytevector->number
    (get-field
      bv
      (ipv6-packet 0 (udp-datagram (dhcpv6-hdr 23 42)))
      'dhcpv6-type)))

;; Extract transaction id from a DHCPv6 message.
(define (dhcp-trans-id bv)
  (bytevector->number
    (get-field
      bv
      (ipv6-packet 0 (udp-datagram (dhcpv6-hdr 23 42)))
      'dhcpv6-id)))

;; Extract DHCPv6 option from message.
(define (dhcp-opt-val bv opt-code)
 (define (%dhcp-opt-val bv)
   (if (zero? (bytevector-length bv))
     #f
     (let ((code (bytevector->number (bytevector-copy bv 0 2)))
           (len  (bytevector->number (bytevector-copy bv 2 4)))
           (rest (bytevector-copy bv 4)))
       (display "code: ") (display code) (newline)
       (display "len: ") (display len) (newline)
       (if (eqv? code opt-code)
         (bytevector-copy rest 0 len)
         (%dhcp-opt-val (bytevector-copy rest len))))))

 (let* ((opt-size (- (bytevector-length bv)
                     (input-format-bytesize
                       (ipv6-packet 0 (udp-datagram (make-input-format))))))
        (base-fmt (ipv6-packet 0 (udp-datagram (dhcpv6-hdr-opt 0 0 opt-size)))))
   (%dhcp-opt-val (get-field bv base-fmt 'dhcpv6-opts))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DHCPv6 message types
;; See: https://datatracker.ietf.org/doc/html/rfc8415#section-7.3
(define dhcpv6-solicit 1)
(define dhcpv6-advertise 2)
(define dhcpv6-request 3)
(define dhcpv6-confirm 4)
(define dhcpv6-renew 5)
(define dhcpv6-rebind 6)
(define dhcpv6-reply 7)

;; DHCPv6 option types
;; See: https://datatracker.ietf.org/doc/html/rfc8415#section-24
(define dhcpv6-opt-client-id 1)
(define dhcpv6-opt-server-id 2)

;; DHCPv6 header format.
;; Primarly intended to extract fields from client messages.
;;
;; See: https://datatracker.ietf.org/doc/html/rfc8415#section-8
(define-input-format (dhcpv6-hdr msg-type id)
  (make-uint 'dhcpv6-type 8 msg-type)
  (make-uint 'dhcpv6-id 24 id))
(define-input-format (dhcpv6-hdr-opt msg-type id opt-size)
  (make-uint 'dhcpv6-type 8 msg-type)
  (make-uint 'dhcpv6-id 24 id)
  (make-uint 'dhcpv6-opts (- (bytes->bits opt-size) 8 24) 0))

;; See:
;;  • Advertise Handling: https://datatracker.ietf.org/doc/html/rfc8415#section-16.3
;;  • Message Format: https://datatracker.ietf.org/doc/html/rfc8415#section-8
;;  • Option Format: https://datatracker.ietf.org/doc/html/rfc8415#section-21.1
(define-input-format (dhcpv6-advertise-fmt id client-id)
  (make-uint 'dhcpv6-type 8 dhcpv6-advertise)
  (make-uint 'dhcpv6-id 24 id)

  ;; Client Identifier Option
  (make-uint 'optcode-client-id 16 1)
  (make-uint 'optlen-client-id 16 (bytevector-length client-id))
  (make-concrete 'optdata-client-id (bytes->bits (bytevector-length client-id)) client-id)

  ;; Server Identifier Option
  (make-uint 'optcode-server-id 16 dhcpv6-opt-server-id)
  (make-uint 'optlen-server-id 16 4)
  (make-concrete 'optdata-server-id (bytes->bits 4) (bytevector #xAA #xBB #xCC #xDD)))

;; See: https://datatracker.ietf.org/doc/html/rfc8415#section-16.10
(define-input-format (dhcpv6-reply-fmt id client-id)
  (make-uint 'dhcpv6-type 8 dhcpv6-reply)
  (make-uint 'dhcpv6-id 24 id)

  ;; Client Identifier Option
  (make-uint 'optcode-client-id 16 1)
  (make-uint 'optlen-client-id 16 (bytevector-length client-id))
  (make-concrete 'optdata-client-id (bytes->bits (bytevector-length client-id)) client-id)

  ;; Server Identifier Option
  (make-uint 'optcode-server-id 16 dhcpv6-opt-server-id)
  (make-uint 'optlen-server-id 16 4)
  (make-concrete 'optdata-server-id (bytes->bits 4) (bytevector #xAA #xBB #xCC #xDD)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-state-machine symbolic-machine
  (start pre-solicit)

  (define-state (pre-solicit input)
    (switch (dhcp-type input)
      ;; See https://datatracker.ietf.org/doc/html/rfc8415#section-16.3
      ((dhcpv6-solicit) (-> (make-response
                              (dhcpv6-advertise-fmt
                                (dhcp-trans-id input)
                                (dhcp-opt-val input dhcpv6-opt-client-id)))
                            advertised))))

  (define-state (advertised input)
    (switch (dhcp-type input)
      ((dhcpv6-request) (-> (make-response
                              (dhcpv6-reply-fmt
                                (dhcp-trans-id input)
                                (dhcp-opt-val input dhcpv6-opt-client-id)))
                            replied))))

  (define-state (replied input)
    (-> (make-response
          (make-input-format
            (make-symbolic
              'mqtt-fully-symbolic
              (bytes->bits 32))))
        replied)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (main args)
  (if (eq? (length args) 3)
    (sm-server symbolic-machine (second args) (string->number (third args)))
    (error "Missing host and port argument")))

(cond-expand
  ((or chicken-script compiling) (main (command-line)))
  (else #t))
