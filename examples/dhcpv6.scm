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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DHCPv6 constants
;; See: https://datatracker.ietf.org/doc/html/rfc8415#section-7.3
(define dhcpv6-solicit 1)
(define dhcpv6-advertise 2)
(define dhcpv6-request 3)
(define dhcpv6-confirm 4)

;; DHCPv6 header format.
;; Primarly intended to extract fields from client messages.
;;
;; See: https://datatracker.ietf.org/doc/html/rfc8415#section-8
(define-input-format (dhcpv6-hdr msg-type id)
  (make-uint 'dhcpv6-type 8 msg-type)
  (make-uint 'dhcpv6-id 24 id))

;; See:
;;  • Advertise Handling: https://datatracker.ietf.org/doc/html/rfc8415#section-16.3
;;  • Message Format: https://datatracker.ietf.org/doc/html/rfc8415#section-8
;;  • Option Format: https://datatracker.ietf.org/doc/html/rfc8415#section-21.1
(define-input-format (dhcpv6-advertise-fmt id)
  (make-uint 'dhcpv6-type 8 dhcpv6-advertise)
  (make-uint 'dhcpv6-id 24 id)

  ;; Client Identifier Option
  ;; XXX: Partially symbolic for now, could also be extracted.
  (make-uint 'optcode-client-id 16 1)
  (make-uint 'optlen-client-id 16 4)
  (make-symbolic 'optdata-client-id 32)

  ;; Server Identifier Option
  (make-uint 'optcode-server-id 16 2)
  (make-uint 'optlen-server-id 16 4)
  (make-concrete 'optdata-server-id (bytes->bits 4) (bytevector #xAA #xBB #xCC #xDD)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-state-machine symbolic-machine
  (start pre-solicit)

  (define-state (pre-solicit input)
    (switch (dhcp-type input)
      ;; TODO https://datatracker.ietf.org/doc/html/rfc8415#section-16.3
      ((dhcpv6-solicit) (-> (make-response
                              (dhcpv6-advertise-fmt (dhcp-trans-id input)))
                            advertised))))

  (define-state advertised))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (main args)
  (if (eq? (length args) 3)
    (sm-server symbolic-machine (second args) (string->number (third args)))
    (error "Missing host and port argument")))

(cond-expand
  ((or chicken-script compiling) (main (command-line)))
  (else #t))
