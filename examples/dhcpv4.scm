(import (scheme base)
        (scheme write)
        (scheme process-context)

        (srfi 1)
        (srfi 151)

        (sisl)
        (sps))

(define (make-response fmt)
  (ipv4-packet
    ipv4-udp-protocol
    (udp-datagram
      fmt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; IPv6 constants
(define ipv4-version-value 4)
(define ipv4-udp-protocol 17) ;; See https://datatracker.ietf.org/doc/html/rfc790
(define ipv4-srcaddr #u8(#x00 #x00 #x00 #x00))
(define ipv4-dstaddr #u8(#xff #xff #xff #xff))

;; UDP constants
(define udp-port 68)     ;; DHCPV4_CLIENT_PORT
(define udp-src-port 67) ;; DHCPV4_SERVER_PORT

;; See https://datatracker.ietf.org/doc/html/rfc791#section-3.1
(define-input-format (ipv4-packet protocol &encapsulate payload)
  (make-uint 'version 4 ipv4-version-value)
  (make-uint 'ihl 4 5)
  (make-uint 'service 8 0)
  (make-uint 'length 16 (+ 20 (input-format-bytesize payload)))
  (make-uint 'identification 16 #x2342)
  (make-uint 'flags 3 #b010) ;; Don't fragment and last fragment
  (make-uint 'fragment-offset 13 0)
  (make-uint 'ttl 8 #xff)
  (make-uint 'protocol 8 protocol)
  (make-uint 'checksum 16 0)
  (make-concrete 'src-addr 32 ipv4-srcaddr)
  (make-concrete 'dst-addr 32 ipv4-dstaddr))

;; See https://datatracker.ietf.org/doc/html/rfc768
(define-input-format (udp-datagram &encapsulate payload)
  (make-uint 'src-port 16 udp-src-port)
  (make-uint 'dst-port 16 udp-port)
  (make-uint 'length 16 (+ 8 (input-format-bytesize payload)))
  (make-uint 'checksum 16 0)) ;; checksum checks disabled

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DHCPv4 constants.
(define dhcpv4-bootrequest 1)
(define dhcpv4-bootreply 2)
(define dhcpv4-cookie #u8(#x63 #x82 #x53 #x63))
(define dhcpv4-eoo #xff) ;; end of options

;; DHCPv4 message types.
;; See https://datatracker.ietf.org/doc/html/rfc2132#section-9.6
(define dhcpv4-discover 1)
(define dhcpv4-offer 2)
(define dhcpv4-request 3)
(define dhcpv4-ack 5)

;; See https://datatracker.ietf.org/doc/html/rfc2131#section-2
(define-input-format (dhcpv4-packet mtype xid flags yiaddr siaddr giaddr chaddr &encapsulate opts)
  (make-uint 'dhcpv4-op 8 dhcpv4-bootreply)
  (make-uint 'dhcpv4-htype 8 1)
  (make-uint 'dhcpv4-hlen 8 6)
  (make-uint 'dhcpv4-hops 8 0)
  (make-concrete 'dhcpv4-xid 32 xid)
  (make-uint 'dhcpv4-secs 16 0)
  (make-concrete 'dhcpv4-flags 16 flags)
  (make-uint 'dhcpv4-ciaddr 32 0)
  (make-concrete 'dhcpv4-yiaddr 32 yiaddr)
  (make-concrete 'dhcpv4-siaddr 32 siaddr)
  (make-concrete 'dhcpv4-giaddr 32 giaddr)
  (make-concrete 'dhcpv4-chaddr (bytes->bits 16) chaddr)
  (make-uint 'dhcpv4-sname  (bytes->bits 64) 0)
  (make-uint 'dhcpv4-file   (bytes->bits 128) 0)

  ;; Magic Cookie
  (make-concrete 'dhcpv4-cookie 32 dhcpv4-cookie)

  ;; Options
  ;; See: https://datatracker.ietf.org/doc/html/rfc2132#section-9.6
  (make-uint 'dhcpv4-mtype-code 8 53)
  (make-uint 'dhcpv4-mtype-len  8 1)
  (make-uint 'dhcpv4-mtype-val  8 mtype))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define dhcpv4-basefmt
  (ipv4-packet
    ipv4-udp-protocol
    (udp-datagram
      (dhcpv4-packet 0 #u8() #u8() #u8() #u8() #u8() #u8() (make-input-format)))))

(define (dhcpv4-field bv field)
  (get-field bv dhcpv4-basefmt field))
(define (dhcpv4-mtype bv)
  (bytevector->number (dhcpv4-field bv 'dhcpv4-mtype-val)))

(define (dhcpv4-make-opts name len)
  (make-input-format
    (make-symbolic name len)
    (make-uint 'dhcpv4-eoo 8 dhcpv4-eoo)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-state-machine symbolic-machine
  (start discover)

  (define-state (discover input)
    (switch (dhcpv4-mtype input)
      ;; See https://datatracker.ietf.org/doc/html/rfc2131#section-4.3.1
      ((dhcpv4-discover) (-> (make-response
                               (dhcpv4-packet dhcpv4-offer
                                 (dhcpv4-field input 'dhcpv4-xid)
                                 (dhcpv4-field input 'dhcpv4-flags)
                                 (dhcpv4-field input 'dhcpv4-yiaddr)
                                 (dhcpv4-field input 'dhcpv4-siaddr)
                                 (dhcpv4-field input 'dhcpv4-giaddr)
                                 (dhcpv4-field input 'dhcpv4-chaddr)
                                 (dhcpv4-make-opts 'discover-opts (bytes->bits 64))))
                             request))))

  (define-state (request input)
    (let ((resp-type (switch (dhcpv4-mtype input)
                             ((dhcpv4-discover) dhcpv4-offer)
                             ((dhcpv4-request)  dhcpv4-ack))))
      (-> (make-response
            (dhcpv4-packet resp-type
                           (dhcpv4-field input 'dhcpv4-xid)
                           (dhcpv4-field input 'dhcpv4-flags)
                           (dhcpv4-field input 'dhcpv4-yiaddr)
                           (dhcpv4-field input 'dhcpv4-siaddr)
                           (dhcpv4-field input 'dhcpv4-giaddr)
                           (dhcpv4-field input 'dhcpv4-chaddr)
                           (dhcpv4-make-opts 'ack-opts (bytes->bits 64))))
          symbolic)))

  (define-state (symbolic input)
    (-> (make-response
          (make-input-format
            ;; Minimum size of a DHCPv4 message.
            (make-symbolic
              'mqtt-fully-symbolic
              (bytes->bits 236))
            ;; Magic Cookie.
            (make-concrete 'magic-cookie 32 dhcpv4-cookie)
            ;; Option End.
            (make-uint 'option-end 8 #xff)))
        symbolic)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (main args)
  (if (eq? (length args) 3)
    (sm-server symbolic-machine (second args) (string->number (third args)))
    (error "Missing host and port argument")))

(cond-expand
  ((or chicken-script compiling) (main (command-line)))
  (else #t))
