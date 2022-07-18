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
  (make-symbolic 'checksum 16)
  (make-concrete 'src-addr 32 ipv4-srcaddr)
  (make-concrete 'dst-addr 32 ipv4-dstaddr))

;; See https://datatracker.ietf.org/doc/html/rfc768
(define-input-format (udp-datagram &encapsulate payload)
  (make-uint 'src-port 16 udp-src-port)
  (make-uint 'dst-port 16 udp-port)
  (make-uint 'length 16 (+ 8 (input-format-bytesize payload)))
  (make-uint 'checksum 16 0)) ;; checksum checks disabled

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-state-machine symbolic-machine
  (start symbolic)

  (define-state (symbolic input)
    (-> (make-response
          (make-input-format
            (make-symbolic
              'mqtt-fully-symbolic
              (bytes->bits 56))))
        symbolic)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (main args)
  (if (eq? (length args) 3)
    (sm-server symbolic-machine (second args) (string->number (third args)))
    (error "Missing host and port argument")))

(cond-expand
  ((or chicken-script compiling) (main (command-line)))
  (else #t))
