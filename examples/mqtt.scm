(import (scheme base)
        (scheme write)
        (scheme process-context)

        (srfi 1)
        (sisl)
        (sps))

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
  (make-uint 'return 16 code))

(define-input-format disconn-fmt
  (make-uint 'length 8 2)
  (make-uint 'type   8 DISCONNECT))

(define (mqtt-msg-type bv)
  ;; TODO: Bounds checks
  (if (eq? (bytevector-u8-ref bv 0) #x01)
    (bytevector-u8-ref bv 3)
    (bytevector-u8-ref bv 0)))

(define-state-machine mqtt-machine
  (start pre-connected)
  (end   disconnected)

  (define-state (pre-connected input)
    (switch (mqtt-msg-type input)
      ((CONNECT) (-> (connack-fmt code-accept) connected))))

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
