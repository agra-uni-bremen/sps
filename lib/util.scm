;; Type alias for bits as represented by SRFI 151.
(define-type bit boolean)

;; Type alias for R7RS bytevectors (unfourtunatly not exported by the R7RS egg).
;; See: https://bugs.call-cc.org/ticket/1796
(define-type bytevector u8vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Like display but prints multiple objects and adds trailing newline.

(define (fprintln port . objs)
  (for-each (lambda (obj) (display obj port)) objs)
  (newline port))

(define (println . objs)
  (apply fprintln (current-output-port) objs))

;; Loop forever, does not terminate.

(define-syntax loop
  (syntax-rules ()
    ((loop BODY ...)
     (do ()
         (#f #f)
       BODY ...))))

;; Increment a number.

(: inc (number -> number))
(define (inc n) (+ n 1))

;; Decrement a number.

(: dec (number -> number))
(define (dec n) (- n 1))
