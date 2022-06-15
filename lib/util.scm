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

;; This is similar to case, but the keys are evaluated & equal? is the test.

; From moremacros by Kon Lovett.
; Licensed under MIT.
;
; See https://wiki.call-cc.org/eggref/5/moremacros#license
(define-syntax %switch
  (er-macro-transformer
    (lambda (frm ren cmp)
      (##sys#check-syntax 'switch frm '(_ _ . _))
      (let ((exp (cadr frm))
            (body (cddr frm))
            (_tmp (ren 'tmp))
            (_else (ren 'else))
            (_or (ren 'or)) )
        `(let ((,_tmp ,exp))
          ,(let expd-form ((clauses body) (seen-else #f))
            (cond
              ((null? clauses)
                '(void) )
              ((not (pair? clauses))
                (syntax-error 'switch "invalid syntax" clauses) )
              (else
                (let ((clause (car clauses))
                      (rclauses (cdr clauses)) )
                  (##sys#check-syntax 'switch clause '#(_ 1))
                  (cond
                    ((cmp _else (car clause))
                      (expd-form rclauses #t)
                      `(begin ,@(cdr clause)) )
                    (seen-else
                      (##sys#notice
                        "non-`else' clause following `else' clause in `switch'"
                        (strip-syntax clause))
                      (expd-form rclauses #t)
                      '(begin) )
                  (else
                    `(if
                      (,_or
                        ,@(map (lambda (x) `(equal? ,_tmp ,x))
                        (car clause)))
                      (##core#begin
                        ,@(cdr clause))
                        ,(expd-form rclauses #f) ) ) ) ) ) ) ) ) ) ) ) )

(define-syntax switch
  (syntax-rules ()
    ((switch ARG BODY ...)
     (%switch ARG
       BODY ...
       (else
         (error (string-append "switch: no match with argument: " (->string ARG))))))))
