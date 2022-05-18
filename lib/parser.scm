;; Converts a variable-length bit list back into a Scheme number.

(: bits->number ((vector-of bit) -> number))
(define (bits->number vec)
  (apply bits (vector->list vec)))

(: bits->bytevector ((vector-of bit) -> bytevector))
(define (bits->bytevector vec)
  (if (<= (vector-length vec) 8)
    (bytevector (bits->number vec))
    (let ((bcar (vector-copy vec 0 8))
          (bcdr (vector-copy vec 8)))
      (bytevector-append
        (bits->bytevector bcdr)
        (bytevector (bits->number bcar))))))

;; Fold a bytevector from LSB to MSB.

(define (bytevector-fold proc seed bv)
  (define (%bytevector-fold-right n)
    (if (>= n (bytevector-length bv))
      seed
      (proc (bytevector-u8-ref bv n)
            (%bytevector-fold-right (inc n)))))

  (if (zero? (bytevector-length bv))
    seed
    (%bytevector-fold-right 0)))

;; Converts a bytevector into a SRFI 151 bitvector.
;; Each byte in the vector is padded with zero bytes.

(: bytevector->bits (bytevector -> (vector-of bit)))
(define (bytevector->bits bv)
  (bytevector-fold
    (lambda (byte vec)
      (let ((padded (make-vector 8 #f)))
        (vector-copy! padded 0 (bits->vector byte))
        (vector-append vec padded)))
    #() bv))

;; Convert a bytevector to a number.

(: bytevector->number (bytevector -> number))
(define (bytevector->number bv)
  (let ((shift-proc (lambda (idx) (* idx 8))))
    (apply bitwise-ior
           (map (lambda (index)
                   (arithmetic-shift
                     (bytevector-u8-ref bv index)
                     (shift-proc index)))
                (iota (bytevector-length bv))))))

;; Pare definition from R7RS specification.

(define-record-type Pare
  (kons x y)
  pare?
  (x kar set-kar!)
  (y kdr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Type for field pairs.
(define-type field-pair (pair symbol bytevector))

(: parse-message (bytevector (struct Input-Format) -> (list-of field-pair)))
(define (parse-message msg fmt)
  (if (< (bytevector-length msg) (input-format-bytesize fmt))
    (error "length of message does not match format length")
    (let ((bitvector (bytevector->bits msg)))
      (kdr
        (fold (lambda (field prev-ret)
                (let* ((name (field-name field))
                       (size (field-bitsize field))
                       (prev-end (kar prev-ret))
                       (prev-lst (kdr prev-ret))
                       (start (- prev-end size)))
                  (kons
                    start
                    (append
                      prev-lst
                      (list (cons name (bits->bytevector
                                         (vector-copy
                                           bitvector
                                           start
                                           prev-end))))))))
            (kons (vector-length bitvector) '())
            (vector->list (input-format-fields fmt)))))))
