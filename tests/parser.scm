(define-input-format two-byte-fmt
  (make-uint 'first  8 #x00)
  (make-uint 'second 8 #x01))

(define-input-format four-nibble-fmt
  (make-uint 'f1 4 0)
  (make-uint 'f2 4 1)
  (make-uint 'f3 4 2)
  (make-uint 'f4 4 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-group "parse-message"
  (test "extract byte fields no padding"
        '((first . #xfe) (second . #xff))
        (parse-message #u8(#xfe #xff) two-byte-fmt))

  (test "extract byte fields with padding"
        '((first . 23) (second . 42))
        (parse-message #u8(23 42) two-byte-fmt))

  (test "extract half byte fields"
        '((f1 . #xA) (f2 . #xB) (f3 . #xC) (f4 . #xD))
        (parse-message #u8(#xAB #xCD) four-nibble-fmt)))
