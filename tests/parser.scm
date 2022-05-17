(define-input-format two-byte-fmt
  (make-uint 'first  8 #x00)
  (make-uint 'second 8 #x01))

(define-input-format four-nibble-fmt
  (make-uint 'f1 4 0)
  (make-uint 'f2 4 1)
  (make-uint 'f3 4 2)
  (make-uint 'f4 4 3))

(define-input-format multiple-bytes-in-field-fmt
  (make-uint 'f1 16 0)
  (make-uint 'f2 16 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-group "parse-message"
  (test "extract byte fields no padding"
        '((first . #u8(#xfe)) (second . #u8(#xff)))
        (parse-message #u8(#xfe #xff) two-byte-fmt))

  (test "extract byte fields with padding"
        '((first . #u8(23)) (second . #u8(42)))
        (parse-message #u8(23 42) two-byte-fmt))

  (test "extract half byte fields"
        '((f1 . #u8(#xA)) (f2 . #u8(#xB)) (f3 . #u8(#xC)) (f4 . #u8(#xD)))
        (parse-message #u8(#xAB #xCD) four-nibble-fmt))

  (test "extract two 16-bit fields"
        '((f1 . #u8(#x23 #x42)) (f2 . #u8(#x42 #x23)))
        (parse-message #u8(#x23 #x42 #x42 #x23) multiple-bytes-in-field-fmt)))
