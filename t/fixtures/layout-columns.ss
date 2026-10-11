;;; Boxed-vector oracle, independent of packed storage and lookup dispatch.
(export reference-layout-columns reference-packed-layout-columns layout-column-sources)

(def (reference-layout-columns source)
  (let* ((bytes (string->utf8 source)) (size (u8vector-length bytes))
         (columns (make-vector (+ size 1) 0)))
    (let loop ((offset 0) (column 0))
      (if (= offset size)
        (begin (vector-set! columns offset column) columns)
        (let* ((byte (u8vector-ref bytes offset))
               (width (cond ((< byte 128) 1) ((< byte 224) 2)
                            ((< byte 240) 3) (else 4)))
               (end (+ offset width)))
          (let fill ((cursor offset))
            (when (< cursor end)
              (vector-set! columns cursor column) (fill (+ cursor 1))))
          (loop end (cond ((or (= byte 10) (= byte 13)) 0)
                          ((= byte 9) (* (+ (quotient column 8) 1) 8))
                          (else (+ column 1)))))))))

(def layout-column-sources
  (list "" "ascii" "\t\t" "1234567\tX\t" "a\r\nb\rc\nd"
        "éλ中😀\tZ\r\n名字\tend"
        (list->string (map integer->char '(0 1 127 128 2047 2048 55295 57344 65535 65536 1114111)))))

;;; Matched former packed producer, independent of the ASCII-specialized writer.
;;; Keep the same capacity proof and private native setter as production.
(def (reference-packed-layout-columns source)
  (if (> (string-length source) (quotient #xffffffff 8))
    (reference-layout-columns source)
    (let* ((bytes (string->utf8 source)) (size (u8vector-length bytes))
           (columns (make-u32vector (+ size 1) 0)))
      (let loop ((offset 0) (column 0))
        (if (= offset size)
          (begin (##u32vector-set! columns offset column) columns)
          (let* ((byte (u8vector-ref bytes offset))
                 (width (cond ((< byte 128) 1) ((< byte 224) 2) ((< byte 240) 3) (else 4)))
                 (end (+ offset width))
                 (next-column (case byte ((10 13) 0)
                                ((9) (* (+ (quotient column 8) 1) 8))
                                (else (+ column 1)))))
            (let fill ((cursor offset))
              (when (< cursor end)
                (##u32vector-set! columns cursor column) (fill (+ cursor 1))))
            (loop end next-column)))))))
