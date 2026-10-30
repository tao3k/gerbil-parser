(import :gerbil-parser/language-test-support)
;;; Source coordinates retain byte semantics across private storage choices.
(import :std/test
        (only-in ./fixtures/layout-columns reference-layout-columns reference-packed-layout-columns layout-column-sources)
        (only-in :gerbil-parser/src/runtime/layout
                 make-layout-columns current-layout-columns layout-token-column)
        (only-in :gerbil-parser/src/runtime/token make-token))


(deflanguage-parser-tests layout-column-storage-test "Layout source coordinate storage"
  (engine)
    (scenario "packed source columns match the byte oracle including UTF-8 interiors and EOF"
      (each (source) (layout-column-sources)
         (sequential ((expected (reference-layout-columns source))
                (columns (make-layout-columns source)))
           (equal (u32vector->list (reference-packed-layout-columns source))  (vector->list expected))
           (equal (u32vector? columns)  #t)
           (equal (u32vector-length columns)  (vector-length expected))
           (parameters ((current-layout-columns columns))
             (iterate visit ((offset 0))
               (when (< offset (vector-length expected))
                 (equal (layout-token-column (make-token 'probe "" offset offset))
                         (vector-ref expected offset))
                 (continue visit (+ offset 1))))))))
    (scenario "ASCII fast path rejoins multibyte filling with exact byte columns"
      (each (source) ((list (list->string (map integer->char (iota 128)))
              (string-append (make-string 1024 #\space) "λ\t\r\n😀" (make-string 1024 #\space))))
        (scope ((expected (vector->list (reference-layout-columns source))))
          (equal (u32vector->list (reference-packed-layout-columns source))  expected)
          (equal (u32vector->list (make-layout-columns source))  expected))))
    (scenario "long tab columns remain exact beyond u16 and retain CRLF resets"
      (sequential ((source (string-append (make-string 10000 #\tab) "中\r\n😀"))
             (columns (make-layout-columns source)))
        (parameters ((current-layout-columns columns))
          (equal (layout-token-column (make-token 'probe "中" 10000 10003))  80000)
          (equal (layout-token-column (make-token 'probe "😀" 10005 10009))  0)
          (equal (layout-token-column (make-token 'probe "" 10009 10009))  1))))
    (scenario "wide exact-integer storage and missing request context retain lookup semantics"
      (parameters ((current-layout-columns (vector 0 #x100000000 #x100000001)))
        (equal (layout-token-column (make-token 'probe "" 1 1))  #x100000000)
        (equal (layout-token-column (make-token 'probe "" 2 2))  #x100000001))
      (parameters ((current-layout-columns #f))
        (equal (layout-token-column (make-token 'probe "" 0 0))  #f))))
