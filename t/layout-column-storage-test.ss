;;; Source coordinates retain byte semantics across private storage choices.
(import :std/test
        (only-in ./fixtures/layout-columns reference-layout-columns reference-packed-layout-columns layout-column-sources)
        (only-in :gerbil-parser/src/runtime/layout
                 make-layout-columns current-layout-columns layout-token-column)
        (only-in :gerbil-parser/src/runtime/token make-token))
(export layout-column-storage-test)

(def layout-column-storage-test
  (test-suite "Layout source coordinate storage"
    (test-case "packed source columns match the byte oracle including UTF-8 interiors and EOF"
      (for-each
       (lambda (source)
         (let* ((expected (reference-layout-columns source))
                (columns (make-layout-columns source)))
           (check (u32vector->list (reference-packed-layout-columns source)) => (vector->list expected))
           (check (u32vector? columns) => #t)
           (check (u32vector-length columns) => (vector-length expected))
           (parameterize ((current-layout-columns columns))
             (let visit ((offset 0))
               (when (< offset (vector-length expected))
                 (check (layout-token-column (make-token 'probe "" offset offset))
                        => (vector-ref expected offset))
                 (visit (+ offset 1)))))))
       layout-column-sources))
    (test-case "ASCII fast path rejoins multibyte filling with exact byte columns"
      (for-each (lambda (source)
        (let (expected (vector->list (reference-layout-columns source)))
          (check (u32vector->list (reference-packed-layout-columns source)) => expected)
          (check (u32vector->list (make-layout-columns source)) => expected)))
        (list (list->string (map integer->char (iota 128)))
              (string-append (make-string 1024 #\space) "λ\t\r\n😀" (make-string 1024 #\space)))))
    (test-case "long tab columns remain exact beyond u16 and retain CRLF resets"
      (let* ((source (string-append (make-string 10000 #\tab) "中\r\n😀"))
             (columns (make-layout-columns source)))
        (parameterize ((current-layout-columns columns))
          (check (layout-token-column (make-token 'probe "中" 10000 10003)) => 80000)
          (check (layout-token-column (make-token 'probe "😀" 10005 10009)) => 0)
          (check (layout-token-column (make-token 'probe "" 10009 10009)) => 1))))
    (test-case "wide exact-integer storage and missing request context retain lookup semantics"
      (parameterize ((current-layout-columns (vector 0 #x100000000 #x100000001)))
        (check (layout-token-column (make-token 'probe "" 1 1)) => #x100000000)
        (check (layout-token-column (make-token 'probe "" 2 2)) => #x100000001))
      (parameterize ((current-layout-columns #f))
        (check (layout-token-column (make-token 'probe "" 0 0)) => #f)))))
