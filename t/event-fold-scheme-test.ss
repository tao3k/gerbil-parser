;;; -*- Gerbil -*-
;;; The explicit native catalog admits the compiler without per-case processes.
(import (only-in :std/test check check-exception test-case test-suite)
        (only-in ./event-strategy-fixture event-lines-language-grammar)
        (only-in ../src/compiler/event-fold-scheme event-fold-scheme-source)
        (only-in ../src/compiler/event-fold-scheme-context current-fold-scheme-diagnostics)
        (only-in ../src/compiler/event-fold-future fold-future-marker-before-boundary?
                 fold-future-named-index)
        (only-in ../src/compiler/event-fold-prefix-family fold-scheme-prefix-family)
        (only-in ../src/compiler/event-fold-scheme-modules event-fold-scheme-module-sources)
        (only-in ../src/runtime/event-fold-lines fold-line-name-set-contains?)
        (only-in ../src/runtime/byte-spans byte-span-skip byte-span-all?)
        (only-in ../src/runtime/source-lines fold-source-lines fold-source-byte-lines
                 fold-source-byte-spans fold-source-frame-boundaries! fold-source-input-bytes))
(export event-fold-scheme-test)

(def (source initial line finish (helpers '()))
  (event-fold-scheme-source 'native-product event-lines-language-grammar 'Document
                            initial line finish helpers))

(def event-fold-scheme-test
  (test-suite "Native EventFold source admission"
    (test-case "native byte frames preserve Unicode CR LF CRLF and terminal bounds"
      (check-exception (fold-source-input-bytes 42) true)
      (for-each
       (lambda (invalid) (check-exception (fold-source-input-bytes invalid) true))
       (list (u8vector 255) (u8vector 192 128) (u8vector 237 160 128)
             (u8vector 244 144 128 128) (u8vector 226 130) (u8vector 128)))
      (check-exception (fold-source-byte-lines "not bytes" '() void) true)
      (let (bytes (u8vector 99 32 9 97 98))
        (check (byte-span-skip bytes 1 5 (lambda (byte) (memv byte '(9 32)))) => 3)
        (check (byte-span-skip bytes 3 3 void) => 3)
        (check (byte-span-all? bytes 1 3 (lambda (byte) (memv byte '(9 32)))) => #t)
        (check (byte-span-all? bytes 1 5 (lambda (byte) (memv byte '(9 32)))) => #f))
      (for-each
       (lambda (text)
         (let* ((bytes (string->utf8 text))
                (before (u8vector->list bytes))
                (oracle (fold-source-lines text '()
                          (lambda (line start end rows)
                            (cons (list line start end) rows))))
                (actual (fold-source-byte-lines bytes '()
                          (lambda (line view start end rows)
                            (check (string->utf8 line) => view)
                            (check (u8vector-length view) => (- end start))
                            (cons (list line start end) rows)))))
           (check (eq? (fold-source-input-bytes bytes) bytes) => #t)
           (check (fold-source-input-bytes text) => bytes)
           (check actual => oracle)
           (check (fold-source-byte-spans bytes '()
                    (lambda (start end rows)
                      (fold-source-frame-boundaries! bytes start end)
                      (cons (list (utf8->string (subu8vector bytes start end)) start end) rows)))
                  => oracle)
           (check (u8vector->list bytes) => before)))
       '("" "abc" "\n" "\r" "\r\n" "é中🦀\r\nx\ry\nz" "a\n\n"))
      (let (bytes (string->utf8 "é中🦀"))
        (for-each (lambda (bounds)
                    (check-exception
                     (fold-source-frame-boundaries! bytes (car bounds) (cadr bounds)) true))
                  '((-1 0) (0 10) (2 0) (1 2) (0 1) (2 3) (5 6)))
        (for-each (lambda (bounds)
                    (fold-source-frame-boundaries! bytes (car bounds) (cadr bounds)))
                  '((0 9) (0 2) (2 5) (5 9) (1 1) (9 9)))))
    (test-case "runtime marker encoding preserves boundaries and cache fast paths"
      (def (query text target stop indent? heading?)
        (fold-future-marker-before-boundary?
         (string->utf8 text) 0 target stop 42 32 indent? heading? #f
         (make-hash-table-eqv)))
      (check (query "x\n  EnD \r\n" "END" #f #t #f) => #t)
      (check (query "x\n  EnD \r\n" "END" #f #f #f) => #f)
      (check (query "stop\nEND\n" "END" "STOP" #f #f) => #f)
      (check (query "END\n" "END" "END" #f #f) => #f)
      (check (query "* Heading\nEND\n" "END" #f #f #t) => #f)
      (check (query "x\n中\n" "中" #f #f #f) => #t)
      (check (fold-future-marker-before-boundary?
              (string->utf8 "x\n中\n") 0 (string->utf8 "中") #f
              42 32 #f #f #f (make-hash-table-eqv)) => #t)
      (check (fold-future-marker-before-boundary?
              (string->utf8 "STOP\nEND\n") 0 (string->utf8 "END") (string->utf8 "STOP")
              42 32 #f #f #f (make-hash-table-eqv)) => #f)
      (check (query "ENDING\n" "END" #f #f #f) => #f)
      (let* ((bytes (string->utf8 "  Stop\r\nEND n\nSTOP\n"))
             (index (fold-future-named-index bytes "END " "" "STOP"
                                            42 32 #t #f #t #f #t)))
        (check (vector->list (vector-ref index 2)) => '(14 0)))
      (check (fold-future-marker-before-boundary?
              (string->utf8 ":KEY: value\nEND\n") 0 "END" #f
              42 32 #f #f 58 (make-hash-table-eqv)) => #t)
      (check (fold-future-marker-before-boundary?
              (string->utf8 "ordinary body\nEND\n") 0 "END" #f
              42 32 #f #f 58 (make-hash-table-eqv)) => #f)
      (let ((bytes (string->utf8 "END\n")) (cache (make-hash-table-eqv)))
        (hash-put! cache 0 1)
        ;; Invalid marker values deliberately prove that these paths never
        ;; invoke the encoder, matching the original cached search semantics.
        (check (fold-future-marker-before-boundary?
                bytes 0 #f #f 42 32 #f #f #f cache) => #t)
        (hash-put! cache 0 2)
        (check (fold-future-marker-before-boundary?
                bytes 0 #f #f 42 32 #f #f #f cache) => #f)
        (check (fold-future-marker-before-boundary?
                bytes (u8vector-length bytes) #f #f 42 32 #f #f #f cache) => #f)))
    (test-case "generated source is deterministic and has no interpreted request path"
      (for-each
       (lambda (operator)
         (let (generated
               (source '()
                       `((if (,operator start (line-step start) (97 98))
                             ((token Line start end)) ())) '()))
           (check (not (string-contains generated "(let loop ((cursor from))")) => #t)
           (check (not (not (string-contains generated "native fold range outside source line"))) => #t)
           (check (not (not (string-contains generated "(u8vector-ref bytes from)"))) => #t)))
       '(line-bytes-all-in? line-bytes-any-in?))
      (let* ((family '((line-starts-with-ascii-ci "#+TITLE:")
                       (line-starts-with-ascii-ci "#+AUTHOR:")
                       (line-starts-with-ascii-ci "#+DATE:")
                       (line-starts-with-ascii-ci "#+CAPTION:")))
             (code (fold-scheme-prefix-family family)))
        (check (not (not code)) => #t)
        (check code => (fold-scheme-prefix-family (reverse family)))
        (check (fold-scheme-prefix-family (cons '(state ready) family)) => #f)
        (check (fold-scheme-prefix-family (take family 3)) => #f)
        (check (fold-scheme-prefix-family (make-list 33 (car family))) => #f)
        (check (fold-scheme-prefix-family (make-list 4 (list 'line-starts-with (make-string 129 #\a)))) => #f))
      (let* ((forms '((if (line-bytes-all-in? start end (10 97))
                         ((token Line start end)) ())))
             (first (source '() forms '())))
        (check (equal? first (source '() forms '())) => #t)
        (check (not (string-contains first "call-with-fold-cost")) => #t)
        (let (observed (parameterize ((current-fold-scheme-diagnostics #t))
                        (source '() forms '())))
          (check (not (not (string-contains observed "call-with-fold-cost"))) => #t)
          (check (not (not (string-contains observed "diagnostic Fold cannot bind"))) => #t))
        (let (counted (parameterize ((current-fold-scheme-diagnostics 'counts))
                       (source '() forms '())))
          (check (not (not (string-contains counted "(fold-cost-count!"))) => #t)
          (check (not (string-contains counted "(call-with-fold-cost '")) => #t)
          (check (not (not (string-contains counted "diagnostic Fold cannot bind"))) => #t))
        (check (source '() forms '()) => first)
        (for-each (lambda (forbidden)
                    (check (not (string-contains first forbidden)) => #t))
                  '("event-fold-runtime" "fold-statements" "fold-predicate" "run-event-fold" "eval "
                    "current-fold-line-view" "fold-line-bytes" "(string->utf8 line)"))
        (check (not (not (string-contains first "(def (bind-native-product expected-digest)"))) => #t)
        (check (not (not (string-contains first "(reverse! (cons '(finish) events))"))) => #t)
        (let (units (event-fold-scheme-module-sources
                     'native-product event-lines-language-grammar 'Document
                     '() forms '() '() '() 1))
          (check units => (event-fold-scheme-module-sources
                          'native-product event-lines-language-grammar 'Document
                          '() forms '() '() '() 1))
          (for-each
           (lambda (unit)
             ;; An empty import compiles, but runtime expander evaluation rejects it.
             (check (not (string-contains (cdr unit) "(import)")) => #t)
             (let (definitions
                   (call-with-input-string (cdr unit)
                    (lambda (port)
                      (let loop ((count 0))
                        (let (form (read port))
                          (if (eof-object? form) count
                            (loop (+ count
                                     (if (and (pair? form) (eq? (car form) 'def)
                                              (pair? (cadr form))
                                              (string-prefix? "%event-fold-" (symbol->string (caadr form))))
                                       1 0)))))))))
               (check (<= definitions 1) => #t)))
           units))
        (let (names (list->vector
                     (map (lambda (index) (string (integer->char (+ 32 index)))) (iota 95))))
          (for-each
           (lambda (name)
             (let (bytes (string->utf8 (string-append "x" name "y")))
               (check (fold-line-name-set-contains? bytes 1 2 names) => #t)))
           (vector->list names))
          (check (fold-line-name-set-contains? (string->utf8 "") 0 0 names) => #f)
          (check (fold-line-name-set-contains? (string->utf8 "é") 0 2 names) => #f)
          (check (fold-line-name-set-contains? (string->utf8 "aa") 0 2 names) => #f))))
    (test-case "helpers are module-level source-parameterized native procedures"
      (let (generated
            (source '() '((call-source-helper slice start end)) '()
                    '((slice () ((token Line start end))))))
        (check (not (not (string-contains generated
                       "(def (%event-fold-native-product-helper-0 source-bytes"))) => #t)
        (check (not (string-contains generated "(utf8->string (subu8vector")) => #t)
        (check (not (string-contains generated "subu8vector")) => #t)
        (check (not (string-contains generated "utf8->string")) => #t)
        (check (not (not (string-contains generated "fold-source-frame-boundaries!"))) => #t)))
    (test-case "untyped slots unknown instructions and callbacks fail before generation"
      (check-exception (source '((flag #f)) '((set-uint flag (uint 1))) '()) true)
      (check-exception (source '() '((foreign-operation)) '()) true)
      (check-exception (source '() (list void) '()) true)
      (check-exception (source '() '((call-source-helper missing start end)) '()) true))
    (test-case "cyclic input and dormant helper recursion are rejected"
      (let (cycle (list '(token Line start end)))
        (set-cdr! cycle cycle)
        (check-exception (source '() cycle '()) true))
      (check-exception
       (source '() '() '() '((cycle () ((call-source-helper cycle start end)))))
       true))))
