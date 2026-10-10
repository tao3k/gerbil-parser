;;; -*- Gerbil -*-
;;; The explicit native catalog admits the compiler without per-case processes.
(import (only-in :std/test check check-exception test-case test-suite)
        (only-in ./event-strategy-fixture event-lines-language-grammar)
        (only-in ../src/compiler/event-fold-scheme event-fold-scheme-source)
        (only-in ../src/compiler/event-fold-scheme-modules event-fold-scheme-module-sources)
        (only-in ../src/runtime/event-fold-lines fold-line-name-set-contains?))
(export event-fold-scheme-test)

(def (source initial line finish (helpers '()))
  (event-fold-scheme-source 'native-product event-lines-language-grammar 'Document
                            initial line finish helpers))

(def event-fold-scheme-test
  (test-suite "Native EventFold source admission"
    (test-case "generated source is deterministic and has no interpreted request path"
      (let* ((forms '((if (line-bytes-all-in? start end (10 97))
                         ((token Line start end)) ())))
             (first (source '() forms '())))
        (check (equal? first (source '() forms '())) => #t)
        (for-each (lambda (forbidden)
                    (check (not (string-contains first forbidden)) => #t))
                  '("event-fold-runtime" "fold-statements" "fold-predicate" "run-event-fold" "eval "))
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
                       "(def (%event-fold-native-product-helper-0 source-bytes"))) => #t)))
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
