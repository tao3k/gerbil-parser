(import :std/test ../src/runtime/fold-tape
        ./fold-tape-oracle ./event-strategy-fixture
        ../src/compiler/event-fold-scheme)
(export fold-tape-test)
(def fold-tape-test
  (test-suite "Direct Fold tape ownership"
    (test-case "wire spans growth and publication are exact"
      (let* ((header (u8vector 79 69 86 49)) (owner (make-fold-tape header)))
        (fold-tape-start! owner 513)
        (for-each (lambda (index) (fold-tape-token! owner 258 (+ 256 index) index)) (iota 1000))
        (fold-tape-finish! owner)
        (let ((published (fold-tape-publish! owner)))
          (check (u8vector-length published) => 19008)
          (check (u8vector->list (subu8vector published 4 10)) => '(1 1 2 2 2 1))
          (check (u8vector-ref published 19007) => 0)
          (check-exception (fold-tape-token! owner 1 0 1) true)
          (check-exception (fold-tape-publish! owner) true)
          (let ((other (make-fold-tape header)))
            (fold-tape-finish! other)
            (check (fold-tape-publish! other) => (u8vector 79 69 86 49 0)))
          (check (u8vector-length published) => 19008))))
    (test-case "numeric bounds reject truncation"
      (let ((owner (make-fold-tape (u8vector))))
        (check-exception (fold-tape-start! owner 65536) true)
        (check-exception (fold-tape-token! owner 0 -1 0) true)
        (check-exception (fold-tape-token! owner 0 0 (expt 2 64)) true)))
    (test-case "direct product cannot materialize event lists"
      (let ((source (event-fold-scheme-source 'direct-product event-lines-language-grammar
                       'Document '() '((start-node Text) (token Line start end) (finish-node))
                       '() '() '() 'tape)))
        (check (not (not (string-contains source "fold-tape-token!"))) => #t)
        (check (not (string-contains source "(cons '(finish)")) => #t)
        (check (not (string-contains source "(list 'token")) => #t)))))
