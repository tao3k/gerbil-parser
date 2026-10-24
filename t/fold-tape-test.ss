(import :gerbil-parser/language-test-support)
(import :std/test ../src/runtime/fold-tape
        ./fold-tape-oracle ./event-strategy-fixture
        ../src/compiler/event-fold-scheme)

(deflanguage-parser-tests fold-tape-test "Direct Fold tape ownership"
  (engine)
    (scenario "wire spans growth and publication are exact"
      (sequential ((header (u8vector 79 69 86 49)) (owner (make-fold-tape header)))
        (act (fold-tape-start! owner 513))
        (each (index) ((iota 1000)) (act (fold-tape-token! owner 258 (+ 256 index) index)))
        (act (fold-tape-finish! owner))
        (scope ((published (fold-tape-publish! owner)))
          (equal (u8vector-length published)  19008)
          (equal (u8vector->list (subu8vector published 4 10))  '(1 1 2 2 2 1))
          (equal (u8vector-ref published 19007)  0)
          (raises (fold-tape-token! owner 1 0 1) true)
          (raises (fold-tape-publish! owner) true)
          (scope ((other (make-fold-tape header)))
            (act (fold-tape-finish! other))
            (equal (fold-tape-publish! other)  (u8vector 79 69 86 49 0)))
          (equal (u8vector-length published)  19008))))
    (scenario "numeric bounds reject truncation"
      (scope ((owner (make-fold-tape (u8vector))))
        (raises (fold-tape-start! owner 65536) true)
        (raises (fold-tape-token! owner 0 -1 0) true)
        (raises (fold-tape-token! owner 0 0 (expt 2 64)) true)))
    (property "direct product cannot materialize event lists"
      (bindings (source (event-fold-scheme-source 'direct-product event-lines-language-grammar
                       'Document '() '((start-node Text) (token Line start end) (finish-node))
                       '() '() '() 'tape)))
      (equal (not (not (string-contains source "fold-tape-token!")))  #t)
      (equal (not (string-contains source "(cons '(finish)"))  #t)
      (equal (not (string-contains source "(list 'token"))  #t)))
