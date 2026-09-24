;;; -*- Gerbil -*-
;;; A single stateful Scheme algorithm is executable and AOT-lowerable.

(import (only-in :std/test check check-exception test-case test-suite)
        (only-in :std/encoding/json JSONReadOptions string->json)
        (only-in "event-strategy-fixture.ss" event-lines-language-grammar)
        (only-in "event-fold-fixture.ss" parse-fold-lines parse_fold_lines)
        (only-in :gerbil-parser/src/compiler/event-fold-aot
                 event-fold-ir-json))
(export event-fold-test)

(def event-fold-test
  (test-suite "Scheme stateful event fold AOT"
    (test-case "the Scheme algorithm owns multi-line node lifetimes"
      (check (parse-fold-lines "a\nb\n* α\r\nc")
             => '((start Document)
                  (start Text) (token Line 0 2) (token Line 2 4) (finish)
                  (start Heading) (token Line 4 10) (finish)
                  (start Text) (token Line 10 11) (finish)
                  (finish)))
      (check (parse-fold-lines "")
             => '((start Document) (finish))))
    (test-case "AOT carries typed state and the source-owned digest"
      (let (ir (string->json parse_fold_lines
                             (JSONReadOptions object-as-hash: #t
                                              array-as-vector: #t)))
        (check (hash-ref ir "schema")
               => "gerbil-scheme-rust.event-function-ir.v1")
        (check (hash-ref ir "name") => "parse_fold_lines")
        (check (string-prefix? "sha256:" (hash-ref ir "parser_digest"))
               => #t)
        (check (vector-length (hash-ref ir "line")) => 1)))
    (test-case "undeclared state and unsupported effects fail closed"
      (check-exception
       (event-fold-ir-json 'invalid event-lines-language-grammar 'Document
                           '() '((set-bool missing (bool #t))) '())
       true)
      (check-exception
       (event-fold-ir-json 'invalid event-lines-language-grammar 'Document
                           '() '((display "not an event")) '())
       true)
      (check-exception
       (event-fold-ir-json 'invalid event-lines-language-grammar 'Document
                           '((a-b #f) (a_b #t)) '() '())
       true)
      (check-exception
       (event-fold-ir-json 'invalid event-lines-language-grammar 'Document
                           '() '()
                           '((if (line-starts-with "*") () ())))
       true))))
