;;; -*- Gerbil -*-
;;; A single stateful Scheme algorithm is executable and AOT-lowerable.

(import (only-in :std/test check check-exception test-case test-suite)
        (only-in :std/encoding/json JSONReadOptions string->json)
        (only-in "event-strategy-fixture.ss" event-lines-language-grammar)
        (only-in "event-fold-fixture.ss" parse-fold-lines parse_fold_lines
                 parse-outline-lines parse_outline_lines)
        (only-in :gerbil-parser/rust-rowan-event-support
                 event-fold-ir-json run-event-fold))
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
    (test-case "Scheme fold owns nested headline structure"
      (check (parse-outline-lines "* Parent\n** Child\nbody\n* Peer\n")
             => '((start Document)
                  (start Section) (start Heading) (token Line 0 9) (finish)
                  (start Section) (start Heading) (token Line 9 18) (finish)
                  (start Text) (token Line 18 23) (finish)
                  (finish) (finish)
                  (start Section) (start Heading) (token Line 23 30) (finish)
                  (finish) (finish)))
      (let (ir (string->json parse_outline_lines
                             (JSONReadOptions object-as-hash: #t
                                              array-as-vector: #t)))
        (check (hash-ref (vector-ref (hash-ref ir "initial") 0) "kind")
               => "let_usize_stack")))
    (test-case "ASCII-insensitive syntax prefix executes and lowers identically"
      (check (run-event-fold "#+BeGiN_SrC rust\n" 'Document '()
                             '((if (line-starts-with-ascii-ci "#+begin_src")
                                   ((start-node Text) (token Line start end)
                                    (finish-node))
                                   ())) '())
             => '((start Document) (start Text) (token Line 0 17)
                  (finish) (finish)))
      (let* ((wire (event-fold-ir-json
                    'ascii_prefix event-lines-language-grammar 'Document '()
                    '((if (line-starts-with-ascii-ci "#+begin_src")
                          ((start-node Text) (token Line start end)
                           (finish-node))
                          ())) '()))
             (ir (string->json wire
                               (JSONReadOptions object-as-hash: #t
                                                array-as-vector: #t))))
        (check (hash-ref (hash-ref (vector-ref (hash-ref ir "line") 0)
                                   "condition") "kind")
               => "line_starts_with_ascii_case_insensitive")))
    (test-case "blank source lines use one typed predicate in both paths"
      (check (run-event-fold " \t\r\nα\n" 'Document '()
                             '((if (line-blank?)
                                   ((start-node Heading) (token Line start end)
                                    (finish-node))
                                   ((start-node Text) (token Line start end)
                                    (finish-node)))) '())
             => '((start Document)
                  (start Heading) (token Line 0 4) (finish)
                  (start Text) (token Line 4 7) (finish)
                  (finish)))
      (let* ((wire (event-fold-ir-json
                    'blank_line event-lines-language-grammar 'Document '()
                    '((if (line-blank?) () ())) '()))
             (ir (string->json wire
                               (JSONReadOptions object-as-hash: #t
                                                array-as-vector: #t))))
        (check (hash-ref (hash-ref (vector-ref (hash-ref ir "line") 0)
                                   "condition") "kind")
               => "line_blank")))
    (test-case "source-backed prefix, word and trivia offsets execute in Scheme"
      (let* ((prefix '(line-prefix-end "#+begin_src"))
             (word-start (list 'line-skip-horizontal prefix))
             (word-end (list 'line-scan-word word-start))
             (forms `((if (line-has-word-after-prefix? "#+begin_src")
                          ((start-node Text)
                           (token Line start ,prefix)
                           (token Line ,prefix ,word-start)
                           (token Line ,word-start ,word-end)
                           (token Line ,word-end end)
                           (finish-node))
                          ((start-node Heading)
                           (token Line start end) (finish-node))))))
        (check (run-event-fold "#+BeGiN_SrC rust :x\n" 'Document '()
                               forms '())
               => '((start Document) (start Text)
                    (token Line 0 11) (token Line 11 12)
                    (token Line 12 16) (token Line 16 20)
                    (finish) (finish)))
        (check (run-event-fold "#+begin_src \n" 'Document '()
                               forms '())
               => '((start Document) (start Heading)
                    (token Line 0 13) (finish) (finish)))
        (let* ((wire (event-fold-ir-json
                      'header_word event-lines-language-grammar
                      'Document '() forms '()))
               (ir (string->json wire
                                 (JSONReadOptions object-as-hash: #t
                                                  array-as-vector: #t)))
               (conditional (vector-ref (hash-ref ir "line") 0))
               (consequent (hash-ref conditional "consequent")))
          (check (hash-ref (hash-ref (vector-ref (hash-ref ir "line") 0)
                                     "condition") "kind")
                 => "line_has_word_after_prefix")
          (check (hash-ref (hash-ref (vector-ref consequent 2) "end")
                           "kind")
                 => "line_skip_horizontal")
          (check (hash-ref (hash-ref (vector-ref consequent 3) "end")
                           "kind")
                 => "line_scan_word"))))
    (test-case "dynamic key offsets keep Org-style key, value and trivia source-backed"
      (let* ((prefix '(line-prefix-end "#+"))
             (key-end (list 'line-scan-key prefix))
             (value-start (list 'line-skip-horizontal
                                (list 'line-step key-end)))
             (forms `((if (line-has-key-after-prefix? "#+")
                          ((start-node Text)
                           (token Line start ,prefix)
                           (token Line ,prefix ,key-end)
                           (token Line ,key-end ,value-start)
                           (token Line ,value-start (line-trim-end))
                           (token Line (line-trim-end) end)
                           (finish-node))
                          ((start-node Heading)
                           (token Line start end) (finish-node))))))
        (check (run-event-fold "#+SEQ_TODO: TODO | DONE \r\n" 'Document '()
                               forms '())
               => '((start Document) (start Text)
                    (token Line 0 2) (token Line 2 10)
                    (token Line 10 12) (token Line 12 23)
                    (token Line 23 26) (finish) (finish)))
        (check (run-event-fold "#+@bad: x\n" 'Document '() forms '())
               => '((start Document) (start Heading)
                    (token Line 0 10) (finish) (finish)))
        (let* ((wire (event-fold-ir-json
                      'dynamic_key event-lines-language-grammar
                      'Document '() forms '()))
               (ir (string->json wire
                                 (JSONReadOptions object-as-hash: #t
                                                  array-as-vector: #t)))
               (conditional (vector-ref (hash-ref ir "line") 0))
               (consequent (hash-ref conditional "consequent")))
          (check (hash-ref (hash-ref conditional "condition") "kind")
                 => "line_has_key_after_prefix")
          (check (hash-ref (hash-ref (vector-ref consequent 2) "end") "kind")
                 => "line_scan_key")
          (check (hash-ref (hash-ref (vector-ref consequent 4) "end") "kind")
                 => "line_trim_end"))))
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
