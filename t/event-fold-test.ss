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
    (test-case "n-ary boolean predicates evaluate every operand and lower to IR"
      (let* ((forms '((if (and (bool #t) (bool #t) (bool #f))
                          ((start-node Heading) (finish-node))
                          ((start-node Text) (finish-node)))
                     (if (or (bool #f) (bool #f) (bool #t))
                         ((start-node Heading) (finish-node)) ())))
             (wire (event-fold-ir-json
                    'nary_boolean event-lines-language-grammar
                    'Document '() forms '()))
             (ir (string->json wire
                               (JSONReadOptions object-as-hash: #t
                                                array-as-vector: #t))))
        (check (run-event-fold "x" 'Document '() forms '())
               => '((start Document) (start Text) (finish)
                    (start Heading) (finish) (finish)))
        (check (hash-ref (hash-ref (vector-ref (hash-ref ir "line") 0)
                                   "condition") "kind") => "and")
        (check (hash-ref (hash-ref (vector-ref (hash-ref ir "line") 1)
                                   "condition") "kind") => "or")))
    (test-case "bounded delimiter scan accepts non-whitespace source keys"
      (let* ((key-end '(line-scan-nonspace-until start ":"))
             (forms `((start-node Text)
                      (token Line start ,key-end)
                      (token Line ,key-end end)
                      (finish-node)))
             (wire (event-fold-ir-json
                    'delimiter_scan event-lines-language-grammar
                    'Document '() forms '()))
             (ir (string->json wire
                               (JSONReadOptions object-as-hash: #t
                                                array-as-vector: #t))))
        (check (run-event-fold "A+B: yes\n" 'Document '() forms '())
               => '((start Document) (start Text)
                    (token Line 0 3) (token Line 3 9)
                    (finish) (finish)))
        (check (hash-ref
                (hash-ref (vector-ref (hash-ref ir "line") 1) "end")
                "kind") => "line_scan_nonspace_until")))
    (test-case "ASCII line markers reject prefix collisions in Scheme and IR"
      (let* ((forms '((if (line-prefix-boundary-ascii-ci "#+begin_src")
                         ((start-node Text) (token Line start end)
                          (finish-node))
                         ((start-node Heading) (token Line start end)
                          (finish-node)))))
             (wire (event-fold-ir-json
                    'bounded_prefix event-lines-language-grammar
                    'Document '() forms '()))
             (ir (string->json wire
                               (JSONReadOptions object-as-hash: #t
                                                array-as-vector: #t))))
        (check (run-event-fold "#+begin_srcx\n" 'Document '() forms '())
               => '((start Document) (start Heading)
                    (token Line 0 13) (finish) (finish)))
        (check (hash-ref (hash-ref (vector-ref (hash-ref ir "line") 0)
                                   "condition") "kind")
               => "line_prefix_boundary_ascii_case_insensitive"))
      (check (run-event-fold ":END: tail\n" 'Document '()
                             '((if (line-marker-ascii-ci ":END:")
                                   ((start-node Text) (finish-node))
                                   ((start-node Heading) (finish-node)))) '())
             => '((start Document) (start Heading) (finish) (finish))))
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
                           (token Line ,value-start
                                  (line-trim-end-from ,value-start))
                           (token Line (line-trim-end-from ,value-start) end)
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
        (check (run-event-fold "#+EMPTY:  \n" 'Document '() forms '())
               => '((start Document) (start Text)
                    (token Line 0 2) (token Line 2 7)
                    (token Line 7 10)
                    (token Line 10 11) (finish) (finish)))
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
                 => "line_trim_end_from"))))
    (test-case "bounded source-byte fold executes and lowers the same events"
      (let* ((index '(line-index cursor))
             (next (list 'line-step index))
             (forms `((set-uint last (offset start))
                      (for-line-bytes cursor start (line-content-end)
                        ((if (line-byte-equal? ,index 124)
                             ((token Line (state-offset last) ,index)
                              (token Line ,index ,next)
                              (set-uint last (offset ,next))) ()) ))
                      (token Line (state-offset last) end))))
        (check (run-event-fold "|x|\n" 'Document '((last 0)) forms '())
               => '((start Document) (token Line 0 1)
                    (token Line 1 2) (token Line 2 3)
                    (token Line 3 4) (finish)))
        (let* ((wire (event-fold-ir-json
                      'byte_fold event-lines-language-grammar
                      'Document '((last 0)) forms '()))
               (ir (string->json wire
                                 (JSONReadOptions object-as-hash: #t
                                                  array-as-vector: #t)))
               (loop (vector-ref (hash-ref ir "line") 1)))
          (check (hash-ref loop "kind") => "for_line_bytes")
          (check (hash-ref (hash-ref loop "until") "kind")
                 => "line_content_end"))))
    (test-case "declared list markers and frame closes execute as Scheme"
      (let* ((initial '((present #f) (column 0) (ordered #f)
                        (bullet-start 0) (bullet-end 0) (content-start 0)
                        (frames (uint-stack))))
             (forms '((scan-list-marker "-+*" #t 8 present column ordered
                                        bullet-start bullet-end content-start)
                      (if (state present)
                          ((close-frames-while frames
                             (uint-greater? (stack-top frames) (state column)) 1)
                           (start-node Text)
                           (push-frame frames (state column))
                           (token Line start (state-offset bullet-start))
                           (token Line (state-offset bullet-start)
                                  (state-offset bullet-end))
                           (token Line (state-offset bullet-end) end))
                          ())))
             (finish '((close-all-frames frames 1)))
             (wire (event-fold-ir-json
                    'list_fold event-lines-language-grammar
                    'Document initial forms finish))
             (ir (string->json wire
                               (JSONReadOptions object-as-hash: #t
                                                array-as-vector: #t))))
        (check (run-event-fold "- a\n  12) b\n- c\n" 'Document
                               initial forms finish)
               => '((start Document)
                    (start Text) (token Line 0 1) (token Line 1 4)
                    (start Text) (token Line 4 6) (token Line 6 9)
                    (token Line 9 12)
                    (finish) (start Text)
                    (token Line 12 13) (token Line 13 16)
                    (finish) (finish) (finish)))
        (check (hash-ref (vector-ref (hash-ref ir "line") 0) "kind")
               => "scan_list_marker")))
    (test-case "headline marker offsets use the declared level in Scheme and IR"
      (let* ((marker '(line-marker-end "*" " "))
             (forms `((if (uint-positive? (line-marker-level "*" " "))
                          ((start-node Heading)
                           (token Line start ,marker)
                           (token Line ,marker end)
                           (finish-node)) ())))
             (wire (event-fold-ir-json
                    'headline_fields event-lines-language-grammar
                    'Document '() forms '()))
             (ir (string->json wire
                               (JSONReadOptions object-as-hash: #t
                                                array-as-vector: #t)))
             (consequent (hash-ref (vector-ref (hash-ref ir "line") 0)
                                   "consequent")))
        (check (run-event-fold "** Work\n" 'Document '() forms '())
               => '((start Document) (start Heading)
                    (token Line 0 2) (token Line 2 8)
                    (finish) (finish)))
        (check (hash-ref (hash-ref (vector-ref consequent 1) "end") "kind")
               => "line_marker_end")))
    (test-case "typed unsigned block state selects a bounded transition"
      (let* ((initial '((active-block 2)))
             (forms '((if (uint-equal? (state active-block) (uint 2))
                          ((start-node Text) (token Line start end)
                           (finish-node))
                          ((start-node Heading) (token Line start end)
                           (finish-node)))))
             (wire (event-fold-ir-json
                    'block_state event-lines-language-grammar
                    'Document initial forms '()))
             (ir (string->json wire
                               (JSONReadOptions object-as-hash: #t
                                                array-as-vector: #t))))
        (check (run-event-fold "body\n" 'Document initial forms '())
               => '((start Document) (start Text) (token Line 0 5)
                    (finish) (finish)))
        (check (hash-ref (hash-ref (vector-ref (hash-ref ir "line") 0)
                                   "condition") "kind")
               => "usize_equal")))
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
