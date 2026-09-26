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
    (test-case "checked source slices compare names across physical lines"
      (let ((initial '((name-start 0) (name-end 0)))
            (forms
             '((if (line-starts-with "name:")
                   ((set-uint name-start (offset (line-prefix-end "name:")))
                    (set-uint name-end (offset (line-content-end)))
                    (token Line start end))
                   ((if (source-slices-equal-ascii-ci?
                         (state-offset name-start) (state-offset name-end)
                         start (line-content-end))
                        ((start-node Heading) (token Line start end)
                         (finish-node))
                        ((start-node Text) (token Line start end)
                         (finish-node))))))))
        (check (run-event-fold "name:ALPHA\nalpha\nother\n"
                               'Document initial forms '())
               => '((start Document) (token Line 0 11)
                    (start Heading) (token Line 11 17) (finish)
                    (start Text) (token Line 17 23) (finish)
                    (finish)))
        (let* ((wire (event-fold-ir-json
                      'source_slices event-lines-language-grammar
                      'Document initial forms '()))
               (ir (string->json wire
                                 (JSONReadOptions object-as-hash: #t
                                                  array-as-vector: #t))))
          (check (hash-ref ir "name") => "source_slices"))))
    (test-case "named future marker respects names headings and parent closes"
      (let* ((name-start '(line-prefix-end "#+BEGIN_"))
             (name-end `(line-scan-key ,name-start))
             (future `(future-named-line-marker-before-boundary?
                       ,name-start ,name-end "#+END_" "" "#+END_CENTER"
                       "*" " " #t #t #t))
             (forms
              `((if (line-starts-with-ascii-ci "#+BEGIN_")
                    ((if ,future
                         ((start-node Heading) (token Line start end)
                          (finish-node))
                         ((start-node Text) (token Line start end)
                          (finish-node))))
                    ((token Line start end))))))
        (check (map cadr
                    (filter (lambda (event) (eq? (car event) 'start))
                            (run-event-fold
                             "#+BEGIN_foo\n#+end_FOO\n#+BEGIN_bar\n* Next\n#+END_bar\n#+BEGIN_baz\n#+END_CENTER\n#+END_baz\n#+BEGIN_qux\n#+END_other\n#+END_QUX\n"
                             'Document '() forms '())))
               => '(Document Heading Text Text Heading))
        (let* ((wire (event-fold-ir-json
                      'future_named event-lines-language-grammar
                      'Document '() forms '()))
               (ir (string->json wire
                                 (JSONReadOptions object-as-hash: #t
                                                  array-as-vector: #t)))
               (outer (vector-ref (hash-ref ir "line") 0))
               (inner (vector-ref (hash-ref outer "consequent") 0)))
          (check (hash-ref (hash-ref inner "condition") "kind")
                 => "future_named_line_marker_before_boundary"))))
    (test-case "named future marker stops at a source-named parent closer"
      (let* ((name-start '(line-prefix-end "#+BEGIN_"))
             (name-end `(line-scan-key ,name-start))
             (future
              `(future-named-line-marker-before-boundary?
                ,name-start ,name-end "#+END_" "" ""
                "*" " " #t #t #t
                (state-offset parent-start) (state-offset parent-end)
                "#+END_" "" #t))
             (initial '((parent-start 0) (parent-end 0)))
             (forms
              `((if (line-starts-with "#+BEGIN_OUTER")
                    ((set-uint parent-start (offset ,name-start))
                     (set-uint parent-end (offset ,name-end))
                     (token Line start end))
                    ((if (line-starts-with "#+BEGIN_INNER")
                         ((if ,future
                              ((start-node Heading) (token Line start end)
                               (finish-node))
                              ((start-node Text) (token Line start end)
                               (finish-node))))
                         ((token Line start end))))))))
        (check (map cadr
                    (filter (lambda (event) (eq? (car event) 'start))
                            (run-event-fold
                             "#+BEGIN_OUTER\n#+BEGIN_INNER\n#+END_outer\n#+END_inner\n"
                             'Document initial forms '())))
               => '(Document Text))
        (check (map cadr
                    (filter (lambda (event) (eq? (car event) 'start))
                            (run-event-fold
                             "#+BEGIN_OUTER\n#+BEGIN_INNER\n#+END_inner\n#+END_outer\n"
                             'Document initial forms '())))
               => '(Document Heading))
        (let* ((wire (event-fold-ir-json
                      'future_named_parent event-lines-language-grammar
                      'Document initial forms '()))
               (ir (string->json wire
                                 (JSONReadOptions object-as-hash: #t
                                                  array-as-vector: #t)))
               (outer (vector-ref (hash-ref ir "line") 0))
               (inner (vector-ref (hash-ref outer "alternate") 0))
               (future-ir (hash-ref (vector-ref (hash-ref inner "consequent") 0)
                                    "condition")))
          (check (hash-ref future-ir "stop_prefix") => "#+END_")
          (check (hash-ref future-ir "stop_ascii_case_insensitive") => #t))))
    (test-case "state-only frame pop does not close Rowan nodes"
      (let ((initial '((saved (uint-stack))))
            (forms '((push-frame saved (uint 7))
                     (pop-frame saved)
                     (token Line start end))))
        (check (run-event-fold "a\n" 'Document initial forms '())
               => '((start Document) (token Line 0 2) (finish)))
        (let* ((wire (event-fold-ir-json
                      'state_pop event-lines-language-grammar
                      'Document initial forms '()))
               (ir (string->json wire
                                 (JSONReadOptions object-as-hash: #t
                                                  array-as-vector: #t))))
          (check (hash-ref (vector-ref (hash-ref ir "line") 1) "kind")
                 => "pop_frame"))))
    (test-case "single frame close preserves nested equal frame scopes"
      (let ((initial '((frames (uint-stack))))
            (forms '((start-node Heading)
                     (push-frame frames (uint 7))
                     (start-node Text)
                     (push-frame frames (uint 7))
                     (close-frame frames 1)
                     (close-frame frames 1))))
        (check (run-event-fold "a\n" 'Document initial forms '())
               => '((start Document) (start Heading) (start Text)
                    (finish) (finish) (finish)))))
    (test-case "byte-set membership is a boolean state value"
      (let (forms '((set-bool match
                               (line-bytes-any-in? start (line-step start)
                                                   (126)))
                    (if (state match)
                        ((start-node Heading) (token Line start end)
                         (finish-node))
                        ((start-node Text) (token Line start end)
                         (finish-node)))))
        (check (run-event-fold "~x\nx\n" 'Document '((match #f)) forms '())
               => '((start Document)
                    (start Heading) (token Line 0 3) (finish)
                    (start Text) (token Line 3 5) (finish)
                    (finish)))
        (check (string? (event-fold-ir-json
                         'byte_set_bool event-lines-language-grammar
                         'Document '((match #f)) forms '()))
               => #t)))
    (test-case "bounded static name set executes and lowers as one predicate"
      (let* ((forms '((if (line-bytes-in-set? start (line-content-end)
                                             ("alpha" "beta"))
                          ((start-node Heading) (token Line start end)
                           (finish-node))
                          ((start-node Text) (token Line start end)
                           (finish-node)))))
             (wire (event-fold-ir-json
                    'static_name_set event-lines-language-grammar
                    'Document '() forms '()))
             (ir (string->json wire
                               (JSONReadOptions object-as-hash: #t
                                                array-as-vector: #t))))
        (check (run-event-fold "alpha\nbeta\nother\n" 'Document '() forms '())
               => '((start Document)
                    (start Heading) (token Line 0 6) (finish)
                    (start Heading) (token Line 6 11) (finish)
                    (start Text) (token Line 11 17) (finish)
                    (finish)))
        (check (hash-ref (hash-ref (vector-ref (hash-ref ir "line") 0)
                                   "condition") "kind")
               => "line_bytes_in_set")))
    (test-case "saved source bounds scan one UTF-8 span across physical lines"
      (let* ((initial '((span-start 0) (span-end 0)))
             (line '((set-uint span-end (offset end))))
             (finish
              '((with-source-bounds (state-offset span-start)
                                    (state-offset span-end)
                  ((if (line-bytes-any-in? start end (10))
                       ((start-node Text) (token Line start end)
                        (finish-node)) ())))))
             (wire (event-fold-ir-json
                    'source_span event-lines-language-grammar
                    'Document initial line finish))
             (ir (string->json wire
                               (JSONReadOptions object-as-hash: #t
                                                array-as-vector: #t))))
        (check (run-event-fold "ab\ncd\n" 'Document initial line finish)
               => '((start Document) (start Text) (token Line 0 6)
                    (finish) (finish)))
        (check (hash-ref (vector-ref (hash-ref ir "finish") 0) "kind")
               => "with_source_bounds")))
    (test-case "source-local helper executes once and stays typed in event IR"
      (let* ((initial '((span-start 0) (span-end 0)))
             (line '((set-uint span-end (offset end))))
             (finish '((call-source-helper inline-span
                                           (state-offset span-start)
                                           (state-offset span-end))))
             (helpers
              '((inline-span
                 ((cursor 0))
                 ((set-uint cursor (offset start))
                  (if (line-bytes-any-in? start end (10))
                      ((start-node Text)
                       (token Line (state-offset cursor) end)
                       (finish-node)) ())))))
             (wire (event-fold-ir-json
                    'source_helper event-lines-language-grammar
                    'Document initial line finish helpers))
             (ir (string->json wire
                               (JSONReadOptions object-as-hash: #t
                                                array-as-vector: #t))))
        (check (run-event-fold "ab\ncd\n" 'Document initial line finish helpers)
               => '((start Document) (start Text) (token Line 0 6)
                    (finish) (finish)))
        (check (hash-ref (vector-ref (hash-ref ir "finish") 0) "kind")
               => "call_source_helper")
        (check (vector-length (hash-ref ir "helpers")) => 1)))
    (test-case "future marker search stops at heading or parent boundary"
      (let* ((condition '(and (line-starts-with-ascii-ci "#+BEGIN_QUOTE")
                              (future-line-marker-before-boundary?
                               "#+END_QUOTE" "#+END_CENTER" "*" " " #t #t "")))
             (forms `((if ,condition
                          ((start-node Heading) (finish-node))
                          ((start-node Text) (finish-node)))))
             (wire (event-fold-ir-json
                    'future_marker event-lines-language-grammar
                    'Document '() forms '()))
             (ir (string->json wire
                               (JSONReadOptions object-as-hash: #t
                                                array-as-vector: #t))))
        (check (run-event-fold "#+BEGIN_QUOTE\n  #+end_quote\n"
                               'Document '() forms '())
               => '((start Document) (start Heading) (finish)
                    (start Text) (finish) (finish)))
        (check (run-event-fold "#+BEGIN_QUOTE\n** Next\n#+END_QUOTE\n"
                               'Document '() forms '())
               => '((start Document) (start Text) (finish)
                    (start Text) (finish) (start Text) (finish)
                    (finish)))
        (check (run-event-fold "#+BEGIN_QUOTE\n#+END_CENTER\n#+END_QUOTE\n"
                               'Document '() forms '())
               => '((start Document) (start Text) (finish)
                    (start Text) (finish) (start Text) (finish)
                    (finish)))
        (check (hash-ref (hash-ref (hash-ref
                                   (vector-ref (hash-ref ir "line") 0)
                                   "condition") "left") "kind")
               => "future_line_marker_before_boundary")))
    (test-case "future marker can require a key-value body"
      (let* ((condition '(future-line-marker-before-boundary?
                          ":END:" "" "*" " " #t #t ":"))
             (forms `((if ,condition
                          ((start-node Heading) (finish-node))
                          ((start-node Text) (finish-node)))))
             (wire (event-fold-ir-json
                    'future_key_body event-lines-language-grammar
                    'Document '() forms '()))
             (ir (string->json wire
                               (JSONReadOptions object-as-hash: #t
                                                array-as-vector: #t))))
        (check (run-event-fold ":PROPERTIES:\n:ID: one\n:END:\n"
                               'Document '() forms '())
               => '((start Document) (start Heading) (finish)
                    (start Heading) (finish) (start Text) (finish)
                    (finish)))
        (check (run-event-fold ":PROPERTIES:\nmalformed\n:END:\n"
                               'Document '() forms '())
               => '((start Document) (start Text) (finish)
                    (start Heading) (finish) (start Text) (finish)
                    (finish)))
        (check (hash-ref (hash-ref (vector-ref (hash-ref ir "line") 0)
                                   "condition") "body_key_marker")
               => 58)))
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
    (test-case "bounded delimiter scan may include source whitespace"
      (let* ((tag-end '(line-scan-until start ":"))
             (forms `((start-node Text)
                      (token Line start ,tag-end)
                      (token Line ,tag-end end)
                      (finish-node)))
             (wire (event-fold-ir-json
                    'tag_scan event-lines-language-grammar
                    'Document '() forms '()))
             (ir (string->json wire
                               (JSONReadOptions object-as-hash: #t
                                                array-as-vector: #t))))
        (check (run-event-fold "term words :: body\n" 'Document '() forms '())
               => '((start Document) (start Text)
                    (token Line 0 11) (token Line 11 19)
                    (finish) (finish)))
        (check (hash-ref
                (hash-ref (vector-ref (hash-ref ir "line") 1) "end")
                "kind") => "line_scan_until")))
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
    (test-case "final transition may flush a saved source-backed token"
      (let* ((initial '((pending #f) (pending-start 0) (pending-end 0)))
             (line '((if (line-blank?)
                         ((set-bool pending (bool #t))
                          (set-uint pending-start (offset start))
                          (set-uint pending-end (offset end))) ())))
             (finish '((if (state pending)
                           ((token Line (state-offset pending-start)
                                   (state-offset pending-end))) ())))
             (wire (event-fold-ir-json
                    'flush_pending event-lines-language-grammar
                    'Document initial line finish))
             (ir (string->json wire
                               (JSONReadOptions object-as-hash: #t
                                                array-as-vector: #t)))
             (flush (vector-ref
                     (hash-ref (vector-ref (hash-ref ir "finish") 0)
                               "consequent") 0)))
        (check (run-event-fold "\n" 'Document initial line finish)
               => '((start Document) (token Line 0 1) (finish)))
        (check (hash-ref (hash-ref flush "start") "kind")
               => "state_offset")
        (check-exception
         (event-fold-ir-json 'invalid event-lines-language-grammar
                             'Document initial '()
                             '((token Line start end))) true)
        (check-exception
         (event-fold-ir-json 'invalid event-lines-language-grammar
                             'Document initial '()
                             '((token Line
                                      (line-step (state-offset pending-start))
                                      (state-offset pending-end)))) true)))
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
