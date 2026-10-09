;;; -*- Gerbil -*-
;;; A single stateful Scheme algorithm is executable and AOT-lowerable.

(import (only-in :std/test check check-exception test-case test-suite)
        (only-in :std/encoding/json JSONReadOptions string->json)
        (only-in "event-strategy-fixture.ss" event-lines-language-grammar)
        (only-in "event-fold-fixture.ss" parse-fold-lines parse_fold_lines
                 parse-outline-lines parse_outline_lines)
        (only-in :gerbil-parser/rust-runtime-event-support
                 event-fold-ir-json run-event-fold)
        (only-in :gerbil-parser/src/compiler/event-fold-runtime
                 prepare-event-fold-program run-event-fold-program)
        (only-in :gerbil-parser/src/compiler/event-strategy-aot
                 fold-source-lines source-line-events)
        (only-in :gerbil-parser/src/compiler/event-fold-state-frame
                 prepare-fold-state-layout instantiate-fold-state-frame
                 fold-frame-ref fold-frame-update! fold-frame-bound?
                 fold-frame-bind! fold-frame-unbind!)
        (only-in :gerbil-parser/src/modules/parser/interface
                 source-offset-after source-pattern-end source-pattern-at?
                 source-ascii-ci-pattern-at?))
(export event-fold-test)

(def event-fold-test
  (test-suite "Scheme stateful event fold AOT"
    (test-case "native source fold retains line endings byte bounds and initial state"
      (for-each
       (lambda (fixture)
         (check
          (reverse
           (fold-source-lines (car fixture) '(seed)
             (lambda (line start end reversed)
               (cons (list line start end) reversed))))
          => (cadr fixture)))
       '(("" (seed))
         ("tail" (seed ("tail" 0 4)))
         ("\n" (seed ("\n" 0 1)))
         ("é\r\nλ\rtail" (seed ("é\r\n" 0 4) ("λ\r" 4 7) ("tail" 7 11)))
         ("a\n\n" (seed ("a\n" 0 2) ("\n" 2 3)))))
      (check (source-line-events "é\r\nλ" 'Document
               (lambda (_ start end) (list (list 'token 'Line start end))))
             => '((start Document) (token Line 0 4) (token Line 4 6) (finish))))
    (test-case "native fold chains multiple line emissions before final transitions"
      (let ((forms '((start-node Text) (token Line start end) (finish-node)))
            (finish '((start-node Ending) (finish-node))))
        (check (run-event-fold "é\r\nλ" 'Document '() forms finish)
               => '((start Document)
                    (start Text) (token Line 0 4) (finish)
                    (start Text) (token Line 4 6) (finish)
                    (start Ending) (finish) (finish)))
        (check (run-event-fold "" 'Document '() forms finish)
               => '((start Document) (start Ending) (finish) (finish)))))
    (test-case "prepared state layout isolates values and temporary bindings"
      (let* ((layout (prepare-fold-state-layout '((flag . #f) (count . 0) (stack))))
             (first (instantiate-fold-state-frame layout))
             (second (instantiate-fold-state-frame layout)))
        (fold-frame-update! first 'flag #t)
        (fold-frame-update! first 'count 9)
        (fold-frame-update! first 'stack '(4 2))
        (fold-frame-bind! first 'temporary #f)
        (check (fold-frame-bound? first 'temporary) => #t)
        (check (fold-frame-bound? second 'temporary) => #f)
        (check (fold-frame-ref second 'flag) => #f)
        (check (fold-frame-ref second 'count) => 0)
        (check (fold-frame-ref second 'stack) => '())
        (fold-frame-unbind! first 'temporary)
        (check (fold-frame-bound? first 'temporary) => #f)
        (check-exception (fold-frame-ref first 'temporary) true)))
    (test-case "prepared frames retain first binding and missing update policy"
      (let* ((layout (prepare-fold-state-layout '((count . 0) (count . 4))))
             (frame (instantiate-fold-state-frame layout)))
        (check (fold-frame-ref frame 'count) => 0)
        (fold-frame-update! frame 'missing 10)
        (check (fold-frame-bound? frame 'missing) => #f)
        (check-exception (fold-frame-bind! frame 'count 2) true)
        (fold-frame-unbind! frame 'count)
        (check (fold-frame-bound? frame 'count) => #f)
        (fold-frame-update! frame 'count 7)
        (check (fold-frame-bound? frame 'count) => #f)
        (fold-frame-bind! frame 'count 3)
        (check (fold-frame-ref frame 'count) => 3)
        (check (fold-frame-ref (instantiate-fold-state-frame layout) 'count) => 0)))
    (test-case "reusable programs snapshot declarations and preserve UTF-8 events"
      (let* ((initial (list (list 'ready #f)))
             (forms (list '(set-bool ready (bool #t)) '(token Line start end)))
             (program (prepare-event-fold-program 'Document initial forms '())))
        (set-car! (cdr (car initial)) #t)
        (set-car! forms '(finish-node))
        (for-each
         (lambda (source)
           (check (run-event-fold-program program source)
                  => (run-event-fold source 'Document '((ready #f))
                                     '((set-bool ready (bool #t)) (token Line start end)) '())))
         '("éx\r\n" "λ\n" "" "tail"))))
    (test-case "reused helpers and parameter overrides never retain request state"
      (let (program
            (prepare-event-fold-program 'Document '((level 1))
              '((call-source-helper piece start end ((state level)))) '()
              '((piece ((level 0) (seen #f))
                       ((if (state seen) ((finish-node))
                            ((set-bool seen (bool #t))
                             (if (uint-equal? (state level) (uint 2))
                                 ((token Other start end)) ((token Line start end))))))
                       (level)))))
        (check (run-event-fold-program program "a\nb\n" '((level . 2)))
               => '((start Document) (token Other 0 2) (token Other 2 4) (finish)))
        (check (run-event-fold-program program "λ\n")
               => '((start Document) (token Line 0 3) (finish)))))
    (test-case "prepared programs reject cycles callbacks and mutable initial values"
      (let (cycle (list 'token))
        (set-cdr! cycle cycle)
        (check-exception (prepare-event-fold-program 'Document '() cycle '()) true))
      (check-exception (prepare-event-fold-program 'Document '() (list void) '()) true)
      (check-exception (prepare-fold-state-layout (list (cons 'state (vector 1)))) true)
      (check-exception (prepare-event-fold-program 'Document '((count -1)) '() '()) true))
    (test-case "engine source patterns advance by UTF-8 bytes"
      (let (end (source-pattern-end 'start "éx"))
        (check end => '(line-step (line-step (line-step start))))
        (check (run-event-fold "éx\n" 'Document '()
                 `((if ,(source-pattern-at? 'start "éx")
                       ((token Prefix start ,end) (token Ending ,end end))
                       ((token Other start end)))) '())
               => '((start Document) (token Prefix 0 3)
                    (token Ending 3 4) (finish)))))
    (test-case "engine ASCII case-insensitive single markers stay admissible"
      (let (predicate (source-ascii-ci-pattern-at? 'start "a"))
        (check predicate => '(line-bytes-any-in? start (line-step start) (65 97)))
        (check (run-event-fold "A\n" 'Document '()
                 `((if ,predicate ((token Line start end)) ())) '())
               => '((start Document) (token Line 0 2) (finish)))
        (check (string? (event-fold-ir-json
                         'single_ci event-lines-language-grammar 'Document '()
                         `((if ,predicate ((token Line start end)) ())) '()))
               => #t)))
    (test-case "engine ASCII patterns retain punctuation and matching boundaries"
      (let (forms `((if ,(source-ascii-ci-pattern-at? 'start "Ab-_")
                       ((token Match start end)) ((token Miss start end)))))
        (check (run-event-fold "aB-_\n" 'Document '() forms '())
               => '((start Document) (token Match 0 5) (finish)))
        (check (run-event-fold "ab-\n" 'Document '() forms '())
               => '((start Document) (token Miss 0 4) (finish)))
        (check (run-event-fold "ab+x\n" 'Document '() forms '())
               => '((start Document) (token Miss 0 5) (finish)))))
    (test-case "engine offset construction rejects invalid counts before iteration"
      (check (source-offset-after 'start 0) => 'start)
      (check (source-pattern-end 'start "") => 'start)
      (for-each (lambda (count)
                  (check-exception (source-offset-after 'start count) true))
                '(-1 1/2 1.0 #f))
      (check-exception (source-pattern-end 'start #f) true))
    (test-case "engine pattern admission distinguishes UTF-8 from ASCII folding"
      (for-each (lambda (pattern)
                  (check-exception (source-ascii-ci-pattern-at? 'start pattern) true))
                '("é" "Σ" "" #f))
      (check-exception (source-pattern-at? 'start "") true)
      (check-exception (source-pattern-at? 'start #f) true))
    (test-case "one event chain preserves nested helper byte-loop and join order"
      (check (run-event-fold
              "ab\n" 'Document '()
              '((start-node Text)
                (token Prefix start (line-prefix-end "a"))
                (for-line-bytes index start (line-content-end)
                  ((with-source-bounds (line-index index) end
                     ((call-source-helper piece start end)))))
                (join-once handled
                  ((if (bool #f) ((set-bool handled (bool #t))) ())
                   (token Ending start end))
                  ((token Space start end)))
                (finish-node)) '()
              '((piece () ((start-node Heading) (token Line start end)
                           (finish-node)))))
             => '((start Document) (start Text) (token Prefix 0 1)
                  (start Heading) (token Line 0 3) (finish)
                  (start Heading) (token Line 1 3) (finish)
                  (token Ending 0 3) (token Space 0 3) (finish) (finish))))
    (test-case "empty emissions and handled joins preserve an existing event chain"
      (check (run-event-fold
              "x\n" 'Document '()
              '((start-node Text) (token Before start end)
                (for-line-bytes index start start ((token Dropped start end)))
                (token Empty start start)
                (join-once handled
                  ((token Branch start end) (set-bool handled (bool #t)))
                  ((token Skipped start end)))
                (finish-node))
              '((start-node Heading) (finish-node)))
             => '((start Document) (start Text) (token Before 0 2)
                  (token Branch 0 2) (finish) (start Heading) (finish)
                  (finish))))
    (test-case "mutation-only byte loops retain state without emitting wrappers"
      (check (run-event-fold
              "aé\n" 'Document '((count 0))
              '((for-line-bytes index start (line-content-end)
                  ((set-uint count (uint-add (state count) (uint 1)))
                   (if (bool #f) ((token Skipped start end)) ())))
                (token Line start (state-offset count))
                (token Ending (state-offset count) end)) '())
             => '((start Document) (token Line 0 3) (token Ending 3 4)
                  (finish))))
    (test-case "join temporary bindings are removed before the next join"
      (check (run-event-fold
              "a\nb\n" 'Document '((seen #f))
              '((join-once handled
                  ((set-bool seen (bool #t)) (set-bool handled (bool #t)))
                  ((token Skipped start end)))
                (join-once handled
                  ((if (bool #f) ((set-bool handled (bool #t))) ()))
                  ((if (state seen) ((token Line start end)) ())))) '())
             => '((start Document) (token Line 0 2) (token Line 2 4)
                  (finish))))
    (test-case "repeated helper calls own independent execution slots"
      (check (run-event-fold
              "a\n" 'Document '((position 2))
              '((call-source-helper head start end)
                (call-source-helper head start end)
                (token Line start (state-offset position))) '()
              '((head ((position 0))
                      ((set-uint position (uint-add (state position) (uint 1)))
                       (token Prefix start (state-offset position))))))
             => '((start Document) (token Prefix 0 1) (token Prefix 0 1)
                  (token Line 0 2) (finish))))
    (test-case "native UTF-8 length keeps multibyte prefix source offsets"
      (check (run-event-fold
              "éx\n" 'Document '()
              '((token Prefix start (line-prefix-end "é"))
                (token Line (line-prefix-end "é") end)) '())
             => '((start Document) (token Prefix 0 2) (token Line 2 4)
                  (finish))))
    (test-case "prepared byte sets preserve empty ranges and UTF-8 membership"
      (check (run-event-fold
              "é\n" 'Document '()
              '((if (and (line-bytes-all-in? start (line-content-end) (195 169 195 255))
                         (line-bytes-any-in? start (line-content-end) (169))
                         (line-bytes-all-in? start start ())
                         (not (line-bytes-any-in? start start ()))
                         (not (line-bytes-any-in? start end ())))
                    ((token Line start end)) ())) '())
             => '((start Document) (token Line 0 3) (finish))))
    (test-case "prepared byte sets reject malformed values only when evaluated"
      (for-each
       (lambda (values)
         (check-exception
          (run-event-fold "a\n" 'Document '()
                          `((if (line-bytes-any-in? start end ,values) () ())) '())
          true))
       '((256) (-1) (1/2) (a) (1 . 2) "a"))
      (check (run-event-fold
              "a\n" 'Document '()
              '((if (or (bool #t) (line-bytes-any-in? start end (256)))
                    ((token Line start end)) ())) '())
             => '((start Document) (token Line 0 2) (finish))))
    (test-case "prepared byte sets do not leak across parse requests"
      (let* ((values (list 97))
             (forms `((if (line-bytes-any-in? start end ,values)
                          ((token Line start end)) ()))))
        (check (run-event-fold "a\n" 'Document '() forms '())
               => '((start Document) (token Line 0 2) (finish)))
        (set-car! values 98)
        (check (run-event-fold "a\n" 'Document '() forms '())
               => '((start Document) (finish)))))
    (test-case "helper parameter updates preserve duplicate-name semantics"
      (check (run-event-fold
              "a\n" 'Document '((caller 0))
              '((call-source-helper head start end ((uint 1))))
              '()
              '((head ((position 0) (position 2))
                      ((token Line start (state-offset position))
                       (token Line (state-offset position) end))
                      (position))))
             => '((start Document) (token Line 0 1) (token Line 1 2)
                  (finish))))
    (test-case "boolean predicate loops preserve short-circuit rejection boundaries"
      (check (run-event-fold
              "a\n" 'Document '()
              '((if (and (bool #f) (state undeclared)) () ())
                (if (or (bool #t) (state undeclared))
                    ((token Line start end)) ())) '())
             => '((start Document) (token Line 0 2) (finish))))
    (test-case "helper execution restores the independent caller frame"
      (check (run-event-fold
              "a\n" 'Document '((position 1))
              '((call-source-helper head start (state-offset position))
                (token Line (state-offset position) end))
              '()
              '((head ((position 0) (position 2))
                      ((set-uint position (uint 1))
                       (token Line start (state-offset position))))))
             => '((start Document) (token Line 0 1) (token Line 1 2)
                  (finish))))
    (test-case "execution slots preserve untouched and repeated bindings"
      (let ((initial '((left 0) (middle 0) (right 2) (enabled #f)))
            (forms
             '((set-uint middle (uint 1))
               (set-uint middle (uint 1))
               (set-bool enabled (bool #t))
               (if (and (uint-equal? (state left) (uint 0))
                        (uint-equal? (state right) (uint 2))
                        (state enabled))
                   ((token Line start (state-offset middle))
                    (token Line (state-offset middle) end)) ()))))
        (check (run-event-fold "a\n" 'Document initial forms '())
               => '((start Document) (token Line 0 1) (token Line 1 2)
                    (finish)))
        (check initial => '((left 0) (middle 0) (right 2) (enabled #f)))
        (check (run-event-fold "b\n" 'Document initial forms '())
               => '((start Document) (token Line 0 1) (token Line 1 2)
                    (finish)))))
    (test-case "duplicate runtime bindings retain the original all-update semantics"
      (check (run-event-fold
              "a\n" 'Document '((offset 0) (offset 2))
              '((set-uint offset (uint 1))
                (token Line start (state-offset offset))
                (token Line (state-offset offset) end)) '())
             => '((start Document) (token Line 0 1) (token Line 1 2)
                  (finish))))
    (test-case "nested UTF-8 helper frames restore their caller byte view"
      (check (run-event-fold
              "αb\nz\n" 'Document '()
              '((if (line-byte-equal? start 206)
                    ((call-source-helper head start (line-step (line-step start)))
                     (if (line-byte-equal? (line-step (line-step start)) 98)
                         ((token Line (line-step (line-step start)) end)) ()))
                    ((token Line start end))))
              '()
              '((head () ((if (line-byte-equal? start 206)
                              ((token Line start end)) ())))))
             => '((start Document) (token Line 0 2) (token Line 2 4)
                  (token Line 4 6) (finish))))
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
        (check (map cadr
                    (filter (lambda (event) (eq? (car event) 'start))
                            (run-event-fold
                             "#+BEGIN_foo\n#+BEGIN_foo\n#+END_foo\n"
                             'Document '() forms '())))
               => '(Document Heading Heading))
        (check (map cadr
                    (filter (lambda (event) (eq? (car event) 'start))
                            (run-event-fold
                             "#+BEGIN_foo\r\n  #+end_FOO \r\n"
                             'Document '() forms '())))
               => '(Document Heading))
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
                "#+CLOSE_" "" #t))
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
                             "#+BEGIN_OUTER\n#+BEGIN_INNER\n#+CLOSE_outer\n#+END_inner\n"
                             'Document initial forms '())))
               => '(Document Text))
        (check (map cadr
                    (filter (lambda (event) (eq? (car event) 'start))
                            (run-event-fold
                             "#+BEGIN_OUTER\n#+BEGIN_INNER\n#+END_inner\n#+CLOSE_outer\n"
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
          (check (hash-ref future-ir "stop_prefix") => "#+CLOSE_")
          (check (hash-ref future-ir "stop_ascii_case_insensitive") => #t))))
    (test-case "named future marker keeps parent case sensitivity independent"
      (let* ((name-start '(line-prefix-end "#+BEGIN_"))
             (name-end `(line-scan-key ,name-start))
             (future `(future-named-line-marker-before-boundary?
                       ,name-start ,name-end "#+END_" "" ""
                       "*" " " #t #t #t
                       (state-offset parent-start) (state-offset parent-end)
                       "#+END_" "" #f))
             (initial '((parent-start 0) (parent-end 0)))
             (forms
              `((if (line-starts-with "#+BEGIN_OUTER")
                    ((set-uint parent-start (offset ,name-start))
                     (set-uint parent-end (offset ,name-end))
                     (token Line start end))
                    ((if (line-starts-with "#+BEGIN_INNER")
                         ((if ,future
                              ((start-node Heading) (finish-node))
                              ((start-node Text) (finish-node))))
                         ((token Line start end))))))))
        (check (map cadr
                    (filter (lambda (event) (eq? (car event) 'start))
                            (run-event-fold
                             "#+BEGIN_OUTER\n#+BEGIN_INNER\n#+END_outer\n#+END_inner\n"
                             'Document initial forms '())))
               => '(Document Heading))
        (check (map cadr
                    (filter (lambda (event) (eq? (car event) 'start))
                            (run-event-fold
                             "#+BEGIN_OUTER\n#+BEGIN_INNER\n#+END_OUTER\n#+END_inner\n"
                             'Document initial forms '())))
               => '(Document Text))))
    (test-case "state-only frame pop does not close syntax nodes"
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
    (test-case "typed helper arguments flow through Scheme execution and IR"
      (let* ((initial '((threshold 2)))
             (line '((call-source-helper local-span start end
                                         ((state threshold)))))
             (helpers
              '((local-span ((threshold 2))
                            ((if (uint-equal? (state threshold) (uint 2))
                                 ((start-node Heading) (finish-node))
                                 ((start-node Text) (finish-node))))
                            (threshold))))
             (wire (event-fold-ir-json
                    'typed_helper event-lines-language-grammar
                    'Document initial line '() helpers))
             (ir (string->json wire
                               (JSONReadOptions object-as-hash: #t
                                                array-as-vector: #t))))
        (check (run-event-fold "x\n" 'Document initial line '() helpers)
               => '((start Document) (start Heading) (finish) (finish)))
        (check (run-event-fold "x\n" 'Document initial line '() helpers
                               '((threshold . 5)))
               => '((start Document) (start Text) (finish) (finish)))
        (check (vector-length (hash-ref
                               (vector-ref (hash-ref ir "line") 0)
                               "arguments"))
               => 1)
        (check (vector-ref
                (hash-ref (vector-ref (hash-ref ir "helpers") 0)
                          "parameters") 0)
               => "threshold")
        (check-exception
         (event-fold-ir-json
          'bad_helper_arity event-lines-language-grammar 'Document initial
          '((call-source-helper local-span start end)) '() helpers)
         true)))
    (test-case "source helpers compose without recursive execution"
      (let* ((line '((call-source-helper outer start end)))
             (helpers
              '((outer () ((call-source-helper inner start end)))
                (inner () ((start-node Text)
                           (token Line start end)
                           (finish-node)))))
             (wire (event-fold-ir-json
                    'nested_helpers event-lines-language-grammar
                    'Document '() line '() helpers))
             (ir (string->json wire
                               (JSONReadOptions object-as-hash: #t
                                                array-as-vector: #t))))
        (check (run-event-fold "ab\n" 'Document '() line '() helpers)
               => '((start Document) (start Text) (token Line 0 3)
                    (finish) (finish)))
        (check (hash-ref (vector-ref
                          (hash-ref (vector-ref (hash-ref ir "helpers") 0)
                                    "body") 0) "kind")
               => "call_source_helper")
        (check-exception
         (run-event-fold "ab\n" 'Document '() line '()
                         '((outer () ((call-source-helper inner start end)))
                           (inner () ((call-source-helper outer start end)))))
         true)))
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
    (test-case "future marker answers stay within one source"
      (let* ((condition '(future-line-marker-before-boundary?
                          "END" "STOP" "*" " " #t #t ""))
             (forms `((if ,condition
                          ((start-node Heading) (finish-node))
                          ((start-node Text) (finish-node))))))
        (check (run-event-fold "x\nx\nEND\n" 'Document '() forms '())
               => '((start Document)
                    (start Heading) (finish)
                    (start Heading) (finish)
                    (start Text) (finish)
                    (finish)))
        (check (run-event-fold "x\nSTOP\nEND\n" 'Document '() forms '())
               => '((start Document)
                    (start Text) (finish)
                    (start Heading) (finish)
                    (start Text) (finish)
                    (finish)))))
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
        (check (run-event-fold ":PROPERTIES:\n:header-args:python: :session local\n:END:\n"
                               'Document '() forms '())
               => '((start Document) (start Heading) (finish)
                    (start Heading) (finish) (start Text) (finish)
                    (finish)))
        (check (run-event-fold ":PROPERTIES:\n:ID: value:more\n:END:\n"
                               'Document '() forms '())
               => '((start Document) (start Heading) (finish)
                    (start Heading) (finish) (start Text) (finish)
                    (finish)))
        (check (run-event-fold ":PROPERTIES:\nmalformed\n:END:\n"
                               'Document '() forms '())
               => '((start Document) (start Text) (finish)
                    (start Heading) (finish) (start Text) (finish)
                    (finish)))
        (check (run-event-fold ":PROPERTIES:\n:header args:python: x\n:END:\n"
                               'Document '() forms '())
               => '((start Document) (start Text) (finish)
                    (start Heading) (finish) (start Text) (finish)
                    (finish)))
        (check (hash-ref (hash-ref (vector-ref (hash-ref ir "line") 0)
                                   "condition") "body_key_marker")
               => 58)))
    (test-case "future heading title respects level and exact suffix"
      (let* ((condition '(future-heading-title? "*" " " 4 "END"))
             (forms `((if ,condition
                          ((start-node Heading) (finish-node))
                          ((start-node Text) (finish-node)))))
             (wire (event-fold-ir-json
                    'future_heading event-lines-language-grammar
                    'Document '() forms '()))
             (ir (string->json wire
                               (JSONReadOptions object-as-hash: #t
                                                array-as-vector: #t))))
        (check (run-event-fold "**** Task\n* Outline\n***** END  \r\n"
                               'Document '() forms '())
               => '((start Document) (start Heading) (finish)
                    (start Heading) (finish) (start Text) (finish) (finish)))
        (check (run-event-fold "**** Task\n*** END\n"
                               'Document '() forms '())
               => '((start Document) (start Text) (finish)
                    (start Text) (finish) (finish)))
        (check (run-event-fold "**** Task\n**** END extra\n"
                               'Document '() forms '())
               => '((start Document) (start Text) (finish)
                    (start Text) (finish) (finish)))
        (check (hash-ref (hash-ref (vector-ref (hash-ref ir "line") 0)
                                   "condition") "kind")
               => "future_heading_title")
        (check-exception
         (event-fold-ir-json
          'bad_heading event-lines-language-grammar 'Document '()
          '((if (future-heading-title? "*" " " 0 "END") () ())) '())
         true)
        (let ((dynamic-forms
               '((if (future-heading-title? "*" " " (state threshold) "END")
                     ((start-node Heading) (finish-node))
                     ((start-node Text) (finish-node))))))
          (check (run-event-fold "x\n END\n" 'Document '((threshold 1))
                                 dynamic-forms '() '() '((threshold . 0)))
                 => '((start Document) (start Text) (finish)
                      (start Text) (finish) (finish))))))
    (test-case "typed parameter has one Scheme fold and AOT declaration"
      (let* ((initial '((threshold 4)))
             (forms '((if (uint-equal? (state threshold) (uint 7))
                          ((start-node Heading) (finish-node))
                          ((start-node Text) (finish-node)))))
             (wire (event-fold-ir-json
                    'parameterized event-lines-language-grammar
                    'Document initial forms '() '()
                    '((configured_threshold threshold 4))))
             (ir (string->json wire
                               (JSONReadOptions object-as-hash: #t
                                                array-as-vector: #t))))
        (check (run-event-fold "x\n" 'Document initial forms '())
               => '((start Document) (start Text) (finish) (finish)))
        (check (run-event-fold "x\n" 'Document initial forms '() '()
                               '((threshold . 7)))
               => '((start Document) (start Heading) (finish) (finish)))
        (check (hash-ref (vector-ref (hash-ref ir "parameters") 0) "state")
               => "threshold")
        (check-exception
         (event-fold-ir-json
          'parameterized event-lines-language-grammar
          'Document initial forms '() '()
          '((configured_threshold threshold 5)))
         true)))
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
    (test-case "final token uses source offsets retained in typed state"
      (let* ((initial '((span-start 0) (span-end 0)))
             (line-forms '((set-uint span-start (offset start))
                           (set-uint span-end (offset end))))
             (finish-forms '((token Line (state-offset span-start)
                                   (state-offset span-end))))
             (wire (event-fold-ir-json
                    'final_state_token event-lines-language-grammar
                    'Document initial line-forms finish-forms))
             (ir (string->json wire
                               (JSONReadOptions object-as-hash: #t
                                                array-as-vector: #t)))
             (token (vector-ref (hash-ref ir "finish") 0)))
        (check (run-event-fold "body\n" 'Document initial
                               line-forms finish-forms)
               => '((start Document) (token Line 0 5) (finish)))
        (check (hash-ref (hash-ref token "start") "kind")
               => "state_offset")
        (check (hash-ref (hash-ref token "end") "kind")
               => "state_offset")))
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
    (test-case "join-once converges branches before the shared continuation"
      (let* ((initial '())
             (forms '((join-once handled
                        ((if (line-starts-with "#")
                             ((start-node Heading) (token Line start end)
                              (finish-node)
                              (set-bool handled (bool #t))) ()))
                        ((start-node Text) (token Line start end)
                         (finish-node)))))
             (wire (event-fold-ir-json
                    'joined event-lines-language-grammar 'Document
                    initial forms '()))
             (ir (string->json wire
                               (JSONReadOptions object-as-hash: #t
                                                array-as-vector: #t)))
             (join (vector-ref (hash-ref ir "line") 0)))
        (check (run-event-fold "# H\nbody\n" 'Document initial forms '())
               => '((start Document)
                    (start Heading) (token Line 0 4) (finish)
                    (start Text) (token Line 4 9) (finish)
                    (finish)))
        (check (hash-ref join "kind") => "join_once")
        (check (vector-length (hash-ref join "continuation")) => 3)
        (check-exception
         (event-fold-ir-json 'invalid event-lines-language-grammar 'Document
                             initial '((join-once handled ((finish-node)) ())) '())
         true)
        (check-exception
         (event-fold-ir-json 'invalid event-lines-language-grammar 'Document
                             initial '((join-once handled ((finish-node))
                                                  ((token Line start end)))) '())
         true)
        (check-exception
         (run-event-fold "text\n" 'Document initial
                         '((join-once handled ((finish-node))
                                      ((token Line start end)))) '())
         true)
        (check-exception
         (event-fold-ir-json 'invalid event-lines-language-grammar 'Document
                             '((handled #f)) forms '())
         true)))
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
       true)
      (check-exception
       (event-fold-ir-json 'invalid event-lines-language-grammar 'Document
                           '() '() '((token Line start end)))
       true))))
