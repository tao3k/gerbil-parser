(import :gerbil-parser/language-test-support)
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


(deflanguage-parser-tests event-fold-test "Scheme stateful event fold AOT"
  (engine)
    (scenario "native source fold retains line endings byte bounds and initial state"
      (each (fixture) ('(("" (seed))
         ("tail" (seed ("tail" 0 4)))
         ("\n" (seed ("\n" 0 1)))
         ("é\r\nλ\rtail" (seed ("é\r\n" 0 4) ("λ\r" 4 7) ("tail" 7 11)))
         ("a\n\n" (seed ("a\n" 0 2) ("\n" 2 3)))))
         (equal
          (reverse
           (fold-source-lines (car fixture) '(seed)
             (lambda (line start end reversed)
               (cons (list line start end) reversed))))
           (cadr fixture)))
      (equal (source-line-events "é\r\nλ" 'Document
               (lambda (_ start end) (list (list 'token 'Line start end))))
              '((start Document) (token Line 0 4) (token Line 4 6) (finish))))
    (scenario "native fold chains multiple line emissions before final transitions"
      (scope ((forms '((start-node Text) (token Line start end) (finish-node)))
            (finish '((start-node Ending) (finish-node))))
        (equal (run-event-fold "é\r\nλ" 'Document '() forms finish)
                '((start Document)
                    (start Text) (token Line 0 4) (finish)
                    (start Text) (token Line 4 6) (finish)
                    (start Ending) (finish) (finish)))
        (equal (run-event-fold "" 'Document '() forms finish)
                '((start Document) (start Ending) (finish) (finish)))))
    (scenario "prepared state layout isolates values and temporary bindings"
      (sequential ((layout (prepare-fold-state-layout '((flag . #f) (count . 0) (stack))))
             (first (instantiate-fold-state-frame layout))
             (second (instantiate-fold-state-frame layout)))
        (act (fold-frame-update! first 'flag #t))
        (act (fold-frame-update! first 'count 9))
        (act (fold-frame-update! first 'stack '(4 2)))
        (act (fold-frame-bind! first 'temporary #f))
        (equal (fold-frame-bound? first 'temporary)  #t)
        (equal (fold-frame-bound? second 'temporary)  #f)
        (equal (fold-frame-ref second 'flag)  #f)
        (equal (fold-frame-ref second 'count)  0)
        (equal (fold-frame-ref second 'stack)  '())
        (act (fold-frame-unbind! first 'temporary))
        (equal (fold-frame-bound? first 'temporary)  #f)
        (raises (fold-frame-ref first 'temporary) true)))
    (scenario "prepared frames retain first binding and missing update policy"
      (sequential ((layout (prepare-fold-state-layout '((count . 0) (count . 4))))
             (frame (instantiate-fold-state-frame layout)))
        (equal (fold-frame-ref frame 'count)  0)
        (act (fold-frame-update! frame 'missing 10))
        (equal (fold-frame-bound? frame 'missing)  #f)
        (raises (fold-frame-bind! frame 'count 2) true)
        (act (fold-frame-unbind! frame 'count))
        (equal (fold-frame-bound? frame 'count)  #f)
        (act (fold-frame-update! frame 'count 7))
        (equal (fold-frame-bound? frame 'count)  #f)
        (act (fold-frame-bind! frame 'count 3))
        (equal (fold-frame-ref frame 'count)  3)
        (equal (fold-frame-ref (instantiate-fold-state-frame layout) 'count)  0)))
    (scenario "reusable programs snapshot declarations and preserve UTF-8 events"
      (sequential ((initial (list (list 'ready #f)))
             (forms (list '(set-bool ready (bool #t)) '(token Line start end)))
             (program (prepare-event-fold-program 'Document initial forms '())))
        (act (set-car! (cdr (car initial)) #t))
        (act (set-car! forms '(finish-node)))
        (each (source) ('("éx\r\n" "λ\n" "" "tail"))
           (equal (run-event-fold-program program source)
                   (run-event-fold source 'Document '((ready #f))
                                     '((set-bool ready (bool #t)) (token Line start end)) '())))))
    (property "reused helpers and parameter overrides never retain request state"
      (bindings (program
            (prepare-event-fold-program 'Document '((level 1))
              '((call-source-helper piece start end ((state level)))) '()
              '((piece ((level 0) (seen #f))
                       ((if (state seen) ((finish-node))
                            ((set-bool seen (bool #t))
                             (if (uint-equal? (state level) (uint 2))
                                 ((token Other start end)) ((token Line start end))))))
                       (level))))))
      (equal (run-event-fold-program program "a\nb\n" '((level . 2)))
                '((start Document) (token Other 0 2) (token Other 2 4) (finish)))
      (equal (run-event-fold-program program "λ\n")
                '((start Document) (token Line 0 3) (finish))))
    (scenario "prepared programs reject cycles callbacks and mutable initial values"
      (scope ((cycle (list 'token)))
        (act (set-cdr! cycle cycle))
        (raises (prepare-event-fold-program 'Document '() cycle '()) true))
      (raises (prepare-event-fold-program 'Document '() (list void) '()) true)
      (raises (prepare-fold-state-layout (list (cons 'state (vector 1)))) true)
      (raises (prepare-event-fold-program 'Document '((count -1)) '() '()) true))
    (property "engine source patterns advance by UTF-8 bytes"
      (bindings (end (source-pattern-end 'start "éx")))
      (equal end  '(line-step (line-step (line-step start))))
      (equal (run-event-fold "éx\n" 'Document '()
                 `((if ,(source-pattern-at? 'start "éx")
                       ((token Prefix start ,end) (token Ending ,end end))
                       ((token Other start end)))) '())
                '((start Document) (token Prefix 0 3)
                    (token Ending 3 4) (finish))))
    (property "engine ASCII case-insensitive single markers stay admissible"
      (bindings (predicate (source-ascii-ci-pattern-at? 'start "a")))
      (equal predicate  '(line-bytes-any-in? start (line-step start) (65 97)))
      (equal (run-event-fold "A\n" 'Document '()
                 `((if ,predicate ((token Line start end)) ())) '())
                '((start Document) (token Line 0 2) (finish)))
      (equal (string? (event-fold-ir-json
                         'single_ci event-lines-language-grammar 'Document '()
                         `((if ,predicate ((token Line start end)) ())) '()))
                #t))
    (property "engine ASCII patterns retain punctuation and matching boundaries"
      (bindings (forms `((if ,(source-ascii-ci-pattern-at? 'start "Ab-_")
                       ((token Match start end)) ((token Miss start end))))))
      (equal (run-event-fold "aB-_\n" 'Document '() forms '())
                '((start Document) (token Match 0 5) (finish)))
      (equal (run-event-fold "ab-\n" 'Document '() forms '())
                '((start Document) (token Miss 0 4) (finish)))
      (equal (run-event-fold "ab+x\n" 'Document '() forms '())
                '((start Document) (token Miss 0 5) (finish))))
    (scenario "engine offset construction rejects invalid counts before iteration"
      (equal (source-offset-after 'start 0)  'start)
      (equal (source-pattern-end 'start "")  'start)
      (each (count) ('(-1 1/2 1.0 #f))
                  (raises (source-offset-after 'start count) true))
      (raises (source-pattern-end 'start #f) true))
    (scenario "engine pattern admission distinguishes UTF-8 from ASCII folding"
      (each (pattern) ('("é" "Σ" "" #f))
                  (raises (source-ascii-ci-pattern-at? 'start pattern) true))
      (raises (source-pattern-at? 'start "") true)
      (raises (source-pattern-at? 'start #f) true))
    (property "one event chain preserves nested helper byte-loop and join order"
      (bindings)
      (equal (run-event-fold
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
              '((start Document) (start Text) (token Prefix 0 1)
                  (start Heading) (token Line 0 3) (finish)
                  (start Heading) (token Line 1 3) (finish)
                  (token Ending 0 3) (token Space 0 3) (finish) (finish))))
    (property "empty emissions and handled joins preserve an existing event chain"
      (bindings)
      (equal (run-event-fold
              "x\n" 'Document '()
              '((start-node Text) (token Before start end)
                (for-line-bytes index start start ((token Dropped start end)))
                (token Empty start start)
                (join-once handled
                  ((token Branch start end) (set-bool handled (bool #t)))
                  ((token Skipped start end)))
                (finish-node))
              '((start-node Heading) (finish-node)))
              '((start Document) (start Text) (token Before 0 2)
                  (token Branch 0 2) (finish) (start Heading) (finish)
                  (finish))))
    (property "mutation-only byte loops retain state without emitting wrappers"
      (bindings)
      (equal (run-event-fold
              "aé\n" 'Document '((count 0))
              '((for-line-bytes index start (line-content-end)
                  ((set-uint count (uint-add (state count) (uint 1)))
                   (if (bool #f) ((token Skipped start end)) ())))
                (token Line start (state-offset count))
                (token Ending (state-offset count) end)) '())
              '((start Document) (token Line 0 3) (token Ending 3 4)
                  (finish))))
    (property "join temporary bindings are removed before the next join"
      (bindings)
      (equal (run-event-fold
              "a\nb\n" 'Document '((seen #f))
              '((join-once handled
                  ((set-bool seen (bool #t)) (set-bool handled (bool #t)))
                  ((token Skipped start end)))
                (join-once handled
                  ((if (bool #f) ((set-bool handled (bool #t))) ()))
                  ((if (state seen) ((token Line start end)) ())))) '())
              '((start Document) (token Line 0 2) (token Line 2 4)
                  (finish))))
    (property "repeated helper calls own independent execution slots"
      (bindings)
      (equal (run-event-fold
              "a\n" 'Document '((position 2))
              '((call-source-helper head start end)
                (call-source-helper head start end)
                (token Line start (state-offset position))) '()
              '((head ((position 0))
                      ((set-uint position (uint-add (state position) (uint 1)))
                       (token Prefix start (state-offset position))))))
              '((start Document) (token Prefix 0 1) (token Prefix 0 1)
                  (token Line 0 2) (finish))))
    (property "native UTF-8 length keeps multibyte prefix source offsets"
      (bindings)
      (equal (run-event-fold
              "éx\n" 'Document '()
              '((token Prefix start (line-prefix-end "é"))
                (token Line (line-prefix-end "é") end)) '())
              '((start Document) (token Prefix 0 2) (token Line 2 4)
                  (finish))))
    (property "prepared byte sets preserve empty ranges and UTF-8 membership"
      (bindings)
      (equal (run-event-fold
              "é\n" 'Document '()
              '((if (and (line-bytes-all-in? start (line-content-end) (195 169 195 255))
                         (line-bytes-any-in? start (line-content-end) (169))
                         (line-bytes-all-in? start start ())
                         (not (line-bytes-any-in? start start ()))
                         (not (line-bytes-any-in? start end ())))
                    ((token Line start end)) ())) '())
              '((start Document) (token Line 0 3) (finish))))
    (scenario "prepared byte sets reject malformed values only when evaluated"
      (each (values) ('((256) (-1) (1/2) (a) (1 . 2) "a"))
         (raises
          (run-event-fold "a\n" 'Document '()
                          `((if (line-bytes-any-in? start end ,values) () ())) '())
          true))
      (equal (run-event-fold
              "a\n" 'Document '()
              '((if (or (bool #t) (line-bytes-any-in? start end (256)))
                    ((token Line start end)) ())) '())
              '((start Document) (token Line 0 2) (finish))))
    (scenario "prepared byte sets do not leak across parse requests"
      (sequential ((values (list 97))
             (forms `((if (line-bytes-any-in? start end ,values)
                          ((token Line start end)) ()))))
        (equal (run-event-fold "a\n" 'Document '() forms '())
                '((start Document) (token Line 0 2) (finish)))
        (act (set-car! values 98))
        (equal (run-event-fold "a\n" 'Document '() forms '())
                '((start Document) (finish)))))
    (property "helper parameter updates preserve duplicate-name semantics"
      (bindings)
      (equal (run-event-fold
              "a\n" 'Document '((caller 0))
              '((call-source-helper head start end ((uint 1))))
              '()
              '((head ((position 0) (position 2))
                      ((token Line start (state-offset position))
                       (token Line (state-offset position) end))
                      (position))))
              '((start Document) (token Line 0 1) (token Line 1 2)
                  (finish))))
    (property "boolean predicate loops preserve short-circuit rejection boundaries"
      (bindings)
      (equal (run-event-fold
              "a\n" 'Document '()
              '((if (and (bool #f) (state undeclared)) () ())
                (if (or (bool #t) (state undeclared))
                    ((token Line start end)) ())) '())
              '((start Document) (token Line 0 2) (finish))))
    (property "helper execution restores the independent caller frame"
      (bindings)
      (equal (run-event-fold
              "a\n" 'Document '((position 1))
              '((call-source-helper head start (state-offset position))
                (token Line (state-offset position) end))
              '()
              '((head ((position 0) (position 2))
                      ((set-uint position (uint 1))
                       (token Line start (state-offset position))))))
              '((start Document) (token Line 0 1) (token Line 1 2)
                  (finish))))
    (scenario "execution slots preserve untouched and repeated bindings"
      (scope ((initial '((left 0) (middle 0) (right 2) (enabled #f)))
            (forms
             '((set-uint middle (uint 1))
               (set-uint middle (uint 1))
               (set-bool enabled (bool #t))
               (if (and (uint-equal? (state left) (uint 0))
                        (uint-equal? (state right) (uint 2))
                        (state enabled))
                   ((token Line start (state-offset middle))
                    (token Line (state-offset middle) end)) ()))))
        (equal (run-event-fold "a\n" 'Document initial forms '())
                '((start Document) (token Line 0 1) (token Line 1 2)
                    (finish)))
        (equal initial  '((left 0) (middle 0) (right 2) (enabled #f)))
        (equal (run-event-fold "b\n" 'Document initial forms '())
                '((start Document) (token Line 0 1) (token Line 1 2)
                    (finish)))))
    (property "duplicate runtime bindings retain the original all-update semantics"
      (bindings)
      (equal (run-event-fold
              "a\n" 'Document '((offset 0) (offset 2))
              '((set-uint offset (uint 1))
                (token Line start (state-offset offset))
                (token Line (state-offset offset) end)) '())
              '((start Document) (token Line 0 1) (token Line 1 2)
                  (finish))))
    (property "nested UTF-8 helper frames restore their caller byte view"
      (bindings)
      (equal (run-event-fold
              "αb\nz\n" 'Document '()
              '((if (line-byte-equal? start 206)
                    ((call-source-helper head start (line-step (line-step start)))
                     (if (line-byte-equal? (line-step (line-step start)) 98)
                         ((token Line (line-step (line-step start)) end)) ()))
                    ((token Line start end))))
              '()
              '((head () ((if (line-byte-equal? start 206)
                              ((token Line start end)) ())))))
              '((start Document) (token Line 0 2) (token Line 2 4)
                  (token Line 4 6) (finish))))
    (property "the Scheme algorithm owns multi-line node lifetimes"
      (bindings)
      (equal (parse-fold-lines "a\nb\n* α\r\nc")
              '((start Document)
                  (start Text) (token Line 0 2) (token Line 2 4) (finish)
                  (start Heading) (token Line 4 10) (finish)
                  (start Text) (token Line 10 11) (finish)
                  (finish)))
      (equal (parse-fold-lines "")
              '((start Document) (finish))))
    (property "AOT carries typed state and the source-owned digest"
      (bindings (ir (string->json parse_fold_lines
                             (JSONReadOptions object-as-hash: #t
                                              array-as-vector: #t))))
      (equal (hash-ref ir "schema")
                "gerbil-scheme-rust.event-function-ir.v1")
      (equal (hash-ref ir "name")  "parse_fold_lines")
      (equal (string-prefix? "sha256:" (hash-ref ir "parser_digest"))
                #t)
      (equal (vector-length (hash-ref ir "line"))  1))
    (scenario "Scheme fold owns nested headline structure"
      (equal (parse-outline-lines "* Parent\n** Child\nbody\n* Peer\n")
              '((start Document)
                  (start Section) (start Heading) (token Line 0 9) (finish)
                  (start Section) (start Heading) (token Line 9 18) (finish)
                  (start Text) (token Line 18 23) (finish)
                  (finish) (finish)
                  (start Section) (start Heading) (token Line 23 30) (finish)
                  (finish) (finish)))
      (scope ((ir (string->json parse_outline_lines
                             (JSONReadOptions object-as-hash: #t
                                              array-as-vector: #t))))
        (equal (hash-ref (vector-ref (hash-ref ir "initial") 0) "kind")
                "let_usize_stack")))
    (scenario "ASCII-insensitive syntax prefix executes and lowers identically"
      (equal (run-event-fold "#+BeGiN_SrC rust\n" 'Document '()
                             '((if (line-starts-with-ascii-ci "#+begin_src")
                                   ((start-node Text) (token Line start end)
                                    (finish-node))
                                   ())) '())
              '((start Document) (start Text) (token Line 0 17)
                  (finish) (finish)))
      (sequential ((wire (event-fold-ir-json
                    'ascii_prefix event-lines-language-grammar 'Document '()
                    '((if (line-starts-with-ascii-ci "#+begin_src")
                          ((start-node Text) (token Line start end)
                           (finish-node))
                          ())) '()))
             (ir (string->json wire
                               (JSONReadOptions object-as-hash: #t
                                                array-as-vector: #t))))
        (equal (hash-ref (hash-ref (vector-ref (hash-ref ir "line") 0)
                                   "condition") "kind")
                "line_starts_with_ascii_case_insensitive")))
    (scenario "blank source lines use one typed predicate in both paths"
      (equal (run-event-fold " \t\r\nα\n" 'Document '()
                             '((if (line-blank?)
                                   ((start-node Heading) (token Line start end)
                                    (finish-node))
                                   ((start-node Text) (token Line start end)
                                    (finish-node)))) '())
              '((start Document)
                  (start Heading) (token Line 0 4) (finish)
                  (start Text) (token Line 4 7) (finish)
                  (finish)))
      (sequential ((wire (event-fold-ir-json
                    'blank_line event-lines-language-grammar 'Document '()
                    '((if (line-blank?) () ())) '()))
             (ir (string->json wire
                               (JSONReadOptions object-as-hash: #t
                                                array-as-vector: #t))))
        (equal (hash-ref (hash-ref (vector-ref (hash-ref ir "line") 0)
                                   "condition") "kind")
                "line_blank")))
    (scenario "checked source slices compare names across physical lines"
      (scope ((initial '((name-start 0) (name-end 0)))
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
        (equal (run-event-fold "name:ALPHA\nalpha\nother\n"
                               'Document initial forms '())
                '((start Document) (token Line 0 11)
                    (start Heading) (token Line 11 17) (finish)
                    (start Text) (token Line 17 23) (finish)
                    (finish)))
        (sequential ((wire (event-fold-ir-json
                      'source_slices event-lines-language-grammar
                      'Document initial forms '()))
               (ir (string->json wire
                                 (JSONReadOptions object-as-hash: #t
                                                  array-as-vector: #t))))
          (equal (hash-ref ir "name")  "source_slices"))))
    (scenario "named future marker respects names headings and parent closes"
      (sequential ((name-start '(line-prefix-end "#+BEGIN_"))
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
        (equal (map cadr
                    (filter (lambda (event) (eq? (car event) 'start))
                            (run-event-fold
                             "#+BEGIN_foo\n#+end_FOO\n#+BEGIN_bar\n* Next\n#+END_bar\n#+BEGIN_baz\n#+END_CENTER\n#+END_baz\n#+BEGIN_qux\n#+END_other\n#+END_QUX\n"
                             'Document '() forms '())))
                '(Document Heading Text Text Heading))
        (equal (map cadr
                    (filter (lambda (event) (eq? (car event) 'start))
                            (run-event-fold
                             "#+BEGIN_foo\n#+BEGIN_foo\n#+END_foo\n"
                             'Document '() forms '())))
                '(Document Heading Heading))
        (equal (map cadr
                    (filter (lambda (event) (eq? (car event) 'start))
                            (run-event-fold
                             "#+BEGIN_foo\r\n  #+end_FOO \r\n"
                             'Document '() forms '())))
                '(Document Heading))
        (sequential ((wire (event-fold-ir-json
                      'future_named event-lines-language-grammar
                      'Document '() forms '()))
               (ir (string->json wire
                                 (JSONReadOptions object-as-hash: #t
                                                  array-as-vector: #t)))
               (outer (vector-ref (hash-ref ir "line") 0))
               (inner (vector-ref (hash-ref outer "consequent") 0)))
          (equal (hash-ref (hash-ref inner "condition") "kind")
                  "future_named_line_marker_before_boundary"))))
    (scenario "named future marker stops at a source-named parent closer"
      (sequential ((name-start '(line-prefix-end "#+BEGIN_"))
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
        (equal (map cadr
                    (filter (lambda (event) (eq? (car event) 'start))
                            (run-event-fold
                             "#+BEGIN_OUTER\n#+BEGIN_INNER\n#+CLOSE_outer\n#+END_inner\n"
                             'Document initial forms '())))
                '(Document Text))
        (equal (map cadr
                    (filter (lambda (event) (eq? (car event) 'start))
                            (run-event-fold
                             "#+BEGIN_OUTER\n#+BEGIN_INNER\n#+END_inner\n#+CLOSE_outer\n"
                             'Document initial forms '())))
                '(Document Heading))
        (sequential ((wire (event-fold-ir-json
                      'future_named_parent event-lines-language-grammar
                      'Document initial forms '()))
               (ir (string->json wire
                                 (JSONReadOptions object-as-hash: #t
                                                  array-as-vector: #t)))
               (outer (vector-ref (hash-ref ir "line") 0))
               (inner (vector-ref (hash-ref outer "alternate") 0))
               (future-ir (hash-ref (vector-ref (hash-ref inner "consequent") 0)
                                    "condition")))
          (equal (hash-ref future-ir "stop_prefix")  "#+CLOSE_")
          (equal (hash-ref future-ir "stop_ascii_case_insensitive")  #t))))
    (property "named future marker keeps parent case sensitivity independent"
      (bindings (name-start '(line-prefix-end "#+BEGIN_")) (name-end `(line-scan-key ,name-start)) (future `(future-named-line-marker-before-boundary?
                       ,name-start ,name-end "#+END_" "" ""
                       "*" " " #t #t #t
                       (state-offset parent-start) (state-offset parent-end)
                       "#+END_" "" #f)) (initial '((parent-start 0) (parent-end 0))) (forms
              `((if (line-starts-with "#+BEGIN_OUTER")
                    ((set-uint parent-start (offset ,name-start))
                     (set-uint parent-end (offset ,name-end))
                     (token Line start end))
                    ((if (line-starts-with "#+BEGIN_INNER")
                         ((if ,future
                              ((start-node Heading) (finish-node))
                              ((start-node Text) (finish-node))))
                         ((token Line start end))))))))
      (equal (map cadr
                    (filter (lambda (event) (eq? (car event) 'start))
                            (run-event-fold
                             "#+BEGIN_OUTER\n#+BEGIN_INNER\n#+END_outer\n#+END_inner\n"
                             'Document initial forms '())))
                '(Document Heading))
      (equal (map cadr
                    (filter (lambda (event) (eq? (car event) 'start))
                            (run-event-fold
                             "#+BEGIN_OUTER\n#+BEGIN_INNER\n#+END_OUTER\n#+END_inner\n"
                             'Document initial forms '())))
                '(Document Text)))
    (scenario "state-only frame pop does not close syntax nodes"
      (scope ((initial '((saved (uint-stack))))
            (forms '((push-frame saved (uint 7))
                     (pop-frame saved)
                     (token Line start end))))
        (equal (run-event-fold "a\n" 'Document initial forms '())
                '((start Document) (token Line 0 2) (finish)))
        (sequential ((wire (event-fold-ir-json
                      'state_pop event-lines-language-grammar
                      'Document initial forms '()))
               (ir (string->json wire
                                 (JSONReadOptions object-as-hash: #t
                                                  array-as-vector: #t))))
          (equal (hash-ref (vector-ref (hash-ref ir "line") 1) "kind")
                  "pop_frame"))))
    (scenario "single frame close preserves nested equal frame scopes"
      (scope ((initial '((frames (uint-stack))))
            (forms '((start-node Heading)
                     (push-frame frames (uint 7))
                     (start-node Text)
                     (push-frame frames (uint 7))
                     (close-frame frames 1)
                     (close-frame frames 1))))
        (equal (run-event-fold "a\n" 'Document initial forms '())
                '((start Document) (start Heading) (start Text)
                    (finish) (finish) (finish)))))
    (property "byte-set membership is a boolean state value"
      (bindings (forms '((set-bool match
                               (line-bytes-any-in? start (line-step start)
                                                   (126)))
                    (if (state match)
                        ((start-node Heading) (token Line start end)
                         (finish-node))
                        ((start-node Text) (token Line start end)
                         (finish-node))))))
      (equal (run-event-fold "~x\nx\n" 'Document '((match #f)) forms '())
                '((start Document)
                    (start Heading) (token Line 0 3) (finish)
                    (start Text) (token Line 3 5) (finish)
                    (finish)))
      (equal (string? (event-fold-ir-json
                         'byte_set_bool event-lines-language-grammar
                         'Document '((match #f)) forms '()))
                #t))
    (property "bounded static name set executes and lowers as one predicate"
      (bindings (forms '((if (line-bytes-in-set? start (line-content-end)
                                             ("alpha" "beta"))
                          ((start-node Heading) (token Line start end)
                           (finish-node))
                          ((start-node Text) (token Line start end)
                           (finish-node))))) (wire (event-fold-ir-json
                    'static_name_set event-lines-language-grammar
                    'Document '() forms '())) (ir (string->json wire
                               (JSONReadOptions object-as-hash: #t
                                                array-as-vector: #t))))
      (equal (run-event-fold "alpha\nbeta\nother\n" 'Document '() forms '())
                '((start Document)
                    (start Heading) (token Line 0 6) (finish)
                    (start Heading) (token Line 6 11) (finish)
                    (start Text) (token Line 11 17) (finish)
                    (finish)))
      (equal (hash-ref (hash-ref (vector-ref (hash-ref ir "line") 0)
                                   "condition") "kind")
                "line_bytes_in_set"))
    (property "saved source bounds scan one UTF-8 span across physical lines"
      (bindings (initial '((span-start 0) (span-end 0))) (line '((set-uint span-end (offset end)))) (finish
              '((with-source-bounds (state-offset span-start)
                                    (state-offset span-end)
                  ((if (line-bytes-any-in? start end (10))
                       ((start-node Text) (token Line start end)
                        (finish-node)) ()))))) (wire (event-fold-ir-json
                    'source_span event-lines-language-grammar
                    'Document initial line finish)) (ir (string->json wire
                               (JSONReadOptions object-as-hash: #t
                                                array-as-vector: #t))))
      (equal (run-event-fold "ab\ncd\n" 'Document initial line finish)
                '((start Document) (start Text) (token Line 0 6)
                    (finish) (finish)))
      (equal (hash-ref (vector-ref (hash-ref ir "finish") 0) "kind")
                "with_source_bounds"))
    (property "source-local helper executes once and stays typed in event IR"
      (bindings (initial '((span-start 0) (span-end 0))) (line '((set-uint span-end (offset end)))) (finish '((call-source-helper inline-span
                                           (state-offset span-start)
                                           (state-offset span-end)))) (helpers
              '((inline-span
                 ((cursor 0))
                 ((set-uint cursor (offset start))
                  (if (line-bytes-any-in? start end (10))
                      ((start-node Text)
                       (token Line (state-offset cursor) end)
                       (finish-node)) ()))))) (wire (event-fold-ir-json
                    'source_helper event-lines-language-grammar
                    'Document initial line finish helpers)) (ir (string->json wire
                               (JSONReadOptions object-as-hash: #t
                                                array-as-vector: #t))))
      (equal (run-event-fold "ab\ncd\n" 'Document initial line finish helpers)
                '((start Document) (start Text) (token Line 0 6)
                    (finish) (finish)))
      (equal (hash-ref (vector-ref (hash-ref ir "finish") 0) "kind")
                "call_source_helper")
      (equal (vector-length (hash-ref ir "helpers"))  1))
    (scenario "typed helper arguments flow through Scheme execution and IR"
      (sequential ((initial '((threshold 2)))
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
        (equal (run-event-fold "x\n" 'Document initial line '() helpers)
                '((start Document) (start Heading) (finish) (finish)))
        (equal (run-event-fold "x\n" 'Document initial line '() helpers
                               '((threshold . 5)))
                '((start Document) (start Text) (finish) (finish)))
        (equal (vector-length (hash-ref
                               (vector-ref (hash-ref ir "line") 0)
                               "arguments"))
                1)
        (equal (vector-ref
                (hash-ref (vector-ref (hash-ref ir "helpers") 0)
                          "parameters") 0)
                "threshold")
        (raises
         (event-fold-ir-json
          'bad_helper_arity event-lines-language-grammar 'Document initial
          '((call-source-helper local-span start end)) '() helpers)
         true)))
    (scenario "source helpers compose without recursive execution"
      (sequential ((line '((call-source-helper outer start end)))
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
        (equal (run-event-fold "ab\n" 'Document '() line '() helpers)
                '((start Document) (start Text) (token Line 0 3)
                    (finish) (finish)))
        (equal (hash-ref (vector-ref
                          (hash-ref (vector-ref (hash-ref ir "helpers") 0)
                                    "body") 0) "kind")
                "call_source_helper")
        (raises
         (run-event-fold "ab\n" 'Document '() line '()
                         '((outer () ((call-source-helper inner start end)))
                           (inner () ((call-source-helper outer start end)))))
         true)))
    (property "future marker search stops at heading or parent boundary"
      (bindings (condition '(and (line-starts-with-ascii-ci "#+BEGIN_QUOTE")
                              (future-line-marker-before-boundary?
                               "#+END_QUOTE" "#+END_CENTER" "*" " " #t #t ""))) (forms `((if ,condition
                          ((start-node Heading) (finish-node))
                          ((start-node Text) (finish-node))))) (wire (event-fold-ir-json
                    'future_marker event-lines-language-grammar
                    'Document '() forms '())) (ir (string->json wire
                               (JSONReadOptions object-as-hash: #t
                                                array-as-vector: #t))))
      (equal (run-event-fold "#+BEGIN_QUOTE\n  #+end_quote\n"
                               'Document '() forms '())
                '((start Document) (start Heading) (finish)
                    (start Text) (finish) (finish)))
      (equal (run-event-fold "#+BEGIN_QUOTE\n** Next\n#+END_QUOTE\n"
                               'Document '() forms '())
                '((start Document) (start Text) (finish)
                    (start Text) (finish) (start Text) (finish)
                    (finish)))
      (equal (run-event-fold "#+BEGIN_QUOTE\n#+END_CENTER\n#+END_QUOTE\n"
                               'Document '() forms '())
                '((start Document) (start Text) (finish)
                    (start Text) (finish) (start Text) (finish)
                    (finish)))
      (equal (hash-ref (hash-ref (hash-ref
                                   (vector-ref (hash-ref ir "line") 0)
                                   "condition") "left") "kind")
                "future_line_marker_before_boundary"))
    (property "future marker answers stay within one source"
      (bindings (condition '(future-line-marker-before-boundary?
                          "END" "STOP" "*" " " #t #t "")) (forms `((if ,condition
                          ((start-node Heading) (finish-node))
                          ((start-node Text) (finish-node))))))
      (equal (run-event-fold "x\nx\nEND\n" 'Document '() forms '())
                '((start Document)
                    (start Heading) (finish)
                    (start Heading) (finish)
                    (start Text) (finish)
                    (finish)))
      (equal (run-event-fold "x\nSTOP\nEND\n" 'Document '() forms '())
                '((start Document)
                    (start Text) (finish)
                    (start Heading) (finish)
                    (start Text) (finish)
                    (finish))))
    (property "future marker can require a key-value body"
      (bindings (condition '(future-line-marker-before-boundary?
                          ":END:" "" "*" " " #t #t ":")) (forms `((if ,condition
                          ((start-node Heading) (finish-node))
                          ((start-node Text) (finish-node))))) (wire (event-fold-ir-json
                    'future_key_body event-lines-language-grammar
                    'Document '() forms '())) (ir (string->json wire
                               (JSONReadOptions object-as-hash: #t
                                                array-as-vector: #t))))
      (equal (run-event-fold ":PROPERTIES:\n:ID: one\n:END:\n"
                               'Document '() forms '())
                '((start Document) (start Heading) (finish)
                    (start Heading) (finish) (start Text) (finish)
                    (finish)))
      (equal (run-event-fold ":PROPERTIES:\n:header-args:python: :session local\n:END:\n"
                               'Document '() forms '())
                '((start Document) (start Heading) (finish)
                    (start Heading) (finish) (start Text) (finish)
                    (finish)))
      (equal (run-event-fold ":PROPERTIES:\n:ID: value:more\n:END:\n"
                               'Document '() forms '())
                '((start Document) (start Heading) (finish)
                    (start Heading) (finish) (start Text) (finish)
                    (finish)))
      (equal (run-event-fold ":PROPERTIES:\nmalformed\n:END:\n"
                               'Document '() forms '())
                '((start Document) (start Text) (finish)
                    (start Heading) (finish) (start Text) (finish)
                    (finish)))
      (equal (run-event-fold ":PROPERTIES:\n:header args:python: x\n:END:\n"
                               'Document '() forms '())
                '((start Document) (start Text) (finish)
                    (start Heading) (finish) (start Text) (finish)
                    (finish)))
      (equal (hash-ref (hash-ref (vector-ref (hash-ref ir "line") 0)
                                   "condition") "body_key_marker")
                58))
    (scenario "future heading title respects level and exact suffix"
      (sequential ((condition '(future-heading-title? "*" " " 4 "END"))
             (forms `((if ,condition
                          ((start-node Heading) (finish-node))
                          ((start-node Text) (finish-node)))))
             (wire (event-fold-ir-json
                    'future_heading event-lines-language-grammar
                    'Document '() forms '()))
             (ir (string->json wire
                               (JSONReadOptions object-as-hash: #t
                                                array-as-vector: #t))))
        (equal (run-event-fold "**** Task\n* Outline\n***** END  \r\n"
                               'Document '() forms '())
                '((start Document) (start Heading) (finish)
                    (start Heading) (finish) (start Text) (finish) (finish)))
        (equal (run-event-fold "**** Task\n*** END\n"
                               'Document '() forms '())
                '((start Document) (start Text) (finish)
                    (start Text) (finish) (finish)))
        (equal (run-event-fold "**** Task\n**** END extra\n"
                               'Document '() forms '())
                '((start Document) (start Text) (finish)
                    (start Text) (finish) (finish)))
        (equal (hash-ref (hash-ref (vector-ref (hash-ref ir "line") 0)
                                   "condition") "kind")
                "future_heading_title")
        (raises
         (event-fold-ir-json
          'bad_heading event-lines-language-grammar 'Document '()
          '((if (future-heading-title? "*" " " 0 "END") () ())) '())
         true)
        (scope ((dynamic-forms
               '((if (future-heading-title? "*" " " (state threshold) "END")
                     ((start-node Heading) (finish-node))
                     ((start-node Text) (finish-node))))))
          (equal (run-event-fold "x\n END\n" 'Document '((threshold 1))
                                 dynamic-forms '() '() '((threshold . 0)))
                  '((start Document) (start Text) (finish)
                      (start Text) (finish) (finish))))))
    (scenario "typed parameter has one Scheme fold and AOT declaration"
      (sequential ((initial '((threshold 4)))
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
        (equal (run-event-fold "x\n" 'Document initial forms '())
                '((start Document) (start Text) (finish) (finish)))
        (equal (run-event-fold "x\n" 'Document initial forms '() '()
                               '((threshold . 7)))
                '((start Document) (start Heading) (finish) (finish)))
        (equal (hash-ref (vector-ref (hash-ref ir "parameters") 0) "state")
                "threshold")
        (raises
         (event-fold-ir-json
          'parameterized event-lines-language-grammar
          'Document initial forms '() '()
          '((configured_threshold threshold 5)))
         true)))
    (property "n-ary boolean predicates evaluate every operand and lower to IR"
      (bindings (forms '((if (and (bool #t) (bool #t) (bool #f))
                          ((start-node Heading) (finish-node))
                          ((start-node Text) (finish-node)))
                     (if (or (bool #f) (bool #f) (bool #t))
                         ((start-node Heading) (finish-node)) ()))) (wire (event-fold-ir-json
                    'nary_boolean event-lines-language-grammar
                    'Document '() forms '())) (ir (string->json wire
                               (JSONReadOptions object-as-hash: #t
                                                array-as-vector: #t))))
      (equal (run-event-fold "x" 'Document '() forms '())
                '((start Document) (start Text) (finish)
                    (start Heading) (finish) (finish)))
      (equal (hash-ref (hash-ref (vector-ref (hash-ref ir "line") 0)
                                   "condition") "kind")  "and")
      (equal (hash-ref (hash-ref (vector-ref (hash-ref ir "line") 1)
                                   "condition") "kind")  "or"))
    (property "bounded delimiter scan accepts non-whitespace source keys"
      (bindings (key-end '(line-scan-nonspace-until start ":")) (forms `((start-node Text)
                      (token Line start ,key-end)
                      (token Line ,key-end end)
                      (finish-node))) (wire (event-fold-ir-json
                    'delimiter_scan event-lines-language-grammar
                    'Document '() forms '())) (ir (string->json wire
                               (JSONReadOptions object-as-hash: #t
                                                array-as-vector: #t))))
      (equal (run-event-fold "A+B: yes\n" 'Document '() forms '())
                '((start Document) (start Text)
                    (token Line 0 3) (token Line 3 9)
                    (finish) (finish)))
      (equal (hash-ref
                (hash-ref (vector-ref (hash-ref ir "line") 1) "end")
                "kind")  "line_scan_nonspace_until"))
    (property "bounded delimiter scan may include source whitespace"
      (bindings (tag-end '(line-scan-until start ":")) (forms `((start-node Text)
                      (token Line start ,tag-end)
                      (token Line ,tag-end end)
                      (finish-node))) (wire (event-fold-ir-json
                    'tag_scan event-lines-language-grammar
                    'Document '() forms '())) (ir (string->json wire
                               (JSONReadOptions object-as-hash: #t
                                                array-as-vector: #t))))
      (equal (run-event-fold "term words :: body\n" 'Document '() forms '())
                '((start Document) (start Text)
                    (token Line 0 11) (token Line 11 19)
                    (finish) (finish)))
      (equal (hash-ref
                (hash-ref (vector-ref (hash-ref ir "line") 1) "end")
                "kind")  "line_scan_until"))
    (scenario "ASCII line markers reject prefix collisions in Scheme and IR"
      (sequential ((forms '((if (line-prefix-boundary-ascii-ci "#+begin_src")
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
        (equal (run-event-fold "#+begin_srcx\n" 'Document '() forms '())
                '((start Document) (start Heading)
                    (token Line 0 13) (finish) (finish)))
        (equal (hash-ref (hash-ref (vector-ref (hash-ref ir "line") 0)
                                   "condition") "kind")
                "line_prefix_boundary_ascii_case_insensitive"))
      (equal (run-event-fold ":END: tail\n" 'Document '()
                             '((if (line-marker-ascii-ci ":END:")
                                   ((start-node Text) (finish-node))
                                   ((start-node Heading) (finish-node)))) '())
              '((start Document) (start Heading) (finish) (finish))))
    (scenario "source-backed prefix, word and trivia offsets execute in Scheme"
      (sequential ((prefix '(line-prefix-end "#+begin_src"))
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
        (equal (run-event-fold "#+BeGiN_SrC rust :x\n" 'Document '()
                               forms '())
                '((start Document) (start Text)
                    (token Line 0 11) (token Line 11 12)
                    (token Line 12 16) (token Line 16 20)
                    (finish) (finish)))
        (equal (run-event-fold "#+begin_src \n" 'Document '()
                               forms '())
                '((start Document) (start Heading)
                    (token Line 0 13) (finish) (finish)))
        (sequential ((wire (event-fold-ir-json
                      'header_word event-lines-language-grammar
                      'Document '() forms '()))
               (ir (string->json wire
                                 (JSONReadOptions object-as-hash: #t
                                                  array-as-vector: #t)))
               (conditional (vector-ref (hash-ref ir "line") 0))
               (consequent (hash-ref conditional "consequent")))
          (equal (hash-ref (hash-ref (vector-ref (hash-ref ir "line") 0)
                                     "condition") "kind")
                  "line_has_word_after_prefix")
          (equal (hash-ref (hash-ref (vector-ref consequent 2) "end")
                           "kind")
                  "line_skip_horizontal")
          (equal (hash-ref (hash-ref (vector-ref consequent 3) "end")
                           "kind")
                  "line_scan_word"))))
    (scenario "dynamic key offsets keep Org-style key, value and trivia source-backed"
      (sequential ((prefix '(line-prefix-end "#+"))
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
        (equal (run-event-fold "#+SEQ_TODO: TODO | DONE \r\n" 'Document '()
                               forms '())
                '((start Document) (start Text)
                    (token Line 0 2) (token Line 2 10)
                    (token Line 10 12) (token Line 12 23)
                    (token Line 23 26) (finish) (finish)))
        (equal (run-event-fold "#+@bad: x\n" 'Document '() forms '())
                '((start Document) (start Heading)
                    (token Line 0 10) (finish) (finish)))
        (equal (run-event-fold "#+EMPTY:  \n" 'Document '() forms '())
                '((start Document) (start Text)
                    (token Line 0 2) (token Line 2 7)
                    (token Line 7 10)
                    (token Line 10 11) (finish) (finish)))
        (sequential ((wire (event-fold-ir-json
                      'dynamic_key event-lines-language-grammar
                      'Document '() forms '()))
               (ir (string->json wire
                                 (JSONReadOptions object-as-hash: #t
                                                  array-as-vector: #t)))
               (conditional (vector-ref (hash-ref ir "line") 0))
               (consequent (hash-ref conditional "consequent")))
          (equal (hash-ref (hash-ref conditional "condition") "kind")
                  "line_has_key_after_prefix")
          (equal (hash-ref (hash-ref (vector-ref consequent 2) "end") "kind")
                  "line_scan_key")
          (equal (hash-ref (hash-ref (vector-ref consequent 4) "end") "kind")
                  "line_trim_end_from"))))
    (scenario "bounded source-byte fold executes and lowers the same events"
      (sequential ((index '(line-index cursor))
             (next (list 'line-step index))
             (forms `((set-uint last (offset start))
                      (for-line-bytes cursor start (line-content-end)
                        ((if (line-byte-equal? ,index 124)
                             ((token Line (state-offset last) ,index)
                              (token Line ,index ,next)
                              (set-uint last (offset ,next))) ()) ))
                      (token Line (state-offset last) end))))
        (equal (run-event-fold "|x|\n" 'Document '((last 0)) forms '())
                '((start Document) (token Line 0 1)
                    (token Line 1 2) (token Line 2 3)
                    (token Line 3 4) (finish)))
        (sequential ((wire (event-fold-ir-json
                      'byte_fold event-lines-language-grammar
                      'Document '((last 0)) forms '()))
               (ir (string->json wire
                                 (JSONReadOptions object-as-hash: #t
                                                  array-as-vector: #t)))
               (loop (vector-ref (hash-ref ir "line") 1)))
          (equal (hash-ref loop "kind")  "for_line_bytes")
          (equal (hash-ref (hash-ref loop "until") "kind")
                  "line_content_end"))))
    (property "declared list markers and frame closes execute as Scheme"
      (bindings (initial '((present #f) (column 0) (ordered #f)
                        (bullet-start 0) (bullet-end 0) (content-start 0)
                        (frames (uint-stack)))) (forms '((scan-list-marker "-+*" #t 8 present column ordered
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
                          ()))) (finish '((close-all-frames frames 1))) (wire (event-fold-ir-json
                    'list_fold event-lines-language-grammar
                    'Document initial forms finish)) (ir (string->json wire
                               (JSONReadOptions object-as-hash: #t
                                                array-as-vector: #t))))
      (equal (run-event-fold "- a\n  12) b\n- c\n" 'Document
                               initial forms finish)
                '((start Document)
                    (start Text) (token Line 0 1) (token Line 1 4)
                    (start Text) (token Line 4 6) (token Line 6 9)
                    (token Line 9 12)
                    (finish) (start Text)
                    (token Line 12 13) (token Line 13 16)
                    (finish) (finish) (finish)))
      (equal (hash-ref (vector-ref (hash-ref ir "line") 0) "kind")
                "scan_list_marker"))
    (property "headline marker offsets use the declared level in Scheme and IR"
      (bindings (marker '(line-marker-end "*" " ")) (forms `((if (uint-positive? (line-marker-level "*" " "))
                          ((start-node Heading)
                           (token Line start ,marker)
                           (token Line ,marker end)
                           (finish-node)) ()))) (wire (event-fold-ir-json
                    'headline_fields event-lines-language-grammar
                    'Document '() forms '())) (ir (string->json wire
                               (JSONReadOptions object-as-hash: #t
                                                array-as-vector: #t))) (consequent (hash-ref (vector-ref (hash-ref ir "line") 0)
                                   "consequent")))
      (equal (run-event-fold "** Work\n" 'Document '() forms '())
                '((start Document) (start Heading)
                    (token Line 0 2) (token Line 2 8)
                    (finish) (finish)))
      (equal (hash-ref (hash-ref (vector-ref consequent 1) "end") "kind")
                "line_marker_end"))
    (property "typed unsigned block state selects a bounded transition"
      (bindings (initial '((active-block 2))) (forms '((if (uint-equal? (state active-block) (uint 2))
                          ((start-node Text) (token Line start end)
                           (finish-node))
                          ((start-node Heading) (token Line start end)
                           (finish-node))))) (wire (event-fold-ir-json
                    'block_state event-lines-language-grammar
                    'Document initial forms '())) (ir (string->json wire
                               (JSONReadOptions object-as-hash: #t
                                                array-as-vector: #t))))
      (equal (run-event-fold "body\n" 'Document initial forms '())
                '((start Document) (start Text) (token Line 0 5)
                    (finish) (finish)))
      (equal (hash-ref (hash-ref (vector-ref (hash-ref ir "line") 0)
                                   "condition") "kind")
                "usize_equal"))
    (property "final token uses source offsets retained in typed state"
      (bindings (initial '((span-start 0) (span-end 0))) (line-forms '((set-uint span-start (offset start))
                           (set-uint span-end (offset end)))) (finish-forms '((token Line (state-offset span-start)
                                   (state-offset span-end)))) (wire (event-fold-ir-json
                    'final_state_token event-lines-language-grammar
                    'Document initial line-forms finish-forms)) (ir (string->json wire
                               (JSONReadOptions object-as-hash: #t
                                                array-as-vector: #t))) (token (vector-ref (hash-ref ir "finish") 0)))
      (equal (run-event-fold "body\n" 'Document initial
                               line-forms finish-forms)
                '((start Document) (token Line 0 5) (finish)))
      (equal (hash-ref (hash-ref token "start") "kind")
                "state_offset")
      (equal (hash-ref (hash-ref token "end") "kind")
                "state_offset"))
    (scenario "final transition may flush a saved source-backed token"
      (sequential ((initial '((pending #f) (pending-start 0) (pending-end 0)))
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
        (equal (run-event-fold "\n" 'Document initial line finish)
                '((start Document) (token Line 0 1) (finish)))
        (equal (hash-ref (hash-ref flush "start") "kind")
                "state_offset")
        (raises
         (event-fold-ir-json 'invalid event-lines-language-grammar
                             'Document initial '()
                             '((token Line start end))) true)
        (raises
         (event-fold-ir-json 'invalid event-lines-language-grammar
                             'Document initial '()
                             '((token Line
                                      (line-step (state-offset pending-start))
                                      (state-offset pending-end)))) true)))
    (scenario "join-once converges branches before the shared continuation"
      (sequential ((initial '())
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
        (equal (run-event-fold "# H\nbody\n" 'Document initial forms '())
                '((start Document)
                    (start Heading) (token Line 0 4) (finish)
                    (start Text) (token Line 4 9) (finish)
                    (finish)))
        (equal (hash-ref join "kind")  "join_once")
        (equal (vector-length (hash-ref join "continuation"))  3)
        (raises
         (event-fold-ir-json 'invalid event-lines-language-grammar 'Document
                             initial '((join-once handled ((finish-node)) ())) '())
         true)
        (raises
         (event-fold-ir-json 'invalid event-lines-language-grammar 'Document
                             initial '((join-once handled ((finish-node))
                                                  ((token Line start end)))) '())
         true)
        (raises
         (run-event-fold "text\n" 'Document initial
                         '((join-once handled ((finish-node))
                                      ((token Line start end)))) '())
         true)
        (raises
         (event-fold-ir-json 'invalid event-lines-language-grammar 'Document
                             '((handled #f)) forms '())
         true)))
    (scenario "undeclared state and unsupported effects fail closed"
      (raises
       (event-fold-ir-json 'invalid event-lines-language-grammar 'Document
                           '() '((set-bool missing (bool #t))) '())
       true)
      (raises
       (event-fold-ir-json 'invalid event-lines-language-grammar 'Document
                           '() '((display "not an event")) '())
       true)
      (raises
       (event-fold-ir-json 'invalid event-lines-language-grammar 'Document
                           '((a-b #f) (a_b #t)) '() '())
       true)
      (raises
       (event-fold-ir-json 'invalid event-lines-language-grammar 'Document
                           '() '()
                           '((if (line-starts-with "*") () ())))
       true)
      (raises
       (event-fold-ir-json 'invalid event-lines-language-grammar 'Document
                           '() '() '((token Line start end)))
       true)))
