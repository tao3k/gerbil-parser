#!/usr/bin/env gxi
(import :gerbil-parser/language-test-support)
;;; -*- Gerbil -*-

(import :std/test
        (only-in :gerbil-parser/languages/arithmetic/parser arithmetic-parser parse-arithmetic)
        (only-in :gerbil-parser/languages/hcl/parser hcl-parser parse-hcl)
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-success?)
        (only-in :gerbil-parser/src/runtime/funcs recognition-sequence->list current-recognition-sequence-fusion-enabled?)
        (only-in :gerbil-parser/src/runtime/recognition
                 recognition-child-value recognition-node? recognition-node-kind recognition-node-children recognition-fragment? recognition-fragment-children)
        (only-in :gerbil-parser/languages/tla-plus/parser tla-plus-layout-language-grammar)
        (only-in :gerbil-parser/src/language/descriptor language-grammar-machine)
        (only-in :gerbil-parser/src/compiler/lr-compiler compile-lr-spec)
        (only-in :gerbil-parser/src/runtime/token make-token)
        (only-in :gerbil-parser/src/runtime/lr-parser
                 current-lr-event-program-enabled? current-lr-recognition-observer lr-prepare lr-parse/prepared
                 install-lr-runtime-direct-step! lr-runtime-fragment-reuse-safe? lr-initial-checkpoint
                 lr-checkpoint-feed lr-checkpoint-before-shift lr-checkpoint-fragment-compatible?
                 lr-recognition-view?
                 lr-recognition-view-base lr-recognition-view-delta
                 lr-recognition-fragment? lr-recognition-fragment-children lr-recognition-fragment-end
                 lr-recognition-fragment-token-count lr-recognition-fragment-value
                 lr-recognition-project)
        (only-in :gerbil-parser/src/runtime/incremental
                 current-lr-probe-reuse-enabled? current-lr-source-index-enabled? incremental-session-source-index incremental-session-source-modes make-incremental-session incremental-session-artifact
                 incremental-session-recognition-root incremental-session-project-artifact
                 parse-incremental-session apply-edit make-edit))

(import (only-in :gerbil-parser/src/runtime/source-index source-index->lists source-index-audit source-index-storage source-index-count))
(def (index-lists session)
  (call-with-values (lambda () (source-index->lists (incremental-session-source-index session))) list))
(defparser-check (check-source-index source session)
  (equal (source-index-audit (incremental-session-source-index session))  #t)
  (branch (incremental-session-source-modes session)
    (equal (cadr (index-lists session))  (incremental-session-source-modes session))
    (equal (incremental-session-source-index session)  #f))
  (equal (car (index-lists session))  (car (index-lists (make-incremental-session hcl-parser source #t)))))

(defparser-check (check-projection machine source session)
  (scope ((root (incremental-session-recognition-root session)))
    (equal (or (lr-recognition-fragment? root) (lr-recognition-view? root))  #t)
    ;; Independent production replay publishes the full canonical artifact,
    ;; including current token identity and coordinates after relative moves.
    (equal (incremental-session-project-artifact session)
            (incremental-session-artifact session))))

(defparser-check (check-edits source edits)
  (parameters ((current-lr-source-index-enabled? #t))
  (scope ((session (make-incremental-session hcl-parser source #t)))
    (each (edit) (edits)
       (scope-values (((next receipt) (parse-incremental-session session edit)))
         (act (set! source (apply-edit source edit)))
         (act (set! session next))
         (verify check-source-index source next)
         (equal (incremental-session-artifact next)  (parse-hcl source))
         (when (incremental-session-recognition-root next)
           (verify check-projection hcl-parser source next)))))))

(def (retained-prefix-piece root bound)
  (let loop ((pending (list root)))
    (and (pair? pending)
         (let (piece (car pending))
           (cond
            ((not (lr-recognition-fragment? piece)) (loop (cdr pending)))
            ((and (<= (lr-recognition-fragment-end piece) bound)
                  (> (lr-recognition-fragment-token-count piece) 1)) piece)
            (else (loop (append (lr-recognition-fragment-children piece)
                                (cdr pending)))))))))

(def (contains-identical-piece? root expected)
  (let loop ((pending (list root)))
    (and (pair? pending)
         (or (eq? (car pending) expected)
             (loop (append
                    (cond ((lr-recognition-view? (car pending))
                           (list (lr-recognition-view-base (car pending))))
                          ((lr-recognition-fragment? (car pending))
                           (lr-recognition-fragment-children (car pending)))
                          (else '()))
                    (cdr pending)))))))

(deflanguage-parser-tests recognition-snapshot-test "private grammar recognition snapshots"
  (engine)
    (scenario "source provenance shares old chunks and matches vector control"
      (parameters ((current-lr-source-index-enabled? #t))
      (sequential ((source (apply string-append (make-list 80 "value = 001\n")))
             (session (make-incremental-session hcl-parser source #t))
             (before (index-lists session)) (edit (make-edit 0 0 "other = 002\n")))
        (scope-values (((next receipt) (parse-incremental-session session edit)))
          (verify check-source-index (apply-edit source edit) next)
          (equal (index-lists session)  before)
          (equal (> (cdr (assq 'sourceIndexSharedTokenCount receipt)) 300)  #t)
          (equal (< (cdr (assq 'sourceIndexFreshTokenCount receipt)) 100)  #t)
          (equal (+ (cdr (assq 'sourceIndexFreshTokenCount receipt)) (cdr (assq 'sourceIndexSharedTokenCount receipt)))
                  (source-index-count (incremental-session-source-index next)))
          (equal (> (length (filter (lambda (storage) (memq storage (source-index-storage (incremental-session-source-index session))))
                                    (source-index-storage (incremental-session-source-index next)))) 5)  #t)
          (parameters ((current-lr-source-index-enabled? #f))
            (scope-values (((control ignored) (parse-incremental-session session edit)))
              (equal (incremental-session-source-index control)  #f)
              (equal (incremental-session-artifact control)  (incremental-session-artifact next))
              (equal (incremental-session-project-artifact control)  (incremental-session-project-artifact next))))))))
    (scenario "event window splices preserve index when grammar capture is dropped"
      (verify check-edits "value = 1\nother = 2\n" (list (make-edit 8 1 "9") (make-edit 9 0 " /* λ中😀 */")
                         (make-edit 9 17 "") (make-edit 8 1 "1"))))
    (property "full artifact capture defaults to the qualified vector path"
      (bindings)
      (equal (current-lr-source-index-enabled?)  #f)
      (equal (incremental-session-source-index (make-incremental-session hcl-parser "value = 1\n" #t))  #f))
    (scenario "fused semantic publication matches original replay across edits"
      (parameters ((current-recognition-sequence-fusion-enabled? #t))
        (sequential ((session (make-incremental-session hcl-parser "a = 1\nb = 2\n" #t))
               (root (incremental-session-recognition-root session))
               (value (recognition-child-value (car (recognition-sequence->list (lr-recognition-fragment-value root))))))
          (equal (recognition-node? value)  #t)
          (equal
           (let loop ((pending (list value)))
             (and (pair? pending)
                  (let* ((value (car pending))
                         (children (cond ((recognition-node? value) (recognition-node-children value))
                                         ((recognition-fragment? value) (recognition-fragment-children value))
                                         (else '()))))
                    (or (not (list? children))
                        (loop (append (map recognition-child-value children) (cdr pending)))))))
            #t))
        (verify check-edits "value = \"λ中😀\" /* trivia */\nother = { nested = [1, 2] }\n" (list (make-edit 0 0 "first = 001\n")
                (make-edit 0 12 "")
                (make-edit 0 0 "block { value = 3 }\n")
                (make-edit 0 (string-length "block { value = 3 }\n") "")))))
    (property "ordinary sessions allocate no grammar snapshot"
      (bindings)
      (equal (incremental-session-recognition-root
              (make-incremental-session arithmetic-parser "1 + 2"))  #f))
    (scenario "grammar structure independently projects complete artifacts"
      (each (row) ((list (cons arithmetic-parser "001 + 002 * (003 + 004)")
             (cons hcl-parser "value = 001\nother = { nested = [1, 2] }\n")
             (cons hcl-parser "value = \"λ中😀\" /* retained trivia */\n")
             (cons hcl-parser "")))
         (verify check-projection (car row) (cdr row) (make-incremental-session (car row) (cdr row) #t))))
    (scenario "captured prefix and successive topology changes remain projectable"
      (scope ((source (apply string-append (make-list 80 "value = 001\n")))
                 (session (make-incremental-session
                           hcl-parser
                           (apply string-append (make-list 80 "value = 001\n")) #t)))
        (each (edit) ((list (make-edit (* 40 12) 0 "other = 002\n")
               (make-edit (* 40 12) 12 "")
               (make-edit 0 0 "other = 002\n")
               (make-edit 0 12 "")))
           (scope-values (((next ignored) (parse-incremental-session session edit)))
             (act (set! source (apply-edit source edit)))
             (act (set! session next))
             (equal (incremental-session-artifact session)  (parse-hcl source))
             (verify check-projection hcl-parser source session)))))
    (scenario "resume physically shares unchanged grammar prefix fragments"
      (sequential ((source (apply string-append (make-list 80 "value = 001\n")))
             (session (make-incremental-session hcl-parser source #t))
             (piece (retained-prefix-piece (incremental-session-recognition-root session) 120)))
        (equal (lr-recognition-fragment? piece)  #t)
        (scope-values (((next ignored)
                      (parse-incremental-session session (make-edit 480 0 "other = 002\n"))))
          (equal (contains-identical-piece? (incremental-session-recognition-root next) piece)
                  #t))))
    (scenario "nonterminal transfers skip unchanged declarations"
      (sequential ((source (apply string-append (make-list 80 "value = 001\n")))
             (session (make-incremental-session hcl-parser source #t)))
        (scope-values (((next receipt)
                      (parse-incremental-session session (make-edit 0 0 "other = 002\n"))))
          (equal (> (cdr (assq 'reusedRecognitionFragmentCount receipt)) 0)  #t)
          (equal (> (cdr (assq 'reusedSignificantTokenCount receipt)) 200)  #t)
          (equal (incremental-session-artifact next)
                  (parse-hcl (string-append "other = 002\n" source)))
          (verify check-projection hcl-parser source next))))
    (scenario "probe reuse preserves the entire captured transfer result"
      (sequential ((source (apply string-append (make-list 80 "value = 001\n")))
             (session (make-incremental-session hcl-parser source #t))
             (edit (make-edit 0 0 "other = 002\n")))
        (scope-values (((cached receipt) (parse-incremental-session session edit)))
          (parameters ((current-lr-probe-reuse-enabled? #f))
            (scope-values (((control control-receipt) (parse-incremental-session session edit)))
              (equal (incremental-session-artifact cached)
                      (incremental-session-artifact control))
              (equal (cdr (assq 'remainingSignificantTokenCount receipt))
                      (cdr (assq 'remainingSignificantTokenCount control-receipt)))
              (equal (> (cdr (assq 'fragmentProbeReuseTokenCount receipt)) 0)  #t)
              (equal (cdr (assq 'fragmentProbeReuseTokenCount control-receipt))  0)
              (verify check-projection hcl-parser source cached)
              (verify check-projection hcl-parser source control))))))
    (scenario "relative positions compose across edits inside reused suffixes"
      (verify check-edits (apply string-append (make-list 12 "value = 001\n")) (list (make-edit 0 0 "other = 002\n")
                         (make-edit 36 0 "third = 003\n")
                         (make-edit 0 12 "")
                         (make-edit 24 12 "")
                         (make-edit 0 0 "other = 002\n")
                         (make-edit 0 12 ""))))
    (scenario "cursor maps bulk deletion and empty-source transitions"
      (scope ((line "value = 001\n"))
        (verify check-edits (apply string-append (make-list 80 line)) (list (make-edit 240 84 "")
                           (make-edit 0 0 (apply string-append (make-list 5 "other = 002\n")))
                           (make-edit 0 60 "")
                           (make-edit 240 0 (apply string-append (make-list 7 line)))
                           (make-edit 0 960 "")
                           (make-edit 0 0 (apply string-append (make-list 80 line)))))))
    (scenario "unchanged nested blocks skip their complete grammar interior"
      (sequential ((body (apply string-append (make-list 80 "  value = 001\n")))
             (source (string-append "group {\n" body "}\n"))
             (session (make-incremental-session hcl-parser source #t)))
        (equal (parse-artifact-success? (incremental-session-artifact session))  #t)
        (scope-values (((next receipt)
                      (parse-incremental-session session (make-edit 0 0 "top = 0\n"))))
          (equal (incremental-session-artifact next)
                  (parse-hcl (string-append "top = 0\n" source)))
          (equal (> (cdr (assq 'reusedSignificantTokenCount receipt)) 300)  #t)
          (equal (< (cdr (assq 'fragmentCursorVisitCount receipt)) 30)  #t)
          (verify check-projection hcl-parser source next))
        (verify check-edits source (list (make-edit 568 0 "  other = 002\n")
                           (make-edit 568 14 "")
                           (make-edit 0 0 "top = 0\n")
                           (make-edit 0 8 "")))))
    (scenario "UTF8 and lexical boundary changes retain canonical ownership"
      (verify check-edits "value = \"λ中😀\"\nother = [1, 2]\n" (list (make-edit 0 0 "prefix = 0\n")
                         (make-edit 0 11 "")
                         (make-edit 0 0 "/* λ */ ")
                         (make-edit 0 9 "")))
      (verify check-edits "value = 1\nother = 2\nthird = 3\n" (list (make-edit 9 0 " + 4")
                         (make-edit 9 4 "")
                         (make-edit 9 0 " /* comment */")
                         (make-edit 9 14 "")
                         (make-edit 0 0 "value = ")
                         (make-edit 0 8 ""))))
    (scenario "UTF8 token leaves move inside actually reused structures"
      (verify check-edits (apply string-append (make-list 30 "value = \"λ中😀\"\n")) (list (make-edit 0 0 "prefix = 0\n")
                         (make-edit 0 11 ""))))
    (scenario "failed parse clears transferred-work receipts"
      ;; Trivia makes this checkpoint tail large enough to admit the catalog;
      ;; unchanged declarations before the bad final identifier can transfer.
      (sequential ((line "value /* a */ = /* b */ 001 /* c */\n")
             (source (apply string-append (make-list 80 line)))
             (edit (make-edit (* 79 (string-length line)) 1 "@"))
             (session (make-incremental-session hcl-parser source #t)))
        (scope-values (((next receipt) (parse-incremental-session session edit)))
          (equal (incremental-session-artifact next)
                  (parse-hcl (apply-edit source edit)))
          (equal (incremental-session-recognition-root next)  #f)
          (equal (cdr (assq 'freshFallback? receipt))  #t)
          (equal (assq 'reusedRecognitionFragmentCount receipt)  #f))))
    (scenario "an equivalent grammar in another prepared runtime is rejected"
      (sequential ((spec (compile-lr-spec
                    '((source-file (alias SourceFile (token identifier))))
                    'source-file))
             (first (lr-prepare spec)) (second (lr-prepare spec))
             (token (make-token 'identifier "x" 0 1)) (captured #f))
        (parameters ((current-lr-recognition-observer
                        (lambda (root) (set! captured root))))
          (scope-values (((root rest) (lr-parse/prepared first (list token))))
            (equal rest  '())))
        (equal (lr-recognition-fragment? captured)  #t)
        (equal (lr-checkpoint-fragment-compatible?
                (lr-checkpoint-before-shift (lr-initial-checkpoint first '()) token)
                captured)  #t)
        (act (install-lr-runtime-direct-step! first
          (lambda args (error "rejection test must not execute replacement reducer"))))
        (equal (lr-checkpoint-fragment-compatible?
                (lr-checkpoint-before-shift (lr-initial-checkpoint first '()) token)
                captured)  #f)
        (equal (lr-checkpoint-fragment-compatible?
                (lr-checkpoint-before-shift (lr-initial-checkpoint second '()) token)
                captured)  #f)))
    (scenario "Wagner context-dependent repetition rejects identical public Item kinds"
      ;; A c+ / B d+ shares the X X yield and semantic Item kind, while
      ;; the distant marker selects a different grammar nonterminal/context.
      (sequential ((runtime
              (lr-prepare
               (compile-lr-spec
                '((source-file
                   (alias SourceFile
                     (choice
                      (sequence (literal "A") (repeat1 (reference c)))
                      (sequence (literal "B") (repeat1 (reference d))))))
                  (c (alias Item (sequence (token identifier) (token identifier))))
                  (d (alias Item (sequence (token identifier) (token identifier)))))
                'source-file)))
             (tokens (map (lambda (index) (make-token 'identifier "X" index (+ index 1)))
                          (iota 80 1)))
             (captured #f))
        (equal (lr-runtime-fragment-reuse-safe? runtime)  #t)
        (parameters ((current-lr-recognition-observer
                        (lambda (root) (set! captured root))))
          (scope-values (((root rest)
                        (lr-parse/prepared runtime
                          (cons (make-token 'identifier "A" 0 1) tokens))))
            (equal rest  '())))
        (scope ((item
              (let walk ((pending (list captured)))
                (and (pair? pending)
                     (let (piece (car pending))
                       (if (not (lr-recognition-fragment? piece)) (walk (cdr pending))
                         (let (children (recognition-sequence->list
                                         (lr-recognition-fragment-value piece)))
                           (if (and (= (lr-recognition-fragment-token-count piece) 2)
                                    (pair? children) (null? (cdr children))
                                    (recognition-node? (recognition-child-value (car children)))
                                    (eq? (recognition-node-kind
                                          (recognition-child-value (car children))) 'Item))
                             piece
                             (walk (append (lr-recognition-fragment-children piece)
                                           (cdr pending)))))))))))
          (equal (lr-recognition-fragment? item)  #t)
          (scope-values (((status after-marker)
                        (lr-checkpoint-feed (lr-initial-checkpoint runtime '())
                                            (make-token 'identifier "B" 0 1))))
            (equal status  'checkpoint)
            (equal (lr-checkpoint-fragment-compatible?
                    (lr-checkpoint-before-shift after-marker (car tokens)) item)  #f)))
        (scope-values (((root rest)
                      (lr-parse/prepared runtime
                        (cons (make-token 'identifier "B" 0 1) tokens))))
          (equal (recognition-node-kind root)  'SourceFile)
          (equal rest  '()))))
    (scenario "short tail edits retain earlier checkpoints for inverse edits on both backends"
      (each (event?) ('(#f #t))
         (parameters ((current-lr-event-program-enabled? event?))
           (each (units) ('(400 800))
              (sequential ((source (string-join (make-list units "a = 1\n") ""))
                     (session (make-incremental-session hcl-parser source #t))
                     (start (* 6 (- units 1))))
                (each (insert?) ('(#t #f))
                   (sequential ((edit (make-edit start (if insert? 0 6) (if insert? "b = 2\n" "")))
                          (inverse (make-edit start (if insert? 6 0) (if insert? "" "a = 1\n"))))
                     (scope-values (((next receipt) (parse-incremental-session session edit)))
                       (equal (incremental-session-artifact next)  (parse-hcl (apply-edit source edit)))
                       (verify check-projection hcl-parser (apply-edit source edit) next)
                       (scope-values (((restored receipt) (parse-incremental-session next inverse)))
                         (equal (assq 'freshFallback? receipt)  #f)
                         (equal (> (cdr (assq 'checkpointReusedShiftCount receipt)) 0)  #t)
                         (equal (< (cdr (assq 'remainingSignificantTokenCount receipt)) units)  #t)
                         (equal (incremental-session-artifact restored)  (parse-hcl source))
                         (verify check-projection hcl-parser source restored))))))))))
    (scenario "deep left recursion projects without recursive Scheme traversal"
      (sequential ((source (string-join (make-list 1600 "001") " + "))
             (session (make-incremental-session arithmetic-parser source #t)))
        (verify check-projection arithmetic-parser source session)
        (equal (lr-recognition-fragment-token-count
                (incremental-session-recognition-root session))  3199)))
    (scenario "event-only fast paths discard stale grammar snapshots"
      (scope ((session (make-incremental-session arithmetic-parser "001 + 002" #t)))
        (scope-values (((next ignored) (parse-incremental-session session (make-edit 6 3 "003"))))
          (equal (incremental-session-artifact next)  (parse-arithmetic "001 + 003"))
          (equal (incremental-session-recognition-root next)  #f)
          (scope-values (((resumed ignored)
                        (parse-incremental-session next (make-edit 0 0 "004 + "))))
            (verify check-projection arithmetic-parser "004 + 001 + 003" resumed)))))
    (scenario "selective GLR cannot publish a deterministic reuse root"
      (scope ((observed 'not-called)
            (runtime
             (lr-prepare
              (compile-lr-spec
               '((source-file (choice (reference first-path) (reference second-path)))
                 (first-path (alias SourceFile (field value (token identifier))))
                 (second-path (alias SourceFile (field value (token identifier)))))
               'source-file 'selective-glr))))
        (parameters ((current-lr-recognition-observer
                        (lambda (root) (set! observed root))))
          (scope-values (((root rest)
                        (lr-parse/prepared runtime (list (make-token 'identifier "x" 0 1)))))
            (equal rest  '())))
        (equal observed  #f)
        (equal (lr-runtime-fragment-reuse-safe? runtime)  #f)))
    (property "layout execution retains only its authoritative artifact"
      (bindings (session
            (make-incremental-session
             (language-grammar-machine tla-plus-layout-language-grammar)
             "---- MODULE J ----\nInit ==\n  /\\ TRUE\n  /\\ FALSE\n====\n" #t)))
      (equal (parse-artifact-success? (incremental-session-artifact session))  #t)
      (equal (incremental-session-recognition-root session)  #f)
      (equal (incremental-session-source-index session)  #f))
    (property "invalid input cannot retain a reusable root"
      (bindings (session (make-incremental-session arithmetic-parser "1 +" #t)))
      (equal (parse-artifact-success? (incremental-session-artifact session))  #f)
      (equal (incremental-session-recognition-root session)  #f)
      (equal (incremental-session-source-index session)  #f)))
