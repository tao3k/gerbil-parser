#!/usr/bin/env gxi
;;; -*- Gerbil -*-

(import :std/test
        (only-in :gerbil-parser/languages/arithmetic/v1/parser arithmetic-parser parse-arithmetic-v1)
        (only-in :gerbil-parser/languages/hcl/v2-24/parser hcl-v2-24-parser parse-hcl-v2-24)
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-success?)
        (only-in :gerbil-parser/src/runtime/funcs recognition-sequence->list current-recognition-sequence-fusion-enabled?)
        (only-in :gerbil-parser/src/runtime/recognition
                 recognition-child-value recognition-node? recognition-node-kind recognition-node-children recognition-fragment? recognition-fragment-children)
        (only-in :gerbil-parser/languages/tla-plus/parser tla-plus-layout-parser)
        (only-in :gerbil-parser/src/compiler/lr-compiler compile-lr-spec)
        (only-in :gerbil-parser/src/runtime/token make-token)
        (only-in :gerbil-parser/src/runtime/lr-parser
                 current-lr-recognition-observer lr-prepare lr-parse/prepared
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
(def (check-source-index source session)
  (check (source-index-audit (incremental-session-source-index session)) => #t)
  (if (incremental-session-source-modes session)
    (check (cadr (index-lists session)) => (incremental-session-source-modes session))
    (check (incremental-session-source-index session) => #f))
  (check (car (index-lists session)) => (car (index-lists (make-incremental-session hcl-v2-24-parser source #t)))))

(def (check-projection machine source session)
  (let (root (incremental-session-recognition-root session))
    (check (or (lr-recognition-fragment? root) (lr-recognition-view? root)) => #t)
    ;; Independent production replay publishes the full canonical artifact,
    ;; including current token identity and coordinates after relative moves.
    (check (incremental-session-project-artifact session)
           => (incremental-session-artifact session))))

(def (check-edits source edits)
  (parameterize ((current-lr-source-index-enabled? #t))
  (let (session (make-incremental-session hcl-v2-24-parser source #t))
    (for-each
     (lambda (edit)
       (let-values (((next receipt) (parse-incremental-session session edit)))
         (set! source (apply-edit source edit))
         (set! session next)
         (check-source-index source next)
         (check (incremental-session-artifact next) => (parse-hcl-v2-24 source))
         (when (incremental-session-recognition-root next)
           (check-projection hcl-v2-24-parser source next))))
     edits))))

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

(def recognition-snapshot-test
  (test-suite "private grammar recognition snapshots"
    (test-case "source provenance shares old chunks and matches vector control"
      (parameterize ((current-lr-source-index-enabled? #t))
      (let* ((source (apply string-append (make-list 80 "value = 001\n")))
             (session (make-incremental-session hcl-v2-24-parser source #t))
             (before (index-lists session)) (edit (make-edit 0 0 "other = 002\n")))
        (let-values (((next receipt) (parse-incremental-session session edit)))
          (check-source-index (apply-edit source edit) next)
          (check (index-lists session) => before)
          (check (> (cdr (assq 'sourceIndexSharedTokenCount receipt)) 300) => #t)
          (check (< (cdr (assq 'sourceIndexFreshTokenCount receipt)) 100) => #t)
          (check (+ (cdr (assq 'sourceIndexFreshTokenCount receipt)) (cdr (assq 'sourceIndexSharedTokenCount receipt)))
                 => (source-index-count (incremental-session-source-index next)))
          (check (> (length (filter (lambda (storage) (memq storage (source-index-storage (incremental-session-source-index session))))
                                    (source-index-storage (incremental-session-source-index next)))) 5) => #t)
          (parameterize ((current-lr-source-index-enabled? #f))
            (let-values (((control ignored) (parse-incremental-session session edit)))
              (check (incremental-session-source-index control) => #f)
              (check (incremental-session-artifact control) => (incremental-session-artifact next))
              (check (incremental-session-project-artifact control) => (incremental-session-project-artifact next))))))))
    (test-case "event window splices preserve index when grammar capture is dropped"
      (check-edits "value = 1\nother = 2\n"
                   (list (make-edit 8 1 "9") (make-edit 9 0 " /* λ中😀 */")
                         (make-edit 9 17 "") (make-edit 8 1 "1"))))
    (test-case "full artifact capture defaults to the qualified vector path"
      (check (current-lr-source-index-enabled?) => #f)
      (check (incremental-session-source-index (make-incremental-session hcl-v2-24-parser "value = 1\n" #t)) => #f))
    (test-case "fused semantic publication matches original replay across edits"
      (parameterize ((current-recognition-sequence-fusion-enabled? #t))
        (let* ((session (make-incremental-session hcl-v2-24-parser "a = 1\nb = 2\n" #t))
               (root (incremental-session-recognition-root session))
               (value (recognition-child-value (car (recognition-sequence->list (lr-recognition-fragment-value root))))))
          (check (recognition-node? value) => #t)
          (check
           (let loop ((pending (list value)))
             (and (pair? pending)
                  (let* ((value (car pending))
                         (children (cond ((recognition-node? value) (recognition-node-children value))
                                         ((recognition-fragment? value) (recognition-fragment-children value))
                                         (else '()))))
                    (or (not (list? children))
                        (loop (append (map recognition-child-value children) (cdr pending)))))))
           => #t))
        (check-edits "value = \"λ中😀\" /* trivia */\nother = { nested = [1, 2] }\n"
          (list (make-edit 0 0 "first = 001\n")
                (make-edit 0 12 "")
                (make-edit 0 0 "block { value = 3 }\n")
                (make-edit 0 (string-length "block { value = 3 }\n") "")))))
    (test-case "ordinary sessions allocate no grammar snapshot"
      (check (incremental-session-recognition-root
              (make-incremental-session arithmetic-parser "1 + 2")) => #f))
    (test-case "grammar structure independently projects complete artifacts"
      (for-each
       (lambda (row)
         (check-projection (car row) (cdr row)
                           (make-incremental-session (car row) (cdr row) #t)))
       (list (cons arithmetic-parser "001 + 002 * (003 + 004)")
             (cons hcl-v2-24-parser "value = 001\nother = { nested = [1, 2] }\n")
             (cons hcl-v2-24-parser "value = \"λ中😀\" /* retained trivia */\n")
             (cons hcl-v2-24-parser ""))))
    (test-case "captured prefix and successive topology changes remain projectable"
      (let ((source (apply string-append (make-list 80 "value = 001\n")))
                 (session (make-incremental-session
                           hcl-v2-24-parser
                           (apply string-append (make-list 80 "value = 001\n")) #t)))
        (for-each
         (lambda (edit)
           (let-values (((next ignored) (parse-incremental-session session edit)))
             (set! source (apply-edit source edit))
             (set! session next)
             (check (incremental-session-artifact session) => (parse-hcl-v2-24 source))
             (check-projection hcl-v2-24-parser source session)))
         (list (make-edit (* 40 12) 0 "other = 002\n")
               (make-edit (* 40 12) 12 "")
               (make-edit 0 0 "other = 002\n")
               (make-edit 0 12 "")))))
    (test-case "resume physically shares unchanged grammar prefix fragments"
      (let* ((source (apply string-append (make-list 80 "value = 001\n")))
             (session (make-incremental-session hcl-v2-24-parser source #t))
             (piece (retained-prefix-piece (incremental-session-recognition-root session) 120)))
        (check (lr-recognition-fragment? piece) => #t)
        (let-values (((next ignored)
                      (parse-incremental-session session (make-edit 480 0 "other = 002\n"))))
          (check (contains-identical-piece? (incremental-session-recognition-root next) piece)
                 => #t))))
    (test-case "nonterminal transfers skip unchanged declarations"
      (let* ((source (apply string-append (make-list 80 "value = 001\n")))
             (session (make-incremental-session hcl-v2-24-parser source #t)))
        (let-values (((next receipt)
                      (parse-incremental-session session (make-edit 0 0 "other = 002\n"))))
          (check (> (cdr (assq 'reusedRecognitionFragmentCount receipt)) 0) => #t)
          (check (> (cdr (assq 'reusedSignificantTokenCount receipt)) 200) => #t)
          (check (incremental-session-artifact next)
                 => (parse-hcl-v2-24 (string-append "other = 002\n" source)))
          (check-projection hcl-v2-24-parser source next))))
    (test-case "probe reuse preserves the entire captured transfer result"
      (let* ((source (apply string-append (make-list 80 "value = 001\n")))
             (session (make-incremental-session hcl-v2-24-parser source #t))
             (edit (make-edit 0 0 "other = 002\n")))
        (let-values (((cached receipt) (parse-incremental-session session edit)))
          (parameterize ((current-lr-probe-reuse-enabled? #f))
            (let-values (((control control-receipt) (parse-incremental-session session edit)))
              (check (incremental-session-artifact cached)
                     => (incremental-session-artifact control))
              (check (cdr (assq 'remainingSignificantTokenCount receipt))
                     => (cdr (assq 'remainingSignificantTokenCount control-receipt)))
              (check (> (cdr (assq 'fragmentProbeReuseTokenCount receipt)) 0) => #t)
              (check (cdr (assq 'fragmentProbeReuseTokenCount control-receipt)) => 0)
              (check-projection hcl-v2-24-parser source cached)
              (check-projection hcl-v2-24-parser source control))))))
    (test-case "relative positions compose across edits inside reused suffixes"
      (check-edits (apply string-append (make-list 12 "value = 001\n"))
                   (list (make-edit 0 0 "other = 002\n")
                         (make-edit 36 0 "third = 003\n")
                         (make-edit 0 12 "")
                         (make-edit 24 12 "")
                         (make-edit 0 0 "other = 002\n")
                         (make-edit 0 12 ""))))
    (test-case "cursor maps bulk deletion and empty-source transitions"
      (let (line "value = 001\n")
        (check-edits (apply string-append (make-list 80 line))
                     (list (make-edit 240 84 "")
                           (make-edit 0 0 (apply string-append (make-list 5 "other = 002\n")))
                           (make-edit 0 60 "")
                           (make-edit 240 0 (apply string-append (make-list 7 line)))
                           (make-edit 0 960 "")
                           (make-edit 0 0 (apply string-append (make-list 80 line)))))))
    (test-case "unchanged nested blocks skip their complete grammar interior"
      (let* ((body (apply string-append (make-list 80 "  value = 001\n")))
             (source (string-append "group {\n" body "}\n"))
             (session (make-incremental-session hcl-v2-24-parser source #t)))
        (check (parse-artifact-success? (incremental-session-artifact session)) => #t)
        (let-values (((next receipt)
                      (parse-incremental-session session (make-edit 0 0 "top = 0\n"))))
          (check (incremental-session-artifact next)
                 => (parse-hcl-v2-24 (string-append "top = 0\n" source)))
          (check (> (cdr (assq 'reusedSignificantTokenCount receipt)) 300) => #t)
          (check (< (cdr (assq 'fragmentCursorVisitCount receipt)) 30) => #t)
          (check-projection hcl-v2-24-parser source next))
        (check-edits source
                     (list (make-edit 568 0 "  other = 002\n")
                           (make-edit 568 14 "")
                           (make-edit 0 0 "top = 0\n")
                           (make-edit 0 8 "")))))
    (test-case "UTF8 and lexical boundary changes retain canonical ownership"
      (check-edits "value = \"λ中😀\"\nother = [1, 2]\n"
                   (list (make-edit 0 0 "prefix = 0\n")
                         (make-edit 0 11 "")
                         (make-edit 0 0 "/* λ */ ")
                         (make-edit 0 9 "")))
      (check-edits "value = 1\nother = 2\nthird = 3\n"
                   (list (make-edit 9 0 " + 4")
                         (make-edit 9 4 "")
                         (make-edit 9 0 " /* comment */")
                         (make-edit 9 14 "")
                         (make-edit 0 0 "value = ")
                         (make-edit 0 8 ""))))
    (test-case "UTF8 token leaves move inside actually reused structures"
      (check-edits (apply string-append (make-list 30 "value = \"λ中😀\"\n"))
                   (list (make-edit 0 0 "prefix = 0\n")
                         (make-edit 0 11 ""))))
    (test-case "failed parse clears transferred-work receipts"
      ;; Trivia makes this checkpoint tail large enough to admit the catalog;
      ;; unchanged declarations before the bad final identifier can transfer.
      (let* ((line "value /* a */ = /* b */ 001 /* c */\n")
             (source (apply string-append (make-list 80 line)))
             (edit (make-edit (* 79 (string-length line)) 1 "@"))
             (session (make-incremental-session hcl-v2-24-parser source #t)))
        (let-values (((next receipt) (parse-incremental-session session edit)))
          (check (incremental-session-artifact next)
                 => (parse-hcl-v2-24 (apply-edit source edit)))
          (check (incremental-session-recognition-root next) => #f)
          (check (cdr (assq 'freshFallback? receipt)) => #t)
          (check (assq 'reusedRecognitionFragmentCount receipt) => #f))))
    (test-case "an equivalent grammar in another prepared runtime is rejected"
      (let* ((spec (compile-lr-spec
                    '((source-file (alias SourceFile (token identifier))))
                    'source-file))
             (first (lr-prepare spec)) (second (lr-prepare spec))
             (token (make-token 'identifier "x" 0 1)) (captured #f))
        (parameterize ((current-lr-recognition-observer
                        (lambda (root) (set! captured root))))
          (let-values (((root rest) (lr-parse/prepared first (list token))))
            (check rest => '())))
        (check (lr-recognition-fragment? captured) => #t)
        (check (lr-checkpoint-fragment-compatible?
                (lr-checkpoint-before-shift (lr-initial-checkpoint first '()) token)
                captured) => #t)
        (install-lr-runtime-direct-step! first
          (lambda args (error "rejection test must not execute replacement reducer")))
        (check (lr-checkpoint-fragment-compatible?
                (lr-checkpoint-before-shift (lr-initial-checkpoint first '()) token)
                captured) => #f)
        (check (lr-checkpoint-fragment-compatible?
                (lr-checkpoint-before-shift (lr-initial-checkpoint second '()) token)
                captured) => #f)))
    (test-case "Wagner context-dependent repetition rejects identical public Item kinds"
      ;; A c+ / B d+ shares the X X yield and semantic Item kind, while
      ;; the distant marker selects a different grammar nonterminal/context.
      (let* ((runtime
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
        (check (lr-runtime-fragment-reuse-safe? runtime) => #t)
        (parameterize ((current-lr-recognition-observer
                        (lambda (root) (set! captured root))))
          (let-values (((root rest)
                        (lr-parse/prepared runtime
                          (cons (make-token 'identifier "A" 0 1) tokens))))
            (check rest => '())))
        (let (item
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
                                           (cdr pending))))))))))
          (check (lr-recognition-fragment? item) => #t)
          (let-values (((status after-marker)
                        (lr-checkpoint-feed (lr-initial-checkpoint runtime '())
                                            (make-token 'identifier "B" 0 1))))
            (check status => 'checkpoint)
            (check (lr-checkpoint-fragment-compatible?
                    (lr-checkpoint-before-shift after-marker (car tokens)) item) => #f)))
        (let-values (((root rest)
                      (lr-parse/prepared runtime
                        (cons (make-token 'identifier "B" 0 1) tokens))))
          (check (recognition-node-kind root) => 'SourceFile)
          (check rest => '()))))
    (test-case "deep left recursion projects without recursive Scheme traversal"
      (let* ((source (string-join (make-list 1600 "001") " + "))
             (session (make-incremental-session arithmetic-parser source #t)))
        (check-projection arithmetic-parser source session)
        (check (lr-recognition-fragment-token-count
                (incremental-session-recognition-root session)) => 3199)))
    (test-case "event-only fast paths discard stale grammar snapshots"
      (let ((session (make-incremental-session arithmetic-parser "001 + 002" #t)))
        (let-values (((next ignored) (parse-incremental-session session (make-edit 6 3 "003"))))
          (check (incremental-session-artifact next) => (parse-arithmetic-v1 "001 + 003"))
          (check (incremental-session-recognition-root next) => #f)
          (let-values (((resumed ignored)
                        (parse-incremental-session next (make-edit 0 0 "004 + "))))
            (check-projection arithmetic-parser "004 + 001 + 003" resumed)))))
    (test-case "selective GLR cannot publish a deterministic reuse root"
      (let ((observed 'not-called)
            (runtime
             (lr-prepare
              (compile-lr-spec
               '((source-file (choice (reference first-path) (reference second-path)))
                 (first-path (alias SourceFile (field value (token identifier))))
                 (second-path (alias SourceFile (field value (token identifier)))))
               'source-file 'selective-glr))))
        (parameterize ((current-lr-recognition-observer
                        (lambda (root) (set! observed root))))
          (let-values (((root rest)
                        (lr-parse/prepared runtime (list (make-token 'identifier "x" 0 1)))))
            (check rest => '())))
        (check observed => #f)
        (check (lr-runtime-fragment-reuse-safe? runtime) => #f)))
    (test-case "layout execution retains only its authoritative artifact"
      (let (session
            (make-incremental-session
             tla-plus-layout-parser
             "---- MODULE J ----\nInit ==\n  /\\ TRUE\n  /\\ FALSE\n====\n" #t))
        (check (parse-artifact-success? (incremental-session-artifact session)) => #t)
        (check (incremental-session-recognition-root session) => #f)
        (check (incremental-session-source-index session) => #f)))
    (test-case "invalid input cannot retain a reusable root"
      (let (session (make-incremental-session arithmetic-parser "1 +" #t))
        (check (parse-artifact-success? (incremental-session-artifact session)) => #f)
        (check (incremental-session-recognition-root session) => #f)
        (check (incremental-session-source-index session) => #f)))))

(export recognition-snapshot-test)
