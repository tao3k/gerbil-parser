;;; Scalar admission is compared with the same executor's borrowed-list policy.
(import :std/test
        (only-in :gerbil-parser/t/fixtures/runtime-record-assignments/languages/records/grammar records-parser)
        (only-in :gerbil-parser/src/compiler/machine parser-machine-runtime parser-machine-trivia parser-machine-with-lr-action-selector
                 parser-machine-grammar-digest install-parser-machine-direct-step!)
        (only-in :gerbil-parser/src/compiler/lr-compiler compile-lr-spec)
        (only-in :gerbil-parser/src/compiler/lr production-lhs)
        (only-in :gerbil-parser/src/runtime/reduction-plan reduce-lr-stack-plan)
        (only-in :gerbil-parser/src/runtime/lr-parser/prepare make-lr-action-selector goto-target)
        (only-in :gerbil-parser/src/runtime/lexer lex-source)
        (only-in :gerbil-parser/src/runtime/parser parse-source parse-source/checkpoints)
        (only-in :gerbil-parser/src/runtime/token make-token token-start)
        (only-in :gerbil-parser/src/runtime/lr-parser lr-initial-checkpoint lr-prepare lr-lexical-mode-id current-lr-recognition-observer)
        (only-in :gerbil-parser/src/runtime/lr-parser/checkpoint-base lr-initial-checkpoint/selected)
        (only-in :gerbil-parser/src/runtime/lr-stack-storage make-lr-token-observer)
        (only-in :gerbil-parser/src/runtime/lr-parser/state lr-checkpoint-states lr-checkpoint-runtime lr-checkpoint-tokens lr-checkpoint-input-end-offset
                 lr-checkpoint-semantic-values lr-checkpoint-rest lr-checkpoint-deterministic-actions
                 lr-checkpoint-deterministic-shifts lr-checkpoint-recognition-stack lr-runtime-table lr-runtime-reduction-widths lr-runtime-reduction-plans)
        (only-in :gerbil-parser/src/runtime/lr-parser/checkpoint
                 lr-run-checkpoint lr-run-persistent-checkpoint))
(export lr-input-protocol-test)
(def (observe-input-driver driver runtime tokens budget fork?)
  (let ((pending tokens) (trace '()))
    (def (next-input mode)
      (set! trace (cons (list 'scan (lr-lexical-mode-id mode) (and (pair? pending) (car pending))) trace))
      (if (null? pending) #f
        (let (token (car pending)) (set! pending (cdr pending)) token)))
    (let (result
          (call-with-values
           (lambda ()
             (driver (lr-initial-checkpoint runtime '()) budget #f #t #f fork? #f next-input
               (lambda (token _states _values actions shifts)
                 (set! trace (cons (list 'shift token actions shifts) trace))))) list))
      (list result pending (reverse trace)))))
(def (check-input-drivers runtime tokens budget fork?)
  (check (observe-input-driver lr-run-checkpoint runtime tokens budget fork?)
         => (observe-input-driver lr-run-persistent-checkpoint runtime tokens budget fork?)))
(def (observe-captured-buffers driver tokens)
  (let ((pending tokens) (frames '()) (root #f) (shifted '()))
    (parameterize ((current-lr-recognition-observer (lambda (value) (set! root value))))
      (let (result
            (call-with-values
             (lambda ()
               (driver (lr-initial-checkpoint (parser-machine-runtime records-parser) '()) #f #f #t #f #t #f
                 (lambda (_mode)
                   (if (null? pending) #f
                     (let (token (car pending)) (set! pending (cdr pending)) token)))
                 (make-lr-token-observer (lambda (token) (set! shifted (cons token shifted))))
                 'installed #f #f 2 (lambda (checkpoint) (set! frames (cons checkpoint frames))))) list))
        ;; Inspect every earlier frozen frontier after all later buffer writes.
        (list result root (reverse shifted) (reverse frames))))))
(def lr-input-protocol-test
  (test-suite "LR scalar input protocol and external frontiers"
    (test-case "selected entry resets request fields and scans every token end"
      (let ((runtime (parser-machine-runtime records-parser))
            (tokens (list (make-token 'word "first" 18 23)
                          (make-token 'word "second" 0 4)
                          (make-token 'word "last" 12 17))))
        (for-each
         (lambda (observer)
           (parameterize ((current-lr-recognition-observer observer))
             (let* ((first (lr-initial-checkpoint/selected runtime tokens))
                    (empty (lr-initial-checkpoint/selected runtime '()))
                    (again (lr-initial-checkpoint/selected runtime tokens)))
               (for-each
                (lambda (checkpoint)
                  (check (eq? (lr-checkpoint-runtime checkpoint) runtime) => #t)
                  (check (lr-checkpoint-states checkpoint) => '(0))
                  (check (lr-checkpoint-semantic-values checkpoint) => '())
                  (check (list (lr-checkpoint-deterministic-actions checkpoint)
                               (lr-checkpoint-deterministic-shifts checkpoint)) => '(0 0))
                  (check (lr-checkpoint-recognition-stack checkpoint) => (if observer '() #f)))
                (list first empty again))
               (check (eq? first again) => #f)
               (check (eq? (lr-checkpoint-tokens first) tokens) => #t)
               (check (eq? (lr-checkpoint-rest first) tokens) => #t)
               (check (eq? (lr-checkpoint-rest again) tokens) => #t)
               (check (lr-checkpoint-rest empty) => '())
               (check (map lr-checkpoint-input-end-offset (list first empty again)) => '(23 0 23)))))
         (list #f (lambda (_value) (void))))))
    (test-case "action budgets preserve pending lookahead, scanner mode and callbacks"
      (let ((runtime (parser-machine-runtime records-parser))
            (tokens (filter (lambda (token) (not ((parser-machine-trivia records-parser) token)))
                            (lex-source records-parser "a=1\nb=2\n"))))
        (for-each (lambda (budget) (check-input-drivers runtime tokens budget #t)) '(0 1 2 3 4 8 32 #f))))
    (test-case "captured owned buffers preserve every earlier immutable frontier"
      (let (tokens (filter (lambda (token) (not ((parser-machine-trivia records-parser) token)))
                          (lex-source records-parser (apply string-append (make-list 64 "a=1\n")))))
        (let* ((owned (observe-captured-buffers lr-run-checkpoint tokens))
               (persistent (observe-captured-buffers lr-run-persistent-checkpoint tokens))
               (frames (cadddr owned))
               (last-cell (lambda (values)
                            (let loop ((values values))
                              (if (null? (cdr values)) values (loop (cdr values)))))))
          (check owned => persistent)
          ;; A genuine owned execution copies each frozen state spine. The
          ;; reference shares its unchanged root cell across these frontiers.
          (check (eq? (last-cell (lr-checkpoint-states (car frames)))
                      (last-cell (lr-checkpoint-states (cadr frames)))) => #f))))
    (test-case "generated steps retain one borrowed lookahead tail across reductions"
      (let* ((machine (parser-machine-with-lr-action-selector records-parser make-lr-action-selector))
             (runtime (parser-machine-runtime machine))
             (table (lr-runtime-table runtime)) (widths (lr-runtime-reduction-widths runtime))
             (plans (lr-runtime-reduction-plans runtime))
             (previous-token #f) (previous-rest #f) (reuses 0)
             (source "a=1\nb=2\n"))
        (install-parser-machine-direct-step! machine (parser-machine-grammar-digest machine)
          (lambda (production states semantic-values rest input-end gotos)
            (when (pair? rest)
              (when (eq? (car rest) previous-token)
                (check (eq? rest previous-rest) => #t)
                (set! reuses (+ reuses 1)))
              (set! previous-token (car rest)) (set! previous-rest rest))
            (let* ((width (vector-ref widths production))
                   (remaining-states (list-tail states width))
                   (remaining-values (list-tail semantic-values width))
                   (offset (if (pair? rest) (token-start (car rest)) input-end))
                   (value (reduce-lr-stack-plan (vector-ref plans production) semantic-values offset))
                   (target (goto-target gotos (car remaining-states) (production-lhs (vector-ref table production)))))
              (values target (cons target remaining-states) (cons value remaining-values)))))
        (let-values (((artifact _tokens _modes _frames) (parse-source/checkpoints machine source 2)))
          (check artifact => (parse-source records-parser source))
          (check (> reuses 0) => #t))))
    (test-case "failure and EOF preserve materialized checkpoint rest"
      (for-each (lambda (tokens)
                  (check-input-drivers (parser-machine-runtime records-parser) tokens #f #t))
                (list '() (list (make-token 'missing "?" 0 1)))))
    (test-case "fork exposes unconsumed input and fallback completes without replay"
      (let* ((rules '((source-file (choice (reference first-path) (reference second-path)))
                      (first-path (alias SourceFile (field value (token identifier))))
                      (second-path (alias SourceFile (field value (token identifier))))))
             (runtime (lr-prepare (compile-lr-spec rules 'source-file 'selective-glr)))
             (tokens (list (make-token 'identifier "x" 0 1))))
        (check-input-drivers runtime tokens #f #t)
        (check-input-drivers runtime tokens #f #f)))))
