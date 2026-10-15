;;; -*- Gerbil -*-
;;; lr-parser: transfer owner. Private execution state and named algorithms.

(import (only-in ../../compiler/lr
                 production-action production-lhs production-rhs base-symbol)
        (only-in ../funcs
                 recognition-sequence-relocate)
        (only-in ../token
                 token? token-end token-kind token-lexeme token-start)
        (only-in "./state.ss"
                 lr-runtime-table lr-runtime-actions lr-runtime-goto-index lr-runtime-dynamic? lr-runtime-layout? lr-runtime-lexical-modes lr-runtime-direct-step make-lr-checkpoint lr-checkpoint? lr-checkpoint-runtime lr-checkpoint-tokens lr-checkpoint-input-end-offset lr-checkpoint-states lr-checkpoint-semantic-values lr-checkpoint-rest lr-checkpoint-deterministic-actions lr-checkpoint-deterministic-shifts lr-checkpoint-recognition-stack make-lr-prefix-snapshot lr-prefix-snapshot? lr-prefix-snapshot-runtime lr-prefix-snapshot-states lr-prefix-snapshot-semantic-values lr-prefix-snapshot-deterministic-actions lr-prefix-snapshot-deterministic-shifts lr-prefix-snapshot-recognition-stack lr-recognition-fragment? lr-recognition-fragment-runtime lr-recognition-fragment-executor lr-recognition-fragment-production-id lr-recognition-fragment-entry-state lr-recognition-fragment-exit-state lr-recognition-fragment-value lr-recognition-fragment-token-count lr-recognition-fragment-terminal-yield lr-terminal-yield-branch? lr-terminal-yield-branch-left lr-terminal-yield-branch-right lr-terminal-yield-view? lr-terminal-yield-view-base lr-terminal-yield-view-delta current-lr-transfer-yield-observer lr-failure-frontier? lr-failure-frontier-checkpoint)
        (only-in "./recognition.ss"
                 lr-recognition-relocate recognition-piece-end)
        (only-in "./checkpoint-base.ss"
                 lr-rejection-condition?)
        (only-in "./prepare.ss"
                 goto-target)
        (only-in "./checkpoint.ss"
                 lr-run-checkpoint))
(export lr-checkpoint-before-shift
        lr-runtime-fragment-reuse-safe?
        lr-checkpoint-fragment-compatible?
        lr-recognition-fragment-exit-mode
        same-transfer-token?
        fragment-yield-matches?
        lr-checkpoint-inject-fragment
        lr-checkpoint-drive
        lr-checkpoint-drive/contextual
        lr-checkpoint-prefix-snapshot
        lr-prefix-snapshot-rebind
        lr-checkpoint-rebind-suffix
        lr-checkpoint-resume-suffix
        lr-checkpoint-resume
        lr-checkpoint-frontier
        lr-failure-frontier-resume)

(def (lr-checkpoint-before-shift checkpoint token)
  (let-values (((status next)
                (lr-run-checkpoint checkpoint #f #f #t #f #t token
                                   #f #f 'installed #f #t)))
    (and (eq? status 'checkpoint) next)))

(def (lr-runtime-fragment-reuse-safe? runtime)
  (and (not (lr-runtime-layout? runtime)) (not (lr-runtime-dynamic? runtime))
       ;; Admit the repeated-structure family first. Operator ancestry has no
       ;; such concat recurrence and does not justify building a reuse catalog.
       (vector-any
        (lambda (production)
          (let (rhs (production-rhs production))
            (and (eq? (production-action production) 'concat)
                 (= (length rhs) 2)
                 (equal? (base-symbol (car rhs))
                         (list 'nonterminal (production-lhs production))))))
        (lr-runtime-table runtime))
       (not (vector-any
             (lambda (row) (any (lambda (entry) (eq? (cadr entry) 'fork)) row))
             (lr-runtime-actions runtime)))))

(def (lr-checkpoint-fragment-compatible? checkpoint fragment)
  (let ((runtime (lr-checkpoint-runtime checkpoint))
        (state (car (lr-checkpoint-states checkpoint))))
    (and (lr-recognition-fragment? fragment)
         (eq? runtime (lr-recognition-fragment-runtime fragment))
         (eq? (lr-runtime-direct-step runtime) (lr-recognition-fragment-executor fragment))
         (not (lr-runtime-layout? runtime)) (not (lr-runtime-dynamic? runtime))
         (= state (lr-recognition-fragment-entry-state fragment))
         (let (target
               (goto-target (lr-runtime-goto-index runtime) state
                            (production-lhs (vector-ref (lr-runtime-table runtime)
                              (lr-recognition-fragment-production-id fragment)))))
           (and target (= target (lr-recognition-fragment-exit-state fragment)))))))

(def (lr-recognition-fragment-exit-mode fragment)
  (vector-ref (lr-runtime-lexical-modes (lr-recognition-fragment-runtime fragment))
              (lr-recognition-fragment-exit-state fragment)))

;;; Compare retained terminal leaves without projecting the published AST.
;;; Views accumulate byte relocation; pending siblings keep their own delta.
(def (same-transfer-token? actual retained delta)
  (and (token? actual) (token? retained)
       (eq? (token-kind actual) (token-kind retained))
       (equal? (token-lexeme actual) (token-lexeme retained))
       (= (token-start actual) (+ delta (token-start retained)))
       (= (token-end actual) (+ delta (token-end retained)))))

(def (fragment-yield-matches? fragment delta tokens)
  (let ((yield (lr-recognition-fragment-terminal-yield fragment))
        (observer (current-lr-transfer-yield-observer)))
    (def (finish matched? visited)
      (when observer (observer visited))
      matched?)
    (let loop ((pending (if yield (list (cons yield delta)) '())) (rest tokens) (visited 0))
      (if (null? pending) (finish (null? rest) visited)
        (let* ((frame (car pending)) (piece (car frame)) (shift (cdr frame))
               (visited (if observer (+ visited 1) visited)))
          (cond
           ((lr-terminal-yield-view? piece)
            (loop (cons (cons (lr-terminal-yield-view-base piece)
                              (+ shift (lr-terminal-yield-view-delta piece)))
                        (cdr pending)) rest visited))
           ((lr-terminal-yield-branch? piece)
            (loop (cons (cons (lr-terminal-yield-branch-left piece) shift)
                        (cons (cons (lr-terminal-yield-branch-right piece) shift) (cdr pending)))
                  rest visited))
           ((and (pair? rest) (same-transfer-token? (car rest) piece shift))
            (loop (cdr pending) (cdr rest) visited))
           (else (finish #f visited))))))))

;;; The source owner certifies unchanged text and both lexical boundaries.
;;; The LR owner independently verifies the transferred terminal yield.
;;; Transfer one actual nonterminal, not an exported AST kind or an LR suffix.
(def (lr-checkpoint-inject-fragment checkpoint fragment delta significant-tokens)
  (unless (and (lr-checkpoint-fragment-compatible? checkpoint fragment)
               (pair? (lr-checkpoint-rest checkpoint))
               (pair? significant-tokens)
               (same-transfer-token? (car significant-tokens)
                                     (car (lr-checkpoint-rest checkpoint)) 0)
               (positive? (lr-recognition-fragment-token-count fragment))
               (= (length significant-tokens) (lr-recognition-fragment-token-count fragment))
               (fragment-yield-matches? fragment delta significant-tokens))
    (error "uncertified LR fragment transfer"))
  (let* ((piece (lr-recognition-relocate fragment delta))
         (end (recognition-piece-end piece))
         (count (lr-recognition-fragment-token-count fragment)))
    (make-lr-checkpoint
     (lr-checkpoint-runtime checkpoint)
     (append (reverse significant-tokens) (cdr (lr-checkpoint-tokens checkpoint)))
     (max (lr-checkpoint-input-end-offset checkpoint) end)
     (cons (lr-recognition-fragment-exit-state fragment) (lr-checkpoint-states checkpoint))
     (cons (recognition-sequence-relocate (lr-recognition-fragment-value fragment) delta)
           (lr-checkpoint-semantic-values checkpoint))
     '() (+ 1 (lr-checkpoint-deterministic-actions checkpoint))
     (+ count (lr-checkpoint-deterministic-shifts checkpoint))
     (and (lr-checkpoint-recognition-stack checkpoint)
          (cons piece (lr-checkpoint-recognition-stack checkpoint))))))

;;; Drives one deterministic LR loop while the source owner supplies tokens
;;; under the current lexical mode. The callback records shifted source tokens.
;;; A fork returns its exact checkpoint for the existing selective-GLR handoff.
;;; The optional override gives conformance tests and matched benchmarks an
;;; indexed baseline without mutating the installed language machine.
(def (lr-checkpoint-drive checkpoint next-input after-shift
                          (observability #f)
                          (direct-step-override 'installed)
                          (snapshot-spacing #f) (capture-snapshot #f))
  (when capture-snapshot
    (unless (and (procedure? capture-snapshot) (integer? snapshot-spacing) (> snapshot-spacing 0))
      (error "streamed checkpoint capture requires positive spacing")))
  (lr-run-checkpoint
   checkpoint #f observability #f #f #t #f
   next-input after-shift direct-step-override #f #f snapshot-spacing capture-snapshot))

;;; Contextual scanning needs the exact LR state as its syntactic position
;;; axis. Existing lexer drivers keep the one-argument callback contract.
(def (lr-checkpoint-drive/contextual checkpoint next-input after-shift
                                     (observability #f))
  (lr-run-checkpoint
   checkpoint #f observability #f #f #t #f
   next-input after-shift 'installed #t))

;;; Rebinds an unconsumed suffix to the same immutable deterministic frontier.
;;; Streaming GLR handoff and explicit incremental sessions share this path.
(def (lr-checkpoint-prefix-snapshot checkpoint)
  (unless (lr-checkpoint? checkpoint)
    (error "LR prefix snapshot requires a checkpoint" checkpoint))
  (make-lr-prefix-snapshot
   (lr-checkpoint-runtime checkpoint)
   (lr-checkpoint-states checkpoint)
   (lr-checkpoint-semantic-values checkpoint)
   (lr-checkpoint-deterministic-actions checkpoint)
   (lr-checkpoint-deterministic-shifts checkpoint)
   (lr-checkpoint-recognition-stack checkpoint)))

(def (lr-prefix-snapshot-rebind snapshot tokens rest
                                (input-end-offset #f))
  (unless (lr-prefix-snapshot? snapshot)
    (error "LR prefix rebind requires a snapshot" snapshot))
  (make-lr-checkpoint
   (lr-prefix-snapshot-runtime snapshot)
   tokens
   (or input-end-offset
       (fold (lambda (input-token offset)
               (max offset (token-end input-token)))
             0 tokens))
   (lr-prefix-snapshot-states snapshot)
   (lr-prefix-snapshot-semantic-values snapshot)
   rest
   (lr-prefix-snapshot-deterministic-actions snapshot)
   (lr-prefix-snapshot-deterministic-shifts snapshot)
   (lr-prefix-snapshot-recognition-stack snapshot)))

(def (lr-checkpoint-rebind-suffix checkpoint tokens rest)
  (lr-prefix-snapshot-rebind
   (lr-checkpoint-prefix-snapshot checkpoint) tokens rest))

(def (lr-checkpoint-resume-suffix checkpoint tokens rest (observability #f))
  (lr-checkpoint-resume
   (lr-checkpoint-rebind-suffix checkpoint tokens rest) observability))

;;; Completes parsing from an initial or advanced immutable checkpoint.
(def (lr-checkpoint-resume checkpoint (observability #f))
  (let-values (((status payload)
                (lr-run-checkpoint checkpoint #f observability)))
    (unless (eq? status 'accepted)
      (error "LR resume did not reach a terminal result" status))
    (values (car payload) (cadr payload))))

;;; Runs to acceptance or to the first deterministic LR failure frontier. An
;;; admitted GLR fork still delegates to the selective-GLR owner.
(def (lr-checkpoint-frontier checkpoint (observability #f))
  (lr-run-checkpoint checkpoint #f observability #t))

;;; Tests one edited suffix from the retained failure state. The returned CST
;;; remains private to recovery; only acceptance is exposed.
(def (lr-failure-frontier-resume frontier edited-rest (observability #f))
  (unless (lr-failure-frontier? frontier)
    (error "LR recovery requires a failure frontier" frontier))
  (let* ((checkpoint (lr-failure-frontier-checkpoint frontier))
         (edited
          (make-lr-checkpoint
           (lr-checkpoint-runtime checkpoint)
           (lr-checkpoint-tokens checkpoint)
           (lr-checkpoint-input-end-offset checkpoint)
           (lr-checkpoint-states checkpoint)
           (lr-checkpoint-semantic-values checkpoint)
           edited-rest
           (lr-checkpoint-deterministic-actions checkpoint)
           (lr-checkpoint-deterministic-shifts checkpoint) #f)))
    (with-catch
     (lambda (condition)
       (if (lr-rejection-condition? condition)
         #f
         (raise condition)))
     (lambda ()
       (let-values (((_root rest) (lr-checkpoint-resume edited observability)))
         (null? rest))))))

