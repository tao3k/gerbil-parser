;;; -*- Gerbil -*-
;;; lr-parser: recognition owner. Private execution state and named algorithms.

(import (only-in ../recognition
                 make-recognition-child make-recognition-fragment)
        (only-in ../funcs
                 current-recognition-sequence-fusion-enabled?)
        (only-in ../token
                 make-token token? token-end token-kind token-lexeme token-start)
        (only-in "./state.ss"
                 lr-capture-plan make-lr-capture-plan lr-runtime-capture-plans lr-runtime-capture-plans-set! lr-runtime-actions make-lr-capture-context lr-capture-context lr-capture-context-executor lr-capture-context-exit-state lr-runtime-table lr-runtime-reduction-widths make-lr-recognition-fragment lr-recognition-fragment lr-recognition-fragment? lr-recognition-fragment-runtime lr-recognition-fragment-production-id lr-recognition-fragment-children lr-recognition-fragment-start lr-recognition-fragment-end lr-recognition-fragment-token-count lr-recognition-fragment-offset lr-recognition-fragment-yield-cache lr-recognition-fragment-yield-cache-set! make-lr-recognition-view lr-recognition-view? lr-recognition-view-base lr-recognition-view-delta make-lr-terminal-yield-branch make-lr-terminal-yield-view lr-terminal-yield-view? lr-terminal-yield-view-base lr-terminal-yield-view-delta)
        (only-in "./reduction.ss"
                 reduce-value))
(export prepare-capture-plans capture-context-from-plan
        lr-recognition-fragment-terminal-yield
        lr-recognition-relocate
        terminal-yield-append
        terminal-yield-relocate
        recognition-piece-terminal-yield
        recognition-piece-start
        recognition-piece-end
        recognition-piece-token-count
        capture-reduction
        lr-recognition-project
        lr-recognition-project/materialized)

(def (lr-recognition-relocate piece delta)
  (if (lr-recognition-view? piece)
    (make-lr-recognition-view (lr-recognition-view-base piece)
                             (+ delta (lr-recognition-view-delta piece)))
    (make-lr-recognition-view piece delta)))

(def (terminal-yield-append left right)
  (cond ((not left) right) ((not right) left)
        (else (make-lr-terminal-yield-branch left right))))

(def (terminal-yield-relocate yield delta)
  (cond ((or (not yield) (zero? delta)) yield)
        ((lr-terminal-yield-view? yield)
         (make-lr-terminal-yield-view (lr-terminal-yield-view-base yield)
                                     (+ delta (lr-terminal-yield-view-delta yield))))
        (else (make-lr-terminal-yield-view yield delta))))

(def (recognition-piece-terminal-yield piece)
  (cond ((lr-recognition-view? piece)
         (terminal-yield-relocate
          (recognition-piece-terminal-yield (lr-recognition-view-base piece))
          (lr-recognition-view-delta piece)))
        ((token? piece) piece)
        (else (lr-recognition-fragment-terminal-yield piece))))

(def (recognition-piece-start piece)
  (cond ((lr-recognition-view? piece)
         (+ (lr-recognition-view-delta piece)
            (recognition-piece-start (lr-recognition-view-base piece))))
        ((token? piece) (token-start piece))
        (else (lr-recognition-fragment-start piece))))

(def (recognition-piece-end piece)
  (cond ((lr-recognition-view? piece)
         (+ (lr-recognition-view-delta piece)
            (recognition-piece-end (lr-recognition-view-base piece))))
        ((token? piece) (token-end piece))
        (else (lr-recognition-fragment-end piece))))

(def (recognition-piece-token-count piece)
  (cond ((lr-recognition-view? piece)
         (recognition-piece-token-count (lr-recognition-view-base piece)))
        ((token? piece) 1)
        (else (lr-recognition-fragment-token-count piece))))

;;; Terminal transfer queries own this derived index. Fresh recognition already
;;; retains immutable grammar topology and exact lookahead; building a second
;;; terminal tree before anyone requests it adds allocation and GC pressure.
;;; The cache is published only when complete. An explicit postorder stack
;;; handles deep left-recursive grammar trees without a Scheme call chain.
(def (lr-recognition-fragment-terminal-yield fragment)
  (when (void? (lr-recognition-fragment-yield-cache fragment))
    (let walk ((pending (list (cons fragment #f))))
      (unless (null? pending)
        (let* ((frame (car pending)) (piece (car frame)) (ready? (cdr frame))
               (rest (cdr pending)))
          (cond
           ((lr-recognition-view? piece)
            (walk (cons (cons (lr-recognition-view-base piece) #f) rest)))
           ((token? piece) (walk rest))
           ((not (void? (lr-recognition-fragment-yield-cache piece))) (walk rest))
           (ready?
            (lr-recognition-fragment-yield-cache-set! piece
              (foldl (lambda (child yield)
                       (terminal-yield-append yield (recognition-piece-terminal-yield child)))
                     #f (lr-recognition-fragment-children piece)))
            (walk rest))
           (else
            (walk (append (map (lambda (child) (cons child #f))
                               (lr-recognition-fragment-children piece))
                          (cons (cons piece #t) rest)))))))))
  (lr-recognition-fragment-yield-cache fragment))

(defrule (with-capture-bounds expression (start end tokens) body ...)
  (let (piece expression)
    (cond
     ((lr-recognition-view? piece)
      (let ((start (recognition-piece-start piece)) (end (recognition-piece-end piece))
            (tokens (recognition-piece-token-count piece))) body ...))
     ((token? piece)
      (let ((start (token-start piece)) (end (token-end piece)) (tokens 1)) body ...))
     (else
      ;; Official checked class binding admits the final record once; its
      ;; slot reads then share that fact, as std/struct/pqueue does.
      (using (piece : lr-recognition-fragment)
        (let ((start piece.start) (end piece.end) (tokens piece.token-count)) body ...))))))

;;; A prepared immutable production plan carries static capture input once.
;;; Only the bounded row cells mutate. Uncaptured execution prepares nothing.
(def (prepare-capture-plans runtime)
  (let (cached (lr-runtime-capture-plans runtime))
    (if (void? cached)
      (let* ((width (vector-length (lr-runtime-table runtime)))
             (states (vector-length (lr-runtime-actions runtime)))
             (widths (lr-runtime-reduction-widths runtime))
             (indexed? (<= (* 2 width states) 65536))
             (plans (list->vector
                     (map (lambda (production)
                            (make-lr-capture-plan runtime production (vector-ref widths production)
                              (and indexed? (make-vector (* 2 states) #f))))
                          (iota width)))))
        (lr-runtime-capture-plans-set! runtime plans) plans)
      cached)))

(definline (capture-context-from-plan (plan : lr-capture-plan) executor entry target kind) => :t
  ;; Disjoint halves preserve custom reuse without weakening canonical admission.
  ;; Only canonical goto transitions enter the first half of a bounded row.
  (if executor
    (let* ((row plan.contexts)
           (index (and row
                    (let (states (quotient (vector-length row) 2))
                      (unless (and (exact-integer? entry) (<= 0 entry) (< entry states))
                        (error "custom capture entry is outside its prepared row" entry))
                      (+ entry states))))
           (context (and row (vector-ref row index))))
      (if (and context
               (using (context : lr-capture-context)
                 (and (eq? executor context.executor) (eqv? target context.exit-state)
                      (eq? kind context.lookahead-kind))))
        context
        (let (next (make-lr-capture-context plan.runtime executor plan.production-id entry target kind))
          (when row (vector-set! row index next)) next)))
    (let* ((row plan.contexts) (context (and row (vector-ref row entry))))
      (if (and context (using (context : lr-capture-context) (eq? kind context.lookahead-kind)))
        context
        (let (next (make-lr-capture-context plan.runtime #f plan.production-id entry target kind))
          (when row (vector-set! row entry next)) next)))))

(definline (capture-reduction plan executor entry-state stack input offset value target) => :t
  (if (not stack) #f
    (using (plan : lr-capture-plan)
      (let* ((count plan.width)
             (kind (and input (token-kind input)))
             (lookahead (and input (token-lexeme input))))
        ;; The pending boundary representation reserves false for EOF and pairs
        ;; for a published signature. Admit its live token values before storage.
        (when (and input (not (and (symbol? kind) (string? lookahead))))
          (error "capture lookahead requires symbolic kind and string lexeme" input))
        (let (context (capture-context-from-plan plan executor entry-state target kind))
          (case count
            ((0)
             (cons (make-lr-recognition-fragment
                    context '() lookahead value
                    offset offset 0 offset #f) stack))
            ((1)
             (let (piece (car stack))
               (with-capture-bounds piece (start end tokens)
                 (cons (make-lr-recognition-fragment
                        context piece lookahead value
                        start end tokens offset (void)) (cdr stack)))))
            (else
             ;; No terminal-index demand occurs in this bounds/count traversal.
             (let split ((left count) (remaining stack) (children '())
                         (tokens 0) (start offset) (end offset))
               (if (zero? left)
                 (cons (make-lr-recognition-fragment
                        context children lookahead value
                        start end tokens offset (void)) remaining)
                 (let (piece (car remaining))
                   (with-capture-bounds piece (piece-start piece-end piece-count)
                     (split (- left 1) (cdr remaining) (cons piece children)
                            (+ tokens piece-count) piece-start
                            (if (= left count) piece-end end)))))))))))))

;;; Iterative postorder projection reevaluates semantic actions from retained
;;; productions and token leaves. It deliberately does not trust cached values.
;;; Left-recursive trees therefore do not consume the Scheme call stack.
;;; Keep the independent replay on the materialized semantic path.
(def (lr-recognition-project root (source-tokens #f))
  (parameterize ((current-recognition-sequence-fusion-enabled? #f))
    (lr-recognition-project/materialized root source-tokens)))

(def (lr-recognition-project/materialized root source-tokens)
  (unless (or (lr-recognition-fragment? root) (lr-recognition-view? root))
    (error "recognition projection requires a grammar fragment" root))
  (let (tokens-by-start (and source-tokens (make-table test: eqv?)))
    (when tokens-by-start
      (for-each (lambda (token) (table-set! tokens-by-start (token-start token) token))
                source-tokens))
    (let loop ((pending (list (vector #f root 0))) (values-stack '()))
      (if (null? pending)
        (if (and (pair? values-stack) (null? (cdr values-stack)))
          (car values-stack) (error "invalid recognition projection stack"))
        (let* ((frame (car pending)) (piece (vector-ref frame 1))
               (delta (vector-ref frame 2)))
          (cond
           ((lr-recognition-view? piece)
            (loop (cons (vector #f (lr-recognition-view-base piece)
                                (+ delta (lr-recognition-view-delta piece))) (cdr pending))
                  values-stack))
           ((token? piece)
            (let (token
                  (if tokens-by-start
                    (table-ref tokens-by-start (+ delta (token-start piece)) #f)
                    (if (zero? delta) piece
                      (make-token (token-kind piece) (token-lexeme piece)
                                  (+ delta (token-start piece)) (+ delta (token-end piece))))))
              (unless (and token (eq? (token-kind token) (token-kind piece))
                           (equal? (token-lexeme token) (token-lexeme piece))
                           (= (token-end token) (+ delta (token-end piece))))
                (error "grammar position view does not bind the current token"))
              (loop (cdr pending)
                    (cons (list (make-recognition-child #f token)) values-stack))))
           ((vector-ref frame 0)
            (let* ((count (length (lr-recognition-fragment-children piece)))
                   (children (reverse (take values-stack count)))
                   (runtime (lr-recognition-fragment-runtime piece))
                   (production (vector-ref (lr-runtime-table runtime)
                                           (lr-recognition-fragment-production-id piece)))
                   (value (reduce-value production children
                                        (+ delta (lr-recognition-fragment-offset piece))
                                        make-recognition-fragment)))
              (loop (cdr pending) (cons value (drop values-stack count)))))
           (else
            (loop (append (map (lambda (child) (vector #f child delta))
                               (lr-recognition-fragment-children piece))
                          (cons (vector #t piece delta) (cdr pending)))
                  values-stack))))))))

