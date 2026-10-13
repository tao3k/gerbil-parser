;;; -*- Gerbil -*-
;;; lr-parser: recognition owner. Private execution state and named algorithms.

(import (only-in ../recognition
                 make-recognition-child make-recognition-fragment)
        (only-in ../funcs
                 current-recognition-sequence-fusion-enabled?)
        (only-in ../token
                 make-token token? token-end token-kind token-lexeme token-start)
        (only-in "./state.ss"
                 lr-runtime-table lr-runtime-reduction-widths make-lr-recognition-fragment lr-recognition-fragment? lr-recognition-fragment-runtime lr-recognition-fragment-production-id lr-recognition-fragment-children lr-recognition-fragment-start lr-recognition-fragment-end lr-recognition-fragment-token-count lr-recognition-fragment-offset lr-recognition-fragment-terminal-yield make-lr-recognition-view lr-recognition-view? lr-recognition-view-base lr-recognition-view-delta make-lr-terminal-yield-branch make-lr-terminal-yield-view lr-terminal-yield-view? lr-terminal-yield-view-base lr-terminal-yield-view-delta)
        (only-in "./reduction.ss"
                 reduce-value))
(export lr-recognition-relocate
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

(def (capture-reduction runtime executor production-id states stack rest offset value target)
  (if (not stack) #f
    (let* ((count (vector-ref (lr-runtime-reduction-widths runtime) production-id))
           (children (reverse (take stack count)))
           (remaining (drop stack count))
           (entry-state (list-ref states count))
           (lookahead (if (pair? rest)
                        (cons (token-kind (car rest)) (token-lexeme (car rest)))
                        #f))
           (node (make-lr-recognition-fragment
                  runtime executor production-id entry-state target children lookahead value
                  (if (pair? children) (recognition-piece-start (car children)) offset)
                  (if (pair? children) (recognition-piece-end (last children)) offset)
                  (foldl (lambda (piece count)
                           (+ count (recognition-piece-token-count piece)))
                         0 children) offset
                  (foldl (lambda (piece yield)
                           (terminal-yield-append yield (recognition-piece-terminal-yield piece)))
                         #f children))))
      (cons node remaining))))

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

