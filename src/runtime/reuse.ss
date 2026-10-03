;;; -*- Gerbil -*-
;;; Source-owner certificates for deterministic nonterminal transfers.
(import (only-in ../compiler/machine parser-machine-runtime parser-machine-trivia)
        (only-in ./lexer scan-source-token)
        (only-in ./probe make-source-probe-cache source-probe-scan source-probe-take!)
        (only-in ./token make-token token? token-start token-end token-kind token-lexeme)
        (only-in ./lr-parser
                 lr-checkpoint-lexical-mode lr-checkpoint-state lr-lexical-mode-id
                 lr-checkpoint-before-shift lr-checkpoint-fragment-compatible?
                 lr-checkpoint-inject-fragment lr-recognition-fragment-exit-mode
                 lr-recognition-fragment? lr-recognition-fragment-entry-state lr-recognition-fragment-runtime
                 lr-recognition-fragment-start lr-recognition-fragment-end
                 lr-recognition-fragment-offset lr-recognition-fragment-token-count
                 lr-recognition-fragment-lookahead lr-recognition-fragment-children
                 lr-recognition-view? lr-recognition-view-base lr-recognition-view-delta))
(export make-fragment-reuser current-lr-probe-reuse-enabled?)
(def current-lr-probe-reuse-enabled? (make-parameter #t))

;;; Stats: fragments, significant tokens, source tokens, source bytes,
;;; scanner-probe bytes, rejected candidates, normalized control probes,
;;; consumed probe tokens/bytes, inspected grammar-cursor frames.
;;; Vectors, cursor and single probe slot live only during this source edit.
(def (make-fragment-reuser machine source old-tokens old-modes root edit-start edit-end delta restart-byte)
  (let* ((tokens (list->vector old-tokens)) (modes (list->vector old-modes))
         (count (vector-length tokens)) (trivia? (parser-machine-trivia machine))
         (stats (make-vector 10 0)) (pending (list (cons root 0)))
         (first-index 0) (source-length (string-length source))
         (probe-cache (and (current-lr-probe-reuse-enabled?)
                           (make-source-probe-cache machine source))))
    (unless (= count (vector-length modes)) (error "invalid captured lexical modes"))
    (def (add-stat! index value)
      (vector-set! stats index (+ (vector-ref stats index) value)))
    (def (advance!) (set! pending (cdr pending)))
    (def (descend! piece position-delta)
      (set! pending
            (append (map (lambda (child) (cons child position-delta))
                         (lr-recognition-fragment-children piece))
                    (cdr pending))))
    ;; A conservative end mapping permits skipping complete passed subtrees.
    ;; Positions inside deleted text collapse to the changed range, not before it.
    (def (mapped-end end)
      (cond ((<= end edit-start) end)
            ((< end edit-end) edit-start)
            (else (+ end delta))))
    (def (same-token? fresh old shift)
      (and (= (token-start fresh) (+ shift (token-start old)))
           (= (token-end fresh) (+ shift (token-end old)))
           (eq? (token-kind fresh) (token-kind old))
           (equal? (token-lexeme fresh) (token-lexeme old))))
    (def (probe character byte mode)
      (with-catch (lambda (_condition) #f)
        (lambda ()
          (let-values (((token next-character)
                        (if probe-cache
                          (source-probe-scan probe-cache character byte mode)
                          (scan-source-token machine source character byte mode))))
            (add-stat! 4 (- (token-end token) byte))
            (cons token next-character)))))
    (def (boundary-matches? piece position-delta character byte eof-offset)
      (let ((expected (lr-recognition-fragment-lookahead piece))
            (mode (lr-recognition-fragment-exit-mode piece)))
        (let loop ((character character) (byte byte))
          (if (= character source-length)
            (and (not expected)
                 (= eof-offset (+ position-delta (lr-recognition-fragment-offset piece))))
            (let (scanned (probe character byte mode))
              (and scanned
                   (let (token (car scanned))
                     (if (trivia? token)
                       (loop (cdr scanned) (token-end token))
                       (and expected
                            (= (token-start token)
                               (+ position-delta (lr-recognition-fragment-offset piece)))
                            (eq? (token-kind token) (car expected))
                            (equal? (token-lexeme token) (cdr expected)))))))))))
    ;; Source-token lookup advances monotonically. Rejected ancestor attempts
    ;; leave its first index in place so a smaller same-start node can retry.
    (def (seek-token! start)
      (let loop ()
        (when (and (< first-index count)
                   (< (token-start (vector-ref tokens first-index)) start))
          (set! first-index (+ first-index 1)) (loop)))
      (and (< first-index count)
           (= (token-start (vector-ref tokens first-index)) start) first-index))
    (def (try-fragment piece position-delta shift old-start old-end
                       character byte mode first-probe normalized)
      (let (first (seek-token! old-start))
        (and first
             (not (trivia? (vector-ref tokens first)))
             (= (lr-lexical-mode-id mode) (vector-ref modes first))
             (same-token? (car first-probe) (vector-ref tokens first) shift)
             (let collect ((index first) (segment '()) (segment-modes '())
                           (significant '()) (significant-count 0)
                           (characters 0) (eof-offset byte))
               (if (and (< index count) (<= (token-end (vector-ref tokens index)) old-end))
                 (let* ((old (vector-ref tokens index))
                        (token (if (zero? shift) old
                                 (make-token (token-kind old) (token-lexeme old)
                                   (+ shift (token-start old)) (+ shift (token-end old)))))
                        (trivia-token? (trivia? token))
                        (next-index (+ index 1))
                        (next-segment (cons token segment))
                        (next-modes (cons (vector-ref modes index) segment-modes))
                        (next-significant (if trivia-token? significant (cons token significant)))
                        (next-count (+ significant-count (if trivia-token? 0 1)))
                        (next-characters (+ characters (string-length (token-lexeme token))))
                        (next-eof-offset (if trivia-token? eof-offset (token-end token))))
                   (if (= (token-end old) old-end)
                     (let ((next-character (+ character next-characters))
                           (next-byte (+ old-end shift)))
                       (and (= next-count (lr-recognition-fragment-token-count piece))
                            (boundary-matches? piece position-delta next-character next-byte next-eof-offset)
                            (begin
                              (add-stat! 0 1) (add-stat! 1 next-count)
                              (add-stat! 2 (- next-index first)) (add-stat! 3 (- next-byte byte))
                              (set! first-index next-index)
                              (vector
                               (lr-checkpoint-inject-fragment normalized piece position-delta
                                                              (reverse next-significant))
                               (reverse next-segment) (reverse next-modes) next-character next-byte))))
                     (collect next-index next-segment next-modes next-significant next-count
                              next-characters next-eof-offset)))
                 #f)))))
    (values
     (lambda (character byte checkpoint)
       (let ((mode (lr-checkpoint-lexical-mode checkpoint))
             (first-probe #f) (normalized #f) (normalization-tried? #f))
         (let seek ()
           (and (pair? pending)
                (let* ((frame (car pending)) (piece (car frame)) (position-delta (cdr frame)))
                  (add-stat! 9 1)
                  (cond
                   ((lr-recognition-view? piece)
                    (set! pending
                          (cons (cons (lr-recognition-view-base piece)
                                      (+ position-delta (lr-recognition-view-delta piece)))
                                (cdr pending)))
                    (seek))
                   ((or (not (lr-recognition-fragment? piece))
                        (< (lr-recognition-fragment-token-count piece) 2))
                    (advance!) (seek))
                   (else
                    (let* ((old-start (+ position-delta (lr-recognition-fragment-start piece)))
                           (old-end (+ position-delta (lr-recognition-fragment-end piece)))
                           (offset (+ position-delta (lr-recognition-fragment-offset piece)))
                           (suffix? (>= old-start edit-end))
                           (prefix? (and (<= old-end edit-start) (<= offset edit-start)))
                           (shift (if suffix? delta 0))
                           (start (+ old-start shift)))
                      (cond
                       ((<= (mapped-end old-end) (max byte restart-byte))
                        (advance!) (seek))
                       ;; A future unedited subtree must remain available when
                       ;; ordinary scanning reaches it. An overlapping ancestor
                       ;; can only descend; it cannot transfer across the edit.
                       ((and (or prefix? suffix?) (> start byte)) #f)
                       ((or (not (or prefix? suffix?)) (< start byte) (<= old-end old-start))
                        (descend! piece position-delta) (seek))
                       (else
                        (unless normalization-tried?
                          (set! normalization-tried? #t)
                          (set! first-probe (probe character byte mode))
                          (when first-probe
                            (add-stat! 6 1)
                            (set! normalized
                                  (lr-checkpoint-before-shift checkpoint (car first-probe)))))
                        (and normalized
                             (let (result
                                   (and (eq? (parser-machine-runtime machine)
                                             (lr-recognition-fragment-runtime piece))
                                        (lr-checkpoint-fragment-compatible? normalized piece)
                                        (try-fragment piece (+ position-delta shift) shift old-start old-end
                                                      character byte mode first-probe normalized)))
                               (if result
                                 (begin (advance!) result)
                                 (begin (add-stat! 5 1)
                                        (descend! piece position-delta) (seek)))))))))))))))
     stats
     (and probe-cache
          (lambda (character byte mode)
            (let (result (source-probe-take! probe-cache character byte mode))
              (when result
                (add-stat! 7 1) (add-stat! 8 (- (token-end (car result)) byte)))
              result))))))
