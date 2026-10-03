;;; -*- Gerbil -*-
;;; Source-owner certificates for deterministic nonterminal transfers.
(import (only-in ./source-index source-index-count make-source-index-cursor
                 source-index-cursor-copy source-index-cursor-token source-index-cursor-mode
                 source-index-cursor-rank source-index-cursor-next! source-index-cursor-seek!)
        (only-in ../compiler/machine parser-machine-runtime parser-machine-trivia parser-machine-lexical-modes-compatible?)
        (only-in ./lexer scan-source-token)
        (only-in ./probe make-source-probe-cache source-probe-scan source-probe-take!
                 current-lr-lexical-plan-reuse-enabled?)
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
;;; consumed probe tokens/bytes, inspected grammar-cursor frames,
;;; old-source-certified lexical probe requests.
;;; With a captured source index, cursors share immutable chunks and no full
;;; token/mode vectors are built. Trial cursors do not consume rejected spans.
;;; Request cursors and the single probe slot expire at the end of the edit.
(def (make-fragment-reuser machine source old-tokens old-modes root edit-start edit-end delta restart-byte
                             (source-index #f) (on-shared #f) (closed-source-owner? #f))
  (let* ((tokens (and (not source-index) (list->vector old-tokens)))
         (modes (and tokens (list->vector old-modes)))
         (input-cursor (and source-index (make-source-index-cursor source-index)))
         (count (if source-index (source-index-count source-index) (vector-length tokens))) (trivia? (parser-machine-trivia machine))
         (stats (make-vector 11 0)) (pending (list (cons root 0)))
         (first-index 0) (source-length (string-length source))
         (certified-scanning? (and closed-source-owner? (current-lr-lexical-plan-reuse-enabled?)))
         (probe-cache #f))
    (unless (or source-index (= count (vector-length modes))) (error "invalid captured lexical modes"))
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
    ;; Speculation must never consume the grammar owner's source cursor.
    ;; Independent lower-bound lookup is O(log n), plus a bounded index leaf.
    (def (certified-source-probe character byte mode)
      (let* ((suffix? (>= byte (+ edit-end delta)))
             (shift delta)
             (old-byte (- byte shift)))
        (and suffix?
             (let* ((cursor (and source-index (make-source-index-cursor source-index)))
                    (rank (if cursor
                            (begin (source-index-cursor-seek! cursor old-byte)
                                   (source-index-cursor-rank cursor))
                            (let search ((low 0) (high count))
                              (if (= low high) low
                                (let (middle (quotient (+ low high) 2))
                                  (if (< (token-start (vector-ref tokens middle)) old-byte)
                                    (search (+ middle 1) high) (search low middle)))))))
                    (old (and (< rank count) (if cursor (source-index-cursor-token cursor) (vector-ref tokens rank))))
                    (old-mode (and old (if cursor (source-index-cursor-mode cursor) (vector-ref modes rank)))))
               (and old (= (token-start old) old-byte)
                    (>= old-byte edit-end)
                    (parser-machine-lexical-modes-compatible? machine old-mode (lr-lexical-mode-id mode)
                      (string-ref (token-lexeme old) 0))
                    (let (token (if (zero? shift) old
                                  (make-token (token-kind old) (token-lexeme old)
                                    (+ shift (token-start old)) (+ shift (token-end old)))))
                      (add-stat! 10 1)
                      (cons token (+ character (string-length (token-lexeme old))))))))))
    (def (scan-certified-probe character byte mode)
      (let (old (certified-source-probe character byte mode))
        (if old (values (car old) (cdr old))
          (let-values (((token next) (scan-source-token machine source character byte mode)))
            (add-stat! 4 (- (token-end token) byte))
            (values token next)))))
    (def (probe character byte mode)
      (with-catch (lambda (_condition) #f)
        (lambda ()
          (let-values (((token next-character)
                        (cond (probe-cache (source-probe-scan probe-cache character byte mode))
                              (certified-scanning? (scan-certified-probe character byte mode))
                              (else (scan-source-token machine source character byte mode)))))
            (unless certified-scanning? (add-stat! 4 (- (token-end token) byte)))
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
    (def (old-token index cursor)
      (if cursor (source-index-cursor-token cursor) (vector-ref tokens index)))
    (def (old-mode index cursor)
      (if cursor (source-index-cursor-mode cursor) (vector-ref modes index)))
    (def (seek-token! start)
      (if input-cursor
        (begin (source-index-cursor-seek! input-cursor start)
               (set! first-index (source-index-cursor-rank input-cursor)))
        (let loop ()
          (when (and (< first-index count) (< (token-start (vector-ref tokens first-index)) start))
            (set! first-index (+ first-index 1)) (loop))))
      (and (< first-index count)
           (= (token-start (old-token first-index input-cursor)) start) first-index))
    (def (try-fragment piece position-delta shift old-start old-end
                       character byte mode first-probe normalized)
      (let (first (seek-token! old-start))
        (and first
             (not (trivia? (old-token first input-cursor)))
             (= (lr-lexical-mode-id mode) (old-mode first input-cursor))
             (same-token? (car first-probe) (old-token first input-cursor) shift)
             (let collect ((index first) (segment '()) (segment-modes '())
                           (significant '()) (significant-count 0)
                           (characters 0) (eof-offset byte)
                           (cursor (and input-cursor (source-index-cursor-copy input-cursor))))
               (if (and (< index count) (<= (token-end (old-token index cursor)) old-end))
                 (let* ((old (old-token index cursor))
                        (token (if (zero? shift) old
                                 (make-token (token-kind old) (token-lexeme old)
                                   (+ shift (token-start old)) (+ shift (token-end old)))))
                        (trivia-token? (trivia? token))
                        (next-index (+ index 1))
                        (next-segment (cons token segment))
                        (next-modes (cons (old-mode index cursor) segment-modes))
                        (next-significant (if trivia-token? significant (cons token significant)))
                        (next-count (+ significant-count (if trivia-token? 0 1)))
                        (next-characters (+ characters (string-length (token-lexeme token))))
                        (next-eof-offset (if trivia-token? eof-offset (token-end token))))
                   (when cursor (source-index-cursor-next! cursor))
                   (if (= (token-end old) old-end)
                     (let ((next-character (+ character next-characters))
                           (next-byte (+ old-end shift)))
                       (and (= next-count (lr-recognition-fragment-token-count piece))
                            (boundary-matches? piece position-delta next-character next-byte next-eof-offset)
                            (begin
                              (add-stat! 0 1) (add-stat! 1 next-count)
                              (add-stat! 2 (- next-index first)) (add-stat! 3 (- next-byte byte))
                              (set! first-index next-index)
                              (when cursor (set! input-cursor cursor))
                              (when on-shared (on-shared first (- next-index first) shift))
                              (vector
                               (lr-checkpoint-inject-fragment normalized piece position-delta
                                                              (reverse next-significant))
                               (reverse next-segment) (reverse next-modes) next-character next-byte))))
                     (collect next-index next-segment next-modes next-significant next-count
                              next-characters next-eof-offset cursor)))
                 #f)))))
    (when (current-lr-probe-reuse-enabled?)
      (set! probe-cache (make-source-probe-cache machine source
                         (and certified-scanning? scan-certified-probe))))
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
