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
;;; scanner-probe bytes (including rejected-candidate probes), rejected probes, control probes,
;;; consumed probe tokens, consumed probe bytes.
;;; Persistent views retain no session/source stream. Callback/catalog/token
;;; vectors and the bounded scanner slot live only during the current edit.
(def (make-fragment-reuser machine source old-tokens old-modes root edit-start edit-end delta restart-byte)
  (let* ((tokens (list->vector old-tokens)) (modes (list->vector old-modes))
         (count (vector-length tokens)) (trivia? (parser-machine-trivia machine))
         (starts (make-table test: eqv?)) (ends (make-table test: eqv?))
         (significant-prefix (make-vector (+ count 1) 0))
         (catalog (make-table test: eqv?)) (stats (make-vector 9 0))
         (source-length (string-length source))
         (probe-cache (and (current-lr-probe-reuse-enabled?)
                           (make-source-probe-cache machine source))))
    (unless (= count (vector-length modes)) (error "invalid captured lexical modes"))
    (def (add-stat! index value)
      (vector-set! stats index (+ (vector-ref stats index) value)))
    (let loop ((index 0))
      (when (< index count)
        (let (token (vector-ref tokens index))
          (when (> (+ (token-end token)
                      (if (>= (token-start token) edit-end) delta 0)) restart-byte)
            (table-set! starts (token-start token) index)
            (table-set! ends (token-end token) (+ index 1)))
          (vector-set! significant-prefix (+ index 1)
                       (+ (vector-ref significant-prefix index) (if (trivia? token) 0 1))))
        (loop (+ index 1))))
    ;; Preorder keeps larger ancestors ahead of their same-start descendants.
    ;; Bucket tails give linear construction even for left-recursive sequences.
    (let walk ((pending (list (cons root 0))))
      (unless (null? pending)
        (let* ((frame (car pending)) (piece (car frame)) (position-delta (cdr frame)))
          (cond
           ((lr-recognition-view? piece)
            (walk (cons (cons (lr-recognition-view-base piece)
                              (+ position-delta (lr-recognition-view-delta piece)))
                        (cdr pending))))
           ((and (lr-recognition-fragment? piece)
                 (or (< (lr-recognition-fragment-token-count piece) 2)
                     (<= (+ position-delta (lr-recognition-fragment-end piece)
                            (if (>= (+ position-delta (lr-recognition-fragment-start piece)) edit-end)
                              delta 0)) restart-byte)))
            (walk (cdr pending)))
           ((lr-recognition-fragment? piece)
            (let* ((start (+ position-delta (lr-recognition-fragment-start piece)))
                   (end (+ position-delta (lr-recognition-fragment-end piece)))
                   (offset (+ position-delta (lr-recognition-fragment-offset piece)))
                   (suffix? (>= start edit-end))
                   (prefix? (and (<= end edit-start) (<= offset edit-start)))
                   (shift (if suffix? delta 0))
                   (first (table-ref starts start #f))
                   (after (table-ref ends end #f)))
              (when (and (or suffix? prefix?) first after (< first after)
                         (> end start) (not (trivia? (vector-ref tokens first)))
                         (positive? (lr-recognition-fragment-token-count piece))
                         (= (- (vector-ref significant-prefix after)
                               (vector-ref significant-prefix first))
                            (lr-recognition-fragment-token-count piece))
                         (eq? (parser-machine-runtime machine)
                              (lr-recognition-fragment-runtime piece)))
                (let* ((key (+ start shift))
                       (states (or (table-ref catalog key #f)
                                   (let (value (make-table test: eqv?))
                                     (table-set! catalog key value) value)))
                       (state (lr-recognition-fragment-entry-state piece))
                       (bucket (or (table-ref states state #f)
                                   (let (value (vector #f #f))
                                     (table-set! states state value) value)))
                       (cell (list (vector piece (+ position-delta shift) shift first after))))
                  (if (vector-ref bucket 1) (set-cdr! (vector-ref bucket 1) cell)
                      (vector-set! bucket 0 cell))
                  (vector-set! bucket 1 cell)))
              (walk (append (map (lambda (child) (cons child position-delta))
                                 (lr-recognition-fragment-children piece))
                            (cdr pending)))))
           (else (walk (cdr pending)))))))
    (def (same-token? fresh old shift)
      (and (= (token-start fresh) (+ shift (token-start old)))
           (= (token-end fresh) (+ shift (token-end old)))
           (eq? (token-kind fresh) (token-kind old))
           (equal? (token-lexeme fresh) (token-lexeme old))))
    (def (probe character byte mode)
      ;; A different exit lexical mode can reject a candidate. Its canonical
      ;; parse still owns diagnostics; speculative lexical failures are vetoes.
      (with-catch (lambda (_condition) #f)
        (lambda ()
          (let-values (((token next-character) (if probe-cache
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
    (values
     (lambda (character byte checkpoint)
       (let (states (table-ref catalog byte #f))
         (and states
              (let* ((mode (lr-checkpoint-lexical-mode checkpoint))
                     (first-probe (probe character byte mode))
                     (normalized
                      (and first-probe
                           (begin (add-stat! 6 1)
                                  (lr-checkpoint-before-shift checkpoint (car first-probe)))))
                     (bucket (and normalized
                                  (table-ref states (lr-checkpoint-state normalized) #f))))
                (and bucket
                     (let choose ((entries (vector-ref bucket 0)))
                       (and (pair? entries)
                            (let* ((entry (car entries)) (piece (vector-ref entry 0))
                                   (position-delta (vector-ref entry 1))
                                   (shift (vector-ref entry 2))
                                   (first (vector-ref entry 3)) (after (vector-ref entry 4)))
                              (if (and (= (lr-lexical-mode-id mode) (vector-ref modes first))
                                       (same-token? (car first-probe) (vector-ref tokens first) shift)
                                       (lr-checkpoint-fragment-compatible? normalized piece))
                                (let collect ((index first) (segment '()) (segment-modes '())
                                              (significant '()) (characters 0) (eof-offset byte))
                                  (if (< index after)
                                    (let* ((old (vector-ref tokens index))
                                           (token (if (zero? shift) old
                                                    (make-token (token-kind old) (token-lexeme old)
                                                      (+ shift (token-start old)) (+ shift (token-end old))))))
                                      (collect (+ index 1) (cons token segment)
                                               (cons (vector-ref modes index) segment-modes)
                                               (if (trivia? token) significant (cons token significant))
                                               (+ characters (string-length (token-lexeme token)))
                                               (if (trivia? token) eof-offset (token-end token))))
                                    (let* ((next-character (+ character characters))
                                           (next-byte (+ position-delta (lr-recognition-fragment-end piece))))
                                      (if (boundary-matches? piece position-delta next-character next-byte eof-offset)
                                        (begin
                                          (add-stat! 0 1)
                                          (add-stat! 1 (lr-recognition-fragment-token-count piece))
                                          (add-stat! 2 (- after first)) (add-stat! 3 (- next-byte byte))
                                          (vector
                                           (lr-checkpoint-inject-fragment normalized piece position-delta
                                                                          (reverse significant))
                                           (reverse segment) (reverse segment-modes) next-character next-byte))
                                        (begin (add-stat! 5 1) (choose (cdr entries)))))))
                                (begin (add-stat! 5 1) (choose (cdr entries))))))))))))
     stats
     (and probe-cache
          (lambda (character byte mode)
            (let (result (source-probe-take! probe-cache character byte mode))
              (when result
                (add-stat! 7 1)
                (add-stat! 8 (- (token-end (car result)) byte)))
              result))))))
