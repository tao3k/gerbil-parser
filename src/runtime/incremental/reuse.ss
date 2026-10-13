;;; -*- Gerbil -*-
;;; incremental: reuse owner. Private execution state and named algorithms.

(import (only-in ../probe
                 current-lr-lexical-plan-reuse-enabled?)
        (only-in ../../compiler/machine
                 parser-machine-runtime parser-machine-trivia parser-machine-lexical-modes-compatible?)
        (only-in ../artifact
                 make-same-width-token-artifact make-shifted-token-artifact make-certified-window-artifact parse-artifact-events parse-artifact-success?)
        (only-in ../lr-parser
                 lr-lexical-mode-id lr-runtime-lexical-mode-catalog)
        (only-in ../lexer
                 scan-source-token)
        (only-in ../token
                 token-end token-kind token-lexeme token-start)
        (only-in "./state.ss"
                 make-incremental-session-state incremental-session-state-machine incremental-session-state-artifact incremental-session-state-tokens incremental-session-state-modes incremental-session-state-checkpoints incremental-session-state-capture? edit-start-byte edit-delete-byte-length)
        (only-in "./support.ss"
                 byte-index->character-index closed-lexer? relocate-token edit-byte-delta generic-lr-lexemes? same-scanned-token?)
        (only-in "./session.ss"
                 splice-session-index))
(export source-mode-compatible?
        certified-token-reuse
        scan-certified-window
        scan-certified-trivia-window
        scan-aligned-window
        certified-token-window-reuse)

;;; Compiler candidate-set certificates affect lexical reuse only.
(def (source-mode-compatible? machine old-mode mode old-token)
  (let (new-mode (lr-lexical-mode-id mode))
    (or (= old-mode new-mode)
        (and (current-lr-lexical-plan-reuse-enabled?)
             (parser-machine-lexical-modes-compatible? machine old-mode new-mode
               (string-ref (token-lexeme old-token) 0))))))

;;; A closed scanner and one-token edit certify unchanged suffix tokenization
;;; modulo its byte delta. Generic significant lexemes preserve LR actions.
;;; Snapshots after a shifted or significant token hold stale semantic values.
(def (certified-token-reuse session source-edit new-source)
  (let* ((machine (incremental-session-state-machine session))
         (old-artifact (incremental-session-state-artifact session))
         (old-tokens (incremental-session-state-tokens session))
         (old-modes (incremental-session-state-modes session))
         (edit-start (edit-start-byte source-edit))
         (edit-end (+ edit-start (edit-delete-byte-length source-edit)))
         (delta (edit-byte-delta source-edit)))
    (and (parse-artifact-success? old-artifact)
         old-modes
         (closed-lexer? machine)
         (let loop ((tokens old-tokens) (modes old-modes) (index 0)
                    (previous #f) (previous-mode #f))
           (and (pair? tokens) (pair? modes)
                (let ((old-token (car tokens))
                      (start (token-start (car tokens)))
                      (end (token-end (car tokens))))
                  (cond
                   ((<= end edit-start)
                    (loop (cdr tokens) (cdr modes) (+ index 1)
                          old-token (car modes)))
                   ((and (<= start edit-start) (<= edit-end end))
                    (let (new-token
                          (with-catch
                           (lambda (_condition) #f)
                           (lambda ()
                             (let* ((mode
                                     (vector-ref
                                      (lr-runtime-lexical-mode-catalog
                                       (parser-machine-runtime machine))
                                      (car modes)))
                                    (character
                                     (byte-index->character-index
                                      (string->utf8 new-source) start)))
                               (let-values (((token _next-character)
                                             (scan-source-token
                                              machine new-source character
                                              start mode)))
                                 token)))))
                      (and new-token
                           (= (token-start new-token) start)
                           (= (token-end new-token) (+ end delta))
                           (eq? (token-kind new-token)
                                (token-kind old-token))
                           (eq? (not ((parser-machine-trivia machine)
                                      old-token))
                                (not ((parser-machine-trivia machine)
                                      new-token)))
                           (or ((parser-machine-trivia machine) new-token)
                               (generic-lr-lexemes?
                                (parser-machine-runtime machine)
                                (token-lexeme old-token)
                                (token-lexeme new-token)))
                           (or (zero? delta)
                               (and
                                (or (not previous)
                                    (same-scanned-token?
                                     machine new-source previous
                                     previous-mode 0))
                                (or (null? (cdr tokens))
                                    (same-scanned-token?
                                     machine new-source (cadr tokens)
                                     (cadr modes) delta))))
                           (let* ((artifact+shared
                                   (if (zero? delta)
                                     (cons
                                      (make-same-width-token-artifact
                                       old-artifact new-source index new-token)
                                      (- (length (parse-artifact-events
                                                  old-artifact)) 1))
                                     (let-values (((shifted shared)
                                                   (make-shifted-token-artifact
                                                    old-artifact new-source
                                                    index start end new-token
                                                    delta)))
                                       (cons shifted shared))))
                                  (artifact (car artifact+shared))
                                  (next-tokens
                                   (append (take old-tokens index)
                                           (cons new-token
                                                 (if (zero? delta)
                                                   (cdr tokens)
                                                   (map
                                                    (lambda (token)
                                                      (relocate-token token
                                                                      delta))
                                                    (cdr tokens))))))
                                  (trivia?
                                   ((parser-machine-trivia machine) new-token))
                                  (next-checkpoints
                                   (if (and trivia? (zero? delta))
                                     (incremental-session-state-checkpoints
                                      session)
                                     (list->vector
                                      (filter
                                       (lambda (record)
                                         (<= (vector-ref record 3) start))
                                       (vector->list
                                        (incremental-session-state-checkpoints
                                         session))))))
                                  (next
                                   (make-incremental-session-state
                                    machine new-source artifact next-tokens
                                    old-modes next-checkpoints
                                    (incremental-session-state-capture? session) #f
                                    (splice-session-index session index 1 (list new-token) (list (car modes)) delta))))
                             (vector next index (length (cdr tokens))
                                     (- (u8vector-length
                                         (string->utf8 new-source))
                                        (token-end new-token))
                                     start
                                     (cdr artifact+shared)
                                     (if trivia? 0 1))))))
                   (else #f))))))))

;;; Re-lex exactly the old token window in its recorded LR modes. The same
;;; number of tokens, kinds, trivia membership, and LR actions certify that
;;; recognition events have the same topology after a multi-token edit.
(def (scan-certified-window machine source old-window mode-ids start target-end)
  (with-catch
   (lambda (_condition) #f)
   (lambda ()
     (let loop ((old old-window) (modes mode-ids)
                (byte start)
                (character
                 (byte-index->character-index (string->utf8 source) start))
                (found '()) (changed-significant 0))
       (if (null? old)
         (and (= byte target-end)
              (cons (reverse found) changed-significant))
         (and (pair? modes)
              (let (mode
                    (vector-ref
                     (lr-runtime-lexical-mode-catalog
                      (parser-machine-runtime machine))
                     (car modes)))
                (let-values (((new-token next-character)
                              (scan-source-token
                               machine source character byte mode)))
                  (and (eq? (token-kind (car old))
                            (token-kind new-token))
                       (eq? (not ((parser-machine-trivia machine)
                                  (car old)))
                            (not ((parser-machine-trivia machine)
                                  new-token)))
                       (or ((parser-machine-trivia machine) new-token)
                           (equal? (token-lexeme (car old))
                                   (token-lexeme new-token))
                           (generic-lr-lexemes?
                            (parser-machine-runtime machine)
                            (token-lexeme (car old))
                            (token-lexeme new-token)))
                       (<= (token-end new-token) target-end)
                       (loop (cdr old) (cdr modes)
                             (token-end new-token) next-character
                             (cons new-token found)
                             (+ changed-significant
                                (if ((parser-machine-trivia machine)
                                     new-token)
                                  0 1))))))))))))

;;; Trivia leaves the LR checkpoint unchanged. A replacement can therefore
;;; contain a different number of trivia tokens when all old modes agree.
(def (scan-certified-trivia-window machine source mode-id start target-end)
  (with-catch
   (lambda (_condition) #f)
   (lambda ()
     (let (mode
           (vector-ref
            (lr-runtime-lexical-mode-catalog
             (parser-machine-runtime machine)) mode-id))
       (let loop ((byte start)
                  (character
                   (byte-index->character-index (string->utf8 source) start))
                  (found '()))
         (cond
          ((= byte target-end) (reverse found))
          ((> byte target-end) #f)
          (else
           (let-values (((token next-character)
                         (scan-source-token machine source character byte mode)))
             (and ((parser-machine-trivia machine) token)
                  (> (token-end token) byte)
                  (<= (token-end token) target-end)
                  (loop (token-end token) next-character
                        (cons token found)))))))))))

;;; Align significant tokens while allowing each existing trivia run to grow
;;; or shrink. Every run keeps its LR lexical mode; only significant tokens
;;; advance LR state. Boundaries inside a changed trivia run remain unmapped
;;; and cause event reuse to fail closed.
(def (scan-aligned-window machine source old-window mode-ids start target-end)
  (with-catch
   (lambda (_condition) #f)
   (lambda ()
     (let ((catalog
            (lr-runtime-lexical-mode-catalog
             (parser-machine-runtime machine)))
           (trivia? (parser-machine-trivia machine)))
       (def (scan byte character mode-id)
         (scan-source-token
          machine source character byte (vector-ref catalog mode-id)))
       (let loop ((old old-window) (modes mode-ids)
                  (byte start)
                  (character
                   (byte-index->character-index (string->utf8 source) start))
                  (tokens-rev '()) (modes-rev '()) (groups-rev '())
                  (boundaries '()) (significant 0))
         (if (null? old)
           (and (= byte target-end)
                (positive? significant)
                (vector (reverse tokens-rev) (reverse modes-rev)
                        (reverse groups-rev) boundaries significant))
           (and (pair? modes)
                (if (trivia? (car old))
                  (let gather ((run old) (run-modes modes)
                               (count 0) (last #f))
                    (if (and (pair? run) (trivia? (car run)))
                      (and (pair? run-modes)
                           (= (car run-modes) (car modes))
                           (gather (cdr run) (cdr run-modes)
                                   (+ count 1) (car run)))
                      (and (or (null? run)
                               (and (pair? run-modes)
                                    (= (car run-modes) (car modes))))
                           (let ()
                             (def (resume at at-character found)
                               (let (replacement (reverse found))
                                 (loop
                                  run run-modes at at-character
                                  (append found tokens-rev)
                                  (append (make-list (length found)
                                                     (car modes))
                                          modes-rev)
                                  (append (make-list (- count 1) '())
                                          (cons replacement groups-rev))
                                  (cons (cons (token-end last) at)
                                        (cons (cons (token-start (car old)) byte)
                                              boundaries))
                                  significant)))
                             (let collect ((at byte) (at-character character)
                                           (found '()))
                               (if (= at target-end)
                                 (resume at at-character found)
                                 (and (< at target-end)
                                      (let-values (((token next-character)
                                                    (scan at at-character
                                                          (car modes))))
                                        (if (trivia? token)
                                          (and (> (token-end token) at)
                                               (<= (token-end token) target-end)
                                               (collect (token-end token)
                                                        next-character
                                                        (cons token found)))
                                          (resume at at-character found))))))))))
                  (let-values (((new-token next-character)
                                (scan byte character (car modes))))
                    (and (not (trivia? new-token))
                         (eq? (token-kind (car old))
                              (token-kind new-token))
                         (or (equal? (token-lexeme (car old))
                                     (token-lexeme new-token))
                             (generic-lr-lexemes?
                              (parser-machine-runtime machine)
                              (token-lexeme (car old))
                              (token-lexeme new-token)))
                         (<= (token-end new-token) target-end)
                         (loop
                          (cdr old) (cdr modes)
                          (token-end new-token) next-character
                          (cons new-token tokens-rev)
                          (cons (car modes) modes-rev)
                          (cons (list new-token) groups-rev)
                          (cons (cons (token-end (car old))
                                      (token-end new-token))
                                (cons (cons (token-start (car old))
                                            (token-start new-token))
                                      boundaries))
                          (+ significant 1))))))))))))

(def (certified-token-window-reuse session source-edit new-source)
  (let* ((machine (incremental-session-state-machine session))
         (old-artifact (incremental-session-state-artifact session))
         (old-tokens (incremental-session-state-tokens session))
         (old-modes (incremental-session-state-modes session))
         (edit-start (edit-start-byte source-edit))
         (edit-end (+ edit-start (edit-delete-byte-length source-edit)))
         (delta (edit-byte-delta source-edit)))
    (and (parse-artifact-success? old-artifact)
         old-modes
         (closed-lexer? machine)
         (let locate ((tokens old-tokens) (modes old-modes)
                      (prefix '()) (prefix-modes '()) (index 0))
           (and (pair? tokens) (pair? modes)
                (if (<= (token-end (car tokens)) edit-start)
                  (locate (cdr tokens) (cdr modes)
                          (cons (car tokens) prefix)
                          (cons (car modes) prefix-modes) (+ index 1))
                  (let collect ((rest tokens) (rest-modes modes)
                                (window '()) (window-modes '())
                                (count 0))
                    (if (and (pair? rest)
                             (or (zero? count)
                                 (< (token-start (car rest)) edit-end)))
                      (and (pair? rest-modes)
                           (collect (cdr rest) (cdr rest-modes)
                                    (cons (car rest) window)
                                    (cons (car rest-modes) window-modes)
                                    (+ count 1)))
                      (and (positive? count)
                           (let* ((old-window (reverse window))
                                  (start (token-start (car old-window)))
                                  (target-end
                                   (+ (token-end (car window)) delta))
                                  (trivia-window?
                                   (and (every
                                         (parser-machine-trivia machine)
                                         old-window)
                                        (every
                                         (lambda (mode-id)
                                           (= mode-id (car window-modes)))
                                         window-modes)))
                                  (scanned
                                   (and (>= count 2)
                                        (scan-certified-window
                                         machine new-source old-window
                                         (reverse window-modes)
                                         start target-end)))
                                  (trivia-scanned
                                   (and (not scanned) trivia-window?
                                        (scan-certified-trivia-window
                                         machine new-source
                                         (car window-modes)
                                         start target-end)))
                                  (aligned
                                   (and (not scanned) (not trivia-scanned)
                                        (scan-aligned-window
                                         machine new-source old-window
                                         (reverse window-modes)
                                         start target-end)))
                                  (new-window
                                   (cond (scanned (car scanned))
                                         (trivia-scanned trivia-scanned)
                                         (aligned (vector-ref aligned 0))
                                         (else #f))))
                             (and new-window
                                  (or (null? prefix)
                                      (same-scanned-token?
                                       machine new-source (car prefix)
                                       (car prefix-modes) 0))
                                  (or (null? rest)
                                      (and (pair? rest-modes)
                                           (same-scanned-token?
                                            machine new-source (car rest)
                                            (car rest-modes) delta)))
                                  (let* ((next-tokens
                                          (append
                                           (reverse prefix) new-window
                                           (if (zero? delta) rest
                                               (map
                                                (lambda (token)
                                                  (relocate-token token delta))
                                                rest))))
                                         (next-modes
                                          (if scanned old-modes
                                              (append
                                               (reverse prefix-modes)
                                               (if aligned
                                                 (vector-ref aligned 1)
                                                 (make-list
                                                  (length new-window)
                                                  (car window-modes)))
                                               rest-modes)))
                                         (next-checkpoints
                                          (list->vector
                                           (filter
                                            (lambda (record)
                                              (<= (vector-ref record 3) start))
                                            (vector->list
                                             (incremental-session-state-checkpoints
                                              session)))))
                                         (suffix-bytes
                                          (- (u8vector-length
                                              (string->utf8 new-source))
                                             target-end)))
                                    (with-catch
                                     (lambda (_condition) #f)
                                     (lambda ()
                                       (let-values (((artifact shared)
                                                     (make-certified-window-artifact
                                                      old-artifact new-source
                                                      index old-window new-window
                                                      delta
                                                      (and aligned
                                                           (vector-ref aligned 2))
                                                      (and aligned
                                                           (vector-ref aligned 3)))))
                                         (vector
                                          (make-incremental-session-state
                                           machine new-source artifact next-tokens
                                           next-modes next-checkpoints
                                           (incremental-session-state-capture? session) #f
                                           (splice-session-index session index (length old-window) new-window
                                             (cond (scanned (reverse window-modes))
                                                   (aligned (vector-ref aligned 1))
                                                   (else (make-list (length new-window) (car window-modes)))) delta))
                                          index (length rest) suffix-bytes
                                          start shared
                                          (cond (scanned (cdr scanned))
                                                (aligned (vector-ref aligned 4))
                                                (else 0))))))))))))))))))

