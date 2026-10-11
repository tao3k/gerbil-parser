;;; -*- Gerbil -*-
;;; Request-local source columns and branch-local indentation references.

(import (only-in ./lr-action-index
                 lookup-action-entry lookup-literal-action-entry
                 lookup-casefolded-literal-action-entry
                 lookup-layout-start-action-entry
                 lookup-layout-next-action-entry lr-action-row-tokens lr-action-row-eof)
        (only-in ./token token-kind token-lexeme token-start token-end))
(export make-layout-columns
        current-layout-columns current-layout-frames
        layout-token-column layout-shift-allowed?
        layout-marker-eligible? layout-after-shift layout-after-end
        layout-current-action-row prepare-layout-action-selector)

(def current-layout-columns (make-parameter #f))
(def current-layout-frames (make-parameter '()))

;;; Stage storage operations once, rather than dispatch for every source byte.
;;; Each UTF-8 byte retains its leading character's column, including EOF.
(defrule (build-layout-columns input storage put!)
  (let* ((bytes input) (columns storage) (size (u8vector-length bytes)))
    (let loop ((offset 0) (column 0))
      (if (= offset size)
        (begin (put! columns offset column) columns)
        (let* ((byte (u8vector-ref bytes offset))
               (width (cond ((< byte 128) 1) ((< byte 224) 2)
                            ((< byte 240) 3) (else 4)))
               (end (+ offset width))
               (next-column
                (case byte
                  ((10 13) 0)
                  ((9) (* (+ (quotient column 8) 1) 8))
                  (else (+ column 1)))))
          (let fill ((cursor offset))
            (when (< cursor end)
              (put! columns cursor column)
              (fill (+ cursor 1))))
          (loop end next-column))))))

;;; Tabs advance at most eight columns per character. Choose packed storage
;;; only when that bound fits u32; larger sources retain exact Scheme integers.
;;; The native UTF-8 encoder retains its ownership of character conversion.
;;; Its valid widths keep private write indices within the allocated buffer;
;;; only that bounded builder uses the native setter. Token reads stay checked.
(def (make-layout-columns source)
  (let* ((bytes (string->utf8 source)) (size (+ (u8vector-length bytes) 1)))
    (if (<= (string-length source) (quotient #xffffffff 8))
      (build-layout-columns bytes (make-u32vector size 0) ##u32vector-set!)
      (build-layout-columns bytes (make-vector size 0) vector-set!))))

(def (layout-column-ref columns offset)
  (if (u32vector? columns)
    (u32vector-ref columns offset)
    (vector-ref columns offset)))

(def (layout-token-column token)
  (let (columns (current-layout-columns))
    (and columns (layout-column-ref columns (token-start token)))))

(def (layout-token-end-column token)
  ;; JavaCC uses the final characters column, not the exclusive token end.
  ;; UTF-8 continuation bytes already carry their leading characters column.
  (layout-column-ref (current-layout-columns) (- (token-end token) 1)))

(def (layout-shift-allowed? token)
  (let (frames (current-layout-frames))
    (or (null? frames)
        (> (layout-token-column token) (caar frames)))))

(def (layout-marker-eligible? role token)
  (let ((frames (current-layout-frames))
        (column (layout-token-column token)))
    (case role
      ((layout-start)
       (or (null? frames) (> column (caar frames))))
      ((layout-next)
       (and (pair? frames)
            (= (layout-token-end-column token) (caar frames))
            (equal? (token-lexeme token) (cdar frames))))
      (else #f))))

(def (layout-after-shift role token)
  (if (eq? role 'layout-start)
    (cons (cons (layout-token-end-column token)
                (token-lexeme token))
          (current-layout-frames))
    (current-layout-frames)))

(def (layout-after-end next-token (boundaries '()))
  (let (frames (current-layout-frames))
    (and (pair? frames)
         (or (not next-token)
             (and (not (layout-marker-eligible? 'layout-next next-token))
                  (or (<= (layout-token-column next-token) (caar frames))
                      (member (token-lexeme next-token) boundaries))))
         (cdr frames))))

(def (layout-ordinary-entry entry shift-allowed?)
  (if (or (not entry) shift-allowed?)
    (if (and entry (eq? (cadr entry) 'layout-guard))
      (cons (car entry) (caddr entry)) entry)
    (let (action (cdr entry))
      (case (car action)
        ((layout-guard) (cons (car entry) (caddr action)))
        ((shift) #f)
        ((fork)
         (let (admitted
               (filter (lambda (branch) (not (eq? (car branch) 'shift)))
                       (cdr action)))
           (cond ((null? admitted) #f)
                 ((null? (cdr admitted))
                  (cons (car entry) (car admitted)))
                 (else (cons (car entry) (cons 'fork admitted))))))
        (else entry)))))

(def (layout-mark-action action role)
  (case (car action)
    ((layout-guard) (layout-mark-action (cadr action) role))
    ((shift) (list 'layout-shift role (cadr action)))
    ((fork)
     (cons 'fork
           (map (lambda (branch) (layout-mark-action branch role))
                (cdr action))))
    (else action)))

(def (layout-mark-entry entry role)
  (and entry
       (let (action (layout-mark-action (cdr entry) role))
         (if (eq? action (cdr entry)) entry (cons (car entry) action)))))

(def (layout-current-action-row row token case-insensitive?)
  (let* ((lexeme (token-lexeme token))
         (next-entry (lookup-layout-next-action-entry row lexeme))
         (next (and next-entry (layout-marker-eligible? 'layout-next token)
                    (layout-mark-entry next-entry 'layout-next)))
         (start-entry (and (not next) (lookup-layout-start-action-entry row lexeme)))
         (start (and start-entry (layout-marker-eligible? 'layout-start token)
                     (layout-mark-entry start-entry 'layout-start)))
         (ordinary
          (layout-ordinary-entry
           (or (lookup-literal-action-entry row lexeme)
               (and case-insensitive?
                    (lookup-casefolded-literal-action-entry row lexeme))
               (lookup-action-entry (lr-action-row-tokens row) (token-kind token)))
           (layout-shift-allowed? token))))
    (or next start ordinary
        (layout-ordinary-entry next-entry #f)
        (layout-ordinary-entry start-entry #f))))


;;; Layout action projections are grammar constants. Prepare both eligibility
;;; outcomes once; requests retain only columns and branch-local frames.
(def (layout-project-entry entry allowed?)
  (case (cadr (car entry))
    ((eof) entry)
    ((layout-start layout-next)
     (if allowed? (layout-mark-entry entry (cadr (car entry)))
         (layout-ordinary-entry entry #f)))
    (else (layout-ordinary-entry entry allowed?))))

(defrule (layout-projected-entry pool-expression entry-expression)
  (let ((pool pool-expression) (entry entry-expression))
    (and entry
         ;; Reductions cannot contain a shift. Keep their original entry and
         ;; avoid even the prepared hash lookup on the dominant LR action.
         (if (eq? (cadr entry) 'reduce) entry (table-ref pool entry entry)))))

(def (prepared-layout-current-action-row row allowed blocked token case-insensitive? columns frames)
  (let* ((lexeme (token-lexeme token))
         (next (lookup-layout-next-action-entry row lexeme)))
    (or (and next (pair? frames)
             (= (layout-column-ref columns (- (token-end token) 1)) (caar frames))
             (equal? lexeme (cdar frames))
             (layout-projected-entry allowed next))
        (let* ((start (lookup-layout-start-action-entry row lexeme))
               (entry (or (lookup-literal-action-entry row lexeme)
                          (and case-insensitive? (lookup-casefolded-literal-action-entry row lexeme))
                          (lookup-action-entry (lr-action-row-tokens row) (token-kind token)))))
          ;; Once an eligible next marker has lost and no start marker exists,
          ;; a pure reduction is independent of the source column relation.
          (if (and (not start) entry (eq? (cadr entry) 'reduce)) entry
            (let (shift-allowed? (or (null? frames)
                                    (> (layout-column-ref columns (token-start token)) (caar frames))))
              ;; Resolve refinement before projection; denied literals retain
              ;; their priority over generic tokens and later duplicates.
              (or (and start shift-allowed? (layout-projected-entry allowed start))
                  (layout-projected-entry (if shift-allowed? allowed blocked) entry)
                  (layout-projected-entry blocked next)
                  (layout-projected-entry blocked start))))))))

(def (prepare-layout-action-selector rows original case-insensitive?)
  (let ((allowed (make-table test: eq?)) (blocked (make-table test: eq?)))
    (let loop ((state 0))
      (when (< state (vector-length rows))
        (for-each (lambda (entry)
          (let ((positive (layout-project-entry entry #t))
                (negative (layout-project-entry entry #f)))
            ;; Preserve unchanged entry identity and retain #f rejections.
            (unless (eq? positive entry) (table-set! allowed entry positive))
            (unless (eq? negative entry) (table-set! blocked entry negative))))
          (vector-ref rows state))
        (loop (+ state 1))))
    (lambda (state tokens)
      (let (row (vector-ref original state))
        (if (null? tokens) (lr-action-row-eof row)
          (let ((columns (current-layout-columns)) (token (car tokens)))
            (if columns
              (prepared-layout-current-action-row row allowed blocked token case-insensitive?
                                                   columns (current-layout-frames))
              (or (lookup-literal-action-entry row (token-lexeme token))
                  (and case-insensitive? (lookup-casefolded-literal-action-entry row (token-lexeme token)))
                  (lookup-action-entry (lr-action-row-tokens row) (token-kind token))))))))))
