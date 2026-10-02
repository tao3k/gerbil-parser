;;; -*- Gerbil -*-
;;; Request-local source columns and branch-local indentation references.

(import (only-in ../compiler/lr
                 production-action production-rhs)
        (only-in ./lr-action-index
                 lookup-action-entry lookup-literal-action-entry
                 lookup-layout-start-action-entry
                 lookup-layout-next-action-entry lr-action-row-tokens)
        (only-in ./token token-kind token-lexeme token-start token-end))
(export make-layout-columns
        current-layout-columns current-layout-frames
        layout-token-column layout-shift-allowed?
        layout-marker-eligible? layout-after-shift layout-after-end
        layout-current-action-row layout-productions?)

(def (layout-productions? productions)
  (any (lambda (production)
         (or (eq? (production-action production) 'layout-end)
             (any (lambda (operand)
                    (let (symbol (if (and (pair? operand)
                                           (eq? (car operand) 'marked))
                                    (cadr operand)
                                    operand))
                      (and (pair? symbol)
                           (eq? (car symbol) 'terminal)
                           (memq (cadr symbol)
                                 '(layout-start layout-next)))))
                  (production-rhs production))))
       productions))

(def current-layout-columns (make-parameter #f))
(def current-layout-frames (make-parameter '()))

;;; Each byte offset maps to the source character column before that byte.
;;; Tabs use JavaCC's eight-column stops; UTF-8 continuation bytes keep their
;;; leading character's column. The table is built only for layout grammars.
(def (make-layout-columns source)
  (let* ((bytes (string->utf8 source))
         (length (u8vector-length bytes))
         (columns (make-vector (+ length 1) 0)))
    (let loop ((offset 0) (column 0))
      (if (= offset length)
        (begin (vector-set! columns offset column) columns)
        (let* ((byte (u8vector-ref bytes offset))
               (width (cond ((< byte 128) 1)
                            ((< byte 224) 2)
                            ((< byte 240) 3)
                            (else 4)))
               (end (min length (+ offset width)))
               (next-column
                (cond ((or (= byte 10) (= byte 13)) 0)
                      ((= byte 9)
                       (* (+ (quotient column 8) 1) 8))
                      (else (+ column 1)))))
          (let fill ((cursor offset))
            (when (< cursor end)
              (vector-set! columns cursor column)
              (fill (+ cursor 1))))
          (loop end next-column))))))

(def (layout-token-column token)
  (let (columns (current-layout-columns))
    (and columns (vector-ref columns (token-start token)))))

(def (layout-token-end-column token)
  ;; A marker can contain a tab or multibyte character; its source end
  ;; offset, rather than its string length, determines the reference column.
  (vector-ref (current-layout-columns) (token-end token)))

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

(def (layout-after-end next-token)
  (let (frames (current-layout-frames))
    (and (pair? frames)
         (or (not next-token)
             (and (<= (layout-token-column next-token) (caar frames))
                  (not (layout-marker-eligible? 'layout-next next-token))))
         (cdr frames))))

(def (layout-ordinary-entry entry shift-allowed?)
  (if (or (not entry) shift-allowed?)
    entry
    (let (action (cdr entry))
      (case (car action)
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
    ((shift) (list 'layout-shift role (cadr action)))
    ((fork)
     (cons 'fork
           (map (lambda (branch) (layout-mark-action branch role))
                (cdr action))))
    (else action)))

(def (layout-mark-entry entry role)
  (and entry (cons (car entry)
                   (layout-mark-action (cdr entry) role))))

(def (layout-current-action-row row token case-insensitive?)
  (let* ((lexeme (token-lexeme token))
         (next-entry (lookup-layout-next-action-entry row lexeme))
         (next
          (and next-entry
               (layout-marker-eligible? 'layout-next token)
               (layout-mark-entry next-entry 'layout-next)))
         (start-entry (and (not next)
                           (lookup-layout-start-action-entry row lexeme)))
         (start
          (and start-entry
               (layout-marker-eligible? 'layout-start token)
               (layout-mark-entry start-entry 'layout-start))))
    (or next start
        (layout-ordinary-entry
         (or (lookup-literal-action-entry row lexeme)
             (and case-insensitive?
                  (lookup-literal-action-entry row (string-upcase lexeme)))
             (lookup-action-entry
              (lr-action-row-tokens row) (token-kind token)))
         (layout-shift-allowed? token)))))
