;;; -*- Gerbil -*-
;;; Indexed LR action rows are shared by prepared and checkpoint execution.

(export index-action-row
        lookup-action-entry
        lookup-literal-action-entry
        lookup-layout-start-action-entry
        lookup-layout-next-action-entry
        lr-action-row-eof
        lr-action-row-tokens)

;;; ASCII punctuation has a direct action slot; longer/Unicode literals and
;;; token kinds retain their existing indexes. Entries remain the original
;;; (terminal . action) pairs for GLR receipt and action identity semantics.
(defstruct lr-action-row
  (ascii-literals literals tokens eof layout-start layout-next)
  transparent: #t)

(def (index-action-entries entries)
  (if (<= (length entries) 8)
    entries
    (let (index (make-table test: equal?))
      (for-each
       (lambda (entry)
         (unless (table-ref index (car entry) #f)
           (table-set! index (car entry) (cdr entry))))
       entries)
      index)))

(def (index-action-row row)
  (let ((ascii-literals #f) (literals '()) (tokens '()) (eof #f)
        (layout-start '()) (layout-next '()))
    (for-each
     (lambda (entry)
       (let (terminal (car entry))
         (case (cadr terminal)
           ((literal)
            (let (literal (caddr terminal))
              (if (and (string? literal)
                       (= (string-length literal) 1)
                       (< (char->integer (string-ref literal 0)) 128))
                (begin
                  (unless ascii-literals
                    (set! ascii-literals (make-vector 128 #f)))
                  (let (code (char->integer (string-ref literal 0)))
                    (unless (vector-ref ascii-literals code)
                      (vector-set! ascii-literals code entry))))
                (set! literals (cons (cons literal entry) literals)))))
           ((token)
            (set! tokens (cons (cons (caddr terminal) entry) tokens)))
           ((layout-start)
            (set! layout-start
                  (cons (cons (caddr terminal) entry) layout-start)))
           ((layout-next)
            (set! layout-next
                  (cons (cons (caddr terminal) entry) layout-next)))
           ((eof)
            (unless eof (set! eof entry)))
           (else (error "unsupported LR action terminal" terminal)))))
     row)
    (make-lr-action-row
     ascii-literals
     (index-action-entries (reverse literals))
     (index-action-entries (reverse tokens))
     eof
     (index-action-entries (reverse layout-start))
     (index-action-entries (reverse layout-next)))))

(def (lookup-action-entry index key)
  ;; Preparation admits only proper lists or tables. Inspect the outer
  ;; representation rather than traversing a whole list before assoc.
  (if (or (pair? index) (null? index))
    (let (found (assoc key index))
      (and found (cdr found)))
    (table-ref index key #f)))

(def (lookup-literal-action-entry row literal)
  (if (and (string? literal)
           (= (string-length literal) 1)
           (< (char->integer (string-ref literal 0)) 128))
    (let (ascii (lr-action-row-ascii-literals row))
      (and ascii
           (vector-ref ascii (char->integer (string-ref literal 0)))))
    (lookup-action-entry (lr-action-row-literals row) literal)))

(def (lookup-layout-start-action-entry row literal)
  (lookup-action-entry (lr-action-row-layout-start row) literal))

(def (lookup-layout-next-action-entry row literal)
  (lookup-action-entry (lr-action-row-layout-next row) literal))
