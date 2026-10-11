;;; -*- Gerbil -*-
;;; Indexed LR action rows are shared by prepared and checkpoint execution.

(import (only-in ./scan make-literal-trie literal-trie-terminal literal-trie-child))

(export index-action-rows index-action-row lookup-casefolded-literal-action-entry
        lookup-action-entry
        lookup-literal-action-entry
        lookup-layout-start-action-entry
        lookup-layout-next-action-entry
        lr-action-row-eof
        lr-action-row-tokens lr-action-row-has-literals? lr-action-row-casefold-literals)

;;; ASCII punctuation has a direct action slot; longer/Unicode literals and
;;; token kinds retain their existing indexes. Entries remain the original
;;; (terminal . action) pairs for GLR receipt and action identity semantics.
(defstruct lr-action-row
  (ascii-literals literals tokens eof layout-start layout-next casefold-literals)
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

(def (index-action-row/with-casefold row casefold-index)
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
     (index-action-entries (reverse layout-next)) casefold-index)))

;;; One grammar vocabulary is shared by every state; row lookup still admits
;;; only that state's original entry and therefore retains first duplicates.
(def (prepare-casefold-literals rows)
  (let (entries '())
    (for-each
     (lambda (row)
       (for-each
        (lambda (entry)
          (let (terminal (car entry))
            (when (and (eq? (cadr terminal) 'literal) (string? (caddr terminal)))
              (set! entries (cons (cons (caddr terminal) (caddr terminal)) entries))))) row)) rows)
    (and (pair? entries) (make-literal-trie entries))))
(def (index-action-row row)
  (index-action-row/with-casefold row (prepare-casefold-literals (list row))))
(def (index-action-rows rows case-insensitive?)
  (let* ((trie (and case-insensitive? (prepare-casefold-literals (vector->list rows))))
         (result (make-vector (vector-length rows))))
    (let loop ((index 0))
      (when (< index (vector-length rows))
        (vector-set! result index (index-action-row/with-casefold (vector-ref rows index) trie))
        (loop (+ index 1))))
    result))

;;; A prepared row without literals cannot match either the source lexeme or
;;; its case-normalized form. Bind the row once; retain the indexed protocol.
(defrule (lr-action-row-has-literals? row-expr)
  (let (row row-expr)
    (or (not (not (lr-action-row-ascii-literals row)))
        (not (null? (lr-action-row-literals row))))))

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

;;; ASCII case conversion is length preserving. Failed prefixes cannot become
;;; a declared literal. Non-ASCII input delegates to the original whole-string
;;; Unicode conversion, including mappings that do not preserve character count.
(def (lookup-casefolded-literal/at row node source index width)
  (if (= index width)
    (let (literal (literal-trie-terminal node))
      (and literal (lookup-literal-action-entry row literal)))
    (let (code (char->integer (string-ref source index)))
      (if (>= code 128)
        (lookup-literal-action-entry row (string-upcase source))
        (let (child (literal-trie-child node
                     (integer->char (if (<= 97 code 122) (- code 32) code))))
          (and child (lookup-casefolded-literal/at row child source (+ index 1) width)))))))
(def (lookup-casefolded-literal-action-entry row source)
  (let (trie (lr-action-row-casefold-literals row))
    (and trie (lr-action-row-has-literals? row) (string? source)
         (lookup-casefolded-literal/at row trie source 0 (string-length source)))))

(def (lookup-layout-start-action-entry row literal)
  (lookup-action-entry (lr-action-row-layout-start row) literal))

(def (lookup-layout-next-action-entry row literal)
  (lookup-action-entry (lr-action-row-layout-next row) literal))
