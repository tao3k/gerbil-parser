;;; -*- Gerbil -*-
(import :std/test
        (only-in :gerbil-parser/src/runtime/funcs
                 association-row-vector->index association-row-index-ref)
        (only-in :gerbil-parser/src/runtime/lr-action-index
                 index-action-row lookup-action-entry lr-action-row-tokens))
(export lr-index-test)

(def lr-index-test
  (test-suite "prepared LR indexes preserve association semantics"
    (test-case "goto list/table boundaries preserve identity, misses and first duplicates"
      (for-each
       (lambda (width)
         (let* ((row (map (lambda (n) (cons (number->string n) n)) (iota width)))
                (rows (vector '() row))
                (indexes (association-row-vector->index rows)))
           (check (association-row-index-ref indexes 0 "missing") => #f)
           (check (association-row-index-ref indexes 1 "missing") => #f)
           (for-each
            (lambda (entry)
              (check (eq? (association-row-index-ref indexes 1 (string-copy (car entry))) entry) => #t))
            row)
           (when (pair? row)
             (let* ((duplicate (cons (string-copy (caar row)) 'later))
                    (indexed (association-row-vector->index (vector (append row (list duplicate))))))
               (check (eq? (association-row-index-ref indexed 0 (caar row)) (car row)) => #t)))))
       '(0 1 8 9 32)))
    (test-case "action list/table boundaries preserve original entries and first duplicates"
      (for-each
       (lambda (width)
         (let* ((entries (map (lambda (n) (cons (list 'terminal 'token (number->string n)) (list 'shift n))) (iota width)))
                (rows (if (pair? entries)
                        (append entries (list (cons (caar entries) '(shift 999)))) entries))
                (index (lr-action-row-tokens (index-action-row rows))))
           (check (lookup-action-entry index "missing") => #f)
           (for-each
            (lambda (entry)
              (check (eq? (lookup-action-entry index (string-copy (caddar entry))) entry) => #t))
            entries)))
       '(0 1 7 8 9 32)))))
