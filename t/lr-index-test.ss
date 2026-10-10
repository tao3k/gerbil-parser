;;; -*- Gerbil -*-
(import :std/test
        (only-in :gerbil-parser/src/compiler/lr-table validate-lr-tables layout-productions?)
        (only-in :gerbil-parser/src/runtime/lr-parser lr-prepare)
        (only-in :gerbil-parser/src/runtime/funcs
                 association-row-vector->index association-row-index-ref)
        (only-in :gerbil-parser/src/runtime/lr-action-index
                 index-action-row lookup-action-entry lr-action-row-tokens))
(export lr-index-test)

(def (check-table-rejected actions gotos)
  (check-exception
   (lr-prepare
    (list (cons 'productions '((0 root () concat #f)))
          (cons 'actions actions) (cons 'gotos gotos)
          (cons 'case-insensitive? #f)))
   (lambda (condition) (string-prefix? "invalid LR table" (error-message condition)))))

(def lr-index-test
  (test-suite "prepared LR indexes preserve association semantics"
    (test-case "canonical table admission preserves original rows and entries"
      (for-each
       (lambda (entry)
         (let ((actions (vector (list entry))) (gotos (vector '((root . 0)))))
           (let-values (((same-actions same-gotos)
                         (validate-lr-tables '((0 root () layout-end #f)) actions gotos)))
             (check (eq? actions same-actions) => #t)
             (check (eq? gotos same-gotos) => #t)
             (check (eq? entry (car (vector-ref same-actions 0))) => #t))))
       '(((terminal token word) shift 0 #f)
         ((terminal token word) shift 0 (left 10))
         ((terminal token word) reduce 0)
         ((terminal eof) accept)
         ((terminal literal "+") reject-nonassoc (terminal literal "+") 10)
         ((terminal token word) fork (shift 0 #f) (reduce 0))
         ((terminal token word) layout-guard (shift 0 #f) (reduce 0))
         ((terminal token word) layout-guard (shift 0 #f) (fork (reduce 0) (reduce 0))))))
    (test-case "layout tables require production-derived request capability"
      (for-each
       (lambda (entry) (check-table-rejected (vector (list entry)) (vector '())))
       '(((terminal token word) layout-guard (shift 0 #f) (reduce 0))
         ((terminal layout-start "{") shift 0 #f)
         ((terminal layout-next ";") shift 0 #f)))
      (for-each
       (lambda (rhs)
         (let ((productions (list (list 0 'root rhs 'concat #f)))
               (actions '#((((terminal layout-start "{") shift 0 #f))))
               (gotos '#(())))
           (check (layout-productions? productions) => #t)
           (let-values (((same-actions _) (validate-lr-tables productions actions gotos)))
             (check (eq? same-actions actions) => #t))))
       '(((terminal layout-start "{"))
         ((marked (terminal layout-next ";") ((field separator)))))))
    (test-case "malformed dimensions and rows fail before indexing"
      (for-each (lambda (pair) (check-table-rejected (car pair) (cadr pair)))
                '((#f #()) (#() #()) (#(()) #()) (#(invalid) #(()))
                  (#(()) #(invalid)) (#((invalid)) #(()))
                  (#(()) #((root))) (#(()) #((("root" . 0))))
                  (#(()) #(((root . -1)))) (#(()) #(((root . 1))))
                  (#(()) #(((root . 0) (root . 0)))))))
    (test-case "action targets and closed wrappers are admitted as one algebra"
      (for-each
       (lambda (action)
         (check-table-rejected (vector (list (cons '(terminal token word) action))) (vector '())))
       '((shift -1 #f) (shift 1 #f) (shift 0) (shift 0 (left "10"))
         (reduce -1) (reduce 1) (reduce 0 extra) (accept) (accept extra)
         (callback 0) (fork) (fork (reduce 0))
         (fork (shift 0 #f) (fork (reduce 0) (reduce 0)))
         (fork (shift 0 #f) (reduce 1))
         (layout-guard (reduce 0) (reduce 0))
         (layout-guard (shift 0 #f) (shift 0 #f))
         (reject-nonassoc (terminal token other) 1)))
      (check-table-rejected (vector '(((terminal token "word") reduce 0))) (vector '()))
      (check-table-rejected (vector '(((terminal token word) reduce 0)
                                     ((terminal token word) shift 0 #f))) (vector '())))
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
