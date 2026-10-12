(import :std/test ../src/compiler/event-fold-scheme-context)
(export fold-tape-tail-test)

(def (shared-statement tape? effect-only?)
  (parameterize ((current-fold-scheme-expressions (make-table))
                 (current-fold-scheme-definitions '())
                 (current-fold-scheme-counter 0)
                 (current-fold-scheme-tape-kinds tape?)
                 (current-fold-scheme-effect-only effect-only?))
    (let (statements (fold-scheme-share-statements
                     (map (lambda (_) '(fold-tape-finish! events)) (iota 12)) '() '()))
      (vector statements (current-fold-scheme-definitions)))))

(def fold-tape-tail-test
  (test-suite "Direct tape effect-only transitions"
    (test-case "private tape calls do not reassign or return the owner"
      (let* ((result (shared-statement '((Text 1)) #t))
             (call (caar (vector-ref result 0)))
             (body (caddar (vector-ref result 1))))
        (check (symbol? call) => #t)
        (check (eq? call 'set!) => #f)
        (check (last body) => '(fold-tape-finish! events))))
    (test-case "list producers and disabled candidates retain threading"
      (for-each
       (lambda (result)
         (check (caar (vector-ref result 0)) => 'set!)
         (check (last (caddar (vector-ref result 1))) => 'events))
       (list (shared-statement #f #t) (shared-statement '((Text 1)) #f))))))
