(import :gerbil-parser/language-test-support)
(import :std/test ../src/compiler/event-fold-scheme-context)


(def (shared-statement tape? effect-only?)
  (parameterize ((current-fold-scheme-expressions (make-table))
                 (current-fold-scheme-definitions '())
                 (current-fold-scheme-counter 0)
                 (current-fold-scheme-tape-kinds tape?)
                 (current-fold-scheme-effect-only effect-only?))
    (let (statements (fold-scheme-share-statements
                     (map (lambda (_) '(fold-tape-finish! events)) (iota 12)) '() '()))
      (vector statements (current-fold-scheme-definitions)))))

(deflanguage-parser-tests fold-tape-tail-test "Direct tape effect-only transitions"
  (engine)
    (property "private tape calls do not reassign or return the owner"
      (bindings (result (shared-statement '((Text 1)) #t)) (call (caar (vector-ref result 0))) (body (caddar (vector-ref result 1))))
      (equal (symbol? call)  #t)
      (equal (eq? call 'set!)  #f)
      (equal (last body)  '(fold-tape-finish! events)))
    (scenario "list producers and disabled candidates retain threading"
      (each (result) ((list (shared-statement #f #t) (shared-statement '((Text 1)) #f)))
         (equal (caar (vector-ref result 0))  'set!)
         (equal (last (caddar (vector-ref result 1)))  'events))))
