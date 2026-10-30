(import :gerbil-parser/language-test-support)
(import :std/test ../src/compiler/event-fold-scheme-context)


(def (shared-call code bindings indices statements?)
  (parameterize ((current-fold-scheme-expressions (make-table test: equal?))
                 (current-fold-scheme-definitions '())
                 (current-fold-scheme-counter 0)
                 (current-fold-scheme-tape-kinds #f))
    (let (call (if statements?
                (car (fold-scheme-share-statements (list code) bindings indices))
                (fold-scheme-share-expression code bindings indices)))
      (list call (car (current-fold-scheme-definitions))))))

(def (large-code expression) `(begin ,@(make-list 32 expression)))
(deflanguage-parser-tests fold-call-environment-test "Dependency-derived private Fold calling convention"
  (engine)
    (property "state-only helpers omit source bounds and owner"
      (bindings (result (shared-call (large-code '(vector-ref slots 0)) '() '() #f)) (call (car result)) (definition (cadr result)))
      (equal (cdr call)  '(slots))
      (equal (cdadr definition)  '(slots)))
    (property "quoted names are not environment dependencies"
      (bindings (result (shared-call (large-code ''(source-bytes start end slots events)) '() '() #f)))
      (equal (cdr (car result))  '())
      (equal (cdadr (cadr result))  '()))
    (property "source and lexical cursor retain their positional binding"
      (bindings (result (shared-call (large-code '(u8vector-ref source-bytes cursor))
                                 '() '((index . cursor)) #f)) (call (car result)) (definition (cadr result)))
      (equal (cdr call)  '(source-bytes cursor))
      (equal (cdadr definition)  '(source-bytes %fold-free-0)))
    (property "list owner publication remains in the lexical caller"
      (bindings (result (shared-call (large-code '(vector-set! slots 0 1)) '() '() #t)) (call (car result)) (definition (cadr result)))
      (equal (take call 2)  '(set! events))
      (equal (cdr (caddr call))  '(slots events))
      (equal (cdadr definition)  '(slots events))))
