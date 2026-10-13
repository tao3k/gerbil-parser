(import :std/test ../src/compiler/event-fold-scheme-context)
(export fold-call-environment-test)

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
(def fold-call-environment-test
  (test-suite "Dependency-derived private Fold calling convention"
    (test-case "state-only helpers omit source bounds and owner"
      (let* ((result (shared-call (large-code '(vector-ref slots 0)) '() '() #f))
             (call (car result)) (definition (cadr result)))
        (check (cdr call) => '(slots))
        (check (cdadr definition) => '(slots))))
    (test-case "quoted names are not environment dependencies"
      (let (result (shared-call (large-code ''(source-bytes start end slots events)) '() '() #f))
        (check (cdr (car result)) => '())
        (check (cdadr (cadr result)) => '())))
    (test-case "source and lexical cursor retain their positional binding"
      (let* ((result (shared-call (large-code '(u8vector-ref source-bytes cursor))
                                 '() '((index . cursor)) #f))
             (call (car result)) (definition (cadr result)))
        (check (cdr call) => '(source-bytes cursor))
        (check (cdadr definition) => '(source-bytes %fold-free-0))))
    (test-case "list owner publication remains in the lexical caller"
      (let* ((result (shared-call (large-code '(vector-set! slots 0 1)) '() '() #t))
             (call (car result)) (definition (cadr result)))
        (check (take call 2) => '(set! events))
        (check (cdr (caddr call)) => '(slots events))
        (check (cdadr definition) => '(slots events))))))
