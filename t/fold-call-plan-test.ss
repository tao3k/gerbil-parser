(import :gerbil-parser/language-test-support)
(import :std/test ../src/compiler/event-fold-call-plan
        ../src/compiler/event-fold-scheme-context)

(def (fuse forms (budget 256))
  (parameterize ((current-fold-scheme-counter 0) (current-fold-scheme-prefix 'test))
    (fold-fuse-single-use-calls forms budget)))
(def (execute forms)
  (eval `(let () ,@(map (lambda (form) (cons 'define (cdr form))) forms) (entry))))
(deflanguage-parser-tests fold-call-plan-test "Whole generated call-edge fusion"
  (engine)
    (property "single edge preserves nested and caller shadowing"
      (bindings (forms '((def (%event-fold-test-expression-0 value)
                       (let (value (+ value 1)) (let* ((next value) (value (+ next 2))) value)))
                     (def (entry) (let ((value 100)) (%event-fold-test-expression-0 7))))) (result (fuse forms)))
      (equal (length result)  1)
      (equal (execute result)  (execute forms)))
    (property "shared consumers are not duplicated"
      (bindings (forms '((def (%event-fold-test-expression-0 x) (+ x 1))
                    (def (entry) (+ (%event-fold-test-expression-0 1)
                                    (%event-fold-test-expression-0 2))))))
      (equal (fuse forms)  forms))
    (property "effectful arguments retain their boundary and order"
      (bindings (forms '((def (%event-fold-test-expression-0 x) x)
                    (def (entry) (%event-fold-test-expression-0 (vector 1))))))
      (equal (fuse forms)  forms))
    (property "unselected recognition branches do not execute errors"
      (bindings (forms '((def (%event-fold-test-expression-0 x) (error "rejected" x))
                     (def (entry) (if #f (%event-fold-test-expression-0 0) 7)))) (result (fuse forms)))
      (equal (execute result)  7)
      (equal (execute result)  (execute forms)))
    (property "expanded budget rejection retains reachable child definitions"
      (bindings (forms '((def (%event-fold-test-expression-0 x) (+ x x x x x x x x))
                     (def (%event-fold-test-expression-1 x)
                       (+ (%event-fold-test-expression-0 x) x x x x x x x))
                     (def (entry) (%event-fold-test-expression-1 2)))) (result (fuse forms 30)))
      (equal (execute result)  (execute forms)))
    (property "named-loop lexical owner and tape mutation survive fusion"
      (bindings (forms '((def (%event-fold-test-expression-0 owner count)
                       (let next ((left count))
                         (if (= left 0) owner
                           (begin (vector-set! owner 0 (+ (vector-ref owner 0) 1))
                                  (next (- left 1))))))
                     (def (entry)
                       (let ((next 999) (owner (vector 0)))
                         (%event-fold-test-expression-0 owner 3)
                         (vector-ref owner 0))))) (result (fuse forms)))
      (equal (execute result)  3)
      (equal (execute result)  (execute forms))))
