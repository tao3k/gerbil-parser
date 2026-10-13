(import :std/test ../src/compiler/event-fold-call-plan
        ../src/compiler/event-fold-scheme-context)
(export fold-call-plan-test)
(def (fuse forms (budget 256))
  (parameterize ((current-fold-scheme-counter 0) (current-fold-scheme-prefix 'test))
    (fold-fuse-single-use-calls forms budget)))
(def (execute forms)
  (eval `(let () ,@(map (lambda (form) (cons 'define (cdr form))) forms) (entry))))
(def fold-call-plan-test
  (test-suite "Whole generated call-edge fusion"
    (test-case "single edge preserves nested and caller shadowing"
      (let* ((forms '((def (%event-fold-test-expression-0 value)
                       (let (value (+ value 1)) (let* ((next value) (value (+ next 2))) value)))
                     (def (entry) (let ((value 100)) (%event-fold-test-expression-0 7)))))
             (result (fuse forms)))
        (check (length result) => 1)
        (check (execute result) => (execute forms))))
    (test-case "shared consumers are not duplicated"
      (let (forms '((def (%event-fold-test-expression-0 x) (+ x 1))
                    (def (entry) (+ (%event-fold-test-expression-0 1)
                                    (%event-fold-test-expression-0 2)))))
        (check (fuse forms) => forms)))
    (test-case "effectful arguments retain their boundary and order"
      (let (forms '((def (%event-fold-test-expression-0 x) x)
                    (def (entry) (%event-fold-test-expression-0 (vector 1)))))
        (check (fuse forms) => forms)))
    (test-case "unselected recognition branches do not execute errors"
      (let* ((forms '((def (%event-fold-test-expression-0 x) (error "rejected" x))
                     (def (entry) (if #f (%event-fold-test-expression-0 0) 7))))
             (result (fuse forms)))
        (check (execute result) => 7)
        (check (execute result) => (execute forms))))
    (test-case "expanded budget rejection retains reachable child definitions"
      (let* ((forms '((def (%event-fold-test-expression-0 x) (+ x x x x x x x x))
                     (def (%event-fold-test-expression-1 x)
                       (+ (%event-fold-test-expression-0 x) x x x x x x x))
                     (def (entry) (%event-fold-test-expression-1 2))))
             (result (fuse forms 30)))
        (check (execute result) => (execute forms))))
    (test-case "named-loop lexical owner and tape mutation survive fusion"
      (let* ((forms '((def (%event-fold-test-expression-0 owner count)
                       (let next ((left count))
                         (if (= left 0) owner
                           (begin (vector-set! owner 0 (+ (vector-ref owner 0) 1))
                                  (next (- left 1))))))
                     (def (entry)
                       (let ((next 999) (owner (vector 0)))
                         (%event-fold-test-expression-0 owner 3)
                         (vector-ref owner 0)))))
             (result (fuse forms)))
        (check (execute result) => 3)
        (check (execute result) => (execute forms))))))
