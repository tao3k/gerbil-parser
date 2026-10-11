;;; Boolean admission must match Core's complete evidence protocol.
(import :std/test
        (only-in :clan/poo/object .o .cc .ref .all-slots)
        (only-in :clan/poo/mop element? validate)
        (only-in :core/types poo-flow-contract-admit
                 poo-flow-validation-evidence-accepted?
                 poo-flow-classification-evidence)
        :gerbil-parser/src/modules/parser/interface)

(def (evidence-member? contract candidate)
  (poo-flow-validation-evidence-accepted?
   (poo-flow-contract-admit contract candidate #f)))
(def (empty-role (rules '()))
  (make-grammar-role 'role '() '() '() rules '() '() '() '() '()))
(def (rejects? thunk)
  (with-catch (lambda (_) #t) (lambda () (thunk) #f)))

(def grammar-contract-admission-test
  (test-suite "grammar boolean admission and complete evidence"
    (test-case "every responsibility and missing ancestry are checked"
      (let* ((role (empty-role)) (grammar (make-grammar 'grammar '() (list role))))
        (for-each
         (lambda (pair)
           (let ((contract (car pair)) (candidate (cdr pair)))
             (check (element? contract candidate) => (evidence-member? contract candidate))
             (check (eq? (validate contract candidate) candidate) => #t)
             (for-each
              (lambda (slot)
                (let (invalid (.cc candidate slot 0))
                  (check (evidence-member? contract invalid) => #f)
                  (check (element? contract invalid) => #f)
                  (check (rejects? (lambda () (validate contract invalid))) => #t)))
              (.all-slots (.ref contract 'responsibilities)))))
         (list (cons GrammarRoleContract role) (cons GrammarContract grammar)))
        (check (element? GrammarRoleContract (.o name: 'role)) => #f)
        (check (element? GrammarContract (.o name: 'grammar)) => #f)))
    (test-case "descriptor refinements keep additional responsibility admission"
      (let* ((role (empty-role))
             (responsibilities (.cc (.ref GrammarRoleContract 'responsibilities)
                                    'extra ParserSymbol))
             (refined (.cc GrammarRoleContract 'responsibilities responsibilities))
             (valid (.cc role 'extra 'symbol))
             (invalid (.cc role 'extra 0)))
        (check (element? refined valid) => (evidence-member? refined valid))
        (check (element? refined invalid) => #f)
        (check (rejects? (lambda () (validate refined invalid))) => #t)))
    (test-case "descriptor refinements retain custom classifier and obligations"
      (let* ((role (empty-role))
             (reclassified (.cc GrammarRoleContract '.classify
                             (lambda (candidate context)
                               (poo-flow-classification-evidence
                                (.ref GrammarRoleContract 'identity) candidate #f
                                '(refined-rejection) context))))
             (constrained (.cc GrammarRoleContract '.obligations
                           (lambda (_candidate _context) '(refined-obligation)))))
        (for-each
         (lambda (contract)
           (check (evidence-member? contract role) => #f)
           (check (element? contract role) => #f)
           (check (rejects? (lambda () (validate contract role))) => #t))
         (list reclassified constrained))))
    (test-case "refined validation invokes its classifier exactly once"
      (let (calls 0)
        (let* ((role (empty-role))
               (refined (.cc GrammarRoleContract '.classify
                         (lambda (candidate context)
                           (set! calls (+ calls 1))
                           (poo-flow-classification-evidence
                            (.ref GrammarRoleContract 'identity) candidate #f
                            '(refined-rejection) context)))))
          (check (rejects? (lambda () (validate refined role))) => #t)
          (check calls => 1))))
    (test-case "mutable declaration lists are rechecked on every call"
      (let* ((rules (list 'rule)) (role (empty-role rules)))
        (check (element? GrammarRoleContract role) => #t)
        (set-cdr! rules 'improper-tail)
        (check (element? GrammarRoleContract role) => #f)
        (check (element? GrammarRoleContract role) => (evidence-member? GrammarRoleContract role))
        (check (rejects? (lambda () (validate GrammarRoleContract role))) => #t)))
    (test-case "composition obligations reject foreign roles and parents"
      (let* ((role (empty-role)) (grammar (make-grammar 'grammar '() (list role))))
        (for-each
         (lambda (invalid)
           (check (evidence-member? GrammarContract invalid) => #f)
           (check (element? GrammarContract invalid) => #f)
           (check (rejects? (lambda () (validate GrammarContract invalid))) => #t))
         (list (.cc grammar 'parents (list (.o)))
               (.cc grammar 'roles (list (.o)))
               (.cc grammar 'composition (list (cons 'unknown role)))
               (.cc grammar 'composition (list (cons 'append (.o))))))))))
(export grammar-contract-admission-test)
