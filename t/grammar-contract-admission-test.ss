(import :gerbil-parser/language-test-support)
;;; Boolean admission must match Core's complete evidence protocol.
(import :std/test
        (only-in :clan/poo/object .o .cc .ref .all-slots)
        (only-in :clan/poo/mop element? validate)
        (only-in :yuanxu/object-types/contracts yuanxu-contract-admit
                 yuanxu-validation-evidence-accepted?
                 yuanxu-classification-evidence)
        :gerbil-parser/src/modules/parser/interface)

(def (evidence-member? contract candidate)
  (yuanxu-validation-evidence-accepted?
   (yuanxu-contract-admit contract candidate #f)))
(def (empty-role (rules '()))
  (make-grammar-role 'role '() '() '() rules '() '() '() '() '()))
(def (rejects? thunk)
  (with-catch (lambda (_) #t) (lambda () (thunk) #f)))

(deflanguage-parser-tests grammar-contract-admission-test "grammar boolean admission and complete evidence"
  (engine)
    (scenario "every responsibility and missing ancestry are checked"
      (sequential ((role (empty-role)) (grammar (make-grammar 'grammar '() (list role))))
        (each (pair) ((list (cons GrammarRoleContract role) (cons GrammarContract grammar)))
           (scope ((contract (car pair)) (candidate (cdr pair)))
             (equal (element? contract candidate)  (evidence-member? contract candidate))
             (equal (eq? (validate contract candidate) candidate)  #t)
             (each (slot) ((.all-slots (.ref contract 'responsibilities)))
                (scope ((invalid (.cc candidate slot 0)))
                  (equal (evidence-member? contract invalid)  #f)
                  (equal (element? contract invalid)  #f)
                  (equal (rejects? (lambda () (validate contract invalid)))  #t)))))
        (equal (element? GrammarRoleContract (.o name: 'role))  #f)
        (equal (element? GrammarContract (.o name: 'grammar))  #f)))
    (property "descriptor refinements keep additional responsibility admission"
      (bindings (role (empty-role)) (responsibilities (.cc (.ref GrammarRoleContract 'responsibilities)
                                    'extra ParserSymbol)) (refined (.cc GrammarRoleContract 'responsibilities responsibilities)) (valid (.cc role 'extra 'symbol)) (invalid (.cc role 'extra 0)))
      (equal (element? refined valid)  (evidence-member? refined valid))
      (equal (element? refined invalid)  #f)
      (equal (rejects? (lambda () (validate refined invalid)))  #t))
    (scenario "descriptor refinements retain custom classifier and obligations"
      (sequential ((role (empty-role))
             (reclassified (.cc GrammarRoleContract '.classify
                             (lambda (candidate context)
                               (yuanxu-classification-evidence
                                (.ref GrammarRoleContract 'identity) candidate #f
                                '(refined-rejection) context))))
             (constrained (.cc GrammarRoleContract '.obligations
                           (lambda (_candidate _context) '(refined-obligation)))))
        (each (contract) ((list reclassified constrained))
           (equal (evidence-member? contract role)  #f)
           (equal (element? contract role)  #f)
           (equal (rejects? (lambda () (validate contract role)))  #t))))
    (property "refined validation invokes its classifier exactly once"
      (bindings (calls 0) (role (empty-role)) (refined (.cc GrammarRoleContract '.classify
                         (lambda (candidate context)
                           (set! calls (+ calls 1))
                           (yuanxu-classification-evidence
                            (.ref GrammarRoleContract 'identity) candidate #f
                            '(refined-rejection) context)))))
      (equal (rejects? (lambda () (validate refined role)))  #t)
      (equal calls  1))
    (scenario "mutable declaration lists are rechecked on every call"
      (sequential ((rules (list 'rule)) (role (empty-role rules)))
        (equal (element? GrammarRoleContract role)  #t)
        (act (set-cdr! rules 'improper-tail))
        (equal (element? GrammarRoleContract role)  #f)
        (equal (element? GrammarRoleContract role)  (evidence-member? GrammarRoleContract role))
        (equal (rejects? (lambda () (validate GrammarRoleContract role)))  #t)))
    (scenario "composition obligations reject foreign roles and parents"
      (sequential ((role (empty-role)) (grammar (make-grammar 'grammar '() (list role))))
        (each (invalid) ((list (.cc grammar 'parents (list (.o)))
               (.cc grammar 'roles (list (.o)))
               (.cc grammar 'composition (list (cons 'unknown role)))
               (.cc grammar 'composition (list (cons 'append (.o))))))
           (equal (evidence-member? GrammarContract invalid)  #f)
           (equal (element? GrammarContract invalid)  #f)
           (equal (rejects? (lambda () (validate GrammarContract invalid)))  #t)))))
