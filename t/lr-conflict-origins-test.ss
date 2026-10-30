(import :gerbil-parser/language-test-support)
;;; Effective expression occurrences must survive rejected LR table admission.
(import :std/test
        (only-in :gerbil-parser/src/compiler/lr-compiler compile-lr-spec)
        (only-in :gerbil-parser/src/compiler/lr lower-rules current-grammar-source-map)
        (only-in :gerbil-parser/src/compiler/normalize compile-grammar/context
                 normalized-grammar-ir normalized-grammar-source-map grammar-ir-ref)
        (only-in :gerbil-parser/src/grammar/declaration-objects make-grammar make-grammar-role))

(def (admission-result rules (construction 'lalr))
  (with-catch
   (lambda (condition) (list (error-message condition) (error-irritants condition)))
   (lambda () (compile-lr-spec rules 'start 'reject #f construction) 'accepted)))
(def (rule-role name rules)
  (make-grammar-role name '() '() '() rules '() '() '() '() '()))
(deflanguage-parser-tests lr-conflict-origins-test "LR conflict source origins"
  (engine)
    (scenario "lowering origin replay preserves tables and distinguishes equal helpers"
      (sequential ((rules '((start (alias Start
                              (sequence (optional (literal "a"))
                                        (optional (literal "a")))))))
             (origins '())
             (plain (lower-rules rules 'start))
             (replayed
              (lower-rules rules 'start #f
                (lambda (production owner path expression)
                  (set! origins (cons (list (car production) owner path (car expression)) origins))))))
        (equal replayed  plain)
        ;; Both alternatives of each optional helper retain that occurrence.
        (equal (map caddr (reverse origins))  '((0 0) (0 0) (0 1) (0 1) (0) ()))
        (equal (map cadr origins)  '(start start start start start start))))
    (scenario "rejected LR conflicts retain effective POO origins across constructions"
      (sequential ((base (make-grammar 'base '()
                    (list (rule-role 'base-rules
                            '((start (reference expr)) (expr (literal "a")))))))
             (derived (make-grammar 'derived (list base) '()
                       (list (cons 'override
                               (rule-role 'ambiguous-expr
                                 '((expr (choice (sequence (reference expr) (literal "+")
                                                           (reference expr))
                                                 (literal "a")))))))))
             (normalized (compile-grammar/context derived))
             (rules (grammar-ir-ref (normalized-grammar-ir normalized) 'rules))
             (source-map (normalized-grammar-source-map normalized "unknown-source")))
        (each (construction) ('(lalr canonical-lr1 partitioned-lr1 follow-partition-lr1))
           (sequential ((result (parameterize ((current-grammar-source-map source-map))
                            (admission-result rules construction)))
                  (details (car (reverse (cadr result))))
                  (conflict (cdr (assq 'lrConflict details)))
                  (reductions (filter-map (lambda (row) (let (origin (assq 'expressionOrigin row))
                                                        (and origin (cdr origin))))
                                          (cdr (assq 'actions conflict))))
                  (shifts (cdr (assq 'shiftItems conflict))))
             (equal (car result)  "unresolved shift/reduce conflict requires precedence")
             (equal (cdr (assq 'terminal conflict))  '(terminal literal "+"))
             (equal (length reductions)  1)
             (equal (pair? shifts)  #t)
             (each (origin) ((append reductions (map (lambda (row) (cdr (assq 'expressionOrigin row))) shifts)))
                (equal (cdr (assq 'rule origin))  'expr)
                (equal (cdr (assq 'expressionPath origin))  '(0))
                (equal (cdr (assq 'loweringOperation origin))  'sequence)
                (scope ((source (cdr (assq 'source origin))))
                  (equal (cdr (assq 'componentOwner source))  'ambiguous-expr)
                  (equal (cdr (assq 'compositionOperation source))  'override)))))
        (equal (current-grammar-source-map)  '())))
    (scenario "helper conflicts report expression occurrences without fabricated sources"
      (each (construction) ('(lalr canonical-lr1 partitioned-lr1 follow-partition-lr1))
         (sequential ((result (admission-result
                         '((start (sequence (optional (literal "a"))
                                            (optional (literal "a"))))) construction))
                (conflict (cdr (assq 'lrConflict (car (reverse (cadr result)))))))
           (equal (car result)  "unresolved shift/reduce conflict requires precedence")
           (each (origin) ((append (filter-map (lambda (row) (let (entry (assq 'expressionOrigin row))
                                              (and entry (cdr entry))))
                                (cdr (assq 'actions conflict)))
                    (map (lambda (row) (cdr (assq 'expressionOrigin row)))
                         (cdr (assq 'shiftItems conflict)))))
              (equal (cdr (assq 'rule origin))  'start)
              (matches (member (cdr (assq 'expressionPath origin)) '((0) (1)))  values)
              (equal (cdr (assq 'loweringOperation origin))  'optional)
              (equal (cdr (assq 'source origin))  '())))))
    (scenario "reduce conflicts retain both source occurrences and no shift witnesses"
      (each (construction) ('(lalr canonical-lr1 partitioned-lr1 follow-partition-lr1))
         (sequential ((result (admission-result
                         '((start (choice (reference left) (reference right)))
                           (left (literal "a")) (right (literal "a"))) construction))
                (conflict (cdr (assq 'lrConflict (car (reverse (cadr result))))))
                (origins (map (lambda (row) (cdr (assq 'expressionOrigin row)))
                              (cdr (assq 'actions conflict)))))
           (equal (car result)  "unresolved reduce/reduce conflict")
           (equal (length origins)  2)
           (scope ((owners (map (lambda (row) (cdr (assq 'rule row))) origins)))
             (matches (memq 'left owners)  values)
             (matches (memq 'right owners)  values))
           (equal (map (lambda (row) (cdr (assq 'expressionPath row))) origins)  '(() ()))
           (equal (cdr (assq 'shiftItems conflict))  '()))))
)
