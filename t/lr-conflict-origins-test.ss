;;; Effective expression occurrences must survive rejected LR table admission.
(import :std/test
        (only-in :gerbil-parser/src/compiler/lr-compiler compile-lr-spec)
        (only-in :gerbil-parser/src/compiler/lr lower-rules current-grammar-source-map)
        (only-in :gerbil-parser/src/compiler/normalize compile-grammar/context
                 normalized-grammar-ir normalized-grammar-source-map grammar-ir-ref)
        (only-in :gerbil-parser/src/modules/parser/objects make-grammar make-grammar-role))
(export lr-conflict-origins-test)
(def (admission-result rules (construction 'lalr))
  (with-catch
   (lambda (condition) (list (error-message condition) (error-irritants condition)))
   (lambda () (compile-lr-spec rules 'start 'reject #f construction) 'accepted)))
(def (rule-role name rules)
  (make-grammar-role name '() '() '() rules '() '() '() '() '()))
(def lr-conflict-origins-test
  (test-suite "LR conflict source origins"
    (test-case "lowering origin replay preserves tables and distinguishes equal helpers"
      (let* ((rules '((start (alias Start
                              (sequence (optional (literal "a"))
                                        (optional (literal "a")))))))
             (origins '())
             (plain (lower-rules rules 'start))
             (replayed
              (lower-rules rules 'start #f
                (lambda (production owner path expression)
                  (set! origins (cons (list (car production) owner path (car expression)) origins))))))
        (check replayed => plain)
        ;; Both alternatives of each optional helper retain that occurrence.
        (check (map caddr (reverse origins)) => '((0 0) (0 0) (0 1) (0 1) (0) ()))
        (check (map cadr origins) => '(start start start start start start))))
    (test-case "rejected LR conflicts retain effective POO origins across constructions"
      (let* ((base (make-grammar 'base '()
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
        (for-each
         (lambda (construction)
           (let* ((result (parameterize ((current-grammar-source-map source-map))
                            (admission-result rules construction)))
                  (details (car (reverse (cadr result))))
                  (conflict (cdr (assq 'lrConflict details)))
                  (reductions (filter-map (lambda (row) (let (origin (assq 'expressionOrigin row))
                                                        (and origin (cdr origin))))
                                          (cdr (assq 'actions conflict))))
                  (shifts (cdr (assq 'shiftItems conflict))))
             (check (car result) => "unresolved shift/reduce conflict requires precedence")
             (check (cdr (assq 'terminal conflict)) => '(terminal literal "+"))
             (check (length reductions) => 1)
             (check (pair? shifts) => #t)
             (for-each
              (lambda (origin)
                (check (cdr (assq 'rule origin)) => 'expr)
                (check (cdr (assq 'expressionPath origin)) => '(0))
                (check (cdr (assq 'loweringOperation origin)) => 'sequence)
                (let (source (cdr (assq 'source origin)))
                  (check (cdr (assq 'componentOwner source)) => 'ambiguous-expr)
                  (check (cdr (assq 'compositionOperation source)) => 'override)))
              (append reductions (map (lambda (row) (cdr (assq 'expressionOrigin row))) shifts)))))
         '(lalr canonical-lr1 partitioned-lr1 follow-partition-lr1))
        (check (current-grammar-source-map) => '())))
    (test-case "helper conflicts report expression occurrences without fabricated sources"
      (for-each
       (lambda (construction)
         (let* ((result (admission-result
                         '((start (sequence (optional (literal "a"))
                                            (optional (literal "a"))))) construction))
                (conflict (cdr (assq 'lrConflict (car (reverse (cadr result)))))))
           (check (car result) => "unresolved shift/reduce conflict requires precedence")
           (for-each
            (lambda (origin)
              (check (cdr (assq 'rule origin)) => 'start)
              (check (member (cdr (assq 'expressionPath origin)) '((0) (1))) ? values)
              (check (cdr (assq 'loweringOperation origin)) => 'optional)
              (check (cdr (assq 'source origin)) => '()))
            (append (filter-map (lambda (row) (let (entry (assq 'expressionOrigin row))
                                              (and entry (cdr entry))))
                                (cdr (assq 'actions conflict)))
                    (map (lambda (row) (cdr (assq 'expressionOrigin row)))
                         (cdr (assq 'shiftItems conflict)))))))
       '(lalr canonical-lr1 partitioned-lr1 follow-partition-lr1)))
    (test-case "reduce conflicts retain both source occurrences and no shift witnesses"
      (for-each
       (lambda (construction)
         (let* ((result (admission-result
                         '((start (choice (reference left) (reference right)))
                           (left (literal "a")) (right (literal "a"))) construction))
                (conflict (cdr (assq 'lrConflict (car (reverse (cadr result))))))
                (origins (map (lambda (row) (cdr (assq 'expressionOrigin row)))
                              (cdr (assq 'actions conflict)))))
           (check (car result) => "unresolved reduce/reduce conflict")
           (check (length origins) => 2)
           (let (owners (map (lambda (row) (cdr (assq 'rule row))) origins))
             (check (memq 'left owners) ? values)
             (check (memq 'right owners) ? values))
           (check (map (lambda (row) (cdr (assq 'expressionPath row))) origins) => '(() ()))
           (check (cdr (assq 'shiftItems conflict)) => '())))
       '(lalr canonical-lr1 partitioned-lr1 follow-partition-lr1)))
))
