;;; Resolved grammar semantics must be checked before target state construction.
(import :std/test
        (only-in :gerbil-parser/src/compiler/lr-compiler compile-lr-spec)
        (only-in :gerbil-parser/src/compiler/lr lr-spec-ref current-grammar-source-map)
        (only-in :gerbil-parser/src/compiler/normalize compile-grammar grammar-ir-ref compile-grammar/context normalized-grammar-ir normalized-grammar-source-map)
        (only-in :gerbil-parser/src/modules/parser/objects make-grammar make-grammar-role)
        (only-in :gerbil-parser/src/runtime/lr-parser lr-parse lr-rejection-condition?)
        (only-in :gerbil-parser/src/runtime/token make-token)
        (only-in :gerbil-parser/src/runtime/recognition
                 recognition-node-kind recognition-node-start recognition-node-end
                 recognition-node-children recognition-child-field recognition-child-value
                 recognition-value-start recognition-value-end))
(export resolved-grammar-test)

(def (admission-result rules (construction 'lalr) (policy 'reject))
  (with-catch
   (lambda (condition) (list (error-message condition) (error-irritants condition)))
   (lambda () (compile-lr-spec rules 'start policy #f construction) 'accepted)))

(def (nullable-repeat-result owner operand (kind 'repeat))
  (list "resolved repetition operand accepts empty input" (list owner kind operand)))

(def (parse-literals rules text (return-root? #f))
  (let (spec (compile-lr-spec
              (map (lambda (row)
                     (if (eq? (car row) 'start)
                       (list 'start (list 'alias 'Start (cadr row))) row)) rules)
              'start))
    (with-catch
     (lambda (condition)
       (if (lr-rejection-condition? condition) 'rejected (raise condition)))
     (lambda ()
       (let-values (((root rest)
                     (lr-parse spec
                      (map (lambda (index)
                             (make-token 'punctuation (string (string-ref text index))
                                         index (+ index 1)))
                           (iota (string-length text))))))
         (unless (null? rest) (error "unconsumed test input" rest))
         (if return-root? root 'accepted))))))

(def (rule-role name rules)
  (make-grammar-role name '() '() '() rules '() '() '() '() '()))

(def resolved-grammar-test
  (test-suite "resolved grammar admission and CFG alternatives"
    (test-case "direct reference to an empty rule rejects before LR conflicts"
      (check (admission-result '((start (repeat (reference item))) (item (empty))))
             => (nullable-repeat-result 'start '(reference item))))
    (test-case "indirect optional reference rejects both repetition forms"
      (for-each
       (lambda (kind)
         (check (admission-result
                 `((start (,kind (reference item)))
                   (item (reference tail)) (tail (optional (literal "a")))))
                => (nullable-repeat-result 'start '(reference item) kind)))
       '(repeat repeat1)))
    (test-case "nullable sequence through field alias and precedence rejects"
      (let (operand '(precedence left 1
                       (alias Item (field value
                         (sequence (reference left) (reference right))))))
        (check (admission-result
                `((start (repeat ,operand)) (left (empty))
                  (right (optional (literal "b")))))
               => (nullable-repeat-result 'start operand))))
    (test-case "nullable alternative propagates through a mutual reference cycle"
      (check (admission-result
              '((start (repeat (reference item)))
                (item (reference tail))
                (tail (choice (reference item) (empty)))))
             => (nullable-repeat-result 'start '(reference item))))
    (test-case "nested repetitions retain the original rule owner"
      (check (admission-result
              '((start (reference container))
                (container (alias Container
                            (sequence (literal "[")
                             (field items (repeat1 (reference item))) (literal "]"))))
                (item (optional (literal "a")))))
             => (nullable-repeat-result 'container '(reference item) 'repeat1)))
    (test-case "boolean analysis never skips nested repetition obligations"
      (for-each
       (lambda (expression)
         (check (admission-result `((start ,expression) (item (empty))))
                => (nullable-repeat-result 'start '(reference item))))
       '((sequence (literal "a") (repeat (reference item)))
         (choice (empty) (repeat (reference item)))
         (optional (repeat (reference item))))))
    (test-case "all LR constructions share resolved repetition admission"
      (for-each
       (lambda (construction)
         (check (admission-result
                 '((start (repeat (reference item))) (item (empty))) construction)
                => (nullable-repeat-result 'start '(reference item))))
       '(lalr canonical-lr1 partitioned-lr1 follow-partition-lr1 lalr-then-follow)))
    (test-case "selective GLR cannot admit a zero-width repetition cycle"
      (for-each
       (lambda (policy)
         (check (admission-result
                 '((start (repeat (reference item))) (item (empty))) 'lalr policy)
                => (nullable-repeat-result 'start '(reference item))))
       '(reject selective-glr probe)))
    (test-case "raw IR empty and layout operands reject before state construction"
      (for-each
       (lambda (operand)
         (check (admission-result `((start (repeat ,operand))))
                => (nullable-repeat-result 'start operand)))
       '((empty) (layout-end))))
    (test-case "a nullable prefix followed by a consuming suffix remains valid"
      (let (rules '((start (repeat (reference item)))
                    (item (sequence (reference prefix) (literal "b")))
                    (prefix (optional (literal "a")))))
        (check (parse-literals rules "") => 'accepted)
        (check (parse-literals rules "babb") => 'accepted)
        (check (parse-literals rules "a") => 'rejected)))
    (test-case "productive left recursion remains valid inside repeat"
      (let (rules '((start (repeat (reference item)))
                    (item (choice (sequence (reference item) (literal "a"))
                                  (literal "b")))))
        (check (parse-literals rules "baabaa") => 'accepted)
        (check (parse-literals rules "a") => 'rejected)))
    (test-case "effective POO override is analyzed rather than its parent declaration"
      (let* ((base (make-grammar 'base '()
                     (list (rule-role 'base-rules
                             '((start (repeat (reference item))) (item (literal "a")))))))
             (nullable-role (rule-role 'nullable-item '((item (empty)))))
             (derived (make-grammar 'derived (list base) '()
                        (list (cons 'override nullable-role))))
             (restored (make-grammar 'restored (list derived) '()
                         (list (cons 'override (rule-role 'consuming-item
                                               '((item (literal "b")))))))))
        (check (admission-result (grammar-ir-ref (compile-grammar derived) 'rules))
               => (nullable-repeat-result 'start '(reference item)))
        (check (parse-literals (grammar-ir-ref (compile-grammar restored) 'rules) "bb")
               => 'accepted)))
    (test-case "nullable diagnostics retain selected POO override and expression occurrence"
      (let* ((base (make-grammar 'base '()
                     (list (rule-role 'base-rules
                             '((start (alias Start (sequence (repeat (reference item))
                                                           (literal "z"))))
                               (item (literal "a")))))))
             (derived (make-grammar 'derived (list base) '()
                        (list (cons 'override (rule-role 'nullable-item '((item (empty))))))))
             (normalized (compile-grammar/context derived))
             (source-map (normalized-grammar-source-map normalized "unknown-source"))
             (result (parameterize ((current-grammar-source-map source-map))
                       (admission-result (grammar-ir-ref (normalized-grammar-ir normalized) 'rules))))
             (details (list-ref (cadr result) 3))
             (origin (cdr (assq 'expressionOrigin details)))
             (references (cdr (assq 'nullableReferencePath details))))
        (check (cdr (assq 'rule origin)) => 'start)
        (check (cdr (assq 'expressionPath origin)) => '(0 0))
        (check (cdr (assq 'componentOwner (cdr (assq 'source origin)))) => 'base-rules)
        (check (map (lambda (row) (cdr (assq 'rule row))) references) => '(item))
        (let (source (cdr (assq 'source (car references))))
          (check (cdr (assq 'componentOwner source)) => 'nullable-item)
          (check (cdr (assq 'compositionOperation source)) => 'override))
        ;; The dynamic context ends with this compile, never contaminating another.
        (check (current-grammar-source-map) => '())))
    (test-case "finite nullable witness skips cycles and retains the effective empty exit"
      (let* ((rules '((start (repeat (reference a)))
                      (a (choice (reference b) (reference epsilon)))
                      (b (reference a)) (epsilon (empty))))
             (result (parameterize ((current-grammar-source-map '((rule))))
                       (admission-result rules)))
             (details (list-ref (cadr result) 3)))
        (check (map (lambda (row) (cdr (assq 'rule row)))
                    (cdr (assq 'nullableReferencePath details))) => '(a epsilon))))
    (test-case "nullable explanation follows a shared DAG without expanding derivation trees"
      (let* ((names (map (lambda (index) (string->symbol (string-append "nullable-" (number->string index)))) (iota 32)))
             (rules (cons (list 'start (list 'repeat (list 'reference (car names))))
                          (map (lambda (name index)
                                 (list name (if (= index 31) '(empty)
                                              (list 'sequence (list 'reference (list-ref names (+ index 1)))
                                                              (list 'reference (list-ref names (+ index 1)))))))
                               names (iota 32))))
             (result (parameterize ((current-grammar-source-map '((rule)))) (admission-result rules)))
             (details (list-ref (cadr result) 3)))
        (check (map (lambda (row) (cdr (assq 'rule row)))
                    (cdr (assq 'nullableReferencePath details))) => names)))
    (test-case "CFG choice reordering preserves acceptance with shared token prefixes"
      (for-each
       (lambda (alternatives)
         (let (rules `((start (alias Start
                               (sequence (field prefix (choice ,@alternatives))
                                         (literal "c"))))))
           (check (parse-literals rules "abc") => 'accepted)
           (check (parse-literals rules "ac") => 'accepted)
           (check (parse-literals rules "ab") => 'rejected)))
       '(((literal "a") (sequence (literal "a") (literal "b")))
         ((sequence (literal "a") (literal "b")) (literal "a")))))
    (test-case "choice ordering preserves node fields and exact ranges"
      (let* ((short '(literal "a"))
             (long '(sequence (literal "a") (literal "b")))
             (forward `((start (sequence (field prefix (choice ,short ,long))
                                        (literal "c")))))
             (reverse `((start (sequence (field prefix (choice ,long ,short))
                                        (literal "c")))))
             (root (parse-literals forward "abc" #t)))
        (check root => (parse-literals reverse "abc" #t))
        (check (recognition-node-kind root) => 'Start)
        (check (list (recognition-node-start root) (recognition-node-end root)) => '(0 3))
        (check (map recognition-child-field (recognition-node-children root)) => '(prefix #f))
        (let (prefix (recognition-child-value (car (recognition-node-children root))))
          (check (list (recognition-value-start prefix) (recognition-value-end prefix)) => '(0 2)))))
    (test-case "CFG helper-reference alternatives preserve enclosing continuation"
      (let (rules '((start (sequence (reference prefix) (literal "c")))
                    (prefix (choice (literal "a") (reference longer)))
                    (longer (sequence (literal "a") (literal "b")))))
        (check (parse-literals rules "abc") => 'accepted)
        (check (parse-literals rules "ac") => 'accepted)
        (check (parse-literals rules "abbc") => 'rejected)))))
