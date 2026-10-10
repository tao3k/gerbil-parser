;;; Resolved grammar semantics must be checked before target state construction.
(import :std/test
        (only-in :std/test/base TestCase test-case-add!)
        (only-in :gerbil-parser/src/compiler/lr-compiler compile-lr-spec)
        (only-in :gerbil-parser/src/compiler/lr lr-spec-ref current-grammar-source-map compute-nullable compute-first compute-productive compute-completion terminal-symbol?
                 lower-rules production-lhs production-rhs production-table production-terminal-catalog
                 sequence-first sequence-nullable? base-symbol nonterminal-symbol? nonterminal-name)
        (only-in :gerbil-parser/src/compiler/funcs compiler-index-set-union)
        (only-in :gerbil-parser/src/compiler/lr-automaton make-item-layout make-core-symbol-catalog)
        (only-in :gerbil-parser/src/compiler/lr-lookahead make-core-suffix-catalog)
        (only-in :gerbil-parser/src/compiler/normalize compile-grammar grammar-ir-ref compile-grammar/context normalized-grammar-ir normalized-grammar-source-map)
        (only-in :gerbil-parser/src/modules/parser/objects make-grammar make-grammar-role)
        (only-in :gerbil-parser/src/runtime/lr-parser lr-parse lr-rejection-condition?)
        (only-in :gerbil-parser/src/runtime/token make-token)
        (only-in :gerbil-parser/src/runtime/recognition
                 recognition-node-kind recognition-node-start recognition-node-end
                 recognition-node-children recognition-child-field recognition-child-value
                 recognition-value-start recognition-value-end))
(import (only-in :gerbil-parser/t/fixtures/lr1-construction
                 lr1-not-lalr-rules shared-lookahead-rules precedence-expression-rules
                 mixed-context-rules lr1-context-family-rules
                 state-local-candidate-family-rules mixed-context-family-rules
                 acyclic-mixed-context-family-rules inactive-core-conflict-rules))
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

;; Independent repeated-scan oracle retained only in this semantic control.
(def (reference-nullable productions)
  (let (index (make-table test: eq?))
    (let converge ()
      (let (changed? #f)
        (for-each
         (lambda (production)
           (when (and (not (table-ref index (production-lhs production) #f))
                      (every (lambda (value)
                               (let (symbol (base-symbol value))
                                 (and (nonterminal-symbol? symbol)
                                      (table-ref index (nonterminal-name symbol) #f))))
                             (production-rhs production)))
             (table-set! index (production-lhs production) #t)
             (set! changed? #t))) productions)
        (when changed? (converge))))
    index))

(def (nullable-production name rhs) (list 0 name rhs #f #f))

(def (check-nullable-reference productions)
  (let (reference (reference-nullable productions))
    (let-values (((names index) (compute-nullable productions)))
      (check names => (filter (lambda (name) (table-ref reference name #f)) '(a b c)))
      (for-each (lambda (name)
                  (check (table-ref index name #f) => (table-ref reference name #f)))
                '(a b c missing)))))

(def (reference-first productions nullable)
  (let ((index (make-table test: eq?)) (names '()) (terminals '((terminal eof))))
    (def (union left right)
      (foldl (lambda (value found) (if (member value found) found (append found (list value)))) left right))
    (for-each
     (lambda (production)
       (set! names (if (memq (production-lhs production) names) names
                      (append names (list (production-lhs production)))))
       (for-each (lambda (value)
                   (let (symbol (base-symbol value))
                     (when (terminal-symbol? symbol)
                       (set! terminals (union terminals (list symbol))))))
                 (production-rhs production))) productions)
    (def (prefix rhs)
      (if (null? rhs) '()
          (let (symbol (base-symbol (car rhs)))
            (if (terminal-symbol? symbol) (list symbol)
                (let* ((name (nonterminal-name symbol)) (found (table-ref index name '())))
                  (if (table-ref nullable name #f) (union found (prefix (cdr rhs))) found))))))
    (let converge ()
      (let (changed? #f)
        (for-each
         (lambda (production)
           (let* ((name (production-lhs production)) (before (table-ref index name '()))
                  (after (union before (prefix (production-rhs production)))))
             (unless (equal? before after)
               (table-set! index name after) (set! changed? #t)))) productions)
        (when changed? (converge))))
    (map (lambda (name)
           (cons name (filter (lambda (terminal) (member terminal (table-ref index name '()))) terminals))) names)))

(def (check-first-reference productions)
  (let* ((nullable (reference-nullable productions))
         (expected (reference-first productions nullable)))
    (let-values (((rows index) (compute-first productions nullable)))
      (check rows => expected)
      (for-each (lambda (row) (check (table-ref index (car row)) => (cdr row))) expected))))

(def (reference-productive productions)
  (let (index (make-table test: eq?))
    (let converge ()
      (let (changed? #f)
        (for-each
         (lambda (production)
           (when (and (not (table-ref index (production-lhs production) #f))
                      (every (lambda (value)
                               (let (symbol (base-symbol value))
                                 (or (terminal-symbol? symbol)
                                     (table-ref index (nonterminal-name symbol) #f))))
                             (production-rhs production)))
             (table-set! index (production-lhs production) #t)
             (set! changed? #t))) productions)
        (when changed? (converge))))
    index))

(def (check-productive-reference productions)
  (let ((expected (reference-productive productions))
        (nullable (reference-nullable productions))
        (names '()))
    (for-each (lambda (production)
                (unless (memq (production-lhs production) names)
                  (set! names (append names (list (production-lhs production)))))) productions)
    (let-values (((nullable-found nullable-index found index) (compute-completion productions)))
      (check nullable-found => (filter (lambda (name) (table-ref nullable name #f)) names))
      (for-each (lambda (name) (check (table-ref nullable-index name #f) => (table-ref nullable name #f))) (cons 'missing names))
      (check found => (filter (lambda (name) (table-ref expected name #f)) names))
      (for-each (lambda (name)
                  (check (table-ref index name #f) => (table-ref expected name #f))
                  (when (table-ref nullable name #f)
                    (check (table-ref index name #f) => #t))) (cons 'missing names)))))

;; Independent forward suffix scans check the entire catalog, including
;; unused/padded slots. The optimized builder folds backwards and uses masks.
(def (check-suffix-catalog productions)
  (let (table (production-table productions))
    (let-values (((terminals terminal-index) (production-terminal-catalog productions))
                 ((names nullable) (compute-nullable productions)))
      (let-values (((first-rows first) (compute-first productions nullable)))
       (let* ((layout (make-item-layout table terminals))
             (symbols (make-core-symbol-catalog table layout))
             (expected-masks (make-vector (vector-length symbols) 0))
             (expected-nullable (make-vector (vector-length symbols) #f)))
        (let production-loop ((id 0))
          (when (< id (vector-length table))
            (let suffix-loop ((rest (production-rhs (vector-ref table id))) (dot 0))
              (let (item (+ dot (* id (cdr layout))))
                (vector-set! expected-masks item
                  (foldl (lambda (terminal mask)
                           (compiler-index-set-union mask
                             (arithmetic-shift 1 (table-ref terminal-index terminal))))
                         0 (sequence-first rest first nullable)))
                (vector-set! expected-nullable item (sequence-nullable? rest nullable)))
              (unless (null? rest)
                (suffix-loop (cdr rest) (+ dot 1))))
            (production-loop (+ id 1))))
        (let-values (((masks nullable-tails)
                      (make-core-suffix-catalog table layout symbols first nullable terminal-index)))
          (check masks => expected-masks)
          (check nullable-tails => expected-nullable)))))))

;;; Named case procedures retain independent assertions and separate native C hosts.

(def (suffix-facts-case)
  (for-each (lambda (rules) (check-suffix-catalog (lower-rules rules 'source-file)))
    [shared-lookahead-rules lr1-not-lalr-rules precedence-expression-rules
     inactive-core-conflict-rules (mixed-context-family-rules 8)])
  ;; Empty FIRST is not nullable; a missing reference blocks a suffix.
  ;; Marked symbols use the same underlying terminal/nonterminal facts.
  (let* ((a '(nonterminal a)) (b '(nonterminal b))
         (dead '(nonterminal dead)) (missing '(nonterminal missing))
         (x '(terminal literal "x"))
         (marked-a (list 'marked a '((alias A))))
         (marked-x (list 'marked x '((alias X))))
         (operands [a b dead missing x marked-a marked-x]))
    (for-each
      (lambda (tail)
        (check-suffix-catalog
          (list (list 0 'root (append operands (list tail)) #f #f)
                (list 1 'a '() #f #f)
                (list 2 'a (list x) #f #f)
                (list 3 'b (list x) #f #f)
                (list 4 'dead (list dead) #f #f)))) operands)
    (check-suffix-catalog
      (list (list 0 'root (make-list 256 marked-a) #f #f)
            (list 1 'a '() #f #f) (list 2 'a (list x) #f #f)))))

(def (completion-publications-case)
  (let ((prefix (list (nullable-production 'c '((nonterminal a) (nonterminal b)))
                      (nullable-production 'a '((nonterminal d)))
                      (nullable-production 'b '((nonterminal a) (nonterminal a)))
                      (nullable-production 'd '((nonterminal b)))
                      (nullable-production 'blocked '((terminal literal "x") (nonterminal missing)))))
        (seeds (list (nullable-production 'a '((marked (terminal literal "x") (field value))))
                     (nullable-production 'b '()))))
    (check-productive-reference (append prefix seeds))
    (check-productive-reference (append prefix (reverse seeds))))
  (check-productive-reference '())
  (check-productive-reference
   (list (nullable-production 'a '((nonterminal missing) (nonterminal missing)))
         (nullable-production 'b '((terminal literal "x") (nonterminal a)))
         (nullable-production 'c '((marked (nonterminal b) (field value)))))))

(def (cyclic-productivity-case)
  (let (operands '(() ((terminal literal "x")) ((nonterminal a))
                     ((nonterminal b)) ((nonterminal c))
                     ((nonterminal a) (nonterminal a))
                     ((terminal literal "x") (nonterminal b))
                     ((marked (nonterminal b) (field value)))
                     ((nonterminal missing))))
    (for-each (lambda (a)
                (for-each (lambda (b)
                            (for-each (lambda (c)
                                        (check-productive-reference
                                         (list (nullable-production 'a a)
                                               (nullable-production 'b b)
                                               (nullable-production 'c c)))) operands)) operands)) operands)))

(def (family-productivity-case)
  (for-each (lambda (rules) (check-productive-reference (lower-rules rules 'source-file)))
            (list lr1-not-lalr-rules shared-lookahead-rules precedence-expression-rules
                  mixed-context-rules (lr1-context-family-rules 8)
                  (state-local-candidate-family-rules 4) (mixed-context-family-rules 4)
                  (acyclic-mixed-context-family-rules 4)))
  (check-productive-reference
   (list (nullable-production 'a '((nonterminal b) (nonterminal b)))
         (nullable-production 'b '((nonterminal c)))
         (nullable-production 'c '((nonterminal a)))
         (nullable-production 'c '((marked (terminal literal "x") (field value))))
         (nullable-production 'a '((nonterminal missing)))
         (nullable-production 'unused '((nonterminal unused))))))

(def (reverse-productivity-case)
  (let* ((names (map (lambda (n) (string->symbol (string-append "productive-chain-" (number->string n)))) (iota 4096)))
         (productions (map (lambda (name next)
                             (nullable-production name (if next (list (list 'nonterminal next))
                                                          '((terminal literal "x")))))
                           names (append (cdr names) '(#f)))))
    (let-values (((found index) (compute-productive productions)))
      (check found => names)
      (check (every (lambda (name) (table-ref index name #f)) names) => #t))))

(def (dead-start-case)
  (for-each
   (lambda (rules)
     (for-each
      (lambda (route)
        (for-each
         (lambda (policy)
           (let (result (admission-result rules route policy))
             (check (car result) => "LR start rule has no finite terminal derivation")
             (check (car (cadr result)) => 'start)))
         '(reject selective-glr probe)))
      '(lalr canonical-lr1 partitioned-lr1 follow-partition-lr1 lalr-then-follow)))
   '(((start (reference start)))
     ((start (sequence (literal "a") (reference tail))) (tail (reference tail))))))

(def (finite-exits-case)
  (check (parse-literals '((start (empty)) (dead (reference dead))) "") => 'accepted)
  (check (parse-literals '((start (choice (sequence (reference start) (literal "a"))
                                        (literal "b")))
                          (dead (reference dead))) "baa") => 'accepted)
  (check (parse-literals '((start (choice (literal "b") (reference dead)))
                          (dead (sequence (literal "a") (reference dead)))) "b") => 'accepted)
  (check (parse-literals '((start (choice (literal "b") (reference dead)))
                          (dead (sequence (literal "a") (reference dead)))) "a") => 'rejected))

(def (blocked-productivity-case)
  (let* ((rules '((start (alias Start (choice (reference a)
                                            (sequence (literal "x") (reference b)))))
                  (a (reference start)) (b (reference b))))
         (result (parameterize ((current-grammar-source-map '((rule))))
                   (admission-result rules)))
         (details (cadr (cadr result)))
         (blockers (cdr (assq 'unproductiveRuleBlockers details)))
         (start (car blockers))
         (references (cdr (assq 'blockedReferences start))))
    (check (map (lambda (row) (cdr (assq 'rule (cdr (assq 'ruleOrigin row))))) blockers)
           => '(start b a))
    (check (map (lambda (row) (cdr (assq 'rule row))) references) => '(a b))
    (check (map (lambda (row) (cdr (assq 'expressionPath (cdr (assq 'referenceOrigin row))))) references)
           => '((0 0) (0 1 1)))
    (check (length blockers) => 3)))

(def (authored-productivity-case)
  (let* ((rules '((start (alias Start (repeat1 (field value (reference dead)))))
                  (dead (reference dead))))
         (result (admission-result rules))
         (details (cadr (cadr result)))
         (blockers (cdr (assq 'unproductiveRuleBlockers details)))
         (reference (car (cdr (assq 'blockedReferences (car blockers))))))
    (check (map (lambda (row) (cdr (assq 'rule (cdr (assq 'ruleOrigin row))))) blockers)
           => '(start dead))
    (check (cdr (assq 'expressionPath (cdr (assq 'referenceOrigin reference)))) => '(0 0 0)))
  (for-each (lambda (kind)
              (check (parse-literals (list (list 'start (list kind '(reference dead)))
                                         '(dead (sequence (literal "a") (reference dead)))) "") => 'accepted))
            '(optional repeat))
  ;; Finite epsilon admission does not discharge independent LR conflicts.
  (check (car (admission-result '((start (optional (reference dead)))
                                 (dead (reference dead)))))
         => "unresolved reduce/reduce conflict"))

(def (poo-productivity-case)
  (let* ((base (make-grammar 'base '()
                 (list (rule-role 'base-rules '((start (reference item)) (item (literal "a")))))))
         (dead (make-grammar 'dead (list base) '()
                 (list (cons 'override (rule-role 'dead-item '((item (reference item))))))))
         (normalized (compile-grammar/context dead))
         (result (parameterize ((current-grammar-source-map (normalized-grammar-source-map normalized "unknown-source")))
                   (admission-result (grammar-ir-ref (normalized-grammar-ir normalized) 'rules))))
         (blockers (cdr (assq 'unproductiveRuleBlockers (cadr (cadr result)))))
         (item-source (cdr (assq 'source (cdr (assq 'ruleOrigin (cadr blockers)))))))
    (check (cdr (assq 'componentOwner item-source)) => 'dead-item)
    (check (cdr (assq 'compositionOperation item-source)) => 'override)
    (for-each
     (lambda (exit)
       (let (restored (make-grammar 'restored (list dead) '()
                       (list (cons 'override (rule-role 'restored-item (list (list 'item exit)))))))
         (check (admission-result (grammar-ir-ref (compile-grammar restored) 'rules)) => 'accepted)))
     '((literal "b") (empty)))
    (check (current-grammar-source-map) => '())))

(def (cyclic-first-case)
  (let (operands '(() ((terminal literal "x")) ((terminal literal "y"))
                     ((nonterminal a)) ((nonterminal b)) ((nonterminal c))
                     ((nonterminal a) (terminal literal "x"))
                     ((marked (nonterminal b) (field value)))))
    (for-each (lambda (a)
                (for-each (lambda (b)
                            (for-each (lambda (c)
                                        (check-first-reference
                                         (list (nullable-production 'a a)
                                               (nullable-production 'b b)
                                               (nullable-production 'c c)))) operands)) operands)) operands)))

(def (family-first-case)
  (for-each (lambda (rules) (check-first-reference (lower-rules rules 'source-file)))
            (list lr1-not-lalr-rules shared-lookahead-rules precedence-expression-rules
                  mixed-context-rules (lr1-context-family-rules 8)
                  (state-local-candidate-family-rules 4) (mixed-context-family-rules 4)
                  (acyclic-mixed-context-family-rules 4))))

(def (first-prefix-case)
  (check-first-reference
   (list (nullable-production 'c '((marked (nonterminal a) (field value))))
         (nullable-production 'a '((nonterminal b) (terminal literal "z")))
         (nullable-production 'b '())
         (nullable-production 'a '((terminal literal "x")))
         (nullable-production 'a '((nonterminal b) (nonterminal b) (terminal literal "z")))
         (nullable-production 'c '((nonterminal c) (terminal eof)))))
  (check-first-reference
   (list (nullable-production 'a '((nonterminal b) (terminal literal "blocked")))
         (nullable-production 'b '((marked (terminal literal "y") (field value))))
         (nullable-production 'c '((nonterminal missing) (terminal literal "blocked"))))))

(def (incomplete-prefix-case)
  (let (productions
        (list (nullable-production 'a '((terminal literal "x") (nonterminal b)))
              (nullable-production 'b '((nonterminal b)))
              (nullable-production 'c '((nonterminal a)))))
    (check-first-reference productions)
    (let-values (((rows index) (compute-first productions (reference-nullable productions))))
      (check (table-ref index 'a) => '((terminal literal "x")))
      (check (table-ref index 'b) => '())
      (check (table-ref index 'c) => '((terminal literal "x"))))))

(def (bignum-first-case)
  (let* ((terminals (map (lambda (n) (list 'terminal 'literal (number->string n))) (iota 128)))
         (productions
          (append (list (nullable-production 'a '((nonterminal b)))
                        (nullable-production 'b '((nonterminal c)))
                        (nullable-production 'c '((nonterminal a))))
                  (map (lambda (terminal) (nullable-production 'b (list terminal))) terminals))))
    (check-first-reference productions)
    (let-values (((rows index) (compute-first productions (reference-nullable productions))))
      (check (table-ref index 'a) => terminals))))

(def (reverse-first-case)
  (let* ((names (map (lambda (n) (string->symbol (string-append "first-chain-" (number->string n)))) (iota 4096)))
         (productions (map (lambda (name next)
                             (nullable-production name (if next (list (list 'nonterminal next))
                                                          '((terminal literal "x")))))
                           names (append (cdr names) '(#f)))))
    (let-values (((rows index) (compute-first productions (reference-nullable productions))))
      (check (map car rows) => names)
      (check (every (lambda (name) (equal? (table-ref index name) '((terminal literal "x")))) names) => #t))))

(def (cyclic-nullability-case)
  (let (operands '(() ((terminal literal "x")) ((nonterminal a))
                     ((nonterminal b)) ((nonterminal c))
                     ((nonterminal a) (nonterminal a))
                     ((nonterminal a) (nonterminal b))
                     ((marked (nonterminal b) (field value)))))
    (for-each (lambda (a)
                (for-each (lambda (b)
                            (for-each (lambda (c)
                                        (check-nullable-reference
                                         (list (nullable-production 'a a)
                                               (nullable-production 'b b)
                                               (nullable-production 'c c)))) operands)) operands)) operands)))

(def (nullable-occurrences-case)
  (check-nullable-reference
   (list (nullable-production 'a '((nonterminal b) (nonterminal b)))
         (nullable-production 'a '((nonterminal missing)))
         (nullable-production 'b '((nonterminal c) (terminal literal "x")))
         (nullable-production 'b '())
         (nullable-production 'c '((nonterminal c)))))
  (check-nullable-reference
   (list (nullable-production 'a '((nonterminal missing)))
         (nullable-production 'b '((nonterminal a)))
         (nullable-production 'c '())))
  (check-nullable-reference
   (list (nullable-production 'a '((marked (terminal literal "x") (field value))))
         (nullable-production 'b '()) (nullable-production 'c '())))
  (let-values (((names index)
                (compute-nullable
                 (list (nullable-production 'c '()) (nullable-production 'b '())
                       (nullable-production 'c '()) (nullable-production 'a '())))))
    (check names => '(c b a))))

(def (reverse-nullability-case)
  (let* ((names (map (lambda (n) (string->symbol (string-append "chain-" (number->string n)))) (iota 4096)))
         (productions (map (lambda (name next)
                             (nullable-production name (if next (list (list 'nonterminal next)) '())))
                           names (append (cdr names) '(#f)))))
    (let-values (((found index) (compute-nullable productions)))
      (check found => names)
      (check (every (lambda (name) (table-ref index name #f)) names) => #t))))

(def (empty-reference-case)
  (check (admission-result '((start (repeat (reference item))) (item (empty))))
         => (nullable-repeat-result 'start '(reference item))))

(def (optional-reference-case)
  (for-each
   (lambda (kind)
     (check (admission-result
             `((start (,kind (reference item)))
               (item (reference tail)) (tail (optional (literal "a")))))
            => (nullable-repeat-result 'start '(reference item) kind)))
   '(repeat repeat1)))

(def (wrapped-nullability-case)
  (let (operand '(precedence left 1
                   (alias Item (field value
                     (sequence (reference left) (reference right))))))
    (check (admission-result
            `((start (repeat ,operand)) (left (empty))
              (right (optional (literal "b")))))
           => (nullable-repeat-result 'start operand))))

(def (mutual-nullability-case)
  (check (admission-result
          '((start (repeat (reference item)))
            (item (reference tail))
            (tail (choice (reference item) (empty)))))
         => (nullable-repeat-result 'start '(reference item))))

(def (nested-repetition-case)
  (check (admission-result
          '((start (reference container))
            (container (alias Container
                        (sequence (literal "[")
                         (field items (repeat1 (reference item))) (literal "]"))))
            (item (optional (literal "a")))))
         => (nullable-repeat-result 'container '(reference item) 'repeat1)))

(def (obligation-traversal-case)
  (for-each
   (lambda (expression)
     (check (admission-result `((start ,expression) (item (empty))))
            => (nullable-repeat-result 'start '(reference item))))
   '((sequence (literal "a") (repeat (reference item)))
     (choice (empty) (repeat (reference item)))
     (optional (repeat (reference item))))))

(def (construction-admission-case)
  (for-each
   (lambda (construction)
     (check (admission-result
             '((start (repeat (reference item))) (item (empty))) construction)
            => (nullable-repeat-result 'start '(reference item))))
   '(lalr canonical-lr1 partitioned-lr1 follow-partition-lr1 lalr-then-follow)))

(def (glr-repetition-case)
  (for-each
   (lambda (policy)
     (check (admission-result
             '((start (repeat (reference item))) (item (empty))) 'lalr policy)
            => (nullable-repeat-result 'start '(reference item))))
   '(reject selective-glr probe)))

(def (raw-nullability-case)
  (for-each
   (lambda (operand)
     (check (admission-result `((start (repeat ,operand))))
            => (nullable-repeat-result 'start operand)))
   '((empty) (layout-end))))

(def (consuming-suffix-case)
  (let (rules '((start (repeat (reference item)))
                (item (sequence (reference prefix) (literal "b")))
                (prefix (optional (literal "a")))))
    (check (parse-literals rules "") => 'accepted)
    (check (parse-literals rules "babb") => 'accepted)
    (check (parse-literals rules "a") => 'rejected)))

(def (productive-recursion-case)
  (let (rules '((start (repeat (reference item)))
                (item (choice (sequence (reference item) (literal "a"))
                              (literal "b")))))
    (check (parse-literals rules "baabaa") => 'accepted)
    (check (parse-literals rules "a") => 'rejected)))

(def (effective-override-case)
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

(def (nullable-origin-case)
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

(def (finite-witness-case)
  (let* ((rules '((start (repeat (reference a)))
                  (a (choice (reference b) (reference epsilon)))
                  (b (reference a)) (epsilon (empty))))
         (result (parameterize ((current-grammar-source-map '((rule))))
                   (admission-result rules)))
         (details (list-ref (cadr result) 3)))
    (check (map (lambda (row) (cdr (assq 'rule row)))
                (cdr (assq 'nullableReferencePath details))) => '(a epsilon))))

(def (shared-witness-case)
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

(def (choice-acceptance-case)
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

(def (choice-span-case)
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

(def (helper-continuation-case)
  (let (rules '((start (sequence (reference prefix) (literal "c")))
                (prefix (choice (literal "a") (reference longer)))
                (longer (sequence (literal "a") (literal "b")))))
    (check (parse-literals rules "abc") => 'accepted)
    (check (parse-literals rules "ac") => 'accepted)
    (check (parse-literals rules "abbc") => 'rejected)))

(def resolved-grammar-test
  (test-suite "resolved grammar admission and CFG alternatives"
    (test-case-add! (TestCase "suffix facts preserve forward scans and marked operands"
                      suffix-facts-case))
    (test-case-add! (TestCase "joint completion preserves delayed and coalesced domain publications"
                      completion-publications-case))
    (test-case-add! (TestCase "productive facts match exhaustive cyclic fixed-point controls"
                      cyclic-productivity-case))
    (test-case-add! (TestCase "productive facts match actual LR families and finite alternative exits"
                      family-productivity-case))
    (test-case-add! (TestCase "reverse productivity chain completes in canonical name order"
                      reverse-productivity-case))
    (test-case-add! (TestCase "LR targets reject a dead start before state construction for every route"
                      dead-start-case))
    (test-case-add! (TestCase "finite epsilon and LR recursive exits survive unreachable dead rules"
                      finite-exits-case))
    (test-case-add! (TestCase "productivity diagnostics retain every blocked choice and finite cyclic graph"
                      blocked-productivity-case))
    (test-case-add! (TestCase "productivity explains authored wrappers rather than generated helpers"
                      authored-productivity-case))
    (test-case-add! (TestCase "productivity sees selected POO exits and their declaration owners"
                      poo-productivity-case))
    (test-case-add! (TestCase "FIRST delta facts match exhaustive cyclic grammar controls"
                      cyclic-first-case))
    (test-case-add! (TestCase "FIRST facts match an independent oracle across actual LR grammar families"
                      family-first-case))
    (test-case-add! (TestCase "FIRST prefix blockers and duplicate edges preserve canonical rows"
                      first-prefix-case))
    (test-case-add! (TestCase "FIRST keeps a terminal prefix without claiming finite completion"
                      incomplete-prefix-case))
    (test-case-add! (TestCase "FIRST bignum deltas preserve terminal catalog order across a cycle"
                      bignum-first-case))
    (test-case-add! (TestCase "long reverse FIRST chain propagates its terminal without full rescans"
                      reverse-first-case))
    (test-case-add! (TestCase "dependency nullable facts match exhaustive cyclic grammar controls"
                      cyclic-nullability-case))
    (test-case-add! (TestCase "nullable alternatives retain duplicate occurrences and terminal blockers"
                      nullable-occurrences-case))
    (test-case-add! (TestCase "long reverse nullable chain publishes every name in production order"
                      reverse-nullability-case))
    (test-case-add! (TestCase "direct reference to an empty rule rejects before LR conflicts"
                      empty-reference-case))
    (test-case-add! (TestCase "indirect optional reference rejects both repetition forms"
                      optional-reference-case))
    (test-case-add! (TestCase "nullable sequence through field alias and precedence rejects"
                      wrapped-nullability-case))
    (test-case-add! (TestCase "nullable alternative propagates through a mutual reference cycle"
                      mutual-nullability-case))
    (test-case-add! (TestCase "nested repetitions retain the original rule owner"
                      nested-repetition-case))
    (test-case-add! (TestCase "boolean analysis never skips nested repetition obligations"
                      obligation-traversal-case))
    (test-case-add! (TestCase "all LR constructions share resolved repetition admission"
                      construction-admission-case))
    (test-case-add! (TestCase "selective GLR cannot admit a zero-width repetition cycle"
                      glr-repetition-case))
    (test-case-add! (TestCase "raw IR empty and layout operands reject before state construction"
                      raw-nullability-case))
    (test-case-add! (TestCase "a nullable prefix followed by a consuming suffix remains valid"
                      consuming-suffix-case))
    (test-case-add! (TestCase "productive left recursion remains valid inside repeat"
                      productive-recursion-case))
    (test-case-add! (TestCase "effective POO override is analyzed rather than its parent declaration"
                      effective-override-case))
    (test-case-add! (TestCase "nullable diagnostics retain selected POO override and expression occurrence"
                      nullable-origin-case))
    (test-case-add! (TestCase "finite nullable witness skips cycles and retains the effective empty exit"
                      finite-witness-case))
    (test-case-add! (TestCase "nullable explanation follows a shared DAG without expanding derivation trees"
                      shared-witness-case))
    (test-case-add! (TestCase "CFG choice reordering preserves acceptance with shared token prefixes"
                      choice-acceptance-case))
    (test-case-add! (TestCase "choice ordering preserves node fields and exact ranges"
                      choice-span-case))
    (test-case-add! (TestCase "CFG helper-reference alternatives preserve enclosing continuation"
                      helper-continuation-case))))
