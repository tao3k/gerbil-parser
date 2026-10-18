;;; -*- Gerbil -*-
;;; Shared grammar fixtures for the LR(1) construction test and benchmark.

(import (only-in :std/list/list append-map)
        (only-in :gerbil-parser/src/compiler/lr lower-rules production-lhs production-rhs
                 base-symbol nonterminal-symbol? nonterminal-name terminal-symbol?
                 production-terminal-catalog production-id canonical-base-symbol? validate-production-semantics compute-completion compute-first)
        (only-in :gerbil-parser/src/testing/source-witness parser-source-data write-parser-source-data parser-source-symbol-text))
(import (only-in :gerbil-parser/t/fixtures/event-walk-source read-source-definition read-source-subdefinition))
(export grammar-model-rules native-fact-rules write-native-grammar-facts write-construction-raw-data
        write-lowering-traversal-data
        lr1-not-lalr-rules shared-lookahead-rules
        state-local-candidate-family-rules
        genuine-reduce-conflict-rules precedence-expression-rules
        mixed-context-rules lr1-context-family-rules
        mixed-context-family-rules acyclic-mixed-context-family-rules
        split-shared-context-family-rules
        unreachable-repeated-context-family-rules
        inactive-core-conflict-rules skew-width-rules)

;;; Declared short rules remain legal composition members. Put the reachable
;;; wide rule last to expose padding in every packed-item construction route.
(def (skew-width-rules short-count width)
  (cons '(source-file (reference wide))
        (append
         (map (lambda (index)
                (list (string->symbol (string-append "short-" (number->string index)))
                      '(literal "y")))
              (iota short-count))
         (list (list 'wide (cons 'sequence (make-list width '(literal "x"))))))))

;; The c-rule item after "c" has no raw action before its nullable optional
;; symbol. It shares an LR(0) state with completed a-rule/b-rule items that do
;; conflict on d/e. Its own initial follow block must not split for that
;; unrelated conflict; backward refinement may still split it if required.
(def inactive-core-conflict-rules
  '((source-file
     (alias SourceFile
      (choice
       (sequence (literal "a") (reference a-rule) (literal "d"))
       (sequence (literal "b") (reference a-rule) (literal "e"))
       (sequence (literal "a") (reference b-rule) (literal "e"))
       (sequence (literal "b") (reference b-rule) (literal "d"))
       (sequence (literal "a") (reference c-rule) (literal "d"))
       (sequence (literal "b") (reference c-rule) (literal "e")))))
    (a-rule (literal "c"))
    (b-rule (literal "c"))
    (c-rule (sequence (literal "c") (optional (literal "x"))))))

;; Replicate the LR(1)-but-not-LALR(1) context under distinct leading literals.
;; Each region adds one LALR merge conflict and four accepted strings.
(def (lr1-context-family-rules count)
  (let ((alternatives '())
        (rules '()))
    (let loop ((index 0))
      (when (< index count)
        (let* ((prefix (string-append "region-" (number->string index) ":"))
               (a-rule (string->symbol
                        (string-append "a-region-" (number->string index))))
               (b-rule (string->symbol
                        (string-append "b-region-" (number->string index)))))
          (set! alternatives
                (cons (list 'sequence (list 'literal prefix)
                            (list 'literal "a") (list 'reference a-rule)
                            (list 'literal "d")) alternatives))
          (set! alternatives
                (cons (list 'sequence (list 'literal prefix)
                            (list 'literal "b") (list 'reference a-rule)
                            (list 'literal "e")) alternatives))
          (set! alternatives
                (cons (list 'sequence (list 'literal prefix)
                            (list 'literal "a") (list 'reference b-rule)
                            (list 'literal "e")) alternatives))
          (set! alternatives
                (cons (list 'sequence (list 'literal prefix)
                            (list 'literal "b") (list 'reference b-rule)
                            (list 'literal "d")) alternatives))
          (set! rules (cons (list a-rule (list 'literal "c")) rules))
          (set! rules (cons (list b-rule (list 'literal "c")) rules))
          (loop (+ index 1)))))
    (cons (list 'source-file
                (list 'alias 'SourceFile
                      (cons 'choice (reverse alternatives))))
          (reverse rules))))

;; Each region has a real A/B reduce conflict on "e" after "a x". The C
;; reduction in that LR(0) state has only "d" locally, but C also appears on a
;; separate "b" path followed by "e". Production-wide follows therefore mark
;; C as a conflict candidate unless the seed consults state-local lookaheads.
(def (state-local-candidate-family-rules count)
  (def (region-name prefix index)
    (string->symbol (string-append prefix (number->string index))))
  (let ((alternatives '())
        (rules '()))
    (let loop ((index 0))
      (when (< index count)
        (let* ((prefix (string-append "region-" (number->string index) ":"))
               (a (region-name "a-" index))
               (b (region-name "b-" index))
               (c (region-name "c-" index)))
          (set! alternatives
                (append
                 alternatives
                 (list
                  (list 'sequence (list 'literal prefix) '(literal "a")
                        (list 'reference c) '(literal "d"))
                  (list 'sequence (list 'literal prefix) '(literal "a")
                        (list 'reference a) '(literal "e"))
                  (list 'sequence (list 'literal prefix) '(literal "a")
                        (list 'reference b) '(literal "e"))
                  (list 'sequence (list 'literal prefix) '(literal "b")
                        (list 'reference c) (list 'reference c)
                        '(literal "e")))))
          (set! rules
                (append rules
                        (list (list a '(literal "x"))
                              (list b '(literal "x"))
                              (list c '(literal "x")))))
          (loop (+ index 1)))))
    (cons (list 'source-file
                (list 'alias 'SourceFile (cons 'choice alternatives)))
          rules)))

;; Add a shared recursive region to independent LR(1)-only contexts. Direct
;; follow construction compresses the shared region's states and blocks, so
;; this family exercises forward refinement rather than canonical reuse.
(def (mixed-context-family-rules count)
  (let* ((base (lr1-context-family-rules count))
         (alternatives (cdr (caddr (cadr (car base)))))
         (recursive
          (let loop ((index 0) (found '()))
            (if (= index count)
              (reverse found)
              (loop (+ index 1)
                    (cons (list 'sequence
                                (list 'literal
                                      (string-append "region-"
                                                     (number->string index) ":"))
                                '(reference component)
                                '(reference component))
                          found))))))
    (cons (list 'source-file
                (list 'alias 'SourceFile
                      (cons 'choice (append alternatives recursive))))
          (append (cdr base)
                  '((component
                     (choice
                      (sequence (literal "c") (reference component))
                      (literal "d"))))))))

;; Share a finite component across all regions. This family has no recursive
;; nonterminal dependency, but its canonical LR(1) table still needs follow
;; compression; acyclicity alone cannot justify a canonical fast route.
(def (acyclic-mixed-context-family-rules count)
  (map (lambda (rule)
         (if (eq? (car rule) 'component)
           '(component (choice (literal "c") (literal "d")))
           rule))
       (mixed-context-family-rules count)))

;; Sharing a component across distinct productions does not by itself
;; imply that direct follow construction can compress canonical LR(1) states.
(def (split-shared-context-family-rules count)
  (let* ((base (lr1-context-family-rules count))
         (alternatives (cdr (caddr (cadr (car base)))))
         (shared
          (let loop ((index 0) (found '()))
            (if (= index count)
              (reverse found)
              (let (prefix (string-append "region-"
                                           (number->string index) ":"))
                (loop (+ index 1)
                      (cons (list 'sequence (list 'literal prefix)
                                  '(reference component) '(literal "x"))
                            (cons (list 'sequence (list 'literal prefix)
                                        '(reference component) '(literal "y"))
                                  found))))))))
    (cons (list 'source-file
                (list 'alias 'SourceFile
                      (cons 'choice (append alternatives shared))))
          (append (cdr base)
                  '((component
                     (choice
                      (sequence (literal "c") (reference component))
                      (literal "d"))))))))

;; An unreachable repeated helper must not divert an otherwise exact
;; canonical trial to direct follow construction.
(def (unreachable-repeated-context-family-rules count)
  (append (lr1-context-family-rules count)
          '((unreachable
             (sequence (reference helper) (reference helper)))
            (helper (literal "x")))))

;; Merging the two LR(1) contexts for A -> c and B -> c creates a spurious
;; reduce/reduce conflict. Canonical LR(1) must keep them distinct.
(def lr1-not-lalr-rules
  '((source-file
     (alias SourceFile
      (choice
       (sequence (literal "a") (reference a-rule) (literal "d"))
       (sequence (literal "b") (reference a-rule) (literal "e"))
       (sequence (literal "a") (reference b-rule) (literal "e"))
       (sequence (literal "b") (reference b-rule) (literal "d")))))
    (a-rule (literal "c"))
    (b-rule (literal "c"))))

(def shared-lookahead-rules
  '((source-file
     (alias SourceFile
      (sequence (reference component) (reference component))))
    (component
     (choice
      (sequence (literal "c") (reference component))
      (literal "d")))))

;; Both reductions are valid after the same prefix even in canonical LR(1).
(def genuine-reduce-conflict-rules
  '((source-file
     (choice (reference a-rule) (reference b-rule)))
    (a-rule (literal "c"))
    (b-rule (literal "c"))))

(def precedence-expression-rules
  '((source-file
     (alias SourceFile (reference expression)))
    (expression
     (choice
      (precedence left 10
       (sequence (reference expression)
                 (literal "+")
                 (reference expression)))
      (precedence left 20
       (sequence (reference expression)
                 (literal "*")
                 (reference expression)))
      (literal "a")))))

;; One LR(1)-only region forces the direct path, while the independent
;; recursive region has contexts that canonical LR(1) could otherwise duplicate.
(def mixed-context-rules
  '((source-file
     (alias SourceFile
      (choice
       (sequence (literal "a") (reference a-rule) (literal "d"))
       (sequence (literal "b") (reference a-rule) (literal "e"))
       (sequence (literal "a") (reference b-rule) (literal "e"))
       (sequence (literal "b") (reference b-rule) (literal "d"))
       (sequence (reference component) (reference component)))))
    (a-rule (literal "c"))
    (b-rule (literal "c"))
    (component
     (choice
      (sequence (literal "c") (reference component))
      (literal "d")))))

(def grammar-model-rules
  '((start (alias Start (sequence (reference A) (token b))))
    (A (choice (sequence (token a) (reference A)) (sequence)))))
(def native-fact-rules
  (list grammar-model-rules
        '((start (sequence (reference a) (reference a) (literal "b")))
          (a (choice (reference b) (sequence))) (b (reference a)))
        '((start (sequence (literal "a") (reference dead)))
          (dead (reference dead)) (missing-user (reference missing)))
        '((start (alias Start (sequence (optional (literal "a")) (repeat (literal "b")))))
          ($start.0 (literal "a")))))

(def (completion-data rules)
  (let* ((productions (lower-rules rules 'start))
         (names (map production-lhs productions)))
    (for-each (lambda (production)
      (for-each (lambda (value)
        (let (symbol (base-symbol value))
          (when (and (nonterminal-symbol? symbol) (not (memq (nonterminal-name symbol) names)))
            (set! names (append names (list (nonterminal-name symbol))))))) (production-rhs production))) productions)
    (def (name-id name)
      (let loop ((rest names) (index 0))
        (cond ((null? rest) (error "missing grammar name" name))
              ((eq? name (car rest)) index)
              (else (loop (cdr rest) (+ index 1))))))
    (let-values (((terminals terminal-index) (production-terminal-catalog productions))
                 ((nullable _nullable _productive _productive-index) (compute-completion productions)))
      (let-values (((first _first-index) (compute-first productions _nullable)))
        (list
          (map (lambda (production)
            (list (name-id (production-lhs production))
              (map (lambda (value)
                (let (symbol (base-symbol value))
                  (if (terminal-symbol? symbol)
                    (list 'terminal (table-ref terminal-index symbol))
                    (list 'nonterminal (name-id (nonterminal-name symbol)))))) (production-rhs production)))) productions)
          (map name-id nullable)
          (append-map (lambda (row)
            (map (lambda (terminal) (list (name-id (car row)) (table-ref terminal-index terminal))) (cdr row))) first)
          (+ 1 (length productions)))))))

;;; Collect actual lowering publications once; the shared macro owns all text.
(def (origin-data rules)
  (let* ((bindings '()) (recipes '()) (observed '())
         (productions (lower-rules rules 'start #t #f
           (lambda (kind value proof)
             (case kind
               ((binding) (set! bindings (cons (cons value proof) bindings)))
               ((production) (set! observed (cons value observed)) (set! recipes (cons proof recipes)))))))
         (names (map car rules)))
    (unless (equal? productions (lower-rules rules 'start #t))
      (error "proof replay changed canonical productions" rules))
    (unless (every eq? (cdr productions) (reverse observed))
      (error "proof replay lost native production identity" rules))
    (def (intern name) (unless (memq name names) (set! names (append names (list name)))))
    (def (reserve expression)
      (when (pair? expression)
        (when (eq? (car expression) 'reference) (intern (cadr expression)))
        (for-each reserve expression)))
    (for-each (lambda (row) (reserve (cadr row))) rules)
    (for-each (lambda (production) (intern (production-lhs production))) productions)
    ;; An allocated empty-choice helper has no production lhs. The binding
    ;; observation, rather than the row set, owns its directory membership.
    (for-each (lambda (binding) (intern (car binding))) (reverse bindings))
    (def (name-id name)
      (let loop ((rest names) (index 0))
        (cond ((null? rest) (error "missing grammar name" name))
              ((eq? name (car rest)) index)
              (else (loop (cdr rest) (+ index 1))))))
    (let-values (((terminals terminal-index) (production-terminal-catalog productions)))
      (def (terminal-id kind value) (table-ref terminal-index (list 'terminal kind value)))
      (def (expression value)
        (case (car value)
          ((field alias) (expression (caddr value)))
          ((precedence) (expression (cadddr value)))
          ((empty layout-end) '(empty))
          ((literal layout-start layout-next token) (list 'terminal (terminal-id (car value) (cadr value))))
          ((reference) (list 'reference (name-id (cadr value))))
          ((sequence choice) (cons (car value) (map expression (cdr value))))
          ((optional repeat repeat1) (list (car value) (expression (cadr value))))
          (else (error "unsupported proof expression" value))))
      (def (owner value)
        (case (car value)
          ((named) (list 'named (name-id (cadr value))))
          ((expression) (list 'expression (expression (cadr value))))
          (else (error "unsupported proof owner" value))))
      ;; Preserve dispatch barriers for the recursive producer model. Labels,
      ;; precedence ranks and AST actions are outside recognition projection.
      (def (syntax value)
        (case (car value)
          ((field alias) (list (car value) 0 (syntax (caddr value))))
          ((precedence) (list 'precedence 0 0 (syntax (cadddr value))))
          ((layout-end) '(layout-end))
          ((sequence choice) (cons (car value) (map syntax (cdr value))))
          ((optional repeat repeat1) (list (car value) (syntax (cadr value))))
          (else (expression value))))
      (def (symbol value)
        (if (eq? (car value) 'terminal)
          (list 'terminal (terminal-id (cadr value) (caddr value)))
          (list 'nonterminal (owner value))))
      (def (recipe value)
        (case (car value)
          ((identity) (list 'identity (map symbol (cadr value))))
          ((expand) (list 'expand (owner (cadr value)) (recipe (caddr value)) (recipe (cadddr value))))
          ((append) (list 'append (recipe (cadr value)) (recipe (caddr value))))
          ((fold-named) (list 'fold-named (name-id (cadr value)) (expression (caddr value))))
          (else (error "unsupported proof recipe" value))))
      (def (grammar-row production)
        (list (name-id (production-lhs production))
          (map (lambda (value)
            (let (base (base-symbol value))
              (if (terminal-symbol? base)
                (list 'terminal (table-ref terminal-index base))
                (list 'nonterminal (name-id (nonterminal-name base)))))) (production-rhs production))))
      (let (decodable? (every (lambda (production)
              (every (lambda (operand) (canonical-base-symbol? (base-symbol operand))) (production-rhs production))) productions))
        (list
          (list (map (lambda (row) (list (name-id (car row)) (expression (cadr row)))) rules)
                (map (lambda (binding) (list (name-id (car binding)) (expression (cdr binding)))) bindings)
                (map grammar-row (cdr productions)) (map recipe (reverse recipes))
                (map (lambda (production)
                  (list (production-id production) (name-id (production-lhs production))
                    (map (lambda (operand)
                      (let (base (base-symbol operand))
                        (cond ((not (canonical-base-symbol? base)) '(eof))
                              ((nonterminal-symbol? base) (list 'nonterminal (name-id (nonterminal-name base))))
                              (else (list (cadr base) (table-ref terminal-index base)))))) (production-rhs production)))) productions)
                (name-id (production-lhs (car productions)))
                (with-catch (lambda (_) #f) (lambda () (validate-production-semantics productions
                  (cons (production-lhs (car productions)) (append (map car rules) (map car bindings)))) #t)) decodable?)
          (list (map parser-source-symbol-text names)
                (map (lambda (production) (list (production-id production) (production-lhs production) (production-rhs production))) productions)
                (production-lhs (car productions)) 'start decodable?)
          (map (lambda (row) (list (name-id (car row)) (syntax (cadr row)))) rules))))))

(def (origin-rule-corpus)
  (append native-fact-rules
       `(((start (repeat (literal "a"))))
         ((start (repeat1 (literal "a"))))
         ((start (empty)))
         ((start (choice)))
         ((start (sequence)))
         ((start (choice (repeat (literal "a")) (literal "b"))))
         ((start (repeat (literal "a"))) (start (literal "b")))
         ((start (precedence left 2 (repeat1 (field value (literal "a"))))))
         ((start (sequence (layout-start indent) (layout-next indent) (layout-end indent))))
         ((start (choice (choice (literal "a") (literal "b")) (literal "c"))))
         ((start (repeat (precedence dynamic 1 (alias Item (token word))))))
         ((start (reference undefined)))
         ((start (field value (alias Word (token word)))))
         ((start (sequence (reference 内容) (token 名称) (literal "é") (literal "é")
                   (literal ,(string #\" #\\ #\newline #\tab (integer->char 0) (integer->char 127) (integer->char #x1f600)))))
          (内容 (literal "漢字"))))))

(def (write-native-grammar-facts output)
  (write-parser-source-data
    (parser-source-data
      (ParserEngine.NativeGrammarData completion (map completion-data native-fact-rules))
      (ParserEngine.NativeGrammarData origins (map (lambda (rules) (car (origin-data rules))) (origin-rule-corpus)))) output))

(def (write-construction-raw-data output)
  (write-parser-source-data
    (parser-source-data
      (ParserEngine.RawGrammarData corpus (map (lambda (rules) (cadr (origin-data rules))) (origin-rule-corpus)))) output))

(def (traversal-rule-corpus)
  (append (origin-rule-corpus)
    '(((start (sequence (optional (repeat1 (token word)))
                (field item (alias Item (choice (sequence) (reference start))))
                (repeat (optional (literal "x"))))))
      ((start (choice (repeat (sequence (literal "a") (optional (token word))))
                (repeat1 (precedence left 2 (alias Item (token word))))
                (layout-end indent))))
      ((start (sequence (precedence left 1 (literal "a"))
                (field item (precedence right 2 (literal "b")))
                (alias Item (precedence dynamic 3 (token word))))))
      ((start (repeat (literal "a"))) (start (repeat (literal "a"))))
      ((start (choice (repeat (literal "a")) (repeat1 (token word))))
       ($start.0 (empty)) ($accept (empty)))
      ((start (choice (sequence (choice)) (literal "x")))))))

(def (traversal-data rules)
  (let (data (origin-data rules)) (list (caddr data) (car data))))

(def (write-lowering-traversal-data output)
  (write-parser-source-data
    (parser-source-data
      (ParserEngine.LoweringTraversalWitness corpus
        (map traversal-data (traversal-rule-corpus)))
      (ParserEngine.LoweringNamesSource freshSource
        (read-source-subdefinition "src/compiler/lr.ss" 'lower-rules 'fresh))
      (ParserEngine.LoweringNamesSource claimSource
        (read-source-subdefinition "src/compiler/lr.ss" 'lower-rules 'claim-name))
      (ParserEngine.LoweringNamesSource symbolSource
        (read-source-subdefinition "src/compiler/lr.ss" 'lower-rules 'lower-symbol))
      (ParserEngine.LoweringNamesSource operandSource
        (read-source-subdefinition "src/compiler/lr.ss" 'lower-rules 'lower-operand))
      (ParserEngine.LoweringBranchSource lowerToSource
        (read-source-subdefinition "src/compiler/lr.ss" 'lower-rules 'lower-to))
      (ParserEngine.LoweringBranchSource recipeSources
        (map (lambda (name) (read-source-subdefinition "src/compiler/lr.ss" 'lower-rules name))
          '(identity body step operand-proof sequence-proof choice-proof)))
      (ParserEngine.LoweringBranchSource pathSources
        (map (lambda (name) (read-source-subdefinition "src/compiler/lr.ss" 'lower-rules name))
          '(child-path map-children)))
      (ParserEngine.LoweringBranchSource rootSource
        (read-source-definition "src/compiler/lr.ss" 'lower-rules))
      (ParserEngine.RuleIndexSource definitionSource
        (read-source-definition "src/compiler/lr.ss" 'rule-index-by-lhs))
      (ParserEngine.SchemeNameEncoding source
        (read-source-subdefinition "t/fixtures/lr1-construction.ss" 'origin-data 'name-id))) output))
