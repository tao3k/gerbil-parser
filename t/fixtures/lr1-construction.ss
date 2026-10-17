;;; -*- Gerbil -*-
;;; Shared grammar fixtures for the LR(1) construction test and benchmark.

(import (only-in :std/list/list append-map)
        (only-in :gerbil-parser/src/compiler/lr lower-rules production-lhs production-rhs
                 base-symbol nonterminal-symbol? nonterminal-name terminal-symbol?
                 production-terminal-catalog production-id canonical-base-symbol? validate-production-semantics compute-completion compute-first)
        (only-in :gerbil-parser/src/testing/source-witness parser-source-data write-parser-source-data parser-source-symbol-text))
(export grammar-model-rules native-fact-rules write-native-grammar-facts write-lowering-origin-facts write-construction-raw-data
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

;;; Serialize typed Lean inputs, not a report. Kernel-checked examples use the
;;; actual native publication; the owner compares this complete generated module
;;; so a stale or manually changed Lean fixture cannot qualify the native code.
(def (write-native-grammar-facts output)
  (parameterize ((current-output-port output))
    (def (lean-list values emit)
      (display "[")
      (let loop ((rest values))
        (unless (null? rest)
          (emit (car rest))
          (unless (null? (cdr rest)) (display ", "))
          (loop (cdr rest))))
      (display "]"))
    (displayln "import ParserEngine.Completion")
    (displayln "import ParserEngine.Names")
    (displayln "import ParserEngine.ConstructionGrammar")
    (displayln "-- Generated by write-native-grammar-facts; checked against native outputs.")
    (displayln "namespace ParserEngine.NativeFacts")
    (displayln "open Grammar Completion")
    (for-each
     (lambda (rules ordinal)
       (let* ((productions (lower-rules rules 'start))
              (names (map production-lhs productions))
              (label (string-append "fixture" (number->string ordinal))))
         (for-each (lambda (production)
           (for-each (lambda (value)
             (let (symbol (base-symbol value))
               (when (and (nonterminal-symbol? symbol) (not (memq (nonterminal-name symbol) names)))
                 (set! names (append names (list (nonterminal-name symbol))))))) (production-rhs production))) productions)
         (def (name-id name)
           (let find ((rest names) (at 0)) (if (eq? name (car rest)) at (find (cdr rest) (+ at 1)))))
         (let-values (((terminals terminal-index) (production-terminal-catalog productions))
                      ((nullable _nullable _productive _productive-index) (compute-completion productions)))
           (let-values (((first _first-index) (compute-first productions _nullable)))
             (display "def ") (display label) (display " : CFG := ")
             (lean-list productions (lambda (production)
               (display "⟨") (display (name-id (production-lhs production))) (display ", ")
               (lean-list (production-rhs production) (lambda (value)
                 (let (symbol (base-symbol value))
                   (if (terminal-symbol? symbol)
                     (begin (display ".terminal ") (display (table-ref terminal-index symbol)))
                     (begin (display ".nonterminal ") (display (name-id (nonterminal-name symbol))))))))
               (display "⟩"))) (newline)
             (display "def ") (display label) (display "Nullable : List Nat := ")
             (lean-list nullable (lambda (name) (display (name-id name)))) (newline)
             (display "def ") (display label) (display "First : List (Nat × Nat) := ")
             (lean-list (append-map (lambda (row) (map (lambda (terminal) (cons (car row) terminal)) (cdr row))) first)
               (lambda (entry) (display "(") (display (name-id (car entry))) (display ", ")
                 (display (table-ref terminal-index (cdr entry))) (display ")"))) (newline)
             (let (fuel (+ 1 (length productions)))
               (display "theorem ") (display label) (display "_nullable_checked : checkNullable ")
               (display label) (display " ") (display label) (display "Nullable ") (display fuel)
               (displayln " = true := by decide")
               (display "theorem ") (display label) (display "_first_checked : checkFirst ")
               (display label) (display " ") (display label) (display "Nullable ")
               (display label) (display "First ") (display fuel) (displayln " = true := by decide")
               (display "theorem ") (display label) (display "_nullable_exact : ∀ name, name ∈ ")
               (display label) (display "Nullable ↔ Nullable ") (display label)
               (display " [.nonterminal name] := nullable_certificate_exact _ _ ")
               (display fuel) (display " ") (display label) (displayln "_nullable_checked")
               (display "theorem ") (display label) (display "_first_exact : ∀ name token, (name, token) ∈ ")
               (display label) (display "First ↔ First ") (display label)
               (display " [.nonterminal name] token := first_certificate_exact _ _ _ ")
               (display fuel) (display " ") (display fuel) (display " ") (display label)
               (display "_nullable_checked ") (display label) (displayln "_first_checked"))))))
     native-fact-rules (iota (length native-fact-rules)))
    (write-lowering-origin-facts output)
    (displayln "end ParserEngine.NativeFacts")))

;;; Serialize the recipes emitted by the real lower-rules branches. The reader
;;; is Lean's general schema checker; this writer supplies no semantic oracle.
(def (write-lowering-origin-facts output (raw-sink #f))
  (parameterize ((current-output-port output))
    (def (items values emit)
      (display "[")
      (let loop ((rest values))
        (unless (null? rest)
          (emit (car rest))
          (unless (null? (cdr rest)) (display ", "))
          (loop (cdr rest))))
      (display "]"))
    (for-each
     (lambda (rules ordinal)
       (let* ((bindings '()) (recipes '()) (observed '())
              (productions
               (lower-rules rules 'start #t #f
                 (lambda (kind value proof)
                   (case kind
                     ((binding) (set! bindings (cons (cons value proof) bindings)))
                     ((production)
                      (set! observed (cons value observed))
                      (set! recipes (cons proof recipes)))))))
              (names (map car rules))
              (label (string-append "origin" (number->string ordinal))))
         (unless (equal? productions (lower-rules rules 'start #t))
           (error "proof replay changed canonical productions" rules))
         (unless (every eq? (cdr productions) (reverse observed))
           (error "proof replay lost native production identity" rules))
         (def (intern name)
           (unless (memq name names) (set! names (append names (list name)))))
         (def (reserve expression)
           (when (pair? expression)
             (when (eq? (car expression) 'reference) (intern (cadr expression)))
             (for-each reserve expression)))
         (for-each (lambda (row) (reserve (cadr row))) rules)
         (for-each (lambda (production) (intern (production-lhs production))) productions)
         (def (name-id name)
           (let loop ((rest names) (index 0))
             (if (eq? name (car rest)) index (loop (cdr rest) (+ index 1)))))
         (let-values (((terminals terminal-index) (production-terminal-catalog productions)))
           (def (terminal-id kind value)
             (table-ref terminal-index (list 'terminal kind value)))
           ;; Recognition projection: annotations carry separate AST/layout and
           ;; precedence obligations. Their terminal-language image is the child.
           (def (expression value)
             (case (car value)
               ((field alias) (expression (caddr value)))
               ((precedence) (expression (cadddr value)))
               ((empty layout-end) (display ".empty"))
               ((literal layout-start layout-next token)
                (display "(.terminal ") (display (terminal-id (car value) (cadr value))) (display ")"))
               ((reference) (display "(.reference ") (display (name-id (cadr value))) (display ")"))
               ((sequence choice)
                (display (if (eq? (car value) 'sequence) "(Lowering.sequenceList " "(Lowering.choiceList "))
                (items (cdr value) expression) (display ")"))
               ((optional repeat repeat1)
                (display "(.") (display (car value)) (display " ") (expression (cadr value)) (display ")"))
               (else (error "unsupported proof expression" value))))
           (def (owner value)
             (case (car value)
               ((named) (display "(.named ") (display (name-id (cadr value))) (display ")"))
               ((expression) (display "(.expression ") (expression (cadr value)) (display ")"))
               (else (error "unsupported proof owner" value))))
           (def (symbol value)
             (if (eq? (car value) 'terminal)
               (begin (display "(.terminal ") (display (terminal-id (cadr value) (caddr value))) (display ")"))
               (begin (display "(.nonterminal ") (owner value) (display ")"))))
           (def (recipe value)
             (display "(.")
             (case (car value)
               ((identity) (display "identity ") (items (cadr value) symbol))
               ((expand) (display "expand ") (owner (cadr value)) (display " ")
                (recipe (caddr value)) (display " ") (recipe (cadddr value)))
               ((append) (display "append ") (recipe (cadr value)) (display " ") (recipe (caddr value)))
               ((fold-named) (display "foldNamed ") (display (name-id (cadr value)))
                (display " ") (expression (caddr value)))
               (else (error "unsupported proof recipe" value)))
             (display ")"))
           (if raw-sink (raw-sink names productions)
            (begin
           (display "def ") (display label) (display "Source : Lowering.Rules := ")
           (items rules (lambda (row) (display "(") (display (name-id (car row)))
             (display ", ") (expression (cadr row)) (display ")"))) (newline)
           (display "def ") (display label) (display "Decode : Nat → Lowering.Owner\n")
           (for-each (lambda (binding)
             (display "  | ") (display (name-id (car binding))) (display " => .expression ")
             (expression (cdr binding)) (newline)) bindings)
           (displayln "  | name => .named name")
           (display "def ") (display label) (display "Native : CFG := ")
           (items (cdr productions) (lambda (production)
             (display "⟨") (display (name-id (production-lhs production))) (display ", ")
             (items (production-rhs production) (lambda (value)
               (let (value (base-symbol value))
                 (if (terminal-symbol? value)
                   (begin (display ".terminal ") (display (table-ref terminal-index value)))
                   (begin (display ".nonterminal ") (display (name-id (nonterminal-name value))))))))
             (display "⟩"))) (newline)
           (display "def ") (display label) (display "Recipes : List Names.Recipe := ")
           (items (reverse recipes) recipe) (newline)
           (display "theorem ") (display label) (display "_checked : Names.admitNative ")
           (display label) (display "Source ") (display label) (display "Decode ")
           (display label) (display "Native ") (display label) (displayln "Recipes = true := by decide")
           (display "theorem ") (display label) (display "_sound : ∀ production ∈ ")
           (display label) (display "Native, Names.OriginSupported ") (display label)
           (display "Source ") (display label) (display "Decode production := Names.admitted_native_sound ")
           (display label) (displayln "_checked")
           (display "theorem ") (display label) (display "_language_sound (word : List Nat) (derivation : Grammar.Derives ")
           (display label) (display "Native [.nonterminal 0] word) : Lowering.Eval ")
           (display label) (display "Source (.reference 0) word := Names.admitted_native_root_sound ")
           (display label) (displayln "_checked 0 0 word rfl derivation")
           ;; Bind the existing source-origin witness to all native records,
           ;; including the actual augmented row and physical IDs. Terminal
           ;; payload ordinals are the existing native catalogue identities.
           (display "def ") (display label) (display "Input : ConstructionGrammar.Input := ⟨")
           (items productions (lambda (production)
             (display "⟨") (display (production-id production)) (display ", ⟨")
             (display (name-id (production-lhs production))) (display ", ")
             (items (production-rhs production) (lambda (operand)
               (let (base (base-symbol operand))
                 (display "⟨")
                 (cond
                   ((not (canonical-base-symbol? base))
                    ;; EOF is forbidden on a native RHS. Preserve a failed
                    ;; raw base decode as rejection, never mint a text ID.
                    (display ".terminal .eof"))
                   ((nonterminal-symbol? base)
                    (display ".nonterminal ") (display (name-id (nonterminal-name base))))
                   (else (begin (display ".terminal (.")
                     (display (case (cadr base)
                       ((token) "token") ((literal) "literal")
                       ((layout-start) "layoutStart") ((layout-next) "layoutNext")
                       (else (error "unsupported construction terminal" base))))
                     (display " ") (display (table-ref terminal-index base)) (display ")"))))
                 (display ", []⟩"))))
             (display ", .ordinary 0⟩⟩")))
           (display ", ") (display (name-id (production-lhs (car productions))))
           (display ", 0, 0, ") (display label) (display "Source, ")
           (display label) (display "Decode, ") (display label) (displayln "Recipes⟩")
           (display "theorem ") (display label) (display "_input_checked : ConstructionGrammar.checks ")
           (display label) (display "Input = ")
           (display (if (with-catch (lambda (_) #f)
                           (lambda () (validate-production-semantics productions) #t)) "true" "false"))
           (displayln " := by decide")
           (when (every (lambda (production)
                   (every (lambda (operand) (canonical-base-symbol? (base-symbol operand))) (production-rhs production))) productions)
             (display "theorem ") (display label) (display "_input_projection : ConstructionGrammar.grammar ")
             (display label) (display "Input = ⟨") (display (name-id (production-lhs (car productions))))
             (display ", [.nonterminal 0]⟩ :: ") (display label) (displayln "Native := by decide")))))))
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
          (内容 (literal "漢字")))))
     (iota (+ (length native-fact-rules) 14)))))

;;; Raw evidence uses the shared declarative source renderer. No generated
;;; Lean declarations or proof text are assembled in this data producer.
(def (write-construction-raw-data output)
  (let (frames '())
    (write-lowering-origin-facts output
      (lambda (names productions)
        (set! frames
          (cons (list (map parser-source-symbol-text names)
                      (map (lambda (production)
                             (list (production-id production) (production-lhs production) (production-rhs production))) productions)
                      (production-lhs (car productions)) 'start
                      (every (lambda (production)
                        (every (lambda (operand) (canonical-base-symbol? (base-symbol operand)))
                          (production-rhs production))) productions)) frames))))
    (write-parser-source-data
      (parser-source-data (ParserEngine.RawGrammarData corpus (reverse frames))) output)))
