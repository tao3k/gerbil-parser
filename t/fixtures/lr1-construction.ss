;;; -*- Gerbil -*-
;;; Shared grammar fixtures for the LR(1) construction test and benchmark.

(import (only-in :std/list/list append-map)
        (only-in :gerbil-parser/src/compiler/lr lower-rules production-lhs production-rhs
                 base-symbol nonterminal-symbol? nonterminal-name terminal-symbol?
                 production-terminal-catalog compute-completion compute-first))
(export grammar-model-rules native-fact-rules write-native-grammar-facts
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
    (displayln "end ParserEngine.NativeFacts")))
