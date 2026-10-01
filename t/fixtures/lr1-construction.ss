;;; -*- Gerbil -*-
;;; Shared grammar fixtures for the LR(1) construction test and benchmark.

(export lr1-not-lalr-rules shared-lookahead-rules
        genuine-reduce-conflict-rules precedence-expression-rules
        mixed-context-rules lr1-context-family-rules
        mixed-context-family-rules acyclic-mixed-context-family-rules
        inactive-core-conflict-rules)

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
