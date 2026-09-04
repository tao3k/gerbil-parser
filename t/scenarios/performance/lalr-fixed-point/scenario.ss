;;; -*- Gerbil -*-
;;; Classic LALR grammar scenarios and algorithmic complexity receipts.

(import :gerbil-parser/src/compiler/lr)
(export lalr-fixed-point-scenario
        lalr-fixed-point-scenario-pass?)

;; The DeRemer-style shared lookahead shape S -> C C, C -> c C | d forces
;; lookaheads from distinct contexts through the same LR(0) cores.
(def shared-lookahead-rules
  '((source-file
     (alias SourceFile
      (sequence (reference component) (reference component))))
    (component
     (choice
      (sequence (literal "c") (reference component))
      (literal "d")))))

;; The classic expression grammar exercises recursive precedence-bearing
;; states without admitting an ambiguous action table.
(def expression-rules
  '((source-file
     (alias SourceFile (field expression (reference expression))))
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
      (sequence (literal "(") (reference expression) (literal ")"))
      (token identifier)))))

(def (spec-metrics name spec)
  (list
   (cons 'name name)
   (cons 'algorithm (lr-spec-ref spec 'algorithm))
   (cons 'stateCount (lr-spec-ref spec 'state-count))
   (cons 'lr0StateVisitCount
         (lr-spec-ref spec 'lr0-state-visit-count))
   (cons 'lookaheadItemVisitCount
         (lr-spec-ref spec 'lookahead-item-visit-count))))

(def (lalr-fixed-point-scenario)
  (list
   (cons 'schema "gerbil-parser.lalr-complexity-receipt.v1")
   (cons 'sharedLookahead
         (spec-metrics
          'shared-lookahead
          (compile-lr-spec shared-lookahead-rules 'source-file)))
   (cons 'expression
         (spec-metrics
          'expression
          (compile-lr-spec expression-rules 'source-file)))))

(def (metric receipt scenario key)
  (cdr (assq key (cdr (assq scenario receipt)))))

(def (scenario-pass? receipt scenario)
  (and (eq? (metric receipt scenario 'algorithm)
            'lalr1-lr0-fixed-point-v1)
       (= (metric receipt scenario 'stateCount)
          (metric receipt scenario 'lr0StateVisitCount))
       (> (metric receipt scenario 'lookaheadItemVisitCount) 0)))

(def (lalr-fixed-point-scenario-pass? receipt)
  (and (equal? (cdr (assq 'schema receipt))
               "gerbil-parser.lalr-complexity-receipt.v1")
       (scenario-pass? receipt 'sharedLookahead)
       (scenario-pass? receipt 'expression)))
