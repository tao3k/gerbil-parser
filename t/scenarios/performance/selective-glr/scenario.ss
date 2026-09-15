;;; -*- Gerbil -*-
;;; Selective-GLR completion, merge, ranking, and ambiguity receipts.

(import (only-in :gerbil-parser/src/compiler/lr-compiler compile-lr-spec)
        (only-in :gerbil-parser/src/runtime/lr-parser lr-parse/receipt)
        (only-in :gerbil-parser/src/runtime/recognition recognition-node-kind)
        (only-in :gerbil-parser/src/runtime/token make-token))
(export selective-glr-scenario
        selective-glr-scenario-pass?)

(def equivalent-rules
  '((source-file (choice (reference first-path) (reference second-path)))
    (first-path (alias SourceFile (field value (token identifier))))
    (second-path (alias SourceFile (field value (token identifier))))))

(def distinct-rules
  '((source-file (choice (reference first-path) (reference second-path)))
    (first-path (alias FirstPath (field value (token identifier))))
    (second-path (alias SecondPath (field value (token identifier))))))

(def dynamic-rules
  '((source-file (choice (reference low-path) (reference high-path)))
    (low-path
     (precedence dynamic 1
      (alias LowPath (field value (token identifier)))))
    (high-path
     (precedence dynamic 2
      (alias HighPath (field value (token identifier)))))))

(def equivalent-spec
  (compile-lr-spec equivalent-rules 'source-file 'selective-glr))
(def distinct-spec
  (compile-lr-spec distinct-rules 'source-file 'selective-glr))
(def dynamic-spec
  (compile-lr-spec dynamic-rules 'source-file 'selective-glr))
(def input-token (make-token 'identifier "x" 0 1))

(def (row-ref row key)
  (let (entry (assq key row)) (and entry (cdr entry))))

(def (parse-receipt spec)
  (let-values (((root rest receipt)
                (lr-parse/receipt spec (list input-token))))
    (list (cons 'rootKind (recognition-node-kind root))
          (cons 'remainingTokenCount (length rest))
          (cons 'receipt receipt))))

(def (ambiguity-receipt)
  (with-catch
   (lambda (condition)
     (let (irritants (error-irritants condition))
       (and (pair? irritants) (car irritants))))
   (lambda ()
     (call-with-values
      (lambda () (lr-parse/receipt distinct-spec (list input-token)))
      (lambda _ #f)))))

(def (selective-glr-scenario)
  (list
   (cons 'schema "gerbil-parser.selective-glr-correctness-receipt.v1")
   (cons 'equivalent (parse-receipt equivalent-spec))
   (cons 'dynamic (parse-receipt dynamic-spec))
   (cons 'ambiguous (ambiguity-receipt))))

(def (parse-case-pass? case expected-root expected-distinct expected-reason)
  (let (receipt (row-ref case 'receipt))
    (and (eq? (row-ref case 'rootKind) expected-root)
         (= (row-ref case 'remainingTokenCount) 0)
         (= (row-ref receipt 'branchesExplored) 2)
         (= (row-ref receipt 'successfulCompletions) 2)
         (= (row-ref receipt 'distinctCompletions) expected-distinct)
         (eq? (row-ref receipt 'winnerReason) expected-reason))))

(def (selective-glr-scenario-pass? receipt)
  (let ((equivalent (row-ref receipt 'equivalent))
        (dynamic (row-ref receipt 'dynamic))
        (ambiguous (row-ref receipt 'ambiguous)))
    (and (equal? (row-ref receipt 'schema)
                 "gerbil-parser.selective-glr-correctness-receipt.v1")
         (parse-case-pass? equivalent 'SourceFile 1 'equivalent-merge)
         (= (row-ref (row-ref equivalent 'receipt) 'mergedBranches) 1)
         (parse-case-pass? dynamic 'HighPath 2 'dynamic-precedence)
         (= (row-ref (row-ref dynamic 'receipt) 'dynamicScore) 2)
         (eq? (row-ref ambiguous 'failureKind) 'selective-glr-ambiguity)
         (= (row-ref ambiguous 'successfulCompletions) 2)
         (= (row-ref ambiguous 'distinctCompletions) 2))))
