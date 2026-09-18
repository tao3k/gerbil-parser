;;; -*- Gerbil -*-
;;; Deterministic recovery search beside fail-closed ParseArtifact v1.

(import (only-in :std/srfi/1 filter-map)
        (only-in ../compiler/machine
                 parser-machine-grammar-digest parser-machine-ir
                 parser-machine-runtime)
        (only-in ../compiler/parser-ir parser-ir-ref)
        (only-in ./artifact
                 parse-artifact-ref parse-artifact-success?)
        (only-in ./lexer lex-source)
        (only-in ./lr-parser
                 lr-checkpoint-frontier lr-failure-frontier?
                 lr-failure-frontier-expected-terminals
                 lr-failure-frontier-remaining-tokens
                 lr-failure-frontier-resume lr-failure-frontier-state
                 lr-initial-checkpoint lr-rejection-condition?)
        (only-in ./parser parse-source)
        (only-in ./significant parser-significant-tokens)
        (only-in ./token
                 make-token token-end token-kind token-lexeme token-start))
(export +recovery-receipt-schema+
        parse-source/recover)

(def +recovery-receipt-schema+
  "gerbil-parser.recovery-receipt.v1")

;; : (-> Terminal (OrFalse String))
(def (terminal-literal terminal)
  (and (pair? terminal)
       (eq? (car terminal) 'terminal)
       (pair? (cdr terminal))
       (eq? (cadr terminal) 'literal)
       (pair? (cddr terminal))
       (caddr terminal)))

;; : (-> LRFailureFrontier (List String))
(def (frontier-literals frontier)
  (filter-map terminal-literal
              (lr-failure-frontier-expected-terminals frontier)))

;; : (-> (List Token) Nat)
(def (tokens-end-offset tokens)
  (if (pair? tokens) (token-end (last tokens)) 0))

;; : (-> String Nat RecoveryOperation)
(def (missing-operation literal offset)
  (list (cons 'kind 'MISSING)
        (cons 'literal literal)
        (cons 'startByte offset)
        (cons 'endByte offset)
        (cons 'cost 1)))

;; : (-> Token RecoveryOperation)
(def (skipped-operation source-token)
  (list (cons 'kind 'SKIPPED)
        (cons 'tokenKind (token-kind source-token))
        (cons 'lexeme (token-lexeme source-token))
        (cons 'startByte (token-start source-token))
        (cons 'endByte (token-end source-token))
        (cons 'cost 1)))

;; : (-> Token String RecoveryOperation)
(def (error-operation source-token literal)
  (list (cons 'kind 'ERROR)
        (cons 'tokenKind (token-kind source-token))
        (cons 'lexeme (token-lexeme source-token))
        (cons 'replacement literal)
        (cons 'startByte (token-start source-token))
        (cons 'endByte (token-end source-token))
        (cons 'cost 2)))

;; : (-> ParserMachine List Nat
;;        (Values (OrFalse List) Nat Boolean (OrFalse LRFailureFrontier)))
(def (find-recovery machine tokens budget)
  (with-catch
   (lambda (condition)
     (if (lr-rejection-condition? condition)
       (values #f 0 #f #f)
       (raise condition)))
   (lambda ()
     (let-values
         (((status payload)
           (lr-checkpoint-frontier
            (lr-initial-checkpoint (parser-machine-runtime machine) tokens))))
       (if (not (and (eq? status 'failure)
                     (lr-failure-frontier? payload)))
         (values #f 0 #f #f)
         (let* ((frontier payload)
                (rest (lr-failure-frontier-remaining-tokens frontier))
                (literals (frontier-literals frontier))
                (offset
                 (if (pair? rest)
                   (token-start (car rest))
                   (tokens-end-offset tokens)))
                (attempts 0)
                (exhausted? #f))
           (def (attempt candidate operation)
             (cond
              ((>= attempts budget)
               (set! exhausted? #t)
               #f)
              (else
               (set! attempts (+ attempts 1))
               (and (lr-failure-frontier-resume frontier candidate)
                    operation))))
           (def (find-insertion rest-literals)
             (and (pair? rest-literals)
                  (let* ((literal (car rest-literals))
                         (missing
                          (make-token 'missing literal offset offset)))
                    (or (attempt (cons missing rest)
                                 (missing-operation literal offset))
                        (find-insertion (cdr rest-literals))))))
           (def (find-replacement rest-literals)
             (and (pair? rest)
                  (pair? rest-literals)
                  (let* ((source-token (car rest))
                         (literal (car rest-literals))
                         (replacement
                          (make-token 'recovery literal
                                      (token-start source-token)
                                      (token-end source-token))))
                    (or (attempt (cons replacement (cdr rest))
                                 (error-operation source-token literal))
                        (find-replacement (cdr rest-literals))))))
           (let (operation
                 (or (find-insertion literals)
                     (and (pair? rest)
                          (attempt (cdr rest)
                                   (skipped-operation (car rest))))
                     (find-replacement literals)))
             (values operation attempts exhausted? frontier))))))))

;; : (-> ParserMachine ParseArtifact (Maybe List) Symbol
;;        (Maybe RecoveryOperation) Nat Nat Boolean RecoveryReceipt)
(def (recovery-receipt machine artifact recovery-row outcome operation
                       attempts budget exhausted? frontier token-count)
  (list
   (cons 'schema +recovery-receipt-schema+)
   (cons 'grammarDigest (parser-machine-grammar-digest machine))
   (cons 'sourceDigest (parse-artifact-ref artifact 'sourceDigest))
   (cons 'outcome outcome)
   (cons 'publicationStatus (parse-artifact-ref artifact 'status))
   (cons 'owner (and recovery-row (car recovery-row)))
   (cons 'strategy (and recovery-row (caddr recovery-row)))
   (cons 'operations (if operation (list operation) '()))
   (cons 'frontierState
         (and frontier (lr-failure-frontier-state frontier)))
   (cons 'frontierExpectedTerminals
         (if frontier
           (lr-failure-frontier-expected-terminals frontier)
           '()))
   (cons 'reusedPrefixTokenCount
         (if frontier
           (- token-count
              (length (lr-failure-frontier-remaining-tokens frontier)))
           0))
   (cons 'attempts attempts)
   (cons 'budget budget)
   (cons 'budgetExhausted exhausted?)))

;;; Searches private one-edit candidates but always returns the original
;;; ParseArtifact v1. A candidate is actionable recovery evidence, never a
;;; recovered publication or a partial CST.
;; parse-source/recover
;;   : (-> ParserMachine String Nat
;;          (Values ParseArtifact RecoveryReceipt))
;;   | doc m%
;;       Searches typed private recovery candidates without widening v1.
;;
;;       # Examples
;;
;;       ```scheme
;;       (parse-source/recover machine "(1")
;;       ;; => rejected ParseArtifact v1 plus MISSING candidate receipt
;;       ```
;;     %
(def (parse-source/recover machine source (budget 256))
  (unless (and (integer? budget) (positive? budget))
    (error "recovery budget must be positive" budget))
  (let* ((artifact (parse-source machine source))
         (recoveries (parser-ir-ref (parser-machine-ir machine) 'recoveries))
         (row (and (pair? recoveries) (car recoveries))))
    (cond
     ((parse-artifact-success? artifact)
      (values artifact
              (recovery-receipt machine artifact row 'not-needed #f 0
                                budget #f #f 0)))
     ((not row)
      (values artifact
              (recovery-receipt machine artifact #f 'disabled #f 0
                                budget #f #f 0)))
     (else
      (let (tokens
            (with-catch (lambda (_) '())
              (lambda ()
                (parser-significant-tokens machine
                                           (lex-source machine source)))))
        (let-values (((operation attempts exhausted? frontier)
                      (find-recovery machine tokens budget)))
          (values artifact
                  (recovery-receipt
                   machine artifact row
                   (if operation 'candidate 'unrecovered)
                   operation attempts budget exhausted? frontier
                   (length tokens)))))))))
