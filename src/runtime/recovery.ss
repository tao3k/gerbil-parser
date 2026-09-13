;;; -*- Gerbil -*-
;;; Deterministic recovery search beside fail-closed ParseArtifact v1.

(import (only-in ../compiler/machine
                 parser-machine-grammar-digest parser-machine-ir
                 parser-machine-parse)
        (only-in ../compiler/parser-ir parser-ir-ref)
        (only-in ./artifact
                 parse-artifact-ref parse-artifact-success?)
        (only-in ./lexer lex-source)
        (only-in ./parser parse-source)
        (only-in ./significant parser-significant-tokens)
        (only-in ./token
                 make-token token-end token-kind token-lexeme token-start))
(export +recovery-receipt-schema+
        parse-source/recover)

(def +recovery-receipt-schema+
  "gerbil-parser.recovery-receipt.v1")

;; : (forall (a) (-> [a] [a]))
;; : (-> List List)
(def (unique values)
  (reverse
   (foldl (lambda (value seen)
            (if (member value seen) seen (cons value seen)))
          '() values)))

;; : (-> GrammarExpr (List String))
(def (grammar-literals expression)
  (case (car expression)
    ((literal) (list (cadr expression)))
    ((sequence choice)
     (apply append (map grammar-literals (cdr expression))))
    ((optional repeat repeat1) (grammar-literals (cadr expression)))
    ((field alias) (grammar-literals (caddr expression)))
    ((precedence) (grammar-literals (cadddr expression)))
    (else '())))

;; : (-> ParserMachine (List String))
(def (parser-literals machine)
  (unique
   (apply append
          (map (lambda (row) (grammar-literals (cadr row)))
               (parser-ir-ref (parser-machine-ir machine) 'rules)))))

;; : (forall (a) (-> [a] Nat [a]))
;; : (-> List Nat List)
(def (remove-at values index)
  (append (take values index) (drop values (+ index 1))))

;; : (forall (a) (-> [a] Nat a [a]))
;; : (-> List Nat Datum List)
(def (insert-at values index value)
  (append (take values index) (list value) (drop values index)))

;; : (-> ParserMachine (List Token) Boolean)
(def (candidate-accepted? machine tokens)
  (with-catch
   (lambda (_) #f)
   (lambda ()
     (let-values (((_root rest) ((parser-machine-parse machine) tokens)))
       (null? rest)))))

;; : (-> (List Token) Nat Nat)
(def (position-offset tokens index)
  (cond
   ((< index (length tokens)) (token-start (list-ref tokens index)))
   ((pair? tokens) (token-end (last tokens)))
   (else 0)))

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

;; : (-> ParserMachine List Nat (Values (OrFalse List) Nat Boolean))
(def (find-recovery machine tokens budget)
  (let ((attempts 0) (exhausted? #f))
    (def (attempt candidate operation)
      (cond
       ((>= attempts budget)
        (set! exhausted? #t)
        #f)
       (else
        (set! attempts (+ attempts 1))
        (and (candidate-accepted? machine candidate) operation))))
    (def (find-deletion index)
      (and (< index (length tokens))
           (or (attempt (remove-at tokens index)
                        (skipped-operation (list-ref tokens index)))
               (find-deletion (+ index 1)))))
    (def (find-insertion index literals)
      (cond
       ((> index (length tokens)) #f)
       ((null? literals)
        (find-insertion (+ index 1) (parser-literals machine)))
       (else
        (let* ((literal (car literals))
               (offset (position-offset tokens index))
               (missing (make-token 'missing literal offset offset)))
          (or (attempt (insert-at tokens index missing)
                       (missing-operation literal offset))
              (find-insertion index (cdr literals)))))))
    (def (find-replacement index literals)
      (cond
       ((>= index (length tokens)) #f)
       ((null? literals)
        (find-replacement (+ index 1) (parser-literals machine)))
       (else
        (let* ((source-token (list-ref tokens index))
               (literal (car literals))
               (replacement
                (make-token 'recovery literal
                            (token-start source-token)
                            (token-end source-token))))
          (or (attempt
               (insert-at (remove-at tokens index) index replacement)
               (error-operation source-token literal))
              (find-replacement index (cdr literals)))))))
    (let (operation
          (or (find-insertion 0 (parser-literals machine))
              (find-deletion 0)
              (find-replacement 0 (parser-literals machine))))
      (values operation attempts exhausted?))))

;; : (-> ParserMachine ParseArtifact (Maybe List) Symbol
;;        (Maybe RecoveryOperation) Nat Nat Boolean RecoveryReceipt)
(def (recovery-receipt machine artifact recovery-row outcome operation
                       attempts budget exhausted?)
  (list
   (cons 'schema +recovery-receipt-schema+)
   (cons 'grammarDigest (parser-machine-grammar-digest machine))
   (cons 'sourceDigest (parse-artifact-ref artifact 'sourceDigest))
   (cons 'outcome outcome)
   (cons 'publicationStatus (parse-artifact-ref artifact 'status))
   (cons 'owner (and recovery-row (car recovery-row)))
   (cons 'strategy (and recovery-row (caddr recovery-row)))
   (cons 'operations (if operation (list operation) '()))
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
                                budget #f)))
     ((not row)
      (values artifact
              (recovery-receipt machine artifact #f 'disabled #f 0
                                budget #f)))
     (else
      (let (tokens
            (with-catch (lambda (_) '())
              (lambda ()
                (parser-significant-tokens machine
                                           (lex-source machine source)))))
        (let-values (((operation attempts exhausted?)
                      (find-recovery machine tokens budget)))
          (values artifact
                  (recovery-receipt
                   machine artifact row
                   (if operation 'candidate 'unrecovered)
                   operation attempts budget exhausted?))))))))
