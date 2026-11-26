;;; -*- Gerbil -*-
;;; Deterministic recovery search beside fail-closed ParseArtifact v1.

(import (only-in ./machine
                 parser-machine-grammar-digest parser-machine-ir
                 parser-machine-runtime)
        (only-in ./parser-ir-data parser-ir-ref)
        (only-in ./artifact
                 parse-artifact-ref parse-artifact-success?
                 parse-artifact-events token-event? token-event-token-kind
                 token-event-lexeme event-start event-end)
        (only-in ./lexer lex-source)
        (only-in ./lr-parser
                 lr-checkpoint-frontier lr-failure-frontier?
                 lr-failure-frontier-expected-terminals
                 lr-failure-frontier-remaining-tokens
                 lr-failure-frontier-resume lr-failure-frontier-state
                 lr-initial-checkpoint lr-rejection-condition?
                 lr-runtime-layout?)
        (only-in ./parser parse-source)
        (only-in ./significant parser-significant-tokens)
        (only-in ./token
                 make-token token-end token-kind token-lexeme token-start))
(export +recovery-receipt-schema+
        +cpct-plus-recovery-receipt-schema+
        parse-source/recover
        parse-source/recover-cpct+)

(def +recovery-receipt-schema+
  "gerbil-parser.recovery-receipt.v1")

(def +cpct-plus-recovery-receipt-schema+
  "gerbil-parser.cpct-plus-recovery-receipt.v1")

;;; The ordinary rejected artifact already owns the complete fallback lexical
;;; stream. Reuse it for private recovery instead of scanning the source again.
;;; A lexical failure may publish no tokens; retain the existing fallback then.
(def (rejected-artifact-tokens artifact)
  (let ((events (parse-artifact-events artifact))
        (length (parse-artifact-ref artifact 'sourceByteLength)))
    (let loop ((rest events) (offset 0) (tokens '()))
      (cond
       ((null? rest) (and (= offset length) (reverse tokens)))
       ((and (token-event? (car rest))
             (= (event-start (car rest)) offset))
        (let (event (car rest))
          (loop (cdr rest) (event-end event)
                (cons (make-token (token-event-token-kind event)
                                  (token-event-lexeme event)
                                  offset (event-end event))
                      tokens))))
       (else #f)))))

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
     ((lr-runtime-layout? (parser-machine-runtime machine))
      (values artifact
              (recovery-receipt machine artifact row 'disabled #f 0
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
                 (or (rejected-artifact-tokens artifact)
                     (lex-source machine source))))))
        (let-values (((operation attempts exhausted? frontier)
                      (find-recovery machine tokens budget)))
          (values artifact
                  (recovery-receipt
                   machine artifact row
                   (if operation 'candidate 'unrecovered)
                   operation attempts budget exhausted? frontier
                   (length tokens)))))))))

;;; ============================================================================
;;; Bounded CPCT+ Minimum-Cost LR Recovery (Diekmann & Tratt, ECOOP 2020)
;;; ============================================================================
;;; Explores multi-edit repair sequences up to `max-cost` (default 3) directly
;;; over `LRFailureFrontier` via cost-ordered BFS and enforces STRICT UNIQUENESS:
;;;   - `'unique-candidate`: exactly 1 minimum-cost repair passes the LR machine
;;;   - `'ambiguous-tie`:    >= 2 distinct minimum-cost repairs tie -> fail-closed
;;;   - `'unrecovered`:      0 repairs within `(max-cost, budget)` -> fail-closed

(def (take-prefix lst count)
  (let loop ((rest lst) (n count) (acc '()))
    (if (or (<= n 0) (null? rest))
      (reverse acc)
      (loop (cdr rest) (- n 1) (cons (car rest) acc)))))

(def (operation-signature op)
  (list (cdr (assq 'kind op))
        (let ((e (assq 'literal op))) (and e (cdr e)))
        (let ((e (assq 'replacement op))) (and e (cdr e)))
        (cdr (assq 'startByte op))
        (cdr (assq 'endByte op))))

(def (ops-signature ops)
  (map operation-signature ops))

(def (find-cpct-plus-recoveries machine tokens max-cost budget)
  (with-catch
   (lambda (condition)
     (if (lr-rejection-condition? condition)
       (values 'unrecovered '() #f 0 0 #f #f)
       (raise condition)))
   (lambda ()
     (let* ((runtime (parser-machine-runtime machine))
            (initial-frontier #f)
            (attempts 0)
            (exhausted? #f)
            (best-cost (+ max-cost 1))
            (best-solutions '())
            (seen-solutions (make-hash-table))
            (queue (list (list 0 '() tokens))))
       (let loop ()
         (when (pair? queue)
           (let* ((item (car queue))
                  (cost (car item))
                  (ops-rev (cadr item))
                  (cur-tokens (caddr item)))
             (set! queue (cdr queue))
             (cond
               ((> cost best-cost) (loop))
               ((>= attempts budget)
                (set! exhausted? #t))
               (else
                (set! attempts (+ attempts 1))
                (let-values (((status payload)
                              (lr-checkpoint-frontier
                               (lr-initial-checkpoint runtime cur-tokens))))
                  (cond
                    ((eq? status 'accepted)
                     (let* ((ops (reverse ops-rev))
                            (sig (ops-signature ops)))
                       (cond
                         ((< cost best-cost)
                          (set! best-cost cost)
                          (set! seen-solutions (make-hash-table))
                          (hash-put! seen-solutions sig #t)
                          (set! best-solutions (list ops)))
                         ((and (= cost best-cost) (not (hash-get seen-solutions sig)))
                          (hash-put! seen-solutions sig #t)
                          (set! best-solutions (cons ops best-solutions)))))
                     (loop))
                    ((and (eq? status 'failure)
                          (lr-failure-frontier? payload)
                          (< cost max-cost)
                          (< cost best-cost))
                     (unless initial-frontier
                       (set! initial-frontier payload))
                     (let* ((frontier payload)
                            (rest (lr-failure-frontier-remaining-tokens frontier))
                            (prefix-len (- (length cur-tokens) (length rest)))
                            (prefix (take-prefix cur-tokens prefix-len))
                            (literals (frontier-literals frontier))
                            (offset (if (pair? rest)
                                      (token-start (car rest))
                                      (tokens-end-offset cur-tokens)))
                            (next-items '()))
                       ;; Cost +1: MISSING(literal)
                       (when (<= (+ cost 1) best-cost)
                         (for-each
                           (lambda (lit)
                             (let ((tok (make-token 'missing lit offset offset)))
                               (set! next-items
                                 (cons (list (+ cost 1)
                                             (cons (missing-operation lit offset) ops-rev)
                                             (append prefix (cons tok rest)))
                                       next-items))))
                           literals)
                         ;; Cost +1: SKIPPED(car rest)
                         (when (pair? rest)
                           (set! next-items
                             (cons (list (+ cost 1)
                                         (cons (skipped-operation (car rest)) ops-rev)
                                         (append prefix (cdr rest)))
                                   next-items))))
                       ;; Cost +2: ERROR(car rest -> literal)
                       (when (and (pair? rest) (<= (+ cost 2) max-cost) (<= (+ cost 2) best-cost))
                         (let ((src-tok (car rest)))
                           (for-each
                             (lambda (lit)
                               (let ((rep (make-token 'recovery lit
                                                      (token-start src-tok)
                                                      (token-end src-tok))))
                                 (set! next-items
                                   (cons (list (+ cost 2)
                                               (cons (error-operation src-tok lit) ops-rev)
                                               (append prefix (cons rep (cdr rest))))
                                         next-items))))
                             literals)))
                       (set! queue (append queue (reverse next-items))))
                     (loop))
                    (else (loop)))))))))
       (let ((count (length best-solutions)))
         (cond
           ((= count 1)
            (values 'unique-candidate (car best-solutions) #t best-cost count attempts exhausted? initial-frontier))
           ((> count 1)
            (values 'ambiguous-tie '() #f best-cost count attempts exhausted? initial-frontier))
           (else
            (values 'unrecovered '() #f 0 0 attempts exhausted? initial-frontier))))))))

(def (cpct-plus-receipt machine artifact row outcome operations unique? total-cost
                        ambiguous-count attempts max-cost budget exhausted? frontier token-count)
  (list
   (cons 'schema +cpct-plus-recovery-receipt-schema+)
   (cons 'grammarDigest (parser-machine-grammar-digest machine))
   (cons 'sourceDigest (parse-artifact-ref artifact 'sourceDigest))
   (cons 'outcome outcome)
   (cons 'uniqueMinimal unique?)
   (cons 'totalCost total-cost)
   (cons 'ambiguousCandidateCount ambiguous-count)
   (cons 'publicationStatus (parse-artifact-ref artifact 'status))
   (cons 'owner (and row (car row)))
   (cons 'strategy 'cpct-plus)
   (cons 'operations operations)
   (cons 'maxCost max-cost)
   (cons 'frontierState (and frontier (lr-failure-frontier-state frontier)))
   (cons 'frontierExpectedTerminals
         (if frontier (lr-failure-frontier-expected-terminals frontier) '()))
   (cons 'reusedPrefixTokenCount
         (if frontier
           (- token-count (length (lr-failure-frontier-remaining-tokens frontier)))
           0))
   (cons 'attempts attempts)
   (cons 'budget budget)
   (cons 'budgetExhausted exhausted?)))

;;; Bounded CPCT+ LR recovery with strict uniqueness proof (`max-cost <= 3`).
;;; Never guesses on equal-cost ties: returns `'ambiguous-tie` fail-closed.
(def (parse-source/recover-cpct+ machine source (max-cost 3) (budget 512))
  (unless (and (integer? max-cost) (positive? max-cost))
    (error "CPCT+ max-cost must be positive" max-cost))
  (unless (and (integer? budget) (positive? budget))
    (error "CPCT+ budget must be positive" budget))
  (let* ((artifact (parse-source machine source))
         (recoveries (parser-ir-ref (parser-machine-ir machine) 'recoveries))
         (row (and (pair? recoveries) (car recoveries))))
    (cond
     ((parse-artifact-success? artifact)
      (values artifact
              (cpct-plus-receipt machine artifact row 'not-needed '() #t 0 1 0
                                 max-cost budget #f #f 0)))
     ((lr-runtime-layout? (parser-machine-runtime machine))
      (values artifact
              (cpct-plus-receipt machine artifact row 'disabled '() #f 0 0 0
                                 max-cost budget #f #f 0)))
     ((not row)
      (values artifact
              (cpct-plus-receipt machine artifact #f 'disabled '() #f 0 0 0
                                 max-cost budget #f #f 0)))
     (else
      (let (tokens
            (with-catch (lambda (_) '())
              (lambda ()
                (parser-significant-tokens machine
                 (or (rejected-artifact-tokens artifact)
                     (lex-source machine source))))))
        (let-values (((outcome ops unique? total-cost ambig-count attempts exhausted? frontier)
                      (find-cpct-plus-recoveries machine tokens max-cost budget)))
          (values artifact
                  (cpct-plus-receipt machine artifact row outcome ops unique? total-cost
                                     ambig-count attempts max-cost budget exhausted?
                                     frontier (length tokens)))))))))
