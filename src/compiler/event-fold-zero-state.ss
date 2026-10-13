;;; Guarded idle-state specialization over admitted IR, not value memoization.
(import (only-in ./event-fold-analysis fold-statement-analysis fold-expression-analysis)
        (only-in ./event-fold-byte-dispatch fold-byte-regions))
(export fold-zero-state-plan fold-zero-state-support fold-ordinary-state-plan fold-consumer-state-body)

;; Minimize the *external* guard by an exact cofactor witness. A payload
;; predicate in an eliminated consumer branch must not make another consumer
;; unavailable. Dropping a cell is admitted only when rerunning specialization
;; without that premise produces the identical ordered body. Build-time only.
(def (fold-zero-state-support forms (complete-zero? #f))
  (let (plan (fold-zero-state-plan forms complete-zero?))
    (and plan
      (let* ((body (vector-ref plan 1))
             (support
               (foldl (lambda (name current)
                        (let* ((remaining (filter (lambda (cell) (not (eq? cell name))) current))
                               (witness (fold-zero-state-plan forms complete-zero? remaining)))
                          (if (equal? body (if witness (vector-ref witness 1) forms)) remaining current)))
                      (vector-ref plan 0) (vector-ref plan 0))))
        (vector support body)))))

;; Facts come from each consumer's executed selector, not an all-machine
;; idle guard. They are compile-time control-flow facts, never retained state.
(def (fold-consumer-state-body forms)
  (let (plan (fold-zero-state-plan forms #t '() #t))
    (if plan (vector-ref plan 1) forms)))

;;; A single common-byte kernel, not one generated kernel per byte region.
;;; The original complete body remains the fallback for every exceptional byte.
(def (fold-ordinary-state-plan forms cursor)
  (let* ((idle (fold-zero-state-plan forms))
         (body (if idle (vector-ref idle 1) forms))
         (regions (fold-byte-regions body cursor)))
    (and regions
         (vector (if idle (vector-ref idle 0) '())
                 (foldr append '() (map car (vector-ref regions 0)))
                 (vector-ref regions 1)))))

(def (fold-zero-state-plan forms (complete-zero? #f) (guarded-cells #f) (consumer-local? #f))
  (let ((seen (make-hash-table-eq)) (names '()))
    (def (collect value)
      (when (pair? value)
        (when (and (memq (car value) '(uint-positive? uint-equal?))
                   (pair? (cadr value)) (eq? (caadr value) 'state)
                   (or (eq? (car value) 'uint-positive?)
                       (equal? (caddr value) '(uint 0))))
          (let (name (cadadr value))
            (unless (hash-get seen name)
              (hash-put! seen name #t) (set! names (cons name names)))))
        (for-each collect (if (symbol? (car value)) (cdr value) value))))
    (for-each collect forms)
    ;; A read-only configuration is not an idle machine state. Requiring its
    ;; default nonzero value to be zero makes the fast branch unreachable.
    ;; Specialize only candidate cells actually written inside this loop;
    ;; all other predicates retain their ordinary evaluation.
    (let (writes (vector-ref (fold-statement-analysis forms) 1))
      (set! names (if guarded-cells guarded-cells (filter (cut memq <> writes) names))))
    ;; The admitted IR has distinct slot cells and an event-tape owner. An
    ;; append, query-cache update or possible range error is observable, but
    ;; is not an arbitrary slot write. Keep its execution/order while dropping
    ;; facts only for declared writes or unknown/helper/frame barriers.
    (def (barrier? effects) (any (cut memq <> '(opaque helper bounds)) effects))
    (def (constant predicate facts)
      (cond
       ((and (pair? predicate) (eq? (car predicate) 'uint-positive?)
             (pair? (cadr predicate)) (eq? (caadr predicate) 'state)
             (memq (cadadr predicate) facts)) '(bool #f))
       ((and (pair? predicate) (eq? (car predicate) 'uint-equal?)
             (pair? (cadr predicate)) (eq? (caadr predicate) 'state)
             (memq (cadadr predicate) facts) (equal? (caddr predicate) '(uint 0))) '(bool #t))
       ((and complete-zero? (pair? predicate)
             (memq (car predicate) '(uint-equal? uint-not-equal? uint-greater?))
             (pair? (cadr predicate)) (eq? (caadr predicate) 'state)
             (memq (cadadr predicate) facts)
             (pair? (caddr predicate)) (eq? (caaddr predicate) 'uint))
        (let (literal (cadr (caddr predicate)))
          (list 'bool (case (car predicate)
                        ((uint-equal?) (= 0 literal))
                        ((uint-not-equal?) (not (= 0 literal)))
                        (else (> 0 literal))))))
       ((and (pair? predicate) (eq? (car predicate) 'not))
        (let (value (constant (cadr predicate) facts))
          (cond ((equal? value '(bool #t)) '(bool #f))
                ((equal? value '(bool #f)) '(bool #t))
                (else (list 'not value)))))
       ((and (pair? predicate) (memq (car predicate) '(and or)))
        (let* ((op (car predicate)) (identity (eq? op 'and)))
          (let loop ((rest (cdr predicate)) (kept '()) (facts facts))
            (if (null? rest)
              (cond ((null? kept) (list 'bool identity))
                    ((null? (cdr kept)) (car kept))
                    (else (cons op (reverse kept))))
              (let (value (constant (car rest) facts))
                (cond ((equal? value (list 'bool identity)) (loop (cdr rest) kept facts))
                      ((equal? value (list 'bool (not identity)))
                       ;; Earlier unknown leaves retain queries/errors/order.
                       (if (null? kept) value (cons op (append (reverse kept) (list value)))))
                      (else (loop (cdr rest) (cons value kept)
                              (if (barrier? (vector-ref (fold-expression-analysis value) 2)) '() facts)))))))))
       (else predicate)))
    (def (intersection left right) (filter (cut memq <> right) left))
    (def (selector-facts predicate truth facts)
      ;; Unknown predicate effects discard knowledge before any deduction.
      ;; Only admitted unsigned cells justify not-positive => exactly zero.
      (if (or (not consumer-local?)
              (barrier? (vector-ref (fold-expression-analysis predicate) 2))) facts
        (case (car predicate)
          ((not) (selector-facts (cadr predicate) (not truth) facts))
          ((and or)
           (if (eq? truth (eq? (car predicate) 'and))
             (foldl (lambda (part known) (selector-facts part truth known)) facts (cdr predicate))
             facts))
          ((uint-positive? uint-equal? uint-not-equal?)
           (if (and (pair? (cadr predicate)) (eq? (caadr predicate) 'state)
                    (or (and (eq? (car predicate) 'uint-positive?) (not truth))
                        (and (not (eq? (car predicate) 'uint-positive?))
                             (equal? (caddr predicate) '(uint 0))
                             (eq? truth (eq? (car predicate) 'uint-equal?)))))
             (let (name (cadadr predicate)) (if (memq name facts) facts (cons name facts)))
             facts))
          (else facts))))
    (def (walk body facts)
      (let loop ((rest body) (facts facts) (out '()))
        (if (null? rest) (vector (reverse out) facts)
          (let (form (car rest))
            (case (car form)
              ((if)
               (let (predicate (constant (cadr form) facts))
                 (if (or (equal? predicate '(bool #t)) (equal? predicate '(bool #f)))
                   (let (chosen (walk (if (cadr predicate) (caddr form) (cadddr form)) facts))
                     (loop (cdr rest) (vector-ref chosen 1)
                           (append (reverse (vector-ref chosen 0)) out)))
                   (let* ((branch-facts (if (barrier? (vector-ref (fold-expression-analysis predicate) 2)) '() facts))
                          (yes (walk (caddr form) (selector-facts predicate #t branch-facts)))
                          (no (walk (cadddr form) (selector-facts predicate #f branch-facts))))
                     (loop (cdr rest) (intersection (vector-ref yes 1) (vector-ref no 1))
                       (cons (list 'if predicate (vector-ref yes 0) (vector-ref no 0)) out))))))
              ((join-once)
               (let* ((outer (filter (lambda (name) (not (eq? name (cadr form)))) facts))
                      (first (walk (caddr form) outer))
                      (fallback (walk (cadddr form) (vector-ref first 1))))
                 (loop (cdr rest) (intersection (vector-ref first 1) (vector-ref fallback 1))
                   (cons (list 'join-once (cadr form) (vector-ref first 0) (vector-ref fallback 0)) out))))
              (else
               (let* ((effect (fold-statement-analysis (list form)))
                      (remaining (if (not (barrier? (vector-ref effect 2)))
                                   (filter (lambda (name) (not (memq name (vector-ref effect 1)))) facts)
                                   '())))
                 ;; Only a literal zero write establishes a new local fact;
                 ;; all other writes invalidate just their declared support.
                 (loop (cdr rest)
                   (if (and consumer-local? (eq? (car form) 'set-uint)
                            (equal? (caddr form) '(uint 0)))
                     (cons (cadr form) remaining) remaining)
                   (cons form out)))))))))
    (and (or consumer-local? (pair? names)) (<= (length names) 32)
         (let (body (vector-ref (walk forms names) 0))
           (and (not (equal? body forms)) (vector (reverse names) body))))))
