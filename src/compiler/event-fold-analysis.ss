;;; Build-time dependence and effect analysis over already admitted Fold IR.
;;; Unknown forms are opaque, never evidence of purity. No request cache.
(import (only-in :std/list/list delete-duplicates/hash duplicates append-map))
(export fold-expression-analysis fold-statement-analysis fold-zero-support-preserved?
        fold-shared-scans fold-rewrite-scans fold-prefix-guard fold-eliminate-inert-branches
        fold-share-statement-scans)

;; An entry support is an invariant, not necessarily an untouched cell.
;; Whole ordered regions may reset that support to zero, but may not change it
;; transiently: a subsequent consumer could observe the intermediate value.
;; Frames, publication and unknown handoffs remain explicit owner boundaries.
(def (fold-zero-support-preserved? forms support)
  (or (null? support)
  (every
   (lambda (form)
     (case (car form)
       ((if)
        (and (null? (vector-ref (fold-expression-analysis (cadr form)) 2))
             (fold-zero-support-preserved? (caddr form) support)
             (fold-zero-support-preserved? (cadddr form) support)))
       ((set-uint set-bool)
        (or (not (memq (cadr form) support))
            (and (eq? (car form) 'set-uint) (equal? (caddr form) '(uint 0)))))
       (else #f))) forms)))

;;; Drop discarded observations only after admitting the absence of effects.
;;; Queries, errors and unknown predicates retain their original execution.
(def (fold-eliminate-inert-branches forms (cursor #f))
  (def (cursor-offset? value)
    (and cursor
         (or (equal? value (list 'line-index cursor))
             (and (pair? value) (eq? (car value) 'line-step)
                  (cursor-offset? (cadr value))))))
  (def (ordered? from until)
    (or (equal? from until)
        (and (pair? until) (eq? (car until) 'line-step)
             (ordered? from (cadr until)))))
  (def (pure-observation? predicate)
    (or (null? (vector-ref (fold-expression-analysis predicate) 2))
        (and (pair? predicate)
             (case (car predicate)
               ((and or not) (every pure-observation? (cdr predicate)))
               ((line-bytes-any-in? line-bytes-all-in? line-bytes-in-set?)
                ;; The emitted loop validates its frame before entering. Each
                ;; step clamps to end; ordered cursor-derived ranges are total.
                (and (cursor-offset? (cadr predicate))
                     (cursor-offset? (caddr predicate))
                     (ordered? (cadr predicate) (caddr predicate))))
               (else #f)))))
  (append-map
   (lambda (form)
     (case (car form)
       ((if)
        (let ((yes (fold-eliminate-inert-branches (caddr form) cursor))
              (no (fold-eliminate-inert-branches (cadddr form) cursor)))
          (if (and (null? yes) (null? no)
                   (pure-observation? (cadr form)))
            '() (list (list 'if (cadr form) yes no)))))
       ((join-once)
        (list (list 'join-once (cadr form)
                    (fold-eliminate-inert-branches (caddr form) cursor)
                    (fold-eliminate-inert-branches (cadddr form) cursor))))
       ((for-line-bytes)
        (list (append (take form 4)
                      (list (fold-eliminate-inert-branches (list-ref form 4) (cadr form))))))
       ((with-source-bounds)
        (list (append (take form 3)
                      (list (fold-eliminate-inert-branches (cadddr form))))))
       (else (list form)))) forms))

;; Summary fields: state reads, state writes, effects. Source bounds/cursors
;; remain lexical dependencies; admitted reuse may cross statements, never frames.
(def (summary reads writes effects) (vector reads writes effects))
(def (unique items) (delete-duplicates/hash items from-end?: #t))
(def (merge summaries)
  (summary (unique (append-map (cut vector-ref <> 0) summaries))
           (unique (append-map (cut vector-ref <> 1) summaries))
           (unique (append-map (cut vector-ref <> 2) summaries))))
(def (fold-expression-analysis value)
  (if (not (pair? value)) (summary '() '() '())
    (let* ((op (car value))
           (children (merge (map fold-expression-analysis (cdr value)))))
      (case op
        ((state state-offset stack-top stack-nonempty?)
         (summary (list (cadr value)) '() '()))
        ((uint bool %line-prefix-hit offset line-index line-prefix-end line-content-end line-trim-end
          line-step line-marker-end line-marker-level line-indent-column
          uint-add uint-multiply uint-positive? uint-equal?
          uint-greater? uint-not-equal? offset-less? not and or
          line-starts-with line-starts-with-ascii-ci line-prefix-boundary-ascii-ci
          line-marker-ascii-ci line-blank? line-has-word-after-prefix?
          line-has-key-after-prefix?) children)
        ((uint-divide)
         ;; A necessary-prefix rejection cannot suppress division by zero.
         (let (divisor (caddr value))
           (if (and (pair? divisor) (eq? (car divisor) 'uint)
                    (number? (cadr divisor)) (> (cadr divisor) 0)) children
             (merge (list children (summary '() '() '(division-error)))))))
        ;; Bounded scans are total only when starting at admitted frame bounds.
        ((line-skip-horizontal line-scan-word line-scan-key line-scan-until
          line-scan-nonspace-until line-trim-end-from line-physical-end)
         (if (safe-offset? (cadr value)) children
           (merge (list children (summary '() '() '(range-error))))))
        ((line-byte-equal?)
         (if (safe-offset? (cadr value)) children
           (merge (list children (summary '() '() '(range-error))))))
        ((line-bytes-any-in? line-bytes-all-in? line-bytes-in-set?)
         ;; Member tables are admitted constants, not expression programs.
         (merge (list (fold-expression-analysis (cadr value))
                      (fold-expression-analysis (caddr value))
                      (summary '() '() '(range-error)))))
        ((future-line-marker-before-boundary? future-heading-title?
          future-named-line-marker-before-boundary?)
         (merge (list children (summary '() '() '(query-cache)))))
        (else (merge (list children (summary '() '() '(opaque)))))))))

(def (safe-offset? value)
  (or (memq value '(start end))
      (and (pair? value)
           (case (car value)
             ((line-index line-prefix-end line-content-end line-trim-end) #t)
             ((line-step) (safe-offset? (cadr value)))
             (else #f)))))

(def (fold-statement-analysis forms)
  (merge
   (map
    (lambda (form)
      (case (car form)
        ((set-bool set-uint)
         (merge (list (fold-expression-analysis (caddr form))
                      (summary '() (list (cadr form)) '()))))
        ((if join-once)
         (merge (list (fold-expression-analysis (cadr form))
                      (fold-statement-analysis (caddr form))
                      (fold-statement-analysis (cadddr form)))))
        ((for-line-bytes)
         (merge (list (fold-expression-analysis (caddr form))
                      (fold-expression-analysis (cadddr form))
                      (fold-statement-analysis (list-ref form 4))
                      (summary '() '() '(iteration)))))
        ((with-source-bounds)
         (merge (list (fold-expression-analysis (cadr form))
                      (fold-expression-analysis (caddr form))
                      (fold-statement-analysis (cadddr form))
                      (summary '() '() '(bounds range-error)))))
        ((token start-node finish-node)
         (merge (list (merge (map fold-expression-analysis (cdr form)))
                      (summary '() '() '(events)))))
        ((call-source-helper) (summary '() '() '(helper events range-error)))
        ((scan-list-marker) (summary '() (list-tail form 4) '(allocation)))
        ((push-frame pop-frame open-level close-frame close-through
          close-frames-while close-all close-all-frames)
         (merge (list (merge (map fold-expression-analysis (cddr form)))
                      (summary (list (cadr form)) (list (cadr form)) '(stack events)))))
        (else (summary '() '() '(opaque))))) forms)))

(def (shared-scan? value)
  (or (equal? value '(line-content-end)) (equal? value '(line-trim-end))
      (equal? value '(line-skip-horizontal start))
      (and (pair? value) (memq (car value) '(line-scan-until line-scan-word line-scan-key))
           (eq? (cadr value) 'start)
           (null? (vector-ref (fold-expression-analysis value) 0))
           (null? (vector-ref (fold-expression-analysis value) 2)))))
(def (scan-occurrences value)
  (cond ((shared-scan? value) (list value))
        ((not (pair? value)) '())
        (else (append-map scan-occurrences (cdr value)))))
(def (guaranteed-scans value)
  (cond ((shared-scan? value) (list value))
        ((not (pair? value)) '())
        ((memq (car value) '(and or))
         (if (null? (cdr value)) '() (guaranteed-scans (cadr value))))
        ;; Only these operators evaluate all children unconditionally.
        ((memq (car value) '(offset-less? offset uint-add uint-multiply uint-divide
                             uint-positive? uint-equal? uint-not-equal? uint-greater?
                             line-step not))
         (append-map guaranteed-scans (cdr value)))
        (else '())))
(def (fold-shared-scans expression)
  (let ((repeated (duplicates (scan-occurrences expression)))
        (facts (fold-expression-analysis expression)))
    ;; Hoisting must not cross an error/cache effect, even with a guaranteed
    ;; scan. Its original first evaluation dominates every reused occurrence.
    (if (not (null? (vector-ref facts 2))) '()
      (filter (lambda (scan) (assoc scan repeated))
              (unique (guaranteed-scans expression))))))
(def (fold-rewrite-scans expression replacements)
  (cond ((assoc expression replacements) => (lambda (entry) `(line-index ,(cdr entry))))
        ((not (pair? expression)) expression)
        (else (cons (car expression)
                    (map (cut fold-rewrite-scans <> replacements) (cdr expression))))))

;; Straight-line source facts are lexical values, not request memoization.
;; Only a total source scan already guaranteed by the first statement may
;; dominate later statements. Never cross a loop, frame or opaque handoff.
(def (fold-share-statement-scans forms (delimiter-only? #f))
  (def (barrier? form)
    (any (cut memq <> '(opaque helper bounds iteration))
         (vector-ref (fold-statement-analysis (list form)) 2)))
  (def (leading form)
    (case (car form)
      ((set-bool set-uint) (guaranteed-scans (caddr form)))
      ((if) (guaranteed-scans (cadr form)))
      ((token) (append (guaranteed-scans (caddr form))
                       (guaranteed-scans (cadddr form))))
      (else '())))
  (def (occurs? scan value)
    (or (equal? scan value)
        (and (pair? value) (or (occurs? scan (car value))
                               (occurs? scan (cdr value))))))
  (let loop ((rest forms))
    (cond ((null? rest) '())
          ((barrier? (car rest)) (cons (car rest) (loop (cdr rest))))
          (else
           (let split ((tail rest) (region '()))
             (if (or (null? tail) (barrier? (car tail)))
               (let* ((region (reverse region))
                      (scans (filter (lambda (scan)
                                       (and (or (not delimiter-only?)
                                                (memq (car scan) '(line-scan-until line-scan-word line-scan-key)))
                                            (every (cut eq? <> 'events)
                                              (vector-ref (fold-statement-analysis (list (car rest))) 2))
                                            (occurs? scan (cdr region))))
                                     (unique (leading (car rest))))))
                 (if (null? scans) (cons (car rest) (loop (cdr rest)))
                   (cons (list '%shared-scans scans region) (loop tail))))
               (split (cdr tail) (cons (car tail) region))))))))

(def (fold-prefix-guard expression)
  (and (null? (vector-ref (fold-expression-analysis expression) 2))
       (let guard ((value expression))
         (and (pair? value)
              (case (car value)
                ((and) (any guard (cdr value)))
                ((line-byte-equal?)
                 (and (safe-offset? (cadr value))
                      (vector (cadr value) (list (caddr value)))))
                ((line-starts-with line-starts-with-ascii-ci
                  line-prefix-boundary-ascii-ci line-marker-ascii-ci)
                 (let (bytes (string->utf8 (cadr value)))
                   (and (> (u8vector-length bytes) 0)
                        (let* ((byte (u8vector-ref bytes 0))
                               (ci? (not (eq? (car value) 'line-starts-with)))
                               (members (cond ((and ci? (<= 65 byte 90)) (list byte (+ byte 32)))
                                              ((and ci? (<= 97 byte 122)) (list byte (- byte 32)))
                                              (else (list byte)))))
                          (vector 'start members)))))
                (else #f))))))
