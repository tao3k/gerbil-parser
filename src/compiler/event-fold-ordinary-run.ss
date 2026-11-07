;;; Build-time admission of overwrite-only common-byte runs. No value cache.
(import (only-in :std/list/list delete-duplicates/hash)
        (only-in ./event-fold-zero-state fold-zero-state-plan fold-zero-state-support)
        (only-in ./event-fold-byte-dispatch fold-byte-regions)
        (only-in ./event-fold-analysis fold-expression-analysis fold-statement-analysis fold-eliminate-inert-branches))
(export fold-ordinary-run-plan fold-consumer-run-plan)

;; A reset/increment transition is summarized in the scanner, not executed
;; against owner slots at every byte. Only an exact current-byte class and a
;; single self-dependent unsigned update are admitted. The residual body must
;; not observe or write the summarized cell; guards must not depend on it.
(def (counter-transition form cursor)
  (and (eq? (car form) 'if)
       (pair? (cadr form)) (eq? (caadr form) 'line-bytes-any-in?)
       (equal? (cadadr form) `(line-index ,cursor))
       (equal? (caddr (cadr form)) `(line-step (line-index ,cursor)))
       (= (length (caddr form)) 1) (= (length (cadddr form)) 1)
       (let* ((yes (caaddr form)) (no (car (cadddr form)))
              (name (and (eq? (car yes) 'set-uint) (cadr yes))))
         (and name
              (equal? yes `(set-uint ,name (uint-add (state ,name) (uint 1))))
              (equal? no `(set-uint ,name (uint 0)))
              (cons name (list-ref (cadr form) 3))))))

(def (extract-counters forms cursor)
  (let ((summaries '()))
    ;; Only unconditional updates can be moved out of their statement owner.
    ;; Do not descend through a conditional: its execution predicate matters.
    (let (body
           (foldr (lambda (form rest)
                    (let (counter (counter-transition form cursor))
                      (if counter
                        (begin (set! summaries (cons counter summaries)) rest)
                        (cons form rest)))) '() forms))
      (vector body summaries))))

(def (fold-consumer-run-plan forms cursor (guards '()))
  ;; Derive a common interval from the complete consumer body, without an
  ;; all-idle premise. Consumer selectors may stay active: their support must
  ;; be disjoint from every write in the interval. Only last-byte overwrites
  ;; and isolated exact counters are admitted; publication retains source order.
  (let* ((cofactor (and (pair? guards) (fold-zero-state-plan forms #t guards)))
         (initial (extract-counters (if cofactor (vector-ref cofactor 1) forms) cursor))
         (regions (fold-byte-regions (vector-ref initial 0) cursor)))
    (and regions
      (let* ((common (fold-eliminate-inert-branches (vector-ref regions 1) cursor))
             (body common)
             (summaries (vector-ref initial 1))
             (writes (vector-ref (fold-statement-analysis body) 1))
             (counts (map car summaries)))
        (def (total? value)
          (null? (fold-eliminate-inert-branches
                  (list (list 'if value '() '())) cursor)))
        (def (admitted? forms)
          (every
           (lambda (form)
             (case (car form)
               ((if)
                (let (selector (fold-expression-analysis (cadr form)))
                  (and (null? (vector-ref selector 2))
                       (not (any (cut memq <> (append writes counts))
                                 (vector-ref selector 0)))
                       ;; Cursor-dependent selectors can change mid-interval.
                       (not (contains-cursor? (cadr form)))
                       (admitted? (caddr form)) (admitted? (cadddr form)))))
               ((set-bool set-uint)
                (and (null? (vector-ref (fold-expression-analysis (caddr form)) 0))
                     (total? (caddr form))))
               (else #f))) forms))
        (def (contains-cursor? value)
          (and (pair? value)
               (or (memq (car value) '(line-index line-step line-byte-equal?
                                      line-bytes-any-in? line-bytes-all-in? line-bytes-in-set?))
                   (any contains-cursor? (cdr value)))))
        (and (= (length counts) (length (delete-duplicates/hash counts)))
             (not (any (cut memq <> (append writes counts)) guards))
             (not (any (cut memq <> writes) counts))
             (not (any (cut memq <> counts)
                       (vector-ref (fold-statement-analysis body) 0)))
             (admitted? body)
             (vector guards (foldr append '() (map car (vector-ref regions 0)))
                     '(bool #t) body summaries))))))

(def (fold-ordinary-run-plan forms cursor)
  (let* ((idle (fold-idle-run-plan forms cursor))
         (selected
          (or (fold-consumer-run-plan forms cursor)
              (and idle
          ;; Remove only a consumer premise with a complete transfer witness.
          ;; The remaining consumers may be active. No runtime mode cache or
          ;; subset enumeration is introduced: one bounded pass over supports.
          (foldl
           (lambda (name plan)
             (let* ((remaining (filter (lambda (cell) (not (eq? cell name)))
                                       (vector-ref plan 0)))
                    (witness (fold-consumer-run-plan forms cursor remaining)))
               (or witness plan))) idle (vector-ref idle 0))))))
    (if (and idle selected
             (equal? (vector-ref idle 2) (vector-ref selected 2))
             (< (length (vector-ref idle 1)) (length (vector-ref selected 1)))
             (every (cut memv <> (vector-ref selected 1)) (vector-ref idle 1)))
      (let* ((support (filter (lambda (name) (not (memq name (vector-ref selected 0))))
                              (vector-ref idle 0)))
             (writes (append (vector-ref (fold-statement-analysis (vector-ref selected 3)) 1)
                             (map car (vector-ref selected 4)))))
        (if (and (pair? support) (not (any (cut memq <> writes) support)))
          ;; Select a precompiled membership table once per interval. Both
          ;; transfers are admitted, and selector support is unchanged while
          ;; scanning. This is not a mode cache or a second source traversal.
          (vector (vector-ref selected 0) (vector-ref selected 1)
                  (vector-ref selected 2) (vector-ref selected 3)
                  (vector-ref selected 4) (vector support (vector-ref idle 1)))
          selected)) selected)))

(def (fold-idle-run-plan forms cursor)
  (let* ((idle (fold-zero-state-support forms #t))
         (extracted (extract-counters (if idle (vector-ref idle 1) forms) cursor))
         (summaries (vector-ref extracted 1))
         (regions (fold-byte-regions (vector-ref extracted 0) cursor)))
    (and regions
         (let* ((guard-cells (if idle (vector-ref idle 0) '()))
                ;; Region rejection removes unreachable effect barriers. Apply
                ;; the same explicit entry facts again, not a cached state.
                (refined (fold-zero-state-plan (vector-ref regions 1) #t guard-cells))
                (common (fold-eliminate-inert-branches
                         (if refined (vector-ref refined 1) (vector-ref regions 1)) cursor))
                ;; A skipped-prefix guard becomes permanently true after the
                ;; cursor reaches its stable bound; never cross a bound write.
                (prefix (and (= (length common) 1)
                             (eq? (caar common) 'if)
                             (null? (caddar common))
                             (pair? (cadar common))
                             (eq? (caadar common) 'offset-less?)
                             (equal? (cadadr (car common)) (list 'line-index cursor))
                             (pair? (caddr (cadar common)))
                             (eq? (car (caddr (cadar common))) 'state-offset)
                             (cadr (caddr (cadar common)))))
                (writes (if prefix (cadddr (car common)) common))
                (targets (map (lambda (form) (and (memq (car form) '(set-bool set-uint))
                                                 (cadr form))) writes)))
           (and (let ((names (map car summaries)))
                  (and (= (length names) (length (delete-duplicates/hash names)))
                       (not (any (cut memq <> guard-cells) names))
                       (not (any (cut memq <> targets) names))
                       (not (any (cut memq <> names)
                              (vector-ref (fold-statement-analysis common) 0)))))
                (every (lambda (form)
                         (and (memq (car form) '(set-bool set-uint))
                              (null? (vector-ref (fold-expression-analysis (caddr form)) 0))
                              ;; Reuse the total-observation admission. Bounds
                              ;; derive from the validated current cursor only.
                              (null? (fold-eliminate-inert-branches
                                      (list (list 'if (caddr form) '() '())) cursor))))
                       writes)
                (not (any (cut memq <> targets) guard-cells))
                (or (not prefix) (not (memq prefix targets)))
                (vector guard-cells
                        (foldr append '() (map car (vector-ref regions 0)))
                        (if prefix
                          `(not (offset-less? (line-index ,cursor) (state-offset ,prefix)))
                          '(bool #t))
                        writes summaries))))))
