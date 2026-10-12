;;; Build-time admission of overwrite-only common-byte runs. No value cache.
(import (only-in ./event-fold-zero-state fold-zero-state-plan)
        (only-in ./event-fold-byte-dispatch fold-byte-regions)
        (only-in ./event-fold-analysis fold-expression-analysis fold-eliminate-inert-branches))
(export fold-ordinary-run-plan)

(def (fold-ordinary-run-plan forms cursor)
  (let* ((idle (fold-zero-state-plan forms #t))
         (regions (fold-byte-regions (if idle (vector-ref idle 1) forms) cursor)))
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
           (and (every (lambda (form)
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
                        writes))))))
