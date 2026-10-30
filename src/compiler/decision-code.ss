;;; Share identical action code after ordered decision selection. No predicate
;;; or action is evaluated by the generator; both retain their caller's scope.
(export shared-decision-expression)

(def (shared-decision-expression selector rows fallback)
  (let ((codes (make-table test: equal?)) (actions '()) (next 0))
    (def (intern expression)
      (or (table-ref codes expression #f)
          (let (code next)
            (set! next (+ next 1))
            (table-set! codes expression code)
            (set! actions (cons `((,code) ,expression) actions))
            code)))
    (let (selection
           `(case ,selector
              ,@(map (lambda (row)
                       `((,(car row))
                         (cond ,@(map (lambda (branch)
                                        (list (car branch) (intern (cadr branch))))
                                      (cdr row))
                               (else #f)))) rows)
              (else #f)))
      `(case ,selection ,@(reverse actions) (else ,fallback)))))
