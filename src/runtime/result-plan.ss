;;; Owned construction data and copied public views, without profile compilers.
(export make-result-plan result-plan? result-plan-owned-recipe
        result-plan-node-index result-plan-token-index result-plan-projection-index
        result-plan-recipe result-plan-catalog copy-result-datum)

(defstruct result-plan (owned-recipe node-index token-index projection-index))
(def (copy-result-datum value)
  (cond ((pair? value) (cons (copy-result-datum (car value)) (copy-result-datum (cdr value))))
        ((string? value) (string-copy value)) (else value)))
(def (result-plan-recipe plan)
  (let (recipe (result-plan-owned-recipe plan))
    (list (map (lambda (row) (list (car row) (map values (cadr row)))) (car recipe))
          (map values (cadr recipe)) (copy-result-datum (caddr recipe)))))
(def (result-plan-catalog plan)
  ;; Catalog publication copies only the catalog, never the projection program.
  (let (recipe (result-plan-owned-recipe plan))
    (list (cons 'syntax-kinds
                (map (lambda (row) (list (car row) 'result (map values (cadr row)))) (car recipe)))
          (cons 'terminals (map (lambda (kind) (list kind 'token)) (cadr recipe))))))
