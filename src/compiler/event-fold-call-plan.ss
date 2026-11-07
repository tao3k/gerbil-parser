;;; Fuse unique generated call edges, never duplicate a shared consumer.
;;; This runs once at compilation, before module partitioning. No value cache.
(import (only-in ./event-fold-scheme-context fold-scheme-fresh-name))
(export fold-fuse-single-use-calls)

(def (private? form)
  (and (pair? form) (eq? (car form) 'def) (pair? (cadr form))
       (string-prefix? "%event-fold-" (symbol->string (caadr form)))
       ;; Helper entrypoints retain their frame/ownership boundary.
       (string-contains (symbol->string (caadr form)) "-expression-")))

(def (size code)
  (if (or (not (pair? code)) (eq? (car code) 'quote)) 1
    (+ 1 (size (car code)) (size (cdr code)))))

;; Alpha-convert binders before moving a body into its caller. Initializers
;; retain let/let* scope, named-loop recursion stays local, quotes stay opaque.
(def (fresh-body code env)
  (def (atom value)
    (let (entry (and (symbol? value) (assq value env)))
      (if entry (cdr entry) value)))
  (cond
   ((not (pair? code)) (atom code))
   ((eq? (car code) 'quote) code)
   ((memq (car code) '(let let*))
    (let* ((named? (symbol? (cadr code)))
           (raw-bindings (if named? (caddr code) (cadr code)))
           (bindings (if (and (pair? raw-bindings) (symbol? (car raw-bindings)))
                       (list raw-bindings) raw-bindings))
           (loop-name (and named? (fold-scheme-fresh-name 'fused-loop)))
           (body-env (if named? (cons (cons (cadr code) loop-name) env) env))
           (out '()))
      (for-each
       (lambda (binding)
         (let* ((name (fold-scheme-fresh-name 'fused-local))
                (value (fresh-body (cadr binding)
                         (if (eq? (car code) 'let*) body-env env))))
           (set! out (cons (list name value) out))
           (set! body-env (cons (cons (car binding) name) body-env)))) bindings)
      (append (list (car code)) (if named? (list loop-name) '())
              (list (reverse out))
              (map (cut fresh-body <> body-env) (if named? (cdddr code) (cddr code))))))
   ((eq? (car code) 'lambda)
    (let* ((names (map (lambda (_) (fold-scheme-fresh-name 'fused-local)) (cadr code)))
           (body-env (append (map cons (cadr code) names) env)))
      `(lambda ,names ,@(map (cut fresh-body <> body-env) (cddr code)))))
   (else (cons (fresh-body (car code) env) (fresh-body (cdr code) env)))))

(def (fold-fuse-single-use-calls forms (budget 256))
  (let ((directory (make-hash-table-eq)) (uses (make-hash-table-eq)))
    (for-each (lambda (form)
                (when (private? form) (hash-put! directory (caadr form) form))) forms)
    (def (count code)
      (cond
       ((symbol? code)
        (when (hash-get directory code)
          (hash-put! uses code (+ 1 (or (hash-get uses code) 0)))))
       ((and (pair? code) (not (eq? (car code) 'quote)))
        (count (car code)) (count (cdr code)))))
    ;; Signatures bind names; export/value references count and prevent removal.
    (for-each (lambda (form)
                (count (if (and (pair? form) (eq? (car form) 'def)) (cddr form) form))) forms)
    (let ((active (make-hash-table-eq)))
      (def (walk code)
        (cond
         ((or (not (pair? code)) (eq? (car code) 'quote)) code)
         ((and (symbol? (car code)) (hash-get directory (car code)))
          (let* ((name (car code)) (definition (hash-get directory name))
                 (parameters (cdadr definition)) (arguments (cdr code)))
            (if (and (= (or (hash-get uses name) 0) 1)
                     (not (hash-get active name))
                     (= (length parameters) (length arguments))
                     ;; Generated closure arguments are scalar references;
                     ;; do not change effectful argument evaluation order.
                     (every (lambda (arg) (not (pair? arg))) arguments)
                     (<= (size (cddr definition)) budget))
              (begin
                (hash-put! active name #t)
                (let* ((names (map (lambda (_) (fold-scheme-fresh-name 'fused-arg)) parameters))
                       (body (map (cut fresh-body <> (map cons parameters names)) (cddr definition)))
                       (expanded (map walk body)))
                  (hash-put! active name #f)
                  ;; A chain is bounded by its expanded size, not each leaf.
                  (if (<= (size expanded) budget)
                    (begin
                      `(let ,(map list names arguments) ,@expanded))
                    code)))
              code)))
         (else (cons (walk (car code)) (walk (cdr code))))))
      ;; Only fuse top-level ordered definitions and public bodies. Unknown
      ;; recursive cycles are still rejected by the module graph validator.
      (let (rewritten
             (map (lambda (form)
                    (if (and (pair? form) (eq? (car form) 'def))
                      (append (take form 2) (map walk (cddr form))) form)) forms))
        ;; Failed parent admission may retain an original call after child
        ;; expansion. Remove only unreachable definitions from the final graph,
        ;; never from speculative expansion side effects.
        (let ((final (make-hash-table-eq)) (reachable (make-hash-table-eq)))
          (for-each (lambda (form)
                      (when (private? form) (hash-put! final (caadr form) form))) rewritten)
          (def (visit code)
            (cond
             ((symbol? code)
              (when (and (hash-get final code) (not (hash-get reachable code)))
                (hash-put! reachable code #t)
                (visit (cddr (hash-get final code)))))
             ((and (pair? code) (not (eq? (car code) 'quote)))
              (visit (car code)) (visit (cdr code)))))
          (for-each (lambda (form)
                      (unless (private? form) (visit form))) rewritten)
          (filter (lambda (form)
                    (or (not (private? form)) (hash-get reachable (caadr form)))) rewritten))))))
