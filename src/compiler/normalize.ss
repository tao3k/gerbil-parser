;;; Grammar object graph validation and deterministic normalization.

(import ../modules/parser/objects
        ../utilities/lists)
(export compile-grammar
        grammar-ir-ref
        grammar-ir-canonical)

(def table-slots
  '(syntax-kinds terminals rules extras keywords prefix-operators
    binary-operators parser-entrypoints recoveries))

(def (section-row-key section)
  (if (memq section '(prefix-operators binary-operators))
    (lambda (row) (list (car row) (cadr row)))
    car))

(def (collect-roles grammar (active '()))
  (when (memq grammar active)
    (error "grammar inheritance cycle" (grammar-name grammar)))
  (let (next (cons grammar active))
    (let ((inherited
           (let loop ((parents (grammar-parents grammar)) (roles '()))
             (if (null? parents)
               roles
               (loop (cdr parents)
                     (append roles (collect-roles (car parents) next)))))))
      (append inherited (grammar-roles grammar)))))

(def (normalize-keyed-table roles section)
  (let role-loop ((rest roles) (rows '()))
    (if (null? rest)
      rows
      (role-loop
       (cdr rest)
       (let row-loop ((found (grammar-role-ref (car rest) section)) (merged rows))
         (if (null? found)
           merged
           (row-loop (cdr found)
                     (merge-keyed-row
                      merged
                      (car found)
                      (section-row-key section)
                      (lambda (key)
                        (error "conflicting grammar role row" section key))))))))))

(def (normalize-flow roles)
  (let role-loop ((rest roles) (edges '()))
    (if (null? rest)
      edges
      (role-loop
       (cdr rest)
       (let edge-loop ((found (grammar-role-ref (car rest) 'flow)) (merged edges))
         (if (null? found)
           merged
           (edge-loop
            (cdr found)
            (append-unique merged (car found)))))))))

(def (compile-grammar grammar)
  (let* ((roles (collect-roles grammar))
         (tables
          (map (lambda (section)
                 (cons section (normalize-keyed-table roles section)))
               table-slots)))
    (append
     (list (cons 'schema "gerbil-parser.grammar-ir.v1")
           (cons 'grammar (grammar-name grammar)))
     tables
     (list (cons 'flow (normalize-flow roles))))))

(def (grammar-ir-ref ir key)
  (let (entry (assq key ir))
    (and entry (cdr entry))))

(def (grammar-ir-canonical ir)
  (call-with-output-string
   (lambda (port) (write ir port))))
