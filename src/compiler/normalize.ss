;;; Grammar object graph validation and deterministic normalization.

(import (only-in ../modules/parser/objects
                 grammar-composition grammar-name grammar-parents
                 grammar-role-name grammar-role-ref grammar-roles)
        (only-in ../runtime/identity sha256-text)
        (only-in ../utilities/lists append-unique merge-keyed-row))
(export compile-grammar
        compile-grammar/receipt
        grammar-composition-receipt
        grammar-ir-ref
        grammar-ir-canonical)

(def table-slots
  '(syntax-kinds terminals lexical-rules rules extras keywords
    parser-entrypoints recoveries))

(def (section-row-key _section) car)

;; : (-> Grammar (List Grammar) (List GrammarRole))
(def (collect-steps grammar (active '()))
  (when (memq grammar active)
    (error "grammar inheritance cycle" (grammar-name grammar)))
  (let (next (cons grammar active))
    (append
     (apply append
            (map (lambda (parent) (collect-steps parent next))
                 (grammar-parents grammar)))
     (map (lambda (role) (cons 'merge role)) (grammar-roles grammar))
     (grammar-composition grammar))))

(def (replace-keyed-row rows row section)
  (let ((key ((section-row-key section) row))
        (found? #f))
    (let (result
          (map (lambda (current)
                 (if (equal? ((section-row-key section) current) key)
                   (begin (set! found? #t) row)
                   current))
               rows))
      (unless found?
        (error "grammar override target does not exist" section key))
      result)))

(def (remove-keyed-row rows row section)
  (let* ((key ((section-row-key section) row))
         (remaining
          (filter (lambda (current)
                    (not (equal? ((section-row-key section) current) key)))
                  rows)))
    (when (= (length remaining) (length rows))
      (error "grammar remove target does not exist" section key))
    remaining))

(def (apply-keyed-operation rows row section operation)
  (case operation
    ((merge)
     (merge-keyed-row
      rows row (section-row-key section)
      (lambda (key) (error "conflicting grammar role row" section key))))
    ((append)
     (let (key ((section-row-key section) row))
       (when (find (lambda (current)
                     (equal? ((section-row-key section) current) key))
                   rows)
         (error "grammar append target already exists" section key))
       (append rows (list row))))
    ((override) (replace-keyed-row rows row section))
    ((remove) (remove-keyed-row rows row section))
    (else (error "unknown grammar composition operation" operation))))

(def (normalize-keyed-table steps section)
  (foldl (lambda (step rows)
           (let ((operation (car step)) (role (cdr step)))
             (foldl (lambda (row merged)
                      (apply-keyed-operation merged row section operation))
                    rows
                    (grammar-role-ref role section))))
         '()
         steps))

;; : (-> List List List)
(def (append-flow merged edge)
  (if (member edge merged)
    (error "grammar append flow already exists" edge)
    (append merged (list edge))))

;; : (-> List List List)
(def (override-flow merged edge)
  (if (member edge merged) merged
      (error "grammar override flow does not exist" edge)))

;; : (-> List List List)
(def (remove-flow merged edge)
  (if (member edge merged)
    (filter (lambda (current) (not (equal? current edge))) merged)
    (error "grammar remove flow does not exist" edge)))

;; : (-> List List Symbol List)
(def (apply-flow-operation merged edge operation)
  (case operation
    ((merge) (append-unique merged edge))
    ((append) (append-flow merged edge))
    ((override) (override-flow merged edge))
    ((remove) (remove-flow merged edge))
    (else (error "unknown grammar composition operation" operation))))

(def (normalize-flow steps)
  (foldl (lambda (step edges)
           (let ((operation (car step)) (role (cdr step)))
             (foldl
              (lambda (edge merged)
                (apply-flow-operation merged edge operation))
              edges
              (grammar-role-ref role 'flow))))
         '()
         steps))

(def (canonical value)
  (call-with-output-string (lambda (port) (write value port))))

(def (step-receipt step index)
  (let ((operation (car step)) (role (cdr step)))
    (list
     (cons 'index index)
     (cons 'operation operation)
     (cons 'role (grammar-role-name role))
     (cons 'sections
           (filter-map
            (lambda (section)
              (let (rows (grammar-role-ref role section))
                (and (pair? rows)
                     (cons section (map (section-row-key section) rows)))))
            (append table-slots '(flow)))))))

(def (grammar-composition-receipt grammar steps ir)
  (let* ((receipts (map step-receipt steps (iota (length steps))))
         (identity (sha256-text (canonical receipts))))
    (list
     (cons 'schema "gerbil-parser.grammar-composition-receipt.v1")
     (cons 'grammar (grammar-name grammar))
     (cons 'steps receipts)
     (cons 'compositionDigest identity)
     (cons 'grammarIrDigest (sha256-text (canonical ir))))))

(def (compile-grammar/receipt grammar)
  (if (and (list? grammar)
           (let (row (assq 'schema grammar))
             (and row (equal? (cdr row) "gerbil-parser.grammar-ir.v1"))))
    (values grammar #f)
    (let* ((steps (collect-steps grammar))
         (tables
          (map (lambda (section)
                 (cons section (normalize-keyed-table steps section)))
               table-slots))
         (body
          (append
           (list (cons 'schema "gerbil-parser.grammar-ir.v1")
                 (cons 'grammar (grammar-name grammar)))
           tables
           (list (cons 'flow (normalize-flow steps)))))
         (receipt (grammar-composition-receipt grammar steps body))
         (ir
          (append (take body 2)
                  (list (cons 'compositionDigest
                              (cdr (assq 'compositionDigest receipt))))
                  (drop body 2))))
      (values ir receipt))))

(def (compile-grammar grammar)
  (let-values (((ir _receipt) (compile-grammar/receipt grammar))) ir))

(def (grammar-ir-ref ir key)
  (let (entry (assq key ir))
    (and entry (cdr entry))))

(def (grammar-ir-canonical ir)
  (call-with-output-string
   (lambda (port) (write ir port))))
