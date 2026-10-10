;;; Grammar object graph validation and deterministic normalization.

(import (only-in ../modules/parser/objects
                 grammar-composition grammar-name grammar-parents
                 grammar-role-name grammar-role-ref grammar-roles)
        (only-in ../runtime/identity sha256-text)
        (only-in :std/list/list-builder with-list-builder)
        (only-in ../utilities/lists append-unique merge-keyed-row))
(export compile-grammar
        compile-grammar/receipt
        compile-grammar/context normalized-grammar-ir normalized-grammar-receipt
        normalized-grammar-source-map
        grammar-composition-receipt
        grammar-ir-ref
        grammar-ir-canonical)

(def table-slots
  '(syntax-kinds terminals lexical-rules rules extras keywords
    parser-entrypoints recoveries))

(def (section-row-key _section) car)

;; : (-> Grammar (List Grammar) (List GrammarRole))
(def (collect-steps grammar (active '()))
  (with-list-builder (emit)
    (def (visit grammar active)
      (when (memq grammar active)
        (error "grammar inheritance cycle" (grammar-name grammar)))
      (let (next (cons grammar active))
        (for-each (lambda (parent) (visit parent next)) (grammar-parents grammar))
        (for-each (lambda (role) (emit (cons 'merge role))) (grammar-roles grammar))
        (for-each emit (grammar-composition grammar))))
    (visit grammar active)))

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

;;; Context is captured by the same fold that admits effective grammar rows.
;;; Occurrence indices distinguish equal role names and repeated role objects.
(defstruct effective-grammar-row (row step operation role source history))
(defstruct normalized-grammar (ir receipt selections))

(def (normalize-keyed-table steps section source-at capture?)
  (def rows '())
  (def selections '())
  (for-each
   (lambda (step index)
     (let ((operation (car step)) (role (cdr step)))
       (for-each
        (lambda (row)
          ;; This remains the sole authority for conflicts and row ordering.
          ;; Source callbacks cannot admit an invalid composition operation.
          (set! rows (apply-keyed-operation rows row section operation))
          (when capture?
            (let* ((key ((section-row-key section) row))
                   (previous (assoc key selections))
                   (history-entry (list index operation (grammar-role-name role))))
              (case operation
                ((remove)
                 (set! selections (filter (lambda (entry) (not (equal? (car entry) key))) selections)))
                (else
                 (let* ((old (and previous (cdr previous)))
                        (history (append (if old (effective-grammar-row-history old) '())
                                         (list history-entry)))
                        (selected
                         (if (and old (eq? operation 'merge))
                           ;; An admitted equal duplicate retains the first owner.
                           (make-effective-grammar-row
                            (effective-grammar-row-row old) (effective-grammar-row-step old)
                            (effective-grammar-row-operation old) (effective-grammar-row-role old)
                            (effective-grammar-row-source old) history)
                           (let (source (and source-at (source-at role index section row)))
                             (unless (or (not source)
                                         (and (list? source)
                                              (every (lambda (entry)
                                                       (and (pair? entry) (symbol? (car entry)))) source)))
                               (error "invalid grammar occurrence source" index section key source))
                             (make-effective-grammar-row row index operation
                                                         (grammar-role-name role) source history)))))
                   (set! selections
                     (if previous
                       (map (lambda (entry) (if (equal? (car entry) key) (cons key selected) entry))
                            selections)
                       (append selections (list (cons key selected)))))))))))
        (grammar-role-ref role section))))
   steps (iota (length steps)))
  (values rows selections))

;;; Project only effective bindings. Removed declarations remain in the receipt,
;;; while a remove/reappend starts a new effective declaration history.
(def (normalized-grammar-source-map normalized origin)
  (def selections (normalized-grammar-selections normalized))
  (def (entries section)
    (let (entry (assq section selections)) (if entry (cdr entry) '())))
  (def (source selected)
    (let* ((provided (or (effective-grammar-row-source selected) '()))
           (facts
            (list (cons 'componentOwner (effective-grammar-row-role selected))
                  (cons 'compositionStep (effective-grammar-row-step selected))
                  (cons 'compositionOperation (effective-grammar-row-operation selected))
                  (cons 'contributionHistory (effective-grammar-row-history selected))))
           (coordinates
            (filter-map (lambda (key) (and (not (assq key provided)) (cons key origin)))
                        '(path location))))
      ;; The selected role owns composition facts; supplied syntax owns reader
      ;; coordinates and generated status. Unknown coordinates stay at origin.
      (append coordinates
              (filter (lambda (entry) (not (assq (car entry) facts))) provided)
              facts)))
  (append
   (map (lambda (mapping)
          (cons (cdr mapping)
                (map (lambda (entry) (cons (car entry) (source (cdr entry))))
                     (entries (car mapping)))))
        '((syntax-kinds . syntax-kind) (terminals . terminal)
          (lexical-rules . lexical-rule) (rules . rule)))
   (list
    (cons 'field
          (with-list-builder (emit)
            (for-each
             (lambda (entry)
               (let ((row (effective-grammar-row-row (cdr entry)))
                     (selected-source (source (cdr entry))))
                 (for-each
                  (lambda (name) (emit (cons (list (car row) name) selected-source)))
                  (caddr row))))
             (entries 'syntax-kinds)))))))

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

(def (normalize-grammar/context grammar source-at capture?)
  (if (and (list? grammar)
           (let (row (assq 'schema grammar))
             (and row (equal? (cdr row) "gerbil-parser.grammar-ir.v1"))))
    (make-normalized-grammar grammar #f '())
    (let* ((steps (collect-steps grammar))
         (normalized-tables
          (map (lambda (section)
                 (let-values (((rows selections)
                               (normalize-keyed-table steps section source-at capture?)))
                   (list section rows selections)))
               table-slots))
         (tables (map (lambda (entry) (cons (car entry) (cadr entry))) normalized-tables))
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
      (make-normalized-grammar
       ir receipt (if capture?
                    (map (lambda (entry) (cons (car entry) (caddr entry))) normalized-tables)
                    '())))))

;;; Existing callers do not request occurrence/source capture.
(def (compile-grammar/receipt grammar)
  (let (normalized (normalize-grammar/context grammar #f #f))
    (values (normalized-grammar-ir normalized) (normalized-grammar-receipt normalized))))

;;; The callback supplies source metadata for the actual accepted occurrence;
;;; it is optional because some POO values have no reader syntax association.
(def (compile-grammar/context grammar (source-at #f))
  (normalize-grammar/context grammar source-at #t))


(def (compile-grammar grammar)
  (let-values (((ir _receipt) (compile-grammar/receipt grammar))) ir))

(def (grammar-ir-ref ir key)
  (let (entry (assq key ir))
    (and entry (cdr entry))))

(def (grammar-ir-canonical ir)
  (call-with-output-string
   (lambda (port) (write ir port))))
