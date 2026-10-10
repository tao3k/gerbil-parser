;;; -*- Gerbil -*-
;;; Expansion-owned declaration identities beside the frozen Grammar IR v1.

(import (only-in ../grammar/algebra
                 grammar-expression-references
                 grammar-expression-terminals)
        (only-in :std/list/list-builder with-list-builder)
        (only-in ../runtime/identity sha256-text))
(export +bound-grammar-ir-schema+
        bind-grammar-ir
        bound-grammar-ir-ref
        bound-grammar-ir-section
        bound-grammar-ir-binding
        bound-grammar-ir-canonical)

;; +bound-grammar-ir-schema+
;; : BoundGrammarIRSchema
(def +bound-grammar-ir-schema+
  "gerbil-parser.bound-grammar-ir.v1")

;; : (forall (a) (-> [(Pair Symbol a)] Symbol (Maybe a)))
;; : (-> Alist Symbol Datum)
(def (bound-grammar-ir-ref ir key)
  (let (row (assq key ir))
    (and row (cdr row))))

;; : (-> BoundGrammarIR Symbol [BoundDeclaration])
(def (bound-grammar-ir-section ir namespace)
  (let* ((sections (bound-grammar-ir-ref ir 'sections))
         (section (and sections (assq namespace sections))))
    (if section (cdr section) '())))

;; : (-> BoundGrammarIR Symbol Symbol [Symbol] (Maybe BoundDeclaration))
(def (bound-grammar-ir-binding ir namespace name (scope #f))
  (find (lambda (binding)
          (and (eq? (bound-grammar-ir-ref binding 'name) name)
               (or (not scope)
                   (eq? (bound-grammar-ir-ref binding 'scope) scope))))
        (bound-grammar-ir-section ir namespace)))

;; : (forall (a) (-> a String))
;; : (-> BoundGrammarDatum CanonicalBoundGrammarText)
(def (canonical value)
  (call-with-output-string (lambda (port) (write value port))))

;;; Binding identity is structured package data.  Native grammar names are not
;;; rewritten into a second slash-delimited namespace.
;; : (-> Symbol Symbol Datum Alist)
(def (binding-id owner namespace name)
  (list (cons 'package 'gerbil-parser)
        (cons 'grammar owner)
        (cons 'namespace namespace)
        (cons 'name name)))

;; : (-> String Alist)
(def (default-source origin)
  (list (cons 'path origin)
        (cons 'location origin)
        (cons 'generated? #f)))

;;; One invocation owns the lookup index. Retain association entries so a
;;; present false source remains distinct from absence; first occurrences win.
(def (index-declaration-sources source-map)
  (let (sections (make-table test: eq?))
    (for-each
     (lambda (namespace)
       (let (section (assq namespace source-map))
         (when section
           (let (names (make-table test: equal?))
             (for-each
              (lambda (row)
                (unless (table-ref names (car row) #f)
                  (table-set! names (car row) row)))
              (cdr section))
             (table-set! sections namespace names)))))
     '(syntax-kind terminal lexical-rule rule field))
    sections))

;; : (-> SourceIndex Symbol Datum String Alist)
(def (declaration-source source-index namespace name origin)
  (let* ((section (table-ref source-index namespace #f))
         (row (and section (table-ref section name #f))))
    (if row (cdr row) (default-source origin))))

;; : (-> Symbol Symbol Datum Alist)
(def (reference-binding-id owner namespace name)
  (binding-id owner namespace name))

;; : (-> Symbol Symbol List [String])
(def (row-references owner namespace row)
  (case namespace
    ((syntax-kind)
     (map (lambda (field)
            (reference-binding-id
             owner 'field
             (list (car row) field)))
          (caddr row)))
    ((terminal)
     (list (reference-binding-id owner 'syntax-kind (cadr row))))
    ((rule)
     (append
      (map (lambda (name) (reference-binding-id owner 'rule name))
           (grammar-expression-references (cadr row)))
      (map (lambda (name) (reference-binding-id owner 'terminal name))
           (grammar-expression-terminals (cadr row)))))
    ((field)
     (list (reference-binding-id owner 'syntax-kind (cadr row))))
    (else '())))

;; : (-> Symbol Symbol List String List SourceIndex Alist)
(def (bind-row owner namespace row origin lineage source-index)
  (let* ((name (car row))
         (scope (and (eq? namespace 'field) (cadr row)))
         (identity-name (if scope (list scope name) name))
         (identity (binding-id owner namespace identity-name))
         (source (declaration-source source-index namespace identity-name origin))
         (references (row-references owner namespace row))
         (declaration
          (list owner namespace row source lineage references)))
    (list
     (cons 'bindingId identity)
     (cons 'declarationId (sha256-text (canonical declaration)))
     (cons 'namespace namespace)
     (cons 'name name)
     (cons 'scope scope)
     (cons 'owner owner)
     (cons 'originModule origin)
     (cons 'source source)
     (cons 'expansionLineage lineage)
     (cons 'references references)
     (cons 'referenceDigest (sha256-text (canonical references))))))

;;; Keep a section-local name index while preserving declaration order.  A
;;; duplicate declaration fails before any partially bound section escapes.
;; : (forall (r b) (-> Symbol Symbol [r] String List SourceIndex [b]))
;; : (-> GrammarOwner GrammarNamespace DeclarationRows OriginModule ExpansionLineage BoundDeclarations)
(def (bind-section owner namespace rows origin lineage source-index)
  (let (seen (make-table test: eq?))
    (with-list-builder (emit)
      (for-each
       (lambda (row)
         (let (name (and (pair? row) (car row)))
           (unless (symbol? name)
             (error "bound grammar declaration requires a symbolic identity"
                    namespace row))
           (when (table-ref seen name #f)
             (error "duplicate bound grammar declaration" namespace name))
           (table-set! seen name #t)
           (emit (bind-row owner namespace row origin lineage source-index))))
       rows))))

;; : (forall (a) (-> [(Pair Symbol a)] Symbol (Maybe a)))
;; : (-> Alist Symbol Datum)
(def (grammar-ir-value grammar key)
  (let (row (assq key grammar))
    (and row (cdr row))))

;;; This is the sole Bound IR publication boundary: it derives every namespace
;;; from canonical Grammar IR and validates the complete reference closure
;;; before publishing digests or counts.
;; : (forall (a b) (-> [(Pair Symbol a)] String List [Alist] [(Pair Symbol b)]))
;; : (-> Alist String List [Alist] Alist)
(def (bind-grammar-ir grammar origin lineage (source-map '()))
  (unless (and (list? grammar)
               (equal? (let (row (assq 'schema grammar))
                         (and row (cdr row)))
                       "gerbil-parser.grammar-ir.v1"))
    (error "binding requires canonical Grammar IR v1" grammar))
  (let* ((owner (grammar-ir-value grammar 'grammar))
         (syntax-kinds (grammar-ir-value grammar 'syntax-kinds))
         (terminals (grammar-ir-value grammar 'terminals))
         (lexical-rules (grammar-ir-value grammar 'lexical-rules))
         (rules (grammar-ir-value grammar 'rules))
         (source-index (index-declaration-sources source-map))
         (sections
          (list
           (cons 'syntax-kind
                 (bind-section owner 'syntax-kind syntax-kinds origin lineage source-index))
           (cons 'terminal
                 (bind-section owner 'terminal terminals origin lineage source-index))
           (cons 'lexical-rule
                 (bind-section owner 'lexical-rule lexical-rules origin lineage source-index))
           (cons 'rule
                 (bind-section owner 'rule rules origin lineage source-index))
           ;; Field identity is qualified by its node owner, so the same field
           ;; spelling in two syntax kinds is not a namespace collision.
           (cons 'field
                 (with-list-builder (emit)
                   (for-each
                    (lambda (row)
                      (for-each
                       (lambda (field)
                         (emit (bind-row owner 'field (list field (car row))
                                         origin lineage source-index)))
                       (caddr row)))
                    syntax-kinds))))))
    (let* ((binding-ids (make-table test: equal?))
           (binding-count 0)
           (references
            (with-list-builder (emit)
              (for-each
               (lambda (section)
                 (for-each
                  (lambda (binding)
                    (set! binding-count (+ binding-count 1))
                    (table-set! binding-ids (bound-grammar-ir-ref binding 'bindingId) #t)
                    (for-each emit (bound-grammar-ir-ref binding 'references)))
                  (cdr section)))
               sections))))
      ;; Admission closes the sidecar over the exact declaration set.  Missing
      ;; targets are never retained as advisory or deferred references.
      (for-each
       (lambda (reference)
         (unless (table-ref binding-ids reference #f)
           (error "unresolved bound grammar reference" reference)))
       references)
      (list
       (cons 'schema +bound-grammar-ir-schema+)
       (cons 'grammar owner)
       (cons 'originModule origin)
       (cons 'expansionLineage lineage)
       (cons 'sections sections)
       (cons 'bindingCount binding-count)
       (cons 'referenceCount (length references))
       (cons 'referenceDigest (sha256-text (canonical references)))
       (cons 'grammarDigest (sha256-text (canonical grammar)))
       (cons 'identityDigest (sha256-text (canonical sections)))))))

;; : (forall (a) (-> [(Pair Symbol a)] String))
;; : (-> BoundGrammarIR CanonicalBoundGrammarText)
(def (bound-grammar-ir-canonical ir) (canonical ir))
