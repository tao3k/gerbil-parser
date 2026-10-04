;;; -*- Gerbil -*-
;;; Single declarative grammar-to-parser generation boundary.

(import (for-syntax (only-in ../compiler/parser-ir compile-parser)
                    (only-in ../compiler/bound-ir bind-grammar-ir)
                    (only-in ../compiler/language-artifact
                             compile-language-declaration-artifacts
                             encode-compiled-language-artifact)
                    (only-in :gerbil/expander core-expand1)
                    (only-in :std/list/list delete-duplicates/hash))
        (only-in ../compiler/machine defgeneral-parser-machine install-parser-machine-backends!)
        (only-in ./descriptor make-language-grammar)
        (only-in ../runtime/language-artifact
                 load-compiled-language-artifact/embedded))
(export deflanguage
        deflanguage-grammar
        defgrammar-syntax)

;;; Defines an ordinary hygienic Gerbil macro whose expansion is consumed only
;;; by deflanguage's compile-time grammar-expression resolver.
;; defgrammar-syntax
;;   : (-> Syntax Syntax)
;;   | doc m%
;;       Defines a compile-time-only hygienic grammar expression macro.
;;
;;       # Examples
;;
;;       ```scheme
;;       (defgrammar-syntax (named token) (node Name (field value token)))
;;       ;; => syntax binding consumed by deflanguage expansion
;;       ```
;;     %
(defrules defgrammar-syntax ()
  ((_ (name argument ...) template)
   (defrules name () ((_ argument ...) template))))

;; This lower-level expander is deliberately private. Language packs have one
;; authoring surface: deflanguage-grammar.
;; Expansion materializes immutable grammar and parser IR once; runtime code
;; only receives the generated machine and never invokes the compiler.
;; assemble-language-parser
;;   : (-> Syntax Syntax)
;;   | doc m%
;;       `assemble-language-parser` expands validated grammar data into bindings.
;;
;;       # Examples
;;
;;       ```scheme
;;       (assemble-language-parser grammar ir parser grammar-data ir-data ...)
;;       ;; => immutable grammar, IR, and parser bindings
;;       ```
;;     %
(defrules assemble-language-parser
  (syntax-kinds terminals lexical-rules rules extras keywords
   parser-entrypoints recoveries flow)
  ((_ grammar-binding bound-binding ir-binding parser-binding
      grammar-encoded grammar-payload
      bound-encoded bound-payload
      ir-encoded ir-payload
      (syntax-kinds (kind-name kind-category (field-name ...)) ...)
      (lexical-rules (lexical-name lexical-expression-value) ...)
      (rules (rule-name rule-expression) ...)
      (extras extra-name ...)
      (parser-entrypoints (entry-name entry-action entry-effect) ...)
      remainder ...)
   (begin
     (def grammar-binding
       (load-compiled-language-artifact/embedded
        "gerbil-parser.grammar-ir.v1" 'grammar-encoded grammar-payload))
     (def bound-binding
       (load-compiled-language-artifact/embedded
        "gerbil-parser.bound-grammar-ir.v1" 'bound-encoded bound-payload))
     (def ir-binding
       (load-compiled-language-artifact/embedded
        "gerbil-parser.parser-ir.v1" 'ir-encoded ir-payload))
     (defgeneral-parser-machine parser-binding ir-binding
       (grammar-digest (cadr 'ir-encoded))
       (lexical-rules (lexical-name lexical-expression-value) ...)
       (rules (rule-name rule-expression) ...)
       (extras extra-name ...)
       (parser-entrypoints
       (entry-name entry-action entry-effect) ...)))))

;;; Owns validation and expansion of the complete versioned language declaration.
;;; Generated grammar, IR, and machine bindings share one expansion-time identity.
;; deflanguage-grammar
;;   : (-> Syntax Syntax)
;;   | doc m%
;;       `deflanguage-grammar` expands a language declaration into immutable bindings.
;;
;;       # Examples
;;
;;       ```scheme
;;       (deflanguage-grammar example-v1 ...)
;;       ;; => grammar, parser IR, and parser machine bindings
;;       ```
;;     %
(defsyntax (deflanguage-grammar stx)
  (def (grammar-expression-datum expression)
    (unless (and (pair? expression) (symbol? (car expression)))
      (raise-syntax-error #f "invalid GrammarExpr declaration" stx))
    (case (car expression)
      ((empty literal layout-start layout-next layout-end token reference)
       expression)
      ((seq sequence)
       (cons 'sequence (map grammar-expression-datum (cdr expression))))
      ((choice)
       (cons 'choice (map grammar-expression-datum (cdr expression))))
      ((optional repeat repeat1)
       (list (car expression)
             (grammar-expression-datum (cadr expression))))
      ((field alias)
       (list (car expression) (cadr expression)
             (grammar-expression-datum (caddr expression))))
      ((prec precedence)
       (list 'precedence (cadr expression) (caddr expression)
             (grammar-expression-datum (cadddr expression))))
      (else
       (raise-syntax-error #f "unknown GrammarExpr constructor" stx))))
  (def (compile-language-declaration
        grammar-name syntax-rows terminal-rows lexical-rows rule-rows
        extra-names keyword-rows entry-rows recovery-rows flow-rows
        conflict-policy case-insensitive? origin lineage explicit-source-map)
     (def (source-location value)
       (let (source (stx-source value))
         (list
          (cons 'path origin)
          (cons 'location
                (if source
                  (call-with-output-string
                   (lambda (port) (display source port)))
                  origin))
          (cons 'generated? #f))))
     (def (section-sources namespace rows)
       (cons
        namespace
        (stx-map
         (lambda (row)
           (syntax-case row ()
             ((name . _)
              (cons (syntax->datum #'name) (source-location #'name)))
             (_ (raise-syntax-error #f "invalid bound declaration row" row))))
         rows)))
     (def (field-sources rows)
       (cons
        'field
        (apply append
               (stx-map
                (lambda (row)
                  (syntax-case row ()
                    ((kind _ (field ...))
                     (map
                      (lambda (field)
                        (cons
                         (list (syntax->datum #'kind)
                               (syntax->datum field))
                         (source-location field)))
                      (stx-map (lambda (value) value) #'(field ...))))
                    (_ (raise-syntax-error #f
                           "invalid bound syntax-kind row" row))))
                rows))))
     (let* ((source-map
             (append
              explicit-source-map
              (list
               (section-sources 'syntax-kind syntax-rows)
               (section-sources 'terminal terminal-rows)
               (section-sources 'lexical-rule lexical-rows)
               (section-sources 'rule rule-rows)
               (field-sources syntax-rows))))
            (grammar
             (list
              (cons 'schema "gerbil-parser.grammar-ir.v1")
              (cons 'grammar (syntax->datum grammar-name))
              (cons 'syntax-kinds (syntax->datum syntax-rows))
              (cons 'terminals (syntax->datum terminal-rows))
              (cons 'lexical-rules (syntax->datum lexical-rows))
              (cons 'rules
                    (map (lambda (row)
                           (list (car row)
                                 (grammar-expression-datum (cadr row))))
                         (syntax->datum rule-rows)))
              (cons 'extras
                    (map list (syntax->datum extra-names)))
              (cons 'keywords (syntax->datum keyword-rows))
              (cons 'parser-entrypoints (syntax->datum entry-rows))
              (cons 'recoveries (syntax->datum recovery-rows))
              (cons 'conflict-policy conflict-policy)
              (cons 'case-insensitive? case-insensitive?)
              (cons 'flow (syntax->datum flow-rows))))
            (declaration-identity
             (list origin lineage source-map)))
       (let-values (((grammar-locator bound-locator parser-locator _status)
                     (compile-language-declaration-artifacts
                      declaration-identity grammar
                      (lambda ()
                        (bind-grammar-ir grammar origin lineage source-map))
                      (lambda ()
                        (compile-parser grammar)))))
         (values grammar-locator
                 (encode-compiled-language-artifact grammar-locator)
                 bound-locator
                 (encode-compiled-language-artifact bound-locator)
                 parser-locator
                 (encode-compiled-language-artifact parser-locator)))))

;; Identity, grammar, canonical IR, and generated parser machine are owned by
;; one declaration. parser.ss consumes the descriptor and adds no authority.
  (syntax-case stx
      (identity syntax-kinds terminals lexical-rules rules extras keywords
                parser-entrypoints recoveries conflicts case-insensitive flow
                lineage source-ownership)
    ((_ prefix
        (identity language-value version-value contract-value)
        (syntax-kinds syntax-row ...)
        (terminals terminal-row ...)
        (lexical-rules lexical-row ...)
        (rules rule-row ...)
        (extras extra-name ...)
        (keywords keyword-row ...)
        (parser-entrypoints entry-row ...)
        (recoveries recovery-row ...)
        (conflicts conflict-policy-value)
        (case-insensitive case-insensitive-value)
        (lineage lineage-item ...)
        (flow flow-row ...))
     #'(deflanguage-grammar prefix
         (identity language-value version-value contract-value)
         (syntax-kinds syntax-row ...)
         (terminals terminal-row ...)
         (lexical-rules lexical-row ...)
         (rules rule-row ...)
         (extras extra-name ...)
         (keywords keyword-row ...)
         (parser-entrypoints entry-row ...)
         (recoveries recovery-row ...)
         (conflicts conflict-policy-value)
         (case-insensitive case-insensitive-value)
         (source-ownership ())
         (lineage lineage-item ...)
         (flow flow-row ...)))
    ((_ prefix
        (identity language-value version-value contract-value)
        (syntax-kinds syntax-row ...)
        (terminals terminal-row ...)
        (lexical-rules lexical-row ...)
        (rules rule-row ...)
        (extras extra-name ...)
        (keywords keyword-row ...)
        (parser-entrypoints entry-row ...)
        (recoveries recovery-row ...)
        (lineage lineage-item ...)
        (flow flow-row ...))
     #'(deflanguage-grammar prefix
         (identity language-value version-value contract-value)
         (syntax-kinds syntax-row ...)
         (terminals terminal-row ...)
         (lexical-rules lexical-row ...)
         (rules rule-row ...)
         (extras extra-name ...)
         (keywords keyword-row ...)
         (parser-entrypoints entry-row ...)
         (recoveries recovery-row ...)
         (source-ownership ())
         (lineage lineage-item ...)
         (flow flow-row ...)))
    ((_ prefix
        (identity language-value version-value contract-value)
        (syntax-kinds syntax-row ...)
        (terminals terminal-row ...)
        (lexical-rules lexical-row ...)
        (rules rule-row ...)
        (extras extra-name ...)
        (keywords keyword-row ...)
        (parser-entrypoints entry-row ...)
        (recoveries recovery-row ...)
        (conflicts conflict-policy-value)
        (case-insensitive case-insensitive-value)
        (source-ownership source-map-value)
        (lineage lineage-item ...)
        (flow flow-row ...))
     (identifier? #'prefix)
     (let* ((stem (symbol->string (syntax->datum #'prefix)))
            (origin
             (let (source (stx-source stx))
               (if source
                 (call-with-output-string
                  (lambda (port) (display source port)))
                 "<unknown>")))
            (binding
             (lambda (suffix)
               (datum->syntax
                #'prefix
                (string->symbol (string-append stem suffix))))))
       (with-syntax ((grammar-binding (binding "-grammar"))
                     (bound-binding (binding "-bound-grammar-ir"))
                     (ir-binding (binding "-parser-ir"))
                     (machine-binding (binding "-parser"))
                     (language-binding (binding "-language-grammar")))
         (let-values (((grammar-encoded grammar-payload
                        bound-encoded bound-payload
                        ir-encoded ir-payload)
                       (compile-language-declaration
                        #'grammar-binding
                        #'(syntax-row ...)
                        #'(terminal-row ...)
                        #'(lexical-row ...)
                        #'(rule-row ...)
                        #'(extra-name ...)
                        #'(keyword-row ...)
                        #'(entry-row ...)
                        #'(recovery-row ...)
                        #'(flow-row ...)
                        (syntax->datum #'conflict-policy-value)
                        (syntax->datum #'case-insensitive-value)
                        origin (syntax->datum #'(lineage-item ...))
                        (syntax->datum #'source-map-value))))
           (with-syntax
               ((grammar-encoded grammar-encoded)
                (grammar-payload grammar-payload)
                (bound-encoded bound-encoded)
                (bound-payload bound-payload)
                (ir-encoded ir-encoded)
                (ir-payload ir-payload))
             #'(begin
                 (assemble-language-parser
                  grammar-binding bound-binding ir-binding machine-binding
                  grammar-encoded grammar-payload
                  bound-encoded bound-payload
                  ir-encoded ir-payload
                  (syntax-kinds syntax-row ...)
                  (lexical-rules lexical-row ...)
                  (rules rule-row ...)
                  (extras extra-name ...)
                  (parser-entrypoints entry-row ...))
                 (def language-binding
                   (make-language-grammar
                    "gerbil-parser.language-grammar.v1"
                    language-value version-value contract-value
                    grammar-binding ir-binding machine-binding #f))))))))
    ((_ prefix
        (identity language-value version-value contract-value)
        (syntax-kinds syntax-row ...)
        (terminals terminal-row ...)
        (lexical-rules lexical-row ...)
        (rules rule-row ...)
        (extras extra-name ...)
        (keywords keyword-row ...)
        (parser-entrypoints entry-row ...)
        (recoveries recovery-row ...)
        (source-ownership source-map-value)
        (lineage lineage-item ...)
        (flow flow-row ...))
     (identifier? #'prefix)
     (let* ((stem (symbol->string (syntax->datum #'prefix)))
            (origin
             (let (source (stx-source stx))
               (if source
                 (call-with-output-string
                  (lambda (port) (display source port)))
                 "<unknown>")))
            (binding
             (lambda (suffix)
               (datum->syntax
                #'prefix
                (string->symbol (string-append stem suffix))))))
       (with-syntax ((grammar-binding (binding "-grammar"))
                     (bound-binding (binding "-bound-grammar-ir"))
                     (ir-binding (binding "-parser-ir"))
                     (machine-binding (binding "-parser"))
                     (language-binding (binding "-language-grammar")))
         (let-values (((grammar-encoded grammar-payload
                        bound-encoded bound-payload
                        ir-encoded ir-payload)
                       (compile-language-declaration
                        #'grammar-binding
                        #'(syntax-row ...)
                        #'(terminal-row ...)
                        #'(lexical-row ...)
                        #'(rule-row ...)
                        #'(extra-name ...)
                        #'(keyword-row ...)
                        #'(entry-row ...)
                        #'(recovery-row ...)
                        #'(flow-row ...)
                        'reject #f origin
                        (syntax->datum #'(lineage-item ...))
                        (syntax->datum #'source-map-value))))
           (with-syntax
               ((grammar-encoded grammar-encoded)
                (grammar-payload grammar-payload)
                (bound-encoded bound-encoded)
                (bound-payload bound-payload)
                (ir-encoded ir-encoded)
                (ir-payload ir-payload))
             #'(begin
                 (assemble-language-parser
                  grammar-binding bound-binding ir-binding machine-binding
                  grammar-encoded grammar-payload
                  bound-encoded bound-payload
                  ir-encoded ir-payload
                  (syntax-kinds syntax-row ...)
                  (lexical-rules lexical-row ...)
                  (rules rule-row ...)
                  (extras extra-name ...)
                  (parser-entrypoints entry-row ...))
                 (def language-binding
                   (make-language-grammar
                    "gerbil-parser.language-grammar.v1"
                    language-value version-value contract-value
                    grammar-binding ir-binding machine-binding #f))))))))
    ((_ prefix
        (identity language-value version-value contract-value)
        (syntax-kinds syntax-row ...)
        (terminals terminal-row ...)
        (lexical-rules lexical-row ...)
        (rules rule-row ...)
        (extras extra-name ...)
        (keywords keyword-row ...)
        (parser-entrypoints entry-row ...)
        (recoveries recovery-row ...)
        (conflicts conflict-policy-value)
        (case-insensitive case-insensitive-value)
        (flow flow-row ...))
     #'(deflanguage-grammar prefix
         (identity language-value version-value contract-value)
         (syntax-kinds syntax-row ...)
         (terminals terminal-row ...)
         (lexical-rules lexical-row ...)
         (rules rule-row ...)
         (extras extra-name ...)
         (keywords keyword-row ...)
         (parser-entrypoints entry-row ...)
         (recoveries recovery-row ...)
         (conflicts conflict-policy-value)
         (case-insensitive case-insensitive-value)
         (source-ownership ())
         (lineage deflanguage-grammar)
         (flow flow-row ...)))
    ((_ prefix
        (identity language-value version-value contract-value)
        (syntax-kinds syntax-row ...)
        (terminals terminal-row ...)
        (lexical-rules lexical-row ...)
        (rules rule-row ...)
        (extras extra-name ...)
        (keywords keyword-row ...)
        (parser-entrypoints entry-row ...)
        (recoveries recovery-row ...)
        (flow flow-row ...))
     #'(deflanguage-grammar prefix
         (identity language-value version-value contract-value)
         (syntax-kinds syntax-row ...)
         (terminals terminal-row ...)
         (lexical-rules lexical-row ...)
         (rules rule-row ...)
         (extras extra-name ...)
         (keywords keyword-row ...)
         (parser-entrypoints entry-row ...)
         (recoveries recovery-row ...)
         (source-ownership ())
         (lineage deflanguage-grammar)
         (flow flow-row ...)))
    (_ (raise-syntax-error #f "invalid language grammar declaration" stx))))

;;; Concise v1 authoring projection.  It infers terminal rows, syntax-kind
;;; rows, rule/token references, the single parser entry, and the connected
;;; parser flow before delegating to deflanguage-grammar.  The latter remains
;;; the sole Grammar IR and parser compilation authority.
;; deflanguage
;;   : (-> Syntax Syntax)
;;   | doc m%
;;       `deflanguage` projects one hygienic language declaration into the
;;       stable `deflanguage-grammar` v1 contract.
;;
;;       # Examples
;;
;;       ```scheme
;;       (deflanguage arithmetic
;;         (identity "arithmetic" "v1" "arithmetic-expression.v1")
;;         (root source-file)
;;         (lex (number Number (number)))
;;         (rules (source-file (node SourceFile (field value number))))
;;         (extras) (keywords) (recoveries)
;;         (conflicts reject) (case-insensitive #f))
;;       ;; => arithmetic grammar, Parser IR, machine, and descriptor bindings
;;       ```
;;       Result: every generated binding retains the stable v1 schemas.
;;     %
(defsyntax (deflanguage stx)
  (def (syntax-failure message)
    (raise-syntax-error #f message stx))
  (def (require condition message)
    (unless condition (syntax-failure message)))
  (def (unique values)
    (delete-duplicates/hash values))
  (def macro-lineage '())
  (def grammar-constructors
    '(node alias seq sequence choice optional repeat repeat1 field
      prec precedence empty literal layout-start layout-next layout-end
      token reference))
  (def (expand-surface-syntax expression)
    (syntax-case expression ()
      ((head argument ...)
       (let (head-value (syntax->datum #'head))
         (if (memq head-value grammar-constructors)
           (cons head-value
                 (stx-map expand-surface-syntax #'(argument ...)))
           (let (expanded (core-expand1 expression))
             (when (eq? expanded expression)
               (syntax-failure "unknown concise GrammarExpr constructor"))
             (set! macro-lineage
                   (cons head-value macro-lineage))
             (expand-surface-syntax expanded)))))
      (atom (syntax->datum #'atom))))
  (syntax-case stx
      (identity root lex rules extras keywords recoveries
                conflicts case-insensitive)
    ((_ prefix
        (identity language-value version-value contract-value)
        (root root-name)
        (lex lexical-row ...)
        (rules rule-row ...)
        (extras extra-name ...)
        (keywords keyword-row ...)
        (recoveries recovery-row ...)
        (conflicts conflict-policy)
        (case-insensitive case-insensitive-value)
        option ...)
     (identifier? #'prefix)
     (let* ((options (syntax->datum #'(option ...)))
            (prefix-name (syntax->datum #'prefix))
            (root-value (syntax->datum #'root-name))
            (lexical-rows (syntax->datum #'(lexical-row ...)))
            (rule-rows
             (stx-map
              (lambda (row)
                (syntax-case row ()
                  ((name expression ...)
                   (cons (syntax->datum #'name)
                         (stx-map expand-surface-syntax
                                  #'(expression ...))))
                  (_ (syntax-failure
                      "invalid concise rule declaration"))))
              #'(rule-row ...)))
            (token-names
             (map (lambda (row)
                    (require (and (list? row)
                                  (= (length row) 3)
                                  (symbol? (car row))
                                  (symbol? (cadr row)))
                             "invalid concise lexical declaration")
                    (car row))
                  lexical-rows))
            (token-kinds (unique (map cadr lexical-rows)))
            (rule-names
             (map (lambda (row)
                    (require (and (list? row)
                                  (pair? (cdr row))
                                  (symbol? (car row)))
                             "invalid concise rule declaration")
                    (car row))
                  rule-rows))
            (node-rows '()))
       (require (memq root-value rule-names)
                "concise language root must name a declared rule")
       (require (every (lambda (row)
                         (and (pair? row) (memq (car row) '(node-fields flow backends))))
                       options)
                "unknown concise language option")
       (require (= (length options) (length (unique (map car options))))
                "duplicate concise language option")
       (def (record-node! kind fields)
         (require (symbol? kind)
                  "concise node kind must be an identifier")
         (let (current (assq kind node-rows))
           (if current
             ;; Alternatives of the same public node contribute to one schema.
             (set-cdr! current (unique (append (cdr current) fields)))
             (set! node-rows
                   (append node-rows (list (cons kind fields)))))))
       ;; Transparent helper rules contribute fields to their nearest node.
       ;; Named nodes stop propagation; visited rule names bound recursive
       ;; helpers without hiding fields on the other alternatives.
       (def (surface-fields expression (seen '()))
         (def (rule-fields name)
           (if (memq name seen) '()
             (let (row (assq name rule-rows))
               (if row
                 (apply append
                        (map (lambda (body)
                               (surface-fields body (cons name seen)))
                             (cdr row)))
                 '()))))
         (cond
          ((symbol? expression)
           (if (and (memq expression rule-names)
                    (not (memq expression token-names)))
             (rule-fields expression) '()))
          ((not (pair? expression)) '())
          ((memq (car expression) '(node alias)) '())
          ((eq? (car expression) 'reference)
           (rule-fields (cadr expression)))
          ((eq? (car expression) 'field)
           (require (= (length expression) 3)
                    "invalid concise field expression")
           (cons (cadr expression)
                 (surface-fields (caddr expression) seen)))
          ((memq (car expression) '(prec precedence))
           (surface-fields (cadddr expression) seen))
          ((memq (car expression) '(optional repeat repeat1))
           (surface-fields (cadr expression) seen))
          ((memq (car expression) '(seq sequence choice))
           (apply append (map (lambda (child) (surface-fields child seen))
                              (cdr expression))))
          (else '())))
       (def (surface-body expressions)
         (require (pair? expressions)
                  "concise rule or node requires an expression")
         (if (null? (cdr expressions))
           (surface-expression (car expressions))
           (cons 'seq (map surface-expression expressions))))
       (def (surface-expression expression)
         (cond
          ((string? expression) (list 'literal expression))
          ((symbol? expression)
           (cond
            ((and (memq expression token-names) (memq expression rule-names))
             (syntax-failure
              "ambiguous concise identifier; use token or reference"))
            ((memq expression token-names) (list 'token expression))
            ((memq expression rule-names) (list 'reference expression))
            (else
             (syntax-failure
              "unresolved concise grammar identifier"))))
          ((not (pair? expression))
           (syntax-failure "invalid concise grammar expression"))
          (else
           (case (car expression)
             ((node alias)
              (require (and (pair? (cdr expression))
                            (symbol? (cadr expression))
                            (pair? (cddr expression)))
                       "invalid concise node expression")
              (let* ((kind (cadr expression))
                     (body (surface-body (cddr expression)))
                     (fields (unique
                              (apply append
                                     (map surface-fields
                                          (cddr expression))))))
                (record-node! kind fields)
                (list 'alias kind body)))
             ((seq sequence choice)
              (require (pair? (cdr expression))
                       "concise sequence or choice cannot be empty")
              (cons (if (eq? (car expression) 'sequence)
                      'seq
                      (car expression))
                    (map surface-expression (cdr expression))))
             ((optional repeat repeat1)
              (require (= (length expression) 2)
                       "invalid concise unary grammar expression")
              (list (car expression)
                    (surface-expression (cadr expression))))
             ((field)
              (require (and (= (length expression) 3)
                            (symbol? (cadr expression)))
                       "invalid concise field expression")
              (list 'field (cadr expression)
                    (surface-expression (caddr expression))))
             ((prec precedence)
              (require (= (length expression) 4)
                       "invalid concise precedence expression")
              (list 'prec (cadr expression) (caddr expression)
                    (surface-expression (cadddr expression))))
             ((empty)
              (require (null? (cdr expression))
                       "invalid concise empty expression")
              expression)
             ((layout-end)
              (require (every (lambda (boundary)
                                (and (string? boundary)
                                     (positive? (string-length boundary))))
                              (cdr expression))
                       "invalid concise layout closing boundary")
              expression)
             ((literal layout-start layout-next)
              (require (and (= (length expression) 2)
                            (string? (cadr expression)))
                       "invalid concise literal expression")
              expression)
             ((token)
              (require (and (= (length expression) 2)
                            (memq (cadr expression) token-names))
                       "explicit token names an unknown lexical declaration")
              expression)
             ((reference)
              (require (and (= (length expression) 2)
                            (memq (cadr expression) rule-names))
                       "explicit reference names an unknown rule")
              expression)
             (else
              (syntax-failure
               "unknown concise GrammarExpr constructor"))))))
       (let* ((normalized-rules
               (map (lambda (row)
                      (list (car row) (surface-body (cdr row))))
                    rule-rows))
              (root-rule (assq root-value normalized-rules))
              (root-expression (and root-rule (cadr root-rule)))
              (root-kind
               (and (pair? root-expression)
                    (eq? (car root-expression) 'alias)
                    (cadr root-expression))))
         (require root-kind
                  "concise language root rule must construct one node")
         ;; Optional fields retain a published ABI even when an alternative
         ;; does not emit them. Ordinary packs infer their complete catalog.
         (let (declared (assq 'node-fields options))
           (when declared
             (for-each
              (lambda (row)
                (require (and (pair? row) (assq (car row) node-rows)
                              (list? (cdr row)) (every symbol? (cdr row)))
                         "node-fields requires a declared node and field identifiers")
                (let (node (assq (car row) node-rows))
                  (set-cdr! node (unique (append (cdr row) (cdr node))))))
              (cdr declared))))
         (for-each
          (lambda (kind)
            (when (assq kind node-rows)
              (syntax-failure
               "concise syntax kind is both a node and a token")))
          token-kinds)
         (let* ((root-node (assq root-kind node-rows))
                (ordered-nodes
                 (cons root-node
                       (filter (lambda (row)
                                 (not (eq? (car row) root-kind)))
                               node-rows)))
                (syntax-kinds
                 (append
                  (map (lambda (row)
                         (list (car row) 'node (cdr row)))
                       ordered-nodes)
                  (map (lambda (kind)
                         (list kind 'token '(text)))
                       token-kinds)))
                (terminals
                 (map (lambda (row) (list (car row) (cadr row)))
                      lexical-rows))
                (lexical-rules
                 (map (lambda (row) (list (car row) (caddr row)))
                      lexical-rows))
                (arguments
                 `(,prefix-name
                   (identity ,(syntax->datum #'language-value)
                             ,(syntax->datum #'version-value)
                             ,(syntax->datum #'contract-value))
                   (syntax-kinds ,@syntax-kinds)
                   (terminals ,@terminals)
                   (lexical-rules ,@lexical-rules)
                   (rules ,@normalized-rules)
                   (extras ,@(syntax->datum #'(extra-name ...)))
                   (keywords ,@(syntax->datum #'(keyword-row ...)))
                   (parser-entrypoints (,root-value parse pure))
                   (recoveries ,@(syntax->datum #'(recovery-row ...)))
                   (conflicts ,(syntax->datum #'conflict-policy))
                   (case-insensitive
                    ,(syntax->datum #'case-insensitive-value))
                   (lineage deflanguage
                            ,@(reverse (unique macro-lineage)))
                   ,(or (assq 'flow options)
                        '(flow (source lexical) (lexical parser) (parser cst))))))
           (let (backends (cdr (or (assq 'backends options) '(backends))))
             (require (every (lambda (row)
                               (and (list? row) (= (length row) 3)
                                    (memq (car row) '(drive source step event-step))))
                             backends)
                      "backends requires (kind digest generated-procedure) rows")
             (require (= (length backends) (length (unique (map car backends))))
                      "duplicate parser backend kind")
             (with-syntax (((argument ...) (datum->syntax #'prefix arguments))
                           (machine-binding
                            (datum->syntax #'prefix
                             (string->symbol (string-append
                                              (symbol->string prefix-name) "-parser"))))
                           ((backend ...)
                            (datum->syntax #'prefix
                             (map (lambda (row)
                                    `(list ',(car row) ,(cadr row) ,(caddr row)))
                                  backends))))
               #'(begin
                   (deflanguage-grammar argument ...)
                   (install-parser-machine-backends!
                    machine-binding (list backend ...)))))))))
    ((_ prefix identity-row root-row lex-row rules-row clause ...)
     (and (identifier? #'prefix)
          (eq? (car (syntax->datum #'identity-row)) 'identity)
          (eq? (car (syntax->datum #'root-row)) 'root)
          (eq? (car (syntax->datum #'lex-row)) 'lex)
          (eq? (car (syntax->datum #'rules-row)) 'rules))
     (let* ((rows (stx-map (lambda (row) row) #'(clause ...)))
            (names (map (lambda (row)
                          (let (datum (syntax->datum row))
                            (and (pair? datum) (car datum)))) rows)))
       (require (every (lambda (name)
                         (memq name '(extras keywords recoveries conflicts
                                      case-insensitive node-fields flow backends))) names)
                "unknown concise language option")
       (require (= (length names) (length (unique names)))
                "duplicate concise language option")
       (let ((sections (map cons names rows)))
         (def (section name default)
           (let (row (assq name sections))
             (if row (cdr row) (datum->syntax #'prefix default))))
         (with-syntax ((extras-row (section 'extras '(extras)))
                       (keywords-row (section 'keywords '(keywords)))
                       (recoveries-row (section 'recoveries '(recoveries)))
                       (conflicts-row (section 'conflicts '(conflicts reject)))
                       (case-row (section 'case-insensitive '(case-insensitive #f)))
                       ((option ...)
                        (filter (lambda (row)
                                  (memq (car (syntax->datum row)) '(node-fields flow backends)))
                                rows)))
           #'(deflanguage prefix identity-row root-row lex-row rules-row
               extras-row keywords-row recoveries-row conflicts-row case-row
               option ...)))))
    (_ (raise-syntax-error #f "invalid concise language declaration" stx))))
