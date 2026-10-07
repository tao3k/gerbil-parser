;;; -*- Gerbil -*-
;;; Concise language elaboration: collect authored syntax, validate rules, derive
;;; catalogs, and submit a declaration to the existing canonical compiler.
;;; Runtime identifiers are supplied by the public macro's hygienic context.

(import (only-in :gerbil/core/expander
                 syntax-case syntax with-syntax datum->syntax syntax->datum
                 stx-map stx-list? syntax->list identifier? raise-syntax-error)
        (only-in :gerbil/expander core-expand1)
        (only-in :std/list/list delete-duplicates/hash)
        (only-in ../grammar/algebra grammar-expression-header?)
        (only-in ./language-artifact project-language-catalog
                 make-language-declaration expand-language-declaration-syntax))
(export expand-lexical-language-syntax lower-source-rule-overlays)

(def (expand-concise-language-syntax stx expand-lexical-rows compile-parser bind-grammar-ir
                                      assembly-binding descriptor-binding
                                      author-entry begin-entry define-entry (author-syntax stx))
  (def (syntax-failure message (offending stx))
    (raise-syntax-error #f message offending author-syntax))
  (def (require condition message)
    (unless condition (syntax-failure message)))
  (def (unique values)
    (delete-duplicates/hash values))
  ;; These associations live only during this declaration's expansion. Pair
  ;; identity distinguishes equal expressions at different author occurrences.
  (def expression-sources (make-table test: eq?))
  (def expression-operands (make-table test: eq?))
  (def rule-sources (make-table test: eq?))
  (def rule-declarations (make-table test: eq?))
  (def node-kind-sources (make-table test: eq?))
  (def macro-lineage '())
  (def grammar-constructors
    '(node seq choice optional repeat repeat1 field
      prec empty literal layout-start layout-next layout-end
      token reference))
  (def (expand-surface-syntax expression (author-call #f))
    (syntax-case expression ()
      ((head argument ...)
       (let (head-value (syntax->datum #'head))
         (if (memq head-value grammar-constructors)
           (let (datum
                 (cons head-value
                       (stx-map (lambda (child)
                                  (expand-surface-syntax child author-call))
                                #'(argument ...))))
             (table-set! expression-sources datum (or author-call expression))
             (table-set! expression-operands datum
                         (stx-map (lambda (child) (or author-call child))
                                  #'(head argument ...)))
             datum)
           (let (expanded (core-expand1 expression))
             (when (eq? expanded expression)
               (raise-syntax-error #f "unknown concise GrammarExpr constructor"
                                   (or author-call #'head) stx))
             (set! macro-lineage
                   (cons head-value macro-lineage))
             ;; Arbitrary transformers do not promise a parameter-origin map.
             ;; Keep the outer author invocation as conservative blame context.
             (expand-surface-syntax expanded (or author-call expression))))))
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
            (lexical-rows (expand-lexical-rows #'(lexical-row ...) stx))
            (rule-rows
             (stx-map
              (lambda (row)
                (syntax-case row ()
                  ((name expression ...)
                   (let (result
                         (cons (syntax->datum #'name)
                               (stx-map expand-surface-syntax
                                        #'(expression ...))))
                     (table-set! rule-declarations result row)
                     (table-set! rule-sources result
                                 (stx-map (lambda (value) value) #'(expression ...)))
                     result))
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
                         (and (pair? row) (memq (car row) '(node-fields catalog))))
                       options)
                "unknown concise language option")
       (require (= (length options) (length (unique (map car options))))
                "duplicate concise language option")
       (def (published-catalog inferred-kinds inferred-terminals)
         (with-catch (lambda (condition) (syntax-failure (error-message condition)))
           (lambda ()
             (project-language-catalog inferred-kinds inferred-terminals
                                       (assq 'catalog options)))))
       (def (record-node! kind fields)
         (require (symbol? kind)
                  "concise node kind must be an identifier")
         (let (current (assq kind node-rows))
           (if current
             ;; Alternatives of the same public node contribute to one schema.
             (set-cdr! current (unique (append (cdr current) fields)))
             (set! node-rows
                   (append node-rows (list (cons kind fields)))))))
       ;; Field inference consumes only fully validated, normalized rules.
       ;; Transparent references retain recursive field propagation; named
       ;; nodes stop it, and visited names bound traversal of recursive helpers.
       (def (surface-fields expression rules (seen '()))
         (def (rule-fields name)
           (if (memq name seen) '()
             (let (row (assq name rules))
               (if row
                 (surface-fields (cadr row) rules (cons name seen))
                 '()))))
         (case (car expression)
           ((alias) '())
           ((reference) (rule-fields (cadr expression)))
           ((field)
            (cons (cadr expression)
                  (surface-fields (caddr expression) rules seen)))
           ((prec)
            (surface-fields (cadddr expression) rules seen))
           ((optional repeat repeat1)
            (surface-fields (cadr expression) rules seen))
           ((seq choice)
            (apply append
                   (map (lambda (child) (surface-fields child rules seen))
                        (cdr expression))))
           (else '())))
       (def (collect-node-fields! expression rules)
         (case (car expression)
           ((alias)
            ;; Match the prior normalizer's postorder node registration.
            (collect-node-fields! (caddr expression) rules)
            (record-node! (cadr expression)
                          (unique (surface-fields (caddr expression) rules))))
           ((field) (collect-node-fields! (caddr expression) rules))
           ((prec) (collect-node-fields! (cadddr expression) rules))
           ((optional repeat repeat1)
            (collect-node-fields! (cadr expression) rules))
           ((seq choice)
            (for-each (lambda (child) (collect-node-fields! child rules))
                      (cdr expression)))
           (else (void))))
       (def (surface-body expressions sources)
         (require (pair? expressions)
                  "concise rule or node requires an expression")
         (if (null? (cdr expressions))
           (surface-expression (car expressions) (car sources))
           (cons 'seq (map surface-expression expressions sources))))
       (def (surface-expression expression source)
         (def blame (table-ref expression-sources expression source))
         (def operands (table-ref expression-operands expression #f))
         (def (operand-source index)
           (if operands (list-ref operands index) blame))
         (def (syntax-failure message (offending blame))
           (raise-syntax-error #f message offending stx))
         (def (require condition message (offending blame))
           (unless condition (syntax-failure message offending)))
         (cond
          ((string? expression)
           (require (grammar-expression-header? (list 'literal expression))
                    "invalid canonical GrammarExpr metadata")
           (list 'literal expression))
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
          ((not (and (pair? expression) (stx-list? expression)))
           (syntax-failure "invalid concise grammar expression"))
          (else
           (case (car expression)
             ((node alias)
              (require (and (pair? (cdr expression))
                            (symbol? (cadr expression))
                            (pair? (cddr expression)))
                       "invalid concise node expression")
              (let ((kind (cadr expression))
                    (body (surface-body (cddr expression) (cddr operands))))
                ;; Keep the first authored occurrence for cross-rule kind checks.
                (unless (table-ref node-kind-sources kind #f)
                  (table-set! node-kind-sources kind (operand-source 1)))
                (list 'alias kind body)))
             ((seq sequence choice)
              (require (pair? (cdr expression))
                       "concise sequence or choice cannot be empty")
              (cons (if (eq? (car expression) 'sequence)
                      'seq
                      (car expression))
                    (map surface-expression (cdr expression) (cdr operands))))
             ((optional repeat repeat1)
              (require (= (length expression) 2)
                       "invalid concise unary grammar expression")
              (list (car expression)
                    (surface-expression (cadr expression) (operand-source 1))))
             ((field)
              (require (and (= (length expression) 3)
                            (symbol? (cadr expression)))
                       "invalid concise field expression")
              (list 'field (cadr expression)
                    (surface-expression (caddr expression) (operand-source 2))))
             ((prec precedence)
              (require (= (length expression) 4)
                       "invalid concise precedence expression")
              (require (grammar-expression-header?
                        (cons 'precedence (cdr expression)))
                       "invalid canonical GrammarExpr metadata")
              (list 'prec (cadr expression) (caddr expression)
                    (surface-expression (cadddr expression) (operand-source 3))))
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
              (require (grammar-expression-header? expression)
                       "invalid canonical GrammarExpr metadata")
              expression)
             ((token)
              (require (= (length expression) 2)
                       "explicit token names an unknown lexical declaration")
              (require (memq (cadr expression) token-names)
                       "explicit token names an unknown lexical declaration" (operand-source 1))
              expression)
             ((reference)
              (require (= (length expression) 2)
                       "explicit reference names an unknown rule")
              (require (memq (cadr expression) rule-names)
                       "explicit reference names an unknown rule" (operand-source 1))
              expression)
             (else
              (syntax-failure
               "unknown concise GrammarExpr constructor"))))))
       (let* ((normalized-rules
               (map (lambda (row)
                      (list (car row) (surface-body (cdr row) (table-ref rule-sources row))))
                    rule-rows))
              (root-rule (assq root-value normalized-rules))
              (root-expression (and root-rule (cadr root-rule)))
              (root-kind
               (and (pair? root-expression)
                    (eq? (car root-expression) 'alias)
                    (cadr root-expression))))
         (unless root-kind
           (let* ((row (assq root-value rule-rows))
                  (sources (table-ref rule-sources row))
                  (source (if (null? (cdr sources))
                            (table-ref expression-sources (cadr row) (car sources))
                            (table-ref rule-declarations row))))
             (syntax-failure
              "concise language root rule must construct one node" source)))
         (for-each
          (lambda (row) (collect-node-fields! (cadr row) normalized-rules))
          normalized-rules)
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
               "concise syntax kind is both a node and a token"
               (table-ref node-kind-sources kind))))
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
                (catalog-values
                 (call-with-values
                  (lambda () (published-catalog syntax-kinds terminals)) list))
                (syntax-kinds (car catalog-values))
                (terminals (cadr catalog-values))
                (declaration
                 (make-language-declaration
                  author-syntax #'prefix
                  (stx-map (lambda (value) value)
                           #'(language-value version-value contract-value))
                  (map (lambda (row)
                         (cons (car row) (datum->syntax #'prefix (cdr row))))
                       `((syntax-kinds ,@syntax-kinds)
                         (terminals ,@terminals)
                         (lexical-rules ,@lexical-rules)
                         (rules ,@normalized-rules)
                         (extras ,@(syntax->datum #'(extra-name ...)))
                         (keywords ,@(syntax->datum #'(keyword-row ...)))
                         (parser-entrypoints (,root-value parse pure))
                         (recoveries ,@(syntax->datum #'(recovery-row ...)))
                         (flow (source lexical) (lexical parser) (parser cst))))
                  (syntax->datum #'conflict-policy)
                  (syntax->datum #'case-insensitive-value)
                  `(deflanguage ,@(reverse (unique macro-lineage))) '())))
           (expand-language-declaration-syntax
            declaration compile-parser bind-grammar-ir
            assembly-binding descriptor-binding begin-entry define-entry)))))
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
                                      case-insensitive node-fields catalog))) names)
                "unknown concise language option")
       (require (= (length names) (length (unique names)))
                "duplicate concise language option")
       (let ((sections (map cons names rows)))
         (def (section name default)
           (let (row (assq name sections))
             (if row (cdr row) (datum->syntax #'prefix default))))
         (with-syntax ((@author-entry author-entry)
                       (extras-row (section 'extras '(extras)))
                       (keywords-row (section 'keywords '(keywords)))
                       (recoveries-row (section 'recoveries '(recoveries)))
                       (conflicts-row (section 'conflicts '(conflicts reject)))
                       (case-row (section 'case-insensitive '(case-insensitive #f)))
                       ((option ...)
                        (filter (lambda (row)
                                  (memq (car (syntax->datum row)) '(node-fields catalog)))
                                rows)))
           (expand-concise-language-syntax
            #'(@author-entry prefix identity-row root-row lex-row rules-row
                extras-row keywords-row recoveries-row conflicts-row case-row option ...)
            expand-lexical-rows compile-parser bind-grammar-ir
            assembly-binding descriptor-binding author-entry
            begin-entry define-entry author-syntax)))))
    (_ (raise-syntax-error #f "invalid concise language declaration" stx))))

;;; The public lexical vocabulary owns syntax policy; rule elaboration and
;;; publication remain in the common compiler. No release identity or VM flow
;;; is accepted from the author declaration.
(def (expand-lexical-language-syntax stx expand-lexical-rows compile-parser bind-grammar-ir
                                     assembly-binding descriptor-binding
                                     author-entry begin-entry define-entry)
  (syntax-case stx ()
    ((_ prefix (syntax-tag (vocabulary (root-tag root-name) (lex-tag lexical-row ...) clause ...))
        (rules-tag rule-row ...))
     (equal? (map syntax->datum (list #'syntax-tag #'vocabulary #'root-tag #'lex-tag #'rules-tag))
             '(syntax lexical root lex rules))
     (let* ((rows (stx-map values #'(clause ...)))
            (names (map (lambda (row)
                          (let (datum (syntax->datum row))
                            (and (pair? datum) (car datum)))) rows)))
       (unless (and (every (lambda (name)
                            (memq name '(extras keywords recoveries conflicts case-insensitive
                                         node-fields catalog))) names)
                    (= (length names) (length (delete-duplicates/hash names))))
         (raise-syntax-error #f
           "lexical syntax requires distinct syntax policies; identity, flow and backends belong to other owners" stx))
       (expand-concise-language-syntax
        #'(deflanguage prefix (identity #f #f #f) (root root-name)
            (lex lexical-row ...) (rules rule-row ...) clause ...)
        expand-lexical-rows compile-parser bind-grammar-ir
        assembly-binding descriptor-binding author-entry
        begin-entry define-entry stx)))
    (_ (raise-syntax-error #f "lexical syntax requires a root and lexical declarations" stx))))

;;; User AST constructors become engine-only aliases after source admission.
;;; The original source witness and replacement are lowered symmetrically.
(def (lower-source-rule-overlays rows owner namespace version commit)
  (def (lower expression)
    (syntax-case expression (node seq prec alias sequence precedence)
      ((node kind body ...)
       (and (identifier? #'kind) (pair? (syntax->list #'(body ...))))
       (let (children (stx-map lower #'(body ...)))
         (with-syntax ((value (if (null? (cdr children)) (car children)
                              (datum->syntax namespace (cons 'sequence children)))))
           #'(alias kind value))))
      ((seq body ...)
       (with-syntax (((child ...) (stx-map lower #'(body ...)))) #'(sequence child ...)))
      ((prec association level body)
       (with-syntax ((child (lower #'body))) #'(precedence association level child)))
      ((alias argument ...)
       (raise-syntax-error #f "alias is an engine constructor; author rules use node" expression owner))
      ((sequence argument ...)
       (raise-syntax-error #f "sequence is an engine constructor; author rules use seq" expression owner))
      ((precedence argument ...)
       (raise-syntax-error #f "precedence is an engine constructor; author rules use prec" expression owner))
      ((head argument ...)
       (with-syntax (((child ...) (stx-map lower #'(argument ...)))) #'(head child ...)))
      (_ expression)))
  (def (source-receipt provenance)
    (let* ((origin (syntax->datum provenance))
           (qualified? (and (list? origin) (= (length origin) 2)
                            (memq (car origin) '(normalization disambiguation))
                            (symbol? (cadr origin))))
           (label (if qualified? (cadr origin) origin)))
      (unless (symbol? label)
        (raise-syntax-error #f "source provenance requires an identifier or a typed normalization/disambiguation identifier" provenance owner))
      (datum->syntax namespace
        (append (list '(schema . "gerbil-parser.iso-bnf-rule-overlay.v1")
                      (cons 'namespace (syntax->datum namespace)) (cons 'name label))
                (if qualified? (list (cons 'kind (car origin))) '())
                (list (cons 'sourceVersion (syntax->datum version))
                      (cons 'upstreamCommit (syntax->datum commit)))))))
  (stx-map
   (lambda (row)
     (syntax-case row ()
       ((name provenance original replacement)
        (identifier? #'name)
        (with-syntax ((before (lower #'original)) (after (lower #'replacement))
                      (receipt (source-receipt #'provenance)))
          #'(name receipt before after)))
       (_ (raise-syntax-error #f "source rule overlay requires name, provenance and two construction witnesses" row owner))))
   rows))
