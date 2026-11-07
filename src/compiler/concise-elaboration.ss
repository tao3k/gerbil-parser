;;; Concise declaration elaboration owns source maps, normalized rules and
;;; catalog publication. Syntax admission calls this worker once per declaration.
(import (only-in :gerbil/core/expander
                 syntax-case syntax with-syntax datum->syntax syntax->datum
                 stx-map stx-list? syntax->list identifier? raise-syntax-error)
        (only-in :gerbil/expander core-expand1)
        (only-in :std/list/list delete-duplicates/hash)
        (only-in ../grammar/algebra grammar-expression-header?)
        (only-in ./language-artifact project-language-catalog
                 make-language-declaration expand-language-declaration-syntax))
(export elaborate-concise-declaration)

(def (elaborate-concise-declaration stx author-syntax expand-lexical-rows
                                    compile-parser bind-grammar-ir assembly-binding
                                    descriptor-binding begin-entry define-entry
                                    prefix-stx root-stx lexical-stx rules-stx options-stx
                                    identity-stx extras-stx keywords-stx recoveries-stx
                                    conflicts-stx case-stx)
  (def (syntax-failure-at message offending)
    (raise-syntax-error #f message offending author-syntax))
  (def (syntax-failure message) (syntax-failure-at message stx))
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
  (def (expand-surface-syntax expression author-call)
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
    (let* ((options (syntax->datum options-stx))
           (prefix-name (syntax->datum prefix-stx))
           (root-value (syntax->datum root-stx))
           (lexical-rows (expand-lexical-rows lexical-stx stx))
           (rule-rows
            (stx-map
             (lambda (row)
               (syntax-case row ()
                 ((name expression ...)
                  (let (result
                        (cons (syntax->datum #'name)
                              (stx-map (lambda (expression) (expand-surface-syntax expression #f))
                                       #'(expression ...))))
                    (table-set! rule-declarations result row)
                    (table-set! rule-sources result
                                (stx-map (lambda (value) value) #'(expression ...)))
                    result))
                 (_ (syntax-failure
                     "invalid concise rule declaration"))))
             rules-stx))
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
      (def (surface-fields/seen expression rules seen)
        (def (rule-fields name)
          (if (memq name seen) '()
            (let (row (assq name rules))
              (if row
                (surface-fields/seen (cadr row) rules (cons name seen))
                '()))))
        (case (car expression)
          ((alias) '())
          ((reference) (rule-fields (cadr expression)))
          ((field)
           (cons (cadr expression)
                 (surface-fields/seen (caddr expression) rules seen)))
          ((prec)
           (surface-fields/seen (cadddr expression) rules seen))
          ((optional repeat repeat1)
           (surface-fields/seen (cadr expression) rules seen))
          ((seq choice)
           (apply append
                  (map (lambda (child) (surface-fields/seen child rules seen))
                       (cdr expression))))
          (else '())))
      (def (surface-fields expression rules)
        (surface-fields/seen expression rules '()))
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
        (def (syntax-failure message)
          (raise-syntax-error #f message blame stx))
        (def (require-at condition message offending)
          (unless condition (raise-syntax-error #f message offending stx)))
        (def (require condition message)
          (require-at condition message blame))
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
             (require-at (memq (cadr expression) token-names)
                      "explicit token names an unknown lexical declaration" (operand-source 1))
             expression)
            ((reference)
             (require (= (length expression) 2)
                      "explicit reference names an unknown rule")
             (require-at (memq (cadr expression) rule-names)
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
            (syntax-failure-at
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
             (syntax-failure-at
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
                 author-syntax prefix-stx
                 (stx-map (lambda (value) value)
                          identity-stx)
                 (map (lambda (row)
                        (cons (car row) (datum->syntax prefix-stx (cdr row))))
                      `((syntax-kinds ,@syntax-kinds)
                        (terminals ,@terminals)
                        (lexical-rules ,@lexical-rules)
                        (rules ,@normalized-rules)
                        (extras ,@(syntax->datum extras-stx))
                        (keywords ,@(syntax->datum keywords-stx))
                        (parser-entrypoints (,root-value parse pure))
                        (recoveries ,@(syntax->datum recoveries-stx))
                        (flow (source lexical) (lexical parser) (parser cst))))
                 (syntax->datum conflicts-stx)
                 (syntax->datum case-stx)
                 `(deflanguage ,@(reverse (unique macro-lineage))) '())))
          (expand-language-declaration-syntax
           declaration compile-parser bind-grammar-ir
           assembly-binding descriptor-binding begin-entry define-entry)))))
