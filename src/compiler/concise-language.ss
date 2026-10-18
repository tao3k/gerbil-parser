;;; -*- Gerbil -*-
;;; Concise language elaboration: collect authored syntax, validate rules, derive
;;; catalogs, and submit a declaration to the existing canonical compiler.
;;; Runtime identifiers are supplied by the public macro's hygienic context.

(import (only-in :gerbil/core/expander
                 syntax-case syntax with-syntax datum->syntax syntax->datum
                 stx-map stx-list? stx-car stx-cdr syntax->list identifier? raise-syntax-error)
        (only-in :gerbil/expander core-expand1)
        (only-in :std/list/list delete-duplicates/hash)
        (only-in ./concise-elaboration elaborate-concise-declaration))
(export expand-lexical-language-syntax lower-source-rule-overlays lower-source-rule-precedences)

(def (expand-concise-language-syntax stx expand-lexical-rows compile-parser bind-grammar-ir
                                      assembly-binding descriptor-binding
                                      author-entry begin-entry define-entry author-syntax)
  (def (syntax-failure-at message offending)
    (raise-syntax-error #f message offending author-syntax))
  (def (syntax-failure message) (syntax-failure-at message stx))
  (def (require condition message)
    (unless condition (syntax-failure message)))
  (def (unique values)
    (delete-duplicates/hash values))
  ;; Admit each section independently. Nesting all repeated-row patterns in
  ;; one matcher makes compiler analysis revisit their shared continuation.
  (def (section-arguments row tag count)
    (let (items (and (stx-list? row) (syntax->list row)))
      (unless (and (pair? items) (eq? (syntax->datum (car items)) tag)
                   (or (not count) (= (length (cdr items)) count)))
        (raise-syntax-error #f "invalid concise language declaration" row author-syntax))
      (stx-cdr row)))
  (syntax-case stx ()
    ((_ prefix identity-row root-row lex-row rules-row clause ...)
     (identifier? #'prefix)
     (let* ((identity-stx (section-arguments #'identity-row 'identity 3))
            (root-stx (section-arguments #'root-row 'root 1))
            (lexical-stx (section-arguments #'lex-row 'lex #f))
            (rules-stx (section-arguments #'rules-row 'rules #f))
            (rows (stx-map values #'(clause ...)))
            (names (map (lambda (row)
                          (let (datum (syntax->datum row))
                            (and (pair? datum) (car datum)))) rows)))
       (require (every (lambda (name)
                         (memq name '(extras keywords recoveries conflicts
                                      case-insensitive node-fields catalog))) names)
                "unknown concise language option")
       (require (= (length names) (length (unique names)))
                "duplicate concise language option")
       (let (sections (map cons names rows))
         (def (section name default count)
           (let (row (assq name sections))
             (section-arguments (if row (cdr row) (datum->syntax #'prefix default))
                                name count)))
         (elaborate-concise-declaration
          stx author-syntax expand-lexical-rows compile-parser bind-grammar-ir
          assembly-binding descriptor-binding begin-entry define-entry
          #'prefix (stx-car root-stx) lexical-stx rules-stx
          (filter (lambda (row) (memq (car (syntax->datum row)) '(node-fields catalog))) rows)
          identity-stx
          (section 'extras '(extras) #f)
          (section 'keywords '(keywords) #f)
          (section 'recoveries '(recoveries) #f)
          (stx-car (section 'conflicts '(conflicts reject) 1))
          (stx-car (section 'case-insensitive '(case-insensitive #f) 1))))))
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

  (stx-map
   (lambda (row)
     (syntax-case row ()
       ((name provenance original replacement)
        (identifier? #'name)
        (with-syntax ((before (lower #'original)) (after (lower #'replacement))
                      (receipt (source-rule-receipt #'provenance owner namespace version commit)))
          #'(name receipt before after)))
       (_ (raise-syntax-error #f "source rule overlay requires name, provenance and two construction witnesses" row owner))))
   rows))

(def (source-rule-receipt provenance owner namespace version commit)
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

(def (lower-source-rule-precedences rows owner namespace version commit)
  (stx-map
   (lambda (row)
     (syntax-case row ()
       ((name provenance association level)
        (identifier? #'name)
        (with-syntax ((receipt (source-rule-receipt #'provenance owner namespace version commit)))
          #'(name receipt association level)))
       (_ (raise-syntax-error #f "source precedence requires rule, provenance, association and level" row owner))))
   rows))
