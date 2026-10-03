;;; -*- Gerbil -*-
;;; Lossless-enough ANTLR4 grammar catalog used only at language compilation.

(import :gerbil-parser/src/runtime/identity
        (only-in ./antlr4-source-lexer
                 antlr4-tokens-from-datum
                 antlr4-tokens->datum
                 collect-rule-expression
                 identifier-token?
                 punctuation-token?
                 scan-antlr4-tokens
                 skip-balanced
                 token-kind
                 token-value))
(export +antlr4-source-schema+
        antlr4-rule?
        antlr4-rule-name
        antlr4-rule-kind
        antlr4-rule-fragment?
        antlr4-rule-expression
        antlr4-rule-references
        antlr4-source?
        antlr4-source-language
        antlr4-source-version
        antlr4-source-commit
        antlr4-source-digest
        antlr4-source-name
        antlr4-source-options
        antlr4-source-rules
        antlr4-source-parser-rules
        antlr4-source-lexer-rules
        antlr4-source-rule
        antlr4-source-parser-grammar-rules
        antlr4-source-parser-syntax-kinds
        antlr4-source-parser-literals
        antlr4-source-declaration-sources
        antlr4-source->datum
        antlr4-source-from-datum
        parse-antlr4-source
        parse-antlr4-source/expected)

;; : String
(def +antlr4-source-schema+ "gerbil-parser.antlr4-source.v1")

;; : (-> String Symbol Boolean GrammarExpr (List String) Antlr4Rule)
(defstruct antlr4-rule (name kind fragment? expression references)
  transparent: #t)
;; : (-> String String String String String (List Antlr4Rule) Antlr4Source)
(defstruct antlr4-source
  (schema language version commit digest name options rules)
  transparent: #t)

;; : (-> String Boolean)
(def (uppercase-rule-name? name)
  (and (positive? (string-length name))
       (char-upper-case? (string-ref name 0))))

;; : (-> String (List String) Boolean)
(def (string-member? value values)
  (and (pair? values)
       (or (string=? value (car values))
           (string-member? value (cdr values)))))

;; : (-> (List String) (List String) (List String))
(def (unique-strings-from rest found)
  (if (null? rest)
    (reverse found)
    (unique-strings-from
     (cdr rest)
     (if (string-member? (car rest) found)
       found
       (cons (car rest) found)))))

;; : (-> (List String) (List String))
(def (unique-strings values)
  (unique-strings-from values '()))

;; : (-> (List Antlr4Token) (List Antlr4Token) (List Antlr4Token))
(def (expression-before-command-from rest found)
  (if (or (null? rest) (punctuation-token? (car rest) "->"))
    (reverse found)
    (expression-before-command-from (cdr rest) (cons (car rest) found))))

;; : (-> (List Antlr4Token) (List Antlr4Token))
(def (expression-before-command expression)
  (expression-before-command-from expression '()))

;; : (-> (Maybe Antlr4Token) Antlr4Token (Maybe Antlr4Token) Boolean)
(def (reference-token? previous current next)
  (and (eq? (token-kind current) 'identifier)
       (not (and previous (punctuation-token? previous "#")))
       (not (and next
                 (or (punctuation-token? next "=")
                     (punctuation-token? next "+="))))
       (not (string=? (token-value current) "options"))))

;; : (-> (List Antlr4Token) (Maybe Antlr4Token) (List String) (List String))
(def (expression-references-from rest previous found)
  (if (null? rest)
    (unique-strings (reverse found))
    (let* ((current (car rest))
           (next (and (pair? (cdr rest)) (cadr rest))))
      (expression-references-from
       (cdr rest) current
       (if (reference-token? previous current next)
         (cons (token-value current) found)
         found)))))

;; : (-> (List Antlr4Token) (List String))
(def (expression-references expression)
  (expression-references-from (expression-before-command expression) #f '()))

;; : (-> (List Antlr4Token) Boolean (Values Antlr4Rule (List Antlr4Token)))
(def (parse-rule tokens fragment?)
  (unless (and (pair? tokens) (eq? (token-kind (car tokens)) 'identifier))
    (error "ANTLR4 rule name expected"))
  (let* ((name (token-value (car tokens)))
         (after-name (cdr tokens))
         (after-options
          (if (and (pair? after-name)
                   (identifier-token? (car after-name) "options"))
            (skip-balanced (cdr after-name) "{" "}")
            after-name)))
    (unless (and (pair? after-options)
                 (punctuation-token? (car after-options) ":"))
      (error "ANTLR4 rule separator expected" name))
    (let-values (((expression rest)
                  (collect-rule-expression (cdr after-options))))
      (values
       (make-antlr4-rule
        name
        (if (or fragment? (uppercase-rule-name? name)) 'lexer 'parser)
        fragment?
        expression
        (expression-references expression))
       rest))))

;; : (-> String (List Antlr4Token) (List Symbol) (List Antlr4Rule) (Values String (List Symbol) (List Antlr4Rule)))
(def (parse-top-level-from name rest options rules)
  (cond
   ((null? rest) (values name (reverse options) (reverse rules)))
   ((identifier-token? (car rest) "options")
    (unless (pair? (cdr rest))
      (error "ANTLR4 options block expected"))
    (parse-top-level-from
     name (skip-balanced (cdr rest) "{" "}")
     (cons 'options options) rules))
   ((identifier-token? (car rest) "fragment")
    (let-values (((rule next) (parse-rule (cdr rest) #t)))
      (parse-top-level-from name next options (cons rule rules))))
   ((eq? (token-kind (car rest)) 'identifier)
    (let-values (((rule next) (parse-rule rest #f)))
      (parse-top-level-from name next options (cons rule rules))))
   (else
    (error "unexpected ANTLR4 top-level token" (car rest)))))

;; : (-> (List Antlr4Token) (Values String (List Symbol) (List Antlr4Rule)))
(def (parse-top-level tokens)
  (unless (and (pair? tokens) (identifier-token? (car tokens) "grammar")
               (pair? (cdr tokens))
               (eq? (token-kind (cadr tokens)) 'identifier)
               (pair? (cddr tokens))
               (punctuation-token? (caddr tokens) ";"))
    (error "ANTLR4 grammar header expected"))
  (parse-top-level-from (token-value (cadr tokens))
                        (cdddr tokens) '() '()))

;; : (-> (List Antlr4Rule) (List String))
(def (rule-names rules)
  (map antlr4-rule-name rules))

;; : (-> (List Antlr4Rule) (List Antlr4Rule))
(def (validate-rules rules)
  ;; Build the catalog once.  The former implementation filtered the complete
  ;; rule-name list once per rule and then linearly scanned it for every
  ;; reference, making admission quadratic for full language grammars.
  (let (catalog (make-table test: equal?))
    (for-each
     (lambda (rule)
       (let (name (antlr4-rule-name rule))
         (when (table-ref catalog name #f)
           (error "duplicate ANTLR4 rule" name))
         (table-set! catalog name rule)))
     rules)
    (for-each
     (lambda (rule)
       (let (name (antlr4-rule-name rule))
         (for-each
          (lambda (reference)
            (unless (or (string=? reference "EOF")
                        (table-ref catalog reference #f))
              (error "unresolved ANTLR4 rule reference" name reference)))
          (antlr4-rule-references rule))))
     rules)
    rules))

;; : (-> String String String String Antlr4Source)
(def (parse-antlr4-source language version commit source)
  (unless (and (string? language) (string? version) (string? commit)
               (string? source))
    (error "ANTLR4 source identity and content must be strings"))
  (let-values (((name options rules)
                (parse-top-level (scan-antlr4-tokens source))))
    (make-antlr4-source
     +antlr4-source-schema+ language version commit (sha256-text source)
     name options (validate-rules rules))))

;; : (-> String String String String String Antlr4Source)
(def (parse-antlr4-source/expected language version commit expected-digest source)
  (let (catalog (parse-antlr4-source language version commit source))
    (unless (string=? (antlr4-source-digest catalog) expected-digest)
      (error "ANTLR4 source digest mismatch"
             expected-digest (antlr4-source-digest catalog)))
    catalog))

;; : (-> Antlr4Source (List Antlr4Rule))
(def (antlr4-source-parser-rules source)
  (filter (lambda (rule) (eq? (antlr4-rule-kind rule) 'parser))
          (antlr4-source-rules source)))

;; : (-> Antlr4Source (List Antlr4Rule))
(def (antlr4-source-lexer-rules source)
  (filter (lambda (rule) (eq? (antlr4-rule-kind rule) 'lexer))
          (antlr4-source-rules source)))

;; : (-> Antlr4Source String (Maybe Antlr4Rule))
(def (antlr4-source-rule source name)
  (find (lambda (rule) (string=? (antlr4-rule-name rule) name))
        (antlr4-source-rules source)))

;; : (-> Antlr4Source (HashTable String Antlr4Rule))
(def (antlr4-source-rule-index source)
  (let (index (make-table test: equal?))
    (for-each
     (lambda (rule)
       (table-set! index (antlr4-rule-name rule) rule))
     (antlr4-source-rules source))
    index))

;; : (-> Antlr4Rule Datum)
(def (antlr4-rule->datum rule)
  (list (antlr4-rule-name rule)
        (antlr4-rule-kind rule)
        (antlr4-rule-fragment? rule)
        (antlr4-tokens->datum (antlr4-rule-expression rule))
        (antlr4-rule-references rule)))

;; : (-> Antlr4Source Datum)
(def (antlr4-source->datum source)
  (list +antlr4-source-schema+
        (antlr4-source-language source)
        (antlr4-source-version source)
        (antlr4-source-commit source)
        (antlr4-source-digest source)
        (antlr4-source-name source)
        (antlr4-source-options source)
        (map antlr4-rule->datum (antlr4-source-rules source))))

;; : (-> Datum Antlr4Source)
(def (antlr4-source-from-datum value)
  (unless (and (list? value) (= (length value) 8)
               (string=? (car value) +antlr4-source-schema+))
    (error "invalid materialized ANTLR4 source v1" value))
  (make-antlr4-source
   (car value) (cadr value) (caddr value) (cadddr value)
   (list-ref value 4) (list-ref value 5) (list-ref value 6)
   (map (lambda (row)
          (make-antlr4-rule
           (car row) (cadr row) (caddr row)
           (antlr4-tokens-from-datum (list-ref row 3))
           (list-ref row 4)))
        (list-ref value 7))))

;; : (-> GrammarExpr)
(def (antlr4-empty)
  '(empty))

;; : (-> (List GrammarExpr) GrammarExpr)
(def (antlr4-sequence expressions)
  (cond
   ((null? expressions) (antlr4-empty))
   ((null? (cdr expressions)) (car expressions))
   (else (cons 'sequence expressions))))

;; : (-> (List GrammarExpr) GrammarExpr)
(def (antlr4-choice expressions)
  (cond
   ((null? expressions) (antlr4-empty))
   ((null? (cdr expressions)) (car expressions))
   (else (cons 'choice expressions))))

;; : (-> (List Antlr4Token) (-> String GrammarExpr) (List GrammarExpr) (Values GrammarExpr (List Antlr4Token)))
(def (parse-expression-choice-from rest resolve alternatives)
  (let-values (((sequence next) (parse-expression-sequence rest resolve)))
    (if (and (pair? next) (punctuation-token? (car next) "|"))
      (parse-expression-choice-from
       (cdr next) resolve (cons sequence alternatives))
      (values (antlr4-choice (reverse (cons sequence alternatives))) next))))

;; : (-> (List Antlr4Token) (-> String GrammarExpr) (Values GrammarExpr (List Antlr4Token)))
(def (parse-expression-choice tokens resolve)
  (parse-expression-choice-from tokens resolve '()))

;; : (-> (List Antlr4Token) (-> String GrammarExpr) (List GrammarExpr) (Values GrammarExpr (List Antlr4Token)))
(def (parse-expression-sequence-from rest resolve expressions)
  (cond
   ((or (null? rest)
        (punctuation-token? (car rest) "|")
        (punctuation-token? (car rest) ")"))
    (values (antlr4-sequence (reverse expressions)) rest))
   ((punctuation-token? (car rest) "#")
    (unless (and (pair? (cdr rest))
                 (eq? (token-kind (cadr rest)) 'identifier))
      (error "ANTLR4 alternative label expected"))
    (values (antlr4-sequence (reverse expressions)) (cddr rest)))
   (else
    (let-values (((expression next) (parse-expression-postfix rest resolve)))
      (parse-expression-sequence-from
       next resolve (cons expression expressions))))))

;; : (-> (List Antlr4Token) (-> String GrammarExpr) (Values GrammarExpr (List Antlr4Token)))
(def (parse-expression-sequence tokens resolve)
  (parse-expression-sequence-from tokens resolve '()))

;; : (-> (List Antlr4Token) (List Antlr4Token))
(def (postfix-tail rest)
  (let (tail (cdr rest))
    (if (and (pair? tail) (punctuation-token? (car tail) "?"))
      (cdr tail)
      tail)))

;; : (-> GrammarExpr (List Antlr4Token) (Values GrammarExpr (List Antlr4Token)))
(def (apply-expression-postfix atom rest)
  (let (operator (token-value (car rest)))
    (cond
     ((string=? operator "?")
      (values (list 'optional atom) (cdr rest)))
     ((string=? operator "*")
      (values (list 'repeat atom) (postfix-tail rest)))
     ((string=? operator "+")
      (values (list 'repeat1 atom) (postfix-tail rest)))
     (else (values atom rest)))))

;; : (-> (List Antlr4Token) (-> String GrammarExpr) (Values GrammarExpr (List Antlr4Token)))
(def (parse-expression-postfix tokens resolve)
  (let-values (((atom rest) (parse-expression-atom tokens resolve)))
    (cond
     ((null? rest) (values atom rest))
     ((not (eq? (token-kind (car rest)) 'punctuation))
      (values atom rest))
     (else (apply-expression-postfix atom rest)))))

;; : (-> (List Antlr4Token) (-> String GrammarExpr) (Values GrammarExpr (List Antlr4Token)))
(def (parse-expression-atom tokens resolve)
  (unless (pair? tokens)
    (error "ANTLR4 expression atom expected"))
  (let ((current (car tokens))
        (next (and (pair? (cdr tokens)) (cadr tokens))))
    (cond
     ((eq? (token-kind current) 'literal)
      (values (list 'literal (token-value current)) (cdr tokens)))
     ((eq? (token-kind current) 'identifier)
      (if (and next
               (or (punctuation-token? next "=")
                   (punctuation-token? next "+=")))
        (let-values (((value rest)
                      (parse-expression-postfix (cddr tokens) resolve)))
          (values (list 'field (string->symbol (token-value current)) value)
                  rest))
        (values (resolve (token-value current)) (cdr tokens))))
     ((punctuation-token? current "(")
      (let-values (((expression rest)
                    (parse-expression-choice (cdr tokens) resolve)))
        (unless (and (pair? rest) (punctuation-token? (car rest) ")"))
          (error "unterminated ANTLR4 grouped expression"))
        (values expression (cdr rest))))
     (else
      (error "unsupported ANTLR4 expression atom" current)))))

;; : (-> Antlr4Rule (-> String GrammarExpr) GrammarExpr)
(def (parse-rule-grammar-expression rule resolve)
  (let-values (((expression rest)
                (parse-expression-choice
                 (expression-before-command (antlr4-rule-expression rule))
                 resolve)))
    (unless (null? rest)
      (error "unconsumed ANTLR4 rule expression"
             (antlr4-rule-name rule) rest))
    expression))

;; : (-> String String Integer Integer Integer Boolean)
(def (string-contains-from? text fragment offset text-length fragment-length)
  (and (<= (+ offset fragment-length) text-length)
       (or (string=? (substring text offset (+ offset fragment-length))
                     fragment)
           (string-contains-from?
            text fragment (+ offset 1) text-length fragment-length))))

;; : (-> String String Boolean)
(def (string-contains? text fragment)
  (string-contains-from?
   text fragment 0 (string-length text) (string-length fragment)))

;; : (-> String Boolean)
(def (identifier-token-name? name)
  (or (string-contains? name "IDENTIFIER")
      (string=? name "PARAMETER_NAME")))

;; : (-> String Boolean)
(def (string-token-name? name)
  (or (string-contains? name "CHARACTER_SEQUENCE")
      (string-contains? name "STRING_LITERAL")))

;; : (-> String Boolean)
(def (number-token-name? name)
  (or (string-contains? name "UNSIGNED_DECIMAL")
      (string-contains? name "UNSIGNED_HEXADECIMAL")
      (string-contains? name "UNSIGNED_OCTAL")
      (string-contains? name "UNSIGNED_BINARY")))

;; : (-> (HashTable String Antlr4Rule) String (List String) (Maybe String))
(def (constant-lexer-expression rule-index name seen)
  (and (not (string-member? name seen))
       (let (rule (table-ref rule-index name #f))
         (and rule
              (eq? (antlr4-rule-kind rule) 'lexer)
              (with-catch
               (lambda (_) #f)
               (lambda ()
                 (parse-rule-grammar-expression
                  rule
                  (lambda (reference)
                    (or (constant-lexer-expression
                         rule-index reference (cons name seen))
                        (error "non-constant lexer rule" reference))))))))))

;; : (-> (HashTable String Antlr4Rule) String GrammarExpr)
(def (parser-terminal-expression rule-index name)
  (def (lower name seen)
    (cond
     ((string=? name "EOF") (antlr4-empty))
     ((constant-lexer-expression rule-index name '()) => values)
     ((identifier-token-name? name) '(token identifier))
     ((string-token-name? name) '(token string))
     ((number-token-name? name) '(token number))
     ((string-member? name seen)
      (error "cyclic ANTLR4 lexer rule" name))
     (else
      (let (rule (table-ref rule-index name #f))
        (unless (and rule (eq? (antlr4-rule-kind rule) 'lexer))
          (error "ANTLR4 lexer rule expected" name))
        (with-catch
         (lambda (_)
           (error "OpenGQL lexer token requires an admitted scanner primitive"
                  name))
         (lambda ()
           (parse-rule-grammar-expression
            rule
            (lambda (reference)
              (lower reference (cons name seen))))))))))
  (lower name '()))

;; ANTLR alternatives are ordered decisions, including outside direct left
;; recursion. Project every alternative rank into the native LR precedence
;; model so the adapter preserves upstream decision semantics instead of
;; manufacturing an unordered selective-GLR fork.
;; : (-> (List GrammarExpr) Integer (List GrammarExpr) (List GrammarExpr))
(def (apply-antlr-precedence-from rest rank found)
  (if (null? rest)
    (reverse found)
    (let (alternative (car rest))
      (apply-antlr-precedence-from
       (cdr rest) (- rank 1)
       (cons (list 'precedence 'left rank alternative) found)))))

;; : (-> GrammarExpr GrammarExpr)
(def (apply-antlr-precedence expression)
  (if (and (pair? expression) (eq? (car expression) 'choice))
    (let (alternatives (cdr expression))
      (cons 'choice
            (apply-antlr-precedence-from
             alternatives (length alternatives) '())))
    expression))

;;; ANTLR direct-left-recursive precedence belongs to the surrounding
;;; alternative.  When an operator is factored through a parser rule (for
;;; example `expression compOp expression`), leaving that reference opaque
;;; makes the referenced rule's local alternative rank leak into the LR shift
;;; action.  Inline only finite terminal parser rules at that operator site,
;;; retaining their alias so the CST stays source-faithful.
;; : (-> GrammarExpr Boolean)
(def (antlr4-terminal-expression? expression)
  (case (car expression)
    ((empty literal token) #t)
    ((field alias) (antlr4-terminal-expression? (caddr expression)))
    ((optional repeat repeat1)
     (antlr4-terminal-expression? (cadr expression)))
    ((choice sequence)
     (andmap antlr4-terminal-expression? (cdr expression)))
    ((precedence)
     (antlr4-terminal-expression? (cadddr expression)))
    (else #f)))

;; : (-> GrammarExpr (HashTable String Antlr4Rule)
;;        (-> String GrammarExpr) GrammarExpr)
(def (inline-antlr4-terminal-reference expression rule-index resolve)
  (if (and (pair? expression) (eq? (car expression) 'reference))
    (let* ((name (symbol->string (cadr expression)))
           (rule (table-ref rule-index name #f)))
      (if (and rule (eq? (antlr4-rule-kind rule) 'parser))
        (let (candidate (parse-rule-grammar-expression rule resolve))
          (if (antlr4-terminal-expression? candidate)
            (list 'alias (upper-initial-symbol name) candidate)
            expression))
        expression))
    expression))

;; : (-> Symbol GrammarExpr (HashTable String Antlr4Rule)
;;        (-> String GrammarExpr) GrammarExpr)
(def (inline-antlr4-left-recursive-operators owner expression rule-index resolve)
  (def owner-reference (list 'reference owner))
  (def (inline-alternative alternative)
    (if (and (pair? alternative)
             (eq? (car alternative) 'sequence)
             (pair? (cdr alternative))
             (equal? (cadr alternative) owner-reference)
             (member owner-reference (cddr alternative)))
      (cons 'sequence
            (map (lambda (operand)
                   (if (equal? operand owner-reference)
                     operand
                     (inline-antlr4-terminal-reference
                      operand rule-index resolve)))
                 (cdr alternative)))
      alternative))
  (if (and (pair? expression) (eq? (car expression) 'choice))
    (cons 'choice (map inline-alternative (cdr expression)))
    (inline-alternative expression)))

;; : (-> Antlr4Source (List GrammarRule))
(def (antlr4-source-parser-grammar-rules source)
  (let ((rule-index (antlr4-source-rule-index source))
        (terminal-cache (make-table test: equal?)))
    (def (resolve reference)
      (if (uppercase-rule-name? reference)
        (or (table-ref terminal-cache reference #f)
            (let (expression
                  (parser-terminal-expression rule-index reference))
              (table-set! terminal-cache reference expression)
              expression))
        (list 'reference (string->symbol reference))))
    (map
     (lambda (rule)
       (let* ((name (string->symbol (antlr4-rule-name rule)))
              (expression (parse-rule-grammar-expression rule resolve))
              (expression
               (inline-antlr4-left-recursive-operators
                name expression rule-index resolve)))
         (list name
               (list 'alias
                     (upper-initial-symbol (antlr4-rule-name rule))
                     (apply-antlr-precedence expression)))))
     (antlr4-source-parser-rules source))))

;; : (-> String Symbol)
(def (upper-initial-symbol name)
  (let (copy (string-copy name))
    (string-set! copy 0 (char-upcase (string-ref copy 0)))
    (string->symbol copy)))

;; : (-> Antlr4Source (List SyntaxKind))
(def (antlr4-source-parser-syntax-kinds source)
  (map (lambda (rule)
         (list (upper-initial-symbol (antlr4-rule-name rule)) 'node '()))
       (antlr4-source-parser-rules source)))

;; : (-> GrammarExpr (List String))
(def (grammar-literals expression)
  (case (car expression)
    ((literal) (list (cadr expression)))
    ((sequence choice)
     (apply append (map grammar-literals (cdr expression))))
    ((optional repeat repeat1)
     (grammar-literals (cadr expression)))
    ((field alias)
     (grammar-literals (caddr expression)))
    ((precedence)
     (grammar-literals (cadddr expression)))
    (else '())))

;; : (-> Antlr4Source (List String))
(def (antlr4-source-parser-literals source)
  (unique-strings
   (apply append
          (map (lambda (row) (grammar-literals (cadr row)))
               (antlr4-source-parser-grammar-rules source)))))

;;; ANTLR rule names remain native grammar names in Bound Grammar IR.  The
;;; catalog currently owns rule identity but not token offsets, so location is
;;; an explicit path-plus-rule coordinate rather than a fabricated line.
;; : (-> Antlr4Source String DeclarationSourceMap)
(def (antlr4-source-declaration-sources source path)
  (list
   (cons
    'rule
    (map (lambda (rule)
           (let (name (string->symbol (antlr4-rule-name rule)))
             (cons
              name
              (list (cons 'path path)
                    (cons 'location
                          (string-append path "#" (antlr4-rule-name rule)))
                    (cons 'rule name)
                    (cons 'generated? #t)))))
         (antlr4-source-parser-rules source)))))
