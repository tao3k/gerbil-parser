;;; -*- Gerbil -*-
;;; Canonical GrammarExpr v1 constructors and structural analysis.

(export grammar-expression
        grammar-expression?
        grammar-expression-kind
        grammar-expression-nullable?
        grammar-expression-fields
        grammar-expression-references
        grammar-expression-terminals)

;; grammar-expression?
;; : (-> Datum Boolean)
(def (grammar-expression? value)
  (and (list? value)
       (pair? value)
       (case (car value)
         ((empty)
          (null? (cdr value)))
         ((literal)
          (and (= (length value) 2)
               (string? (cadr value))
               (positive? (string-length (cadr value)))))
         ((token reference)
          (and (= (length value) 2)
               (symbol? (cadr value))))
         ((sequence choice)
          (and (pair? (cdr value))
               (grammar-expressions? (cdr value))))
         ((optional repeat repeat1)
          (and (= (length value) 2)
               (grammar-expression? (cadr value))))
         ((field alias)
          (and (= (length value) 3)
               (symbol? (cadr value))
               (grammar-expression? (caddr value))))
         ((precedence)
          (and (= (length value) 4)
               (memq (cadr value) '(none left right dynamic))
               (integer? (caddr value))
               (grammar-expression? (cadddr value))))
         (else #f))))

;; grammar-expressions?
;; : (-> List Boolean)
(def (grammar-expressions? expressions)
  (let loop ((rest expressions))
    (or (null? rest)
        (and (grammar-expression? (car rest))
             (loop (cdr rest))))))

;; require-expression
;; : (-> Datum Symbol List)
(def (require-expression expression owner)
  (unless (grammar-expression? expression)
    (error "invalid grammar expression" owner expression))
  expression)

;; grammar-empty
;; : (-> List)
(def (grammar-empty)
  '(empty))

;; grammar-literal
;; : (-> String List)
(def (grammar-literal value)
  (unless (and (string? value) (positive? (string-length value)))
    (error "grammar literal must be a non-empty string" value))
  (list 'literal value))

;; grammar-token
;; : (-> Symbol List)
(def (grammar-token name)
  (unless (symbol? name)
    (error "grammar token reference must be a symbol" name))
  (list 'token name))

;; grammar-reference
;; : (-> Symbol List)
(def (grammar-reference name)
  (unless (symbol? name)
    (error "grammar rule reference must be a symbol" name))
  (list 'reference name))

;; grammar-sequence
;; : (-> List List)
(def (grammar-sequence expressions)
  (unless (and (pair? expressions) (grammar-expressions? expressions))
    (error "grammar sequence requires one or more expressions" expressions))
  (cons 'sequence expressions))

;; grammar-choice
;; : (-> List List)
(def (grammar-choice expressions)
  (unless (and (pair? expressions) (grammar-expressions? expressions))
    (error "grammar choice requires one or more expressions" expressions))
  (cons 'choice expressions))

;; grammar-optional
;; : (-> List List)
(def (grammar-optional expression)
  (list 'optional (require-expression expression 'optional)))

;; grammar-repeat
;; : (-> List List)
(def (grammar-repeat expression)
  (require-expression expression 'repeat)
  (when (grammar-expression-nullable? expression)
    (error "grammar repeat operand must consume input" expression))
  (list 'repeat expression))

;; grammar-repeat1
;; : (-> List List)
(def (grammar-repeat1 expression)
  (require-expression expression 'repeat1)
  (when (grammar-expression-nullable? expression)
    (error "grammar repeat1 operand must consume input" expression))
  (list 'repeat1 expression))

;; grammar-field
;; : (-> Symbol List List)
(def (grammar-field name expression)
  (unless (symbol? name)
    (error "grammar field name must be a symbol" name))
  (list 'field name (require-expression expression 'field)))

;; grammar-alias
;; : (-> Symbol List List)
(def (grammar-alias name expression)
  (unless (symbol? name)
    (error "grammar alias must be a symbol" name))
  (list 'alias name (require-expression expression 'alias)))

;; grammar-precedence
;; : (-> Symbol Integer List List)
(def (grammar-precedence direction rank expression)
  (unless (memq direction '(none left right dynamic))
    (error "unknown grammar precedence direction" direction))
  (unless (integer? rank)
    (error "grammar precedence rank must be an integer" rank))
  (list 'precedence direction rank
        (require-expression expression 'precedence)))

;;; Expands the closed grammar algebra while preserving declared child order.
;;; Each template delegates validation and runtime data construction to ordinary helpers.
;; grammar-expression
;;   : (-> Syntax Syntax)
;;   | doc m%
;;       `grammar-expression` expands one declarative grammar expression.
;;
;;       # Examples
;;
;;       ```scheme
;;       (grammar-expression (seq (token Identifier)))
;;       ;; => canonical sequence expression
;;       ```
;;     %
(defrules grammar-expression
  (empty literal token reference seq choice optional repeat repeat1
   field alias prec none left right dynamic)
  ((_ (empty))
   (grammar-empty))
  ((_ (literal value))
   (grammar-literal value))
  ((_ (token name))
   (grammar-token 'name))
  ((_ (reference name))
   (grammar-reference 'name))
  ((_ (seq expression ...))
   (grammar-sequence (list (grammar-expression expression) ...)))
  ((_ (choice expression ...))
   (grammar-choice (list (grammar-expression expression) ...)))
  ((_ (optional expression))
   (grammar-optional (grammar-expression expression)))
  ((_ (repeat expression))
   (grammar-repeat (grammar-expression expression)))
  ((_ (repeat1 expression))
   (grammar-repeat1 (grammar-expression expression)))
  ((_ (field name expression))
   (grammar-field 'name (grammar-expression expression)))
  ((_ (alias name expression))
   (grammar-alias 'name (grammar-expression expression)))
  ((_ (prec none rank expression))
   (grammar-precedence 'none rank (grammar-expression expression)))
  ((_ (prec left rank expression))
   (grammar-precedence 'left rank (grammar-expression expression)))
  ((_ (prec right rank expression))
   (grammar-precedence 'right rank (grammar-expression expression)))
  ((_ (prec dynamic rank expression))
   (grammar-precedence 'dynamic rank (grammar-expression expression))))

;; grammar-expression-kind
;; : (-> List Symbol)
(def (grammar-expression-kind expression)
  (car (require-expression expression 'kind)))

;; grammar-expression-nullable?
;;   : (-> List Boolean)
;;   | doc m%
;;       `grammar-expression-nullable?` decides whether an expression accepts empty input.
;;
;;       # Examples
;;
;;       ```scheme
;;       (grammar-expression-nullable? (grammar-empty))
;;       ;; => #t
;;       ```
;;     %
(def (grammar-expression-nullable? expression)
  (require-expression expression 'nullable)
  (case (car expression)
    ((empty optional repeat) #t)
    ((sequence)
     (let loop ((rest (cdr expression)))
       (or (null? rest)
           (and (grammar-expression-nullable? (car rest))
                (loop (cdr rest))))))
    ((choice)
     (let loop ((rest (cdr expression)))
       (and (pair? rest)
            (or (grammar-expression-nullable? (car rest))
                (loop (cdr rest))))))
    ((field alias) (grammar-expression-nullable? (caddr expression)))
    ((precedence) (grammar-expression-nullable? (cadddr expression)))
    ((repeat1) (grammar-expression-nullable? (cadr expression)))
    (else #f)))

;; grammar-expression-collect
;; : (-> List Symbol List)
(def (grammar-expression-collect expression wanted)
  (require-expression expression 'collect)
  (case (car expression)
    ((reference token)
     (if (eq? (car expression) wanted) (list (cadr expression)) '()))
    ((sequence choice)
     (apply append
            (map (lambda (child)
                   (grammar-expression-collect child wanted))
                 (cdr expression))))
    ((optional repeat repeat1)
     (grammar-expression-collect (cadr expression) wanted))
    ((field)
     (let (nested (grammar-expression-collect (caddr expression) wanted))
       (if (eq? wanted 'field)
         (cons (cadr expression) nested)
         nested)))
    ((alias)
     (grammar-expression-collect (caddr expression) wanted))
    ((precedence)
     (grammar-expression-collect (cadddr expression) wanted))
    (else '())))

;; grammar-expression-references
;; : (-> List List)
(def (grammar-expression-references expression)
  (grammar-expression-collect expression 'reference))

;; grammar-expression-fields
;; : (-> List List)
(def (grammar-expression-fields expression)
  (grammar-expression-collect expression 'field))

;; grammar-expression-terminals
;; : (-> List List)
(def (grammar-expression-terminals expression)
  (grammar-expression-collect expression 'token))
