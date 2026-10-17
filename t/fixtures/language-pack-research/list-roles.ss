;;; -*- Gerbil -*-
;;; Checked POO contributions built at the importing module's phase.
(import (only-in :clan/poo/object .o)
        (only-in "../../../src/modules/parser/objects" make-grammar-role make-grammar grammar-role-ref)
        (only-in "../../../src/modules/parser/syntax" defgrammar-role)
        (only-in "../../../src/grammar/algebra" grammar-expression?))
(export make-nonempty-list-role make-list-study-grammar make-single-argument-list-grammar list-study-base-role
        make-native-list-grammar make-native-single-list-grammar)

(def (make-nonempty-list-role owner entry item separator field-name)
  (unless (and (symbol? owner) (symbol? entry) (symbol? field-name)
               (grammar-expression? item) (grammar-expression? separator))
    (error "invalid nonempty list component" owner entry))
  ;; Stable domain identity; no fresh host binding is used as a grammar key.
  (let* ((tail (string->symbol (string-append (symbol->string owner) "/tail")))
         (item-field (list 'field field-name item))
         (rows
          (list
           (list entry (list 'sequence item-field (list 'reference tail)))
           (list tail (list 'repeat (list 'sequence separator item-field))))))
    (make-grammar-role owner '() '() '() rows '() '() '() '() '())))

(defgrammar-role list-study-base-role
  (syntax-kinds
   (SourceFile node (form)) (Call node (callee argument))
   (Array node (element)) (Name node (value))
   (Identifier token (text)) (Punctuation token (text)) (Whitespace token (text)))
  (terminals (identifier Identifier) (punctuation Punctuation) (whitespace Whitespace))
  (lexical-rules
   (identifier (identifier))
   (punctuation (literals "(" ")" "[" "]" ","))
   (whitespace (whitespace+)))
  (rules
   (source-file
    (alias SourceFile (field form (choice (reference call) (reference array)))))
   (call
    (alias Call
      (seq (field callee (token identifier)) (literal "(")
           (optional (reference arguments)) (literal ")"))))
   (array
    (alias Array
      (seq (literal "[") (optional (reference elements)) (literal "]"))))
   (name (alias Name (field value (token identifier)))))
  (extras whitespace) (keywords)
  (parser-entrypoints (source-file parse pure))
  (recoveries) (flow (source lexical) (lexical cst)))

(def (make-list-study-grammar)
  (let ((arguments
         (make-nonempty-list-role 'call-arguments 'arguments
           '(reference name) '(literal ",") 'argument))
        (elements
         (make-nonempty-list-role 'array-elements 'elements
           '(reference name) '(literal ",") 'element)))
    (make-grammar 'list-study '() (list list-study-base-role)
      (list (cons 'append arguments) (cons 'append elements)))))

;;; Independent policy variant exercises accepted override and helper removal.
(def (make-single-argument-list-grammar)
  (let ((replacement
         (make-grammar-role 'single-argument-extension '() '() '()
           '((arguments (field argument (reference name)))) '() '() '() '() '()))
        (removed
         (make-grammar-role 'retired-tail '() '() '()
           '((call-arguments/tail (empty))) '() '() '() '() '())))
    (make-grammar 'list-study-single (list (make-list-study-grammar)) '()
      (list (cons 'override replacement) (cons 'remove removed)))))

;;; Author control uses upstream POO slot syntax, not a composition-op DSL.
(def (native-row-source owner generated? section row)
  (list (cons 'path "t/fixtures/language-pack-research/list-roles.ss")
        (cons 'location (string-append "native prototype " (symbol->string owner)
                                      "; " (symbol->string section) " "
                                      (symbol->string (car row))))
        (cons 'generated? generated?) (cons 'declarationOwner owner)))

(def (make-native-list-role)
  (let ((arguments (make-nonempty-list-role 'call-arguments 'arguments
                     '(reference name) '(literal ",") 'argument))
        (elements (make-nonempty-list-role 'array-elements 'elements
                    '(reference name) '(literal ",") 'element)))
    (.o (:: self list-study-base-role)
        name: 'native-list-role
        (rules => append (grammar-role-ref arguments 'rules) (grammar-role-ref elements 'rules))
        row-source:
        (lambda (section row)
          (let (owner (if (eq? section 'rules)
                        (case (car row)
                          ((arguments call-arguments/tail) 'call-arguments)
                          ((elements array-elements/tail) 'array-elements)
                          (else 'list-study-base-role))
                        'list-study-base-role))
            (native-row-source owner (not (eq? owner 'list-study-base-role)) section row))))))

(def (make-native-list-grammar)
  (make-grammar 'native-list-study '() (list (make-native-list-role))))

(def (make-native-single-list-grammar)
  (let (role
        (.o (:: self (make-native-list-role))
            name: 'native-single-list-role
            ;; Native inherited computation supplies the original table. Scheme
            ;; values express the selected rule policy; there is no new keyword.
            (rules (inherited)
              (filter-map
               (lambda (row)
                 (case (car row)
                   ((arguments) '(arguments (field argument (reference name))))
                   ((call-arguments/tail) #f)
                   (else row)))
               (inherited)))
            (row-source (inherited)
              (lambda (section row)
                (if (and (eq? section 'rules) (eq? (car row) 'arguments))
                  (native-row-source 'single-argument-extension #t section row)
                  ((inherited) section row))))))
    (make-grammar 'native-list-study-single '() (list role))))
