;;; -*- Gerbil -*-
;;; Column-sensitive TLA+ syntax compiled by the language-neutral engine.

(import (only-in :gerbil-parser/src/language/grammar deflanguage-grammar)
        ../source)
(export (import: ../source)
        tla-plus-sany-candidate-language-grammar
        tla-plus-sany-candidate-grammar tla-plus-sany-candidate-parser-ir tla-plus-sany-candidate-parser)


(deflanguage-grammar tla-plus-sany-candidate
  (identity "tla-plus" "p4-draft" "tla-plus.native-sany-candidate.p4")
  (syntax-kinds
   (SourceFile node (module)) (Module node (name item))
   (ExtendsDeclaration node (module))
   (ConstantDeclaration node (name parameter))
   (VariableDeclaration node (name))
   (OperatorDefinition node (name parameter body))
   (RecursiveDeclaration node (name parameter))
   (InstanceDeclaration node (module substitution))
   (InstanceExpression node (module substitution))
   (QualifiedNameExpression node (module name))
   (InstanceQualifiedExpression node (instance name))
   (JunctionExpression node (operator body))
   (Substitution node (name value))
   (AssumptionDeclaration node (name body))
   (TheoremDeclaration node (name body))
   (UseHideDeclaration node (item))
   (Separator node ()) (EmptyLine node ())
   (Expression node (left operator right))
   (IfExpression node (condition consequent alternative))
   (ChooseExpression node (name domain predicate))
   (QuantifiedExpression node (quantifier name domain predicate))
   (LetExpression node (definition body))
   (LocalDefinition node (name parameter body))
   (CaseExpression node (arm other)) (CaseArm node (condition result))
   (PrefixExpression node (operator operand))
   (PostfixExpression node (operand operator))
   (FunctionApplication node (function argument))
   (OperatorApplication node (operator argument))
   (RecordFieldExpression node (record field))
   (FunctionConstructor node (name domain body))
   (FunctionSetExpression node (domain codomain))
   (ExceptExpression node (base update))
   (ExceptUpdate node (path value))
   (ExceptIndex node (index)) (ExceptField node (name))
   (AtExpression node ())
   (RecordExpression node (name value))
   (RecordSetExpression node (name domain))
   (SetFilterExpression node (name domain predicate))
   (SetMapExpression node (body name domain))
   (TemporalSubscriptExpression node (action subscript))
   (GroupedExpression node (expression))
   (TupleExpression node (item)) (SetExpression node (item))
   (NameExpression node (name)) (NumberExpression node (value))
   (StringExpression node (value)) (Identifier token (text))
   (Number token (text)) (String token (text))
   (HorizontalWhitespace token (text)) (Newline token (text))
   (Comment token (text)) (ModuleBorder token (text))
   (ModuleEnd token (text)) (SeparatorLine token (text))
   (Punctuation token (text)))
  (terminals
   (identifier Identifier) (number Number) (string String)
   (horizontal-whitespace HorizontalWhitespace) (newline Newline)
   (comment Comment) (module-border ModuleBorder) (module-end ModuleEnd)
   (punctuation Punctuation))
  (lexical-rules
   (horizontal-whitespace (horizontal-whitespace+)) (newline (newline+))
   (comment
    (choice (line-comment "\\*") (nested-block-comment "(*" "*)")))
   (string (quoted-string "\""))
   (module-end (character-run "=" 4))
   (module-border (character-run "-" 4))
   (number (number)) (identifier (identifier))
   (punctuation
    (literals "<=>" "|->" "[]" "<>" "=>" "~>" "==" "/\\" "\\/"
              "\\AA" "\\EE" "\\A" "\\E" "\\notin" "\\intersect"
              "\\union" "\\cap" "\\cup" "\\subseteq" "\\X" "\\in" "\\"
              "\\div" "\\leq" "\\geq"
              ".." "<<" ">>" "]_" "<=" ">=" "=<" "/=" "->" "<-"
              "#" "=" "<" ">" "+" "-" "*" "/" "%" "^" "'" "~" "@"
              "(" ")" "[" "]" "{" "}" "," ":" "!" "_" "."))
  )
  (rules
   (source-file (alias SourceFile (field module (reference module))))
   (module
    (alias Module
      (seq (token module-border) (literal "MODULE")
           (field name (token identifier)) (token module-border)
           (repeat (field item (reference module-item)))
           (token module-end))))
   (module-item
    (choice (reference extends-declaration) (reference constant-declaration)
            (reference variable-declaration) (reference operator-definition)
            (reference infix-operator-definition)
            (reference recursive-declaration) (reference instance-declaration)
            (reference assumption-declaration) (reference theorem-declaration)
            (reference use-hide-declaration) (reference separator)))
   (extends-declaration
    (alias ExtendsDeclaration
      (seq (literal "EXTENDS") (field module (token identifier))
           (repeat (seq (literal ",") (field module (token identifier)))))))
   (constant-declaration
    (alias ConstantDeclaration
      (seq (choice (literal "CONSTANT") (literal "CONSTANTS"))
           (field name (token identifier))
           (optional
            (seq (literal "(") (field parameter (literal "_"))
                 (repeat (seq (literal ",")
                              (field parameter (literal "_"))))
                 (literal ")")))
           (repeat
            (seq (literal ",") (field name (token identifier))
                 (optional
                  (seq (literal "(") (field parameter (literal "_"))
                       (repeat (seq (literal ",")
                                    (field parameter (literal "_"))))
                       (literal ")"))))))))
   (variable-declaration
    (alias VariableDeclaration
      (seq (choice (literal "VARIABLE") (literal "VARIABLES"))
           (field name (token identifier))
           (repeat (seq (literal ",") (field name (token identifier)))))))
   (operator-definition
    (alias OperatorDefinition
      (seq (optional (literal "LOCAL")) (field name (token identifier))
           (optional
            (seq (literal "(") (field parameter (token identifier))
                 (repeat (seq (literal ",") (field parameter (token identifier))))
                 (literal ")")))
           (literal "==")
           (field body (choice (reference instance-expression)
                               (reference expression))))))
   (infix-operator-definition
    (alias OperatorDefinition
     (seq (optional (literal "LOCAL"))
          (field parameter (token identifier))
          (field name
                 (choice (literal "+") (literal "-") (literal "*")
                         (literal "/") (literal "%") (literal "\\div")
                         (literal "^") (literal "..")
                         (literal "<") (literal ">")
                         (literal "<=") (literal ">=")
                         (literal "\\leq") (literal "\\geq")))
          (field parameter (token identifier))
          (literal "==") (field body (reference expression)))))
   (recursive-declaration
    (alias RecursiveDeclaration
     (seq (literal "RECURSIVE")
          (field name (token identifier))
          (optional
           (seq (literal "(") (field parameter (token identifier))
                (repeat (seq (literal ",")
                             (field parameter (token identifier))))
                (literal ")")))
          (repeat (seq (literal ",") (field name (token identifier)))))))
   (instance-declaration
    (alias InstanceDeclaration
     (seq (optional (literal "LOCAL")) (literal "INSTANCE")
          (field module (token identifier))
          (optional
           (seq (literal "WITH")
                (field substitution (reference substitution))
                (repeat
                 (seq (literal ",")
                      (field substitution (reference substitution)))))))))
   (substitution
    (alias Substitution
     (seq (field name (token identifier)) (literal "<-")
          (field value (reference expression)))))
   (instance-expression
    (alias InstanceExpression
     (seq (literal "INSTANCE") (field module (token identifier))
          (optional
           (seq (literal "WITH")
                (field substitution (reference substitution))
                (repeat (seq (literal ",")
                             (field substitution (reference substitution)))))))))
   ;; Each list item is governed by the parser's source-column reference.
   (junction-expression
    (choice
     (alias JunctionExpression
      (seq (field operator (layout-start "/\\"))
           (field body (reference expression))
           (repeat (seq (field operator (layout-next "/\\"))
                        (field body (reference expression))))
           (layout-end)))
     (alias JunctionExpression
      (seq (field operator (layout-start "\\/"))
           (field body (reference expression))
           (repeat (seq (field operator (layout-next "\\/"))
                        (field body (reference expression))))
           (layout-end)))))
   (assumption-declaration
    (alias AssumptionDeclaration
     (seq (choice (literal "ASSUME") (literal "ASSUMPTION")
                  (literal "AXIOM"))
          (optional
           (seq (field name (token identifier)) (literal "==")))
          (field body (reference expression)))))
   (theorem-declaration
    (alias TheoremDeclaration
      (seq (choice (literal "THEOREM") (literal "PROPOSITION"))
           (optional
            (seq (field name (token identifier)) (literal "==")))
           (field body (reference expression)))))
   (use-hide-declaration
    (alias UseHideDeclaration
     (seq (choice (literal "USE") (literal "HIDE"))
          (field item (reference expression))
          (repeat (seq (literal ",") (field item (reference expression)))))))
   (separator (alias Separator (token module-border)))
   (expression
    (choice
     (prec right 10
      (alias Expression
       (seq (field left (reference expression))
            (field operator (choice (literal "<=>") (literal "=>")))
            (field right (reference expression)))))
     (prec none 15
      (alias Expression
       (seq (field left (reference expression))
            (field operator (literal "~>"))
            (field right (reference expression)))))
     (prec left 20
      (alias Expression
       (seq (field left (reference expression))
            (field operator (literal "\\/"))
            (field right (reference expression)))))
     (prec left 30
      (alias Expression
       (seq (field left (reference expression))
            (field operator (literal "/\\"))
            (field right (reference expression)))))
     (prec none 40
      (alias Expression
       (seq (field left (reference expression))
            (field operator
             (choice (literal "=") (literal "#") (literal "/=")
                     (literal "<") (literal ">")
                     (literal "<=") (literal "=<") (literal ">=")
                     (literal "\\leq") (literal "\\geq")
                     (literal "\\in")
                     (literal "\\notin") (literal "\\subseteq")))
            (field right (reference expression)))))
     ;; Set union and intersection share precedence 8 in TLA+ and associate
     ;; to the left. Their ASCII aliases are spellings of the same operators.
     (prec left 45
      (alias Expression
       (seq (field left (reference expression))
            (field operator
             (choice (literal "\\cup") (literal "\\union")
                     (literal "\\cap") (literal "\\intersect")
                     (literal "\\")))
            (field right (reference expression)))))
     (prec left 50
      (alias Expression
       (seq (field left (reference expression)) (field operator (literal ".."))
            (field right (reference expression)))))
     (prec left 60
      (alias Expression
       (seq (field left (reference expression))
            (field operator (literal "+"))
            (field right (reference expression)))))
     ;; SANY's pinned operator table gives subtraction a tighter level than
     ;; addition; A + B - C must group as A + (B - C).
     (prec left 65
      (alias Expression
       (seq (field left (reference expression))
            (field operator (literal "-"))
            (field right (reference expression)))))
     (prec left 70
      (alias Expression
       (seq (field left (reference expression))
            (field operator (choice (literal "*") (literal "/")
                                    (literal "%") (literal "\\div")
                                    (literal "\\X")))
            (field right (reference expression)))))
     (prec right 80
      (alias Expression
       (seq (field left (reference expression)) (field operator (literal "^"))
            (field right (reference expression)))))
     (prec right 90 (reference prefix-expression))
     (prec left 100 (reference postfix-expression))
     (prec left 110 (reference function-application))
     (prec left 110 (reference operator-application))
     (prec left 110 (reference record-field-expression))
     (reference if-expression)
     ;; TLA+ binders and LET extend as far right as possible. Their low
     ;; precedence lets comparison and application shifts remain inside the
     ;; predicate/body instead of also completing an outer expression tree.
     (prec right 1 (reference choose-expression))
     (prec right 1 (reference quantified-expression))
     (prec right 1 (reference let-expression))
     (reference case-expression) (reference function-constructor)
     (reference function-set-expression)
     (reference except-expression)
     (reference record-expression) (reference set-filter-expression)
     (reference record-set-expression)
     (reference set-map-expression)
     (reference temporal-subscript-expression)
     (reference grouped-expression) (reference tuple-expression)
     (reference set-expression) (reference name-expression)
     (reference at-expression)
     (reference junction-expression)
     (reference qualified-name-expression)
     (prec left 120 (reference instance-qualified-expression))
     (reference number-expression) (reference string-expression)))
   (if-expression
    (prec right 1
     (alias IfExpression
      (seq (literal "IF") (field condition (reference expression))
           (literal "THEN") (field consequent (reference expression))
           (literal "ELSE")
           (field alternative (reference expression))))))
   (choose-expression
    (prec right 1
     (alias ChooseExpression
      (seq (literal "CHOOSE") (field name (token identifier))
           (optional (seq (literal "\\in")
                          (field domain (reference expression))))
           (literal ":") (field predicate (reference expression))))))
   (quantified-expression
    (prec right 1
     (alias QuantifiedExpression
      (seq (field quantifier
                  (choice (literal "\\A") (literal "\\E")
                          (literal "\\AA") (literal "\\EE")))
           (field name (token identifier))
           (optional (seq (literal "\\in")
                          (field domain (reference expression))))
           (repeat
            (seq (literal ",") (field name (token identifier))
                 (optional (seq (literal "\\in")
                                (field domain (reference expression))))))
           (literal ":")
           (field predicate (reference expression))))))
   (let-expression
    (prec right 1
     (alias LetExpression
      (seq (literal "LET") (field definition (reference local-definition))
           (literal "IN") (field body (reference expression))))))
   (local-definition
    (alias LocalDefinition
     (seq (field name (token identifier))
          (optional
           (seq (literal "(") (field parameter (token identifier))
                (repeat (seq (literal ",")
                             (field parameter (token identifier))))
                (literal ")")))
          (literal "==") (field body (reference expression)))))
   (case-expression
    (alias CaseExpression
     (seq (literal "CASE") (field arm (reference case-arm))
          (repeat (seq (literal "[]") (field arm (reference case-arm))))
          (optional
           (seq (literal "[]") (literal "OTHER") (literal "->")
                (field other (reference expression)))))))
   (case-arm
    (alias CaseArm
     (seq (field condition (reference expression)) (literal "->")
          (field result (reference expression)))))
   (prefix-expression
    (prec right 90
     (alias PrefixExpression
      (seq (field operator
                  (choice (literal "~") (literal "-") (literal "[]")
                          (literal "<>") (literal "ENABLED")
                          (literal "UNCHANGED") (literal "SUBSET")
                          (literal "UNION") (literal "DOMAIN")))
           (field operand (reference expression))))))
   (postfix-expression
    (prec left 100
     (alias PostfixExpression
      (seq (field operand (reference expression))
           (field operator (literal "'"))))))
   (function-application
    (prec left 110
     (alias FunctionApplication
      (seq (field function (reference expression)) (literal "[")
           (field argument (reference expression))
           (repeat (seq (literal ",")
                        (field argument (reference expression))))
           (literal "]")))))
   (operator-application
    (prec left 110
     (alias OperatorApplication
      (seq (field operator
                  (choice (token identifier)
                          (reference qualified-name-expression)))
           (literal "(")
           (optional (field argument (reference expression)))
           (repeat (seq (literal ",")
                        (field argument (reference expression))))
           (literal ")")))))
   (record-field-expression
    (prec left 110
     (alias RecordFieldExpression
      (seq (field record (reference expression)) (literal ".")
           (field field (token identifier))))))
   (function-constructor
    (alias FunctionConstructor
     (seq (literal "[") (field name (token identifier)) (literal "\\in")
          (field domain (reference expression)) (literal "|->")
          (field body (reference expression)) (literal "]"))))
   (function-set-expression
    (alias FunctionSetExpression
     (seq (literal "[") (field domain (reference expression))
          (literal "->") (field codomain (reference expression))
          (literal "]"))))
   (except-expression
    (alias ExceptExpression
     (seq (literal "[") (field base (reference expression))
          (literal "EXCEPT") (field update (reference except-update))
          (repeat (seq (literal ",")
                       (field update (reference except-update))))
          (literal "]"))))
   (except-update
    (alias ExceptUpdate
     (seq (literal "!") (field path (reference except-path))
          (repeat (field path (reference except-path)))
          (literal "=") (field value (reference expression)))))
   (except-path
    (choice
     (alias ExceptIndex
      (seq (literal "[") (field index (reference expression))
           (literal "]")))
     (alias ExceptField
      (seq (literal ".") (field name (token identifier))))))
   (at-expression (alias AtExpression (literal "@")))
   (record-expression
    (alias RecordExpression
     (seq (literal "[") (field name (token identifier)) (literal "|->")
          (field value (reference expression))
          (repeat
           (seq (literal ",") (field name (token identifier))
                (literal "|->") (field value (reference expression))))
          (literal "]"))))
   (record-set-expression
    (alias RecordSetExpression
     (seq (literal "[") (field name (token identifier)) (literal ":")
          (field domain (reference expression))
          (repeat
           (seq (literal ",") (field name (token identifier))
                (literal ":") (field domain (reference expression))))
          (literal "]"))))
   (set-filter-expression
    (alias SetFilterExpression
     (seq (literal "{") (field name (token identifier)) (literal "\\in")
          (field domain (reference expression)) (literal ":")
          (field predicate (reference expression)) (literal "}"))))
   (set-map-expression
    (alias SetMapExpression
     (seq (literal "{") (field body (reference expression)) (literal ":")
          (field name (token identifier)) (literal "\\in")
          (field domain (reference expression)) (literal "}"))))
   (temporal-subscript-expression
    (alias TemporalSubscriptExpression
     (seq (literal "[") (field action (reference expression)) (literal "]_")
          ;; SANY's ReducedExpression admits delimited expressions and names.
          (field subscript
                 (choice (reference name-expression)
                         (reference tuple-expression)
                         (reference grouped-expression))))))
   (grouped-expression
    (alias GroupedExpression
     (seq (literal "(") (field expression (reference expression))
          (literal ")"))))
   (tuple-expression
    (alias TupleExpression
     (seq (literal "<<") (optional (field item (reference expression)))
          (repeat (seq (literal ",") (field item (reference expression))))
          (literal ">>"))))
   (set-expression
    (alias SetExpression
     (seq (literal "{") (optional (field item (reference expression)))
          (repeat (seq (literal ",") (field item (reference expression))))
          (literal "}"))))
   (name-expression
    (alias NameExpression (field name (token identifier))))
   (qualified-name-expression
    (alias QualifiedNameExpression
     (seq (field module (token identifier))
          (repeat1
           (seq (literal "!")
                (field name
                       (choice (token identifier) (literal "+")
                               (literal "-") (literal "*") (literal "/")
                               (literal "%") (literal "\\div")
                               (literal "^") (literal "..")
                               (literal "\\leq") (literal "\\geq"))))))))
   (instance-qualified-expression
    (prec left 120
     (alias InstanceQualifiedExpression
      (seq (field instance (reference operator-application))
           (literal "!") (field name (token identifier))
           (repeat (seq (literal "!")
                        (field name (token identifier))))))))
   (number-expression
    (alias NumberExpression (field value (token number))))
   (string-expression
    (alias StringExpression (field value (token string)))))
  ;; Native TLA+ declarations are delimited by grammar, not physical lines.
  ;; Newlines remain lossless trivia; no downstream source normalization.
  (extras horizontal-whitespace newline comment)
  (keywords (module "MODULE") (extends "EXTENDS") (constant "CONSTANT")
            (constants "CONSTANTS") (variable "VARIABLE")
            (variables "VARIABLES") (theorem "THEOREM")
            (if "IF") (then "THEN") (else "ELSE") (choose "CHOOSE")
            (let "LET") (in "IN") (case "CASE") (other "OTHER")
            (enabled "ENABLED") (unchanged "UNCHANGED")
            (subset "SUBSET") (union "UNION") (domain "DOMAIN")
            (local "LOCAL") (recursive "RECURSIVE")
            (instance "INSTANCE") (with "WITH")
            (except "EXCEPT")
            (assume "ASSUME") (assumption "ASSUMPTION") (axiom "AXIOM")
            (proposition "PROPOSITION") (use "USE") (hide "HIDE"))
  (parser-entrypoints (source-file parse pure))
  (recoveries (module "GERBIL-PARSER-TLA-PLUS-SANY-CANDIDATE" preserve-source))
  (conflicts selective-glr)
  (case-insensitive #f)
  (flow (source lexical) (lexical tla-plus-module) (tla-plus-module cst)))
