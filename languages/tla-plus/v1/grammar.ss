;;; -*- Gerbil -*-
;;; Native TLA+ core syntax compiled by the language-neutral engine.

(import :gerbil-parser/src/language/grammar ./source)
(export (import: ./source)
        +tla-plus-contract-version+ +tla-plus-syntax-contract+
        +tla-plus-examples-commit+ tla-plus-v1-language-grammar
        tla-plus-v1-grammar tla-plus-v1-parser-ir tla-plus-v1-parser)

(def +tla-plus-contract-version+ "v1")
(def +tla-plus-syntax-contract+ "tla-plus.native-core.v1")
(def +tla-plus-examples-commit+ "ceeaa904140e3e03781cb2a79cd6c6d8b8b08e10")

(deflanguage-grammar tla-plus-v1
  (identity "tla-plus" +tla-plus-contract-version+ +tla-plus-syntax-contract+)
  (syntax-kinds
   (SourceFile node (module)) (Module node (name item))
   (ExtendsDeclaration node (module)) (ConstantDeclaration node (name))
   (VariableDeclaration node (name))
   (OperatorDefinition node (name parameter body))
   (RecursiveDeclaration node (name parameter))
   (InstanceDeclaration node (module substitution))
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
   (FunctionConstructor node (name domain body))
   (RecordExpression node (name value))
   (SetFilterExpression node (name domain predicate))
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
   (separator-line SeparatorLine) (punctuation Punctuation))
  (lexical-rules
   (horizontal-whitespace (horizontal-whitespace+)) (newline (newline+))
   (comment
    (choice (line-comment "\\*") (nested-block-comment "(*" "*)")))
   (string (quoted-string "\""))
   (module-end (literals "==============================================================" "===="))
   (separator-line (literals "--------------------------------------------------------------"))
   (module-border (literals "----------------------" "----"))
   (number (number)) (identifier (identifier))
   (punctuation
    (literals "<=>" "|->" "[]" "<>" "=>" "==" "/\\" "\\/"
              "\\AA" "\\EE" "\\A" "\\E" "\\notin" "\\in"
              ".." "<<" ">>" "<=" ">=" "->" "<-"
              "#" "=" "<" ">" "+" "-" "*" "/" "^" "'"
              "(" ")" "[" "]" "{" "}" "," ":" "!" "_" "."))
  )
  (rules
   (source-file (alias SourceFile (field module (reference module))))
   (module
    (alias Module
      (seq (token module-border) (literal "MODULE")
           (field name (token identifier)) (token module-border) (token newline)
           (repeat (field item (reference module-item)))
           (token module-end) (optional (token newline)))))
   (module-item
    (choice (reference extends-declaration) (reference constant-declaration)
            (reference variable-declaration) (reference operator-definition)
            (reference recursive-declaration) (reference instance-declaration)
            (reference assumption-declaration) (reference theorem-declaration)
            (reference use-hide-declaration) (reference separator)
            (reference empty-line)))
   (extends-declaration
    (alias ExtendsDeclaration
      (seq (literal "EXTENDS") (field module (token identifier))
           (repeat (seq (literal ",") (field module (token identifier))))
           (token newline))))
   (constant-declaration
    (alias ConstantDeclaration
      (seq (choice (literal "CONSTANT") (literal "CONSTANTS"))
           (field name (token identifier))
           (repeat (seq (literal ",") (field name (token identifier))))
           (token newline))))
   (variable-declaration
    (alias VariableDeclaration
      (seq (choice (literal "VARIABLE") (literal "VARIABLES"))
           (field name (token identifier))
           (repeat (seq (literal ",") (field name (token identifier))))
           (token newline))))
   (operator-definition
    (alias OperatorDefinition
      (seq (optional (literal "LOCAL")) (field name (token identifier))
           (optional
            (seq (literal "(") (field parameter (token identifier))
                 (repeat (seq (literal ",") (field parameter (token identifier))))
                 (literal ")")))
           (literal "==") (field body (reference expression))
           (token newline))))
   (recursive-declaration
    (alias RecursiveDeclaration
     (seq (literal "RECURSIVE")
          (field name (token identifier))
          (optional
           (seq (literal "(") (field parameter (token identifier))
                (repeat (seq (literal ",")
                             (field parameter (token identifier))))
                (literal ")")))
          (repeat (seq (literal ",") (field name (token identifier))))
          (token newline))))
   (instance-declaration
    (alias InstanceDeclaration
     (seq (optional (literal "LOCAL")) (literal "INSTANCE")
          (field module (token identifier))
          (optional
           (seq (literal "WITH")
                (field substitution (reference substitution))
                (repeat
                 (seq (literal ",")
                      (field substitution (reference substitution))))))
          (token newline))))
   (substitution
    (alias Substitution
     (seq (field name (token identifier)) (literal "<-")
          (field value (reference expression)))))
   (assumption-declaration
    (alias AssumptionDeclaration
     (seq (choice (literal "ASSUME") (literal "ASSUMPTION")
                  (literal "AXIOM"))
          (optional
           (seq (field name (token identifier)) (literal "==")))
          (field body (reference expression)) (token newline))))
   (theorem-declaration
    (alias TheoremDeclaration
      (seq (choice (literal "THEOREM") (literal "PROPOSITION"))
           (optional
            (seq (field name (token identifier)) (literal "==")))
           (field body (reference expression))
           (token newline))))
   (use-hide-declaration
    (alias UseHideDeclaration
     (seq (choice (literal "USE") (literal "HIDE"))
          (field item (reference expression))
          (repeat (seq (literal ",") (field item (reference expression))))
          (token newline))))
   (separator (alias Separator (seq (token separator-line) (token newline))))
   (empty-line (alias EmptyLine (token newline)))
   (expression
    (choice
     (prec right 10
      (alias Expression
       (seq (field left (reference expression))
            (field operator (choice (literal "<=>") (literal "=>")))
            (field right (reference expression)))))
     (prec left 20
      (alias Expression
       (seq (field left (reference expression)) (field operator (literal "\\/"))
            (field right (reference expression)))))
     (prec left 30
      (alias Expression
       (seq (field left (reference expression)) (field operator (literal "/\\"))
            (field right (reference expression)))))
     (prec none 40
      (alias Expression
       (seq (field left (reference expression))
            (field operator
             (choice (literal "=") (literal "#") (literal "<") (literal ">")
                     (literal "<=") (literal ">=") (literal "\\in")
                     (literal "\\notin")))
            (field right (reference expression)))))
     (prec left 50
      (alias Expression
       (seq (field left (reference expression)) (field operator (literal ".."))
            (field right (reference expression)))))
     (prec left 60
      (alias Expression
       (seq (field left (reference expression))
            (field operator (choice (literal "+") (literal "-")))
            (field right (reference expression)))))
     (prec left 70
      (alias Expression
       (seq (field left (reference expression))
            (field operator (choice (literal "*") (literal "/")))
            (field right (reference expression)))))
     (prec right 80
      (alias Expression
       (seq (field left (reference expression)) (field operator (literal "^"))
            (field right (reference expression)))))
     (prec right 90 (reference prefix-expression))
     (prec left 100 (reference postfix-expression))
     (prec left 110 (reference function-application))
     (prec left 110 (reference operator-application))
     (reference if-expression) (reference choose-expression)
     (reference quantified-expression) (reference let-expression)
     (reference case-expression) (reference function-constructor)
     (reference record-expression) (reference set-filter-expression)
     (reference temporal-subscript-expression)
     (reference grouped-expression) (reference tuple-expression)
     (reference set-expression) (reference name-expression)
     (reference number-expression) (reference string-expression)))
   (if-expression
    (alias IfExpression
     (seq (literal "IF") (field condition (reference expression))
          (literal "THEN") (field consequent (reference expression))
          (literal "ELSE") (field alternative (reference expression)))))
   (choose-expression
    (alias ChooseExpression
     (seq (literal "CHOOSE") (field name (token identifier))
          (optional (seq (literal "\\in")
                         (field domain (reference expression))))
          (literal ":") (field predicate (reference expression)))))
   (quantified-expression
    (alias QuantifiedExpression
     (seq (field quantifier
                 (choice (literal "\\A") (literal "\\E")
                         (literal "\\AA") (literal "\\EE")))
          (field name (token identifier))
          (optional (seq (literal "\\in")
                         (field domain (reference expression))))
          (literal ":") (field predicate (reference expression)))))
   (let-expression
    (alias LetExpression
     (seq (literal "LET") (field definition (reference local-definition))
          (literal "IN") (field body (reference expression)))))
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
    (alias PrefixExpression
     (seq (field operator
                 (choice (literal "~") (literal "-") (literal "[]")
                         (literal "<>") (literal "ENABLED")
                         (literal "UNCHANGED") (literal "SUBSET")
                         (literal "UNION") (literal "DOMAIN")))
          (field operand (reference expression)))))
   (postfix-expression
    (alias PostfixExpression
     (seq (field operand (reference expression))
          (field operator (literal "'")))))
   (function-application
    (alias FunctionApplication
     (seq (field function (reference expression)) (literal "[")
          (field argument (reference expression))
          (repeat (seq (literal ",")
                       (field argument (reference expression))))
          (literal "]"))))
   (operator-application
    (alias OperatorApplication
     (seq (field operator (reference expression)) (literal "(")
          (optional (field argument (reference expression)))
          (repeat (seq (literal ",")
                       (field argument (reference expression))))
          (literal ")"))))
   (function-constructor
    (alias FunctionConstructor
     (seq (literal "[") (field name (token identifier)) (literal "\\in")
          (field domain (reference expression)) (literal "|->")
          (field body (reference expression)) (literal "]"))))
   (record-expression
    (alias RecordExpression
     (seq (literal "[") (field name (token identifier)) (literal "|->")
          (field value (reference expression))
          (repeat
           (seq (literal ",") (field name (token identifier))
                (literal "|->") (field value (reference expression))))
          (literal "]"))))
   (set-filter-expression
    (alias SetFilterExpression
     (seq (literal "{") (field name (token identifier)) (literal "\\in")
          (field domain (reference expression)) (literal ":")
          (field predicate (reference expression)) (literal "}"))))
   (temporal-subscript-expression
    (alias TemporalSubscriptExpression
     (seq (literal "[") (field action (reference expression)) (literal "]")
          ;; SANY lexes the lossless `_name` spelling as one identifier.
          (field subscript (token identifier)))))
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
   (number-expression
    (alias NumberExpression (field value (token number))))
   (string-expression
    (alias StringExpression (field value (token string)))))
  (extras horizontal-whitespace comment)
  (keywords (module "MODULE") (extends "EXTENDS") (constant "CONSTANT")
            (constants "CONSTANTS") (variable "VARIABLE")
            (variables "VARIABLES") (theorem "THEOREM")
            (if "IF") (then "THEN") (else "ELSE") (choose "CHOOSE")
            (let "LET") (in "IN") (case "CASE") (other "OTHER")
            (enabled "ENABLED") (unchanged "UNCHANGED")
            (subset "SUBSET") (union "UNION") (domain "DOMAIN")
            (local "LOCAL") (recursive "RECURSIVE")
            (instance "INSTANCE") (with "WITH")
            (assume "ASSUME") (assumption "ASSUMPTION") (axiom "AXIOM")
            (proposition "PROPOSITION") (use "USE") (hide "HIDE"))
  (parser-entrypoints (source-file parse pure))
  (recoveries (module "GERBIL-PARSER-TLA-PLUS-V1" preserve-source))
  (conflicts selective-glr)
  (case-insensitive #f)
  (flow (source lexical) (lexical tla-plus-module) (tla-plus-module cst)))
