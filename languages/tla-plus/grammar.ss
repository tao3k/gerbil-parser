;;; -*- Gerbil -*-
;;; Language declarations, source identity and fixtures; execution is engine-owned.
(import (only-in :gerbil-parser/language-support deflanguage deftext-profile defsyntax-corpus syntax-fixture-expected-status)
        (only-in :gerbil-parser/language-support/grammar-source defsyntax-javacc-source)
        (only-in :gerbil-parser/src/language/descriptor language-grammar-language language-grammar-version language-grammar-contract))
(export tla-proof-name tla-proof-start tla-proof-reference tla-identifier +tla-plus-syntax-source+ +tla-plus-examples-commit+ +tla-plus-sany-release+ +tla-plus-sany-commit+ +tla-plus-sany-grammar-blob+ +tla-plus-sany-grammar-digest+ tla-plus-sany-source tla-plus-core-fixtures tla-plus-core-accepted-fixtures tla-plus-core-rejected-fixtures tla-plus-core-language-grammar tla-plus-core-grammar tla-plus-core-parser-ir tla-plus-core-parser tla-plus-layout-language-grammar tla-plus-layout-grammar tla-plus-layout-parser-ir tla-plus-layout-parser tla-plus-sany-candidate-language-grammar tla-plus-sany-candidate-grammar tla-plus-sany-candidate-parser-ir tla-plus-sany-candidate-parser)
;;; Local recognition is canonical lexical data, including its spelling.
(deftext-profile tla-proof-name
  (seq (literal "<")
       (if-next (characters "+*") (run (characters "+*") 1 1) (run (numeric) 1 #f))
       (literal ">")
       (if-next (union (alphabetic) (numeric) (characters "_"))
         (run (union (alphabetic) (numeric) (characters "_")) 1 #f)
         (optional (run (characters "*-") 1 1)))
       (run (characters ".") 0 #f)))
(deftext-profile tla-proof-start
  (ends-not-in (union (alphabetic) (numeric) (characters "_")) (ref tla-proof-name)))
(deftext-profile tla-proof-reference
  (ends-in (union (alphabetic) (numeric) (characters "_")) (ref tla-proof-name)))
(deftext-profile tla-identifier
  (run-containing (union (alphabetic) (numeric) (characters "_"))
                  (union (alphabetic) (characters "_")) 1 #f))
(def +tla-plus-syntax-source+ "Specifying Systems, Chapter 15: TLAPlusGrammar")
(def +tla-plus-examples-commit+ "ceeaa904140e3e03781cb2a79cd6c6d8b8b08e10")
(def +tla-plus-sany-release+ "v1.7.4")
(def +tla-plus-sany-commit+ "5a47802b5c391f59ecdd44117981f4ff8c0656ba")
(def +tla-plus-sany-grammar-blob+ "bf9e7acb5337f4b6c2a4d6a973a1a65c95e72f56")
(def +tla-plus-sany-grammar-digest+
  "sha256:15edd079cf16cf91556ba66b496c9ffc16f26ab30856753ed58e60b0d54f2d07")

(defsyntax-javacc-source tla-plus-sany-source
  (identity "tla-plus" "v1.7.4"
            "5a47802b5c391f59ecdd44117981f4ff8c0656ba")
  (digest
   "sha256:15edd079cf16cf91556ba66b496c9ffc16f26ab30856753ed58e60b0d54f2d07")
  (source "grammar-source/tla+.jj"))

(deflanguage tla-plus-core
  (identity "tla-plus" "v1" "tla-plus.native-core.v1")

  (root source-file)
  (lex
   (horizontal-whitespace HorizontalWhitespace (horizontal-whitespace+))
   (newline Newline (newline+))
   (comment Comment
    (choice (line-comment "\\*") (nested-block-comment "(*" "*)")))
   (string String (quoted-string "\""))
   (module-end ModuleEnd (literals "==============================================================" "===="))
   (separator-line SeparatorLine (literals "--------------------------------------------------------------"))
   (module-border ModuleBorder (literals "----------------------" "----"))
   (number Number (number))
   (identifier Identifier (identifier))
   (punctuation Punctuation
    (literals "<=>" "|->" "[]" "<>" "=>" "==" "/\\" "\\/"
              "\\AA" "\\EE" "\\A" "\\E" "\\notin" "\\intersect"
              "\\union" "\\cap" "\\cup" "\\subseteq" "\\X" "\\in" "\\"
              ".." "<<" ">>" "<=" ">=" "->" "<-"
              "#" "=" "<" ">" "+" "-" "*" "/" "^" "'" "~"
              "(" ")" "[" "]" "{" "}" "," ":" "!" "_" ".")))
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
           (repeat (seq (literal ",") (field name (token identifier)))))))
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
                               (reference junction-expression)
                               (reference expression))))))
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
   ;; Flat junction lists retain their leading marker in the native CST.
   ;; Nested indentation-sensitive junction lists are not admitted here.
   (junction-expression
    (alias JunctionExpression
     (seq (field operator (choice (literal "/\\") (literal "\\/")))
          (field body (reference expression)))))
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
   (separator (alias Separator (token separator-line)))
   (expression
    (choice
     (prec right 10
      (alias Expression
       (seq (field left (reference expression))
            (field operator (choice (literal "<=>") (literal "=>")))
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
             (choice (literal "=") (literal "#") (literal "<") (literal ">")
                     (literal "<=") (literal ">=") (literal "\\in")
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
            (field operator (choice (literal "+") (literal "-")))
            (field right (reference expression)))))
     (prec left 70
      (alias Expression
       (seq (field left (reference expression))
            (field operator (choice (literal "*") (literal "/")
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
     (reference if-expression)
     ;; TLA+ binders and LET extend as far right as possible. Their low
     ;; precedence lets comparison and application shifts remain inside the
     ;; predicate/body instead of also completing an outer expression tree.
     (prec right 1 (reference choose-expression))
     (prec right 1 (reference quantified-expression))
     (prec right 1 (reference let-expression))
     (reference case-expression) (reference function-constructor)
     (reference record-expression) (reference set-filter-expression)
     (reference set-map-expression)
     (reference temporal-subscript-expression)
     (reference grouped-expression) (reference tuple-expression)
     (reference set-expression) (reference name-expression)
     (reference qualified-name-expression)
     (reference number-expression) (reference string-expression)))
   (if-expression
    (alias IfExpression
     (seq (literal "IF") (field condition (reference expression))
          (literal "THEN") (field consequent (reference expression))
          (literal "ELSE") (field alternative (reference expression)))))
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
           (literal ":") (field predicate (reference expression))))))
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
      (seq (field operator (reference expression)) (literal "(")
           (optional (field argument (reference expression)))
           (repeat (seq (literal ",")
                        (field argument (reference expression))))
           (literal ")")))))
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
   (set-map-expression
    (alias SetMapExpression
     (seq (literal "{") (field body (reference expression)) (literal ":")
          (field name (token identifier)) (literal "\\in")
          (field domain (reference expression)) (literal "}"))))
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
   (qualified-name-expression
    (alias QualifiedNameExpression
     (seq (field module (token identifier))
          (repeat1 (seq (literal "!") (field name (token identifier)))))))
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
            (assume "ASSUME") (assumption "ASSUMPTION") (axiom "AXIOM")
            (proposition "PROPOSITION") (use "USE") (hide "HIDE"))

  (recoveries (module "GERBIL-PARSER-TLA-PLUS-V1" preserve-source))
  (conflicts selective-glr)
  (case-insensitive #f)
  (flow (source lexical) (lexical tla-plus-module) (tla-plus-module cst))
  (catalog
   (syntax-kinds
    (SourceFile node (module)) (Module node (name item))
    (ExtendsDeclaration node (module)) (ConstantDeclaration node (name))
    (VariableDeclaration node (name))
    (OperatorDefinition node (name parameter body))
    (RecursiveDeclaration node (name parameter))
    (InstanceDeclaration node (module substitution))
    (InstanceExpression node (module substitution))
    (QualifiedNameExpression node (module name))
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
    (FunctionConstructor node (name domain body))
    (RecordExpression node (name value))
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
    (separator-line SeparatorLine) (punctuation Punctuation))))

(deflanguage tla-plus-layout
  (identity "tla-plus" "v2" "tla-plus.native-layout.v2")

  (root source-file)
  (lex
   (horizontal-whitespace HorizontalWhitespace (horizontal-whitespace+))
   (newline Newline (newline+))
   (comment Comment
    (choice (line-comment "\\*") (nested-block-comment "(*" "*)")))
   (string String (quoted-string "\""))
   (module-end ModuleEnd (literals "==============================================================" "===="))
   (separator-line SeparatorLine (literals "--------------------------------------------------------------"))
   (module-border ModuleBorder (literals "----------------------" "----"))
   (number Number (number))
   (identifier Identifier (identifier))
   (punctuation Punctuation
    (literals "<=>" "|->" "[]" "<>" "=>" "==" "/\\" "\\/"
              "\\AA" "\\EE" "\\A" "\\E" "\\notin" "\\intersect"
              "\\union" "\\cap" "\\cup" "\\subseteq" "\\X" "\\in" "\\"
              ".." "<<" ">>" "<=" ">=" "->" "<-"
              "#" "=" "<" ">" "+" "-" "*" "/" "^" "'" "~"
              "(" ")" "[" "]" "{" "}" "," ":" "!" "_" ".")))
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
           (repeat (seq (literal ",") (field name (token identifier)))))))
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
   (separator (alias Separator (token separator-line)))
   (expression
    (choice
     (prec right 10
      (alias Expression
       (seq (field left (reference expression))
            (field operator (choice (literal "<=>") (literal "=>")))
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
             (choice (literal "=") (literal "#") (literal "<") (literal ">")
                     (literal "<=") (literal ">=") (literal "\\in")
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
            (field operator (choice (literal "+") (literal "-")))
            (field right (reference expression)))))
     (prec left 70
      (alias Expression
       (seq (field left (reference expression))
            (field operator (choice (literal "*") (literal "/")
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
     (reference if-expression)
     ;; TLA+ binders and LET extend as far right as possible. Their low
     ;; precedence lets comparison and application shifts remain inside the
     ;; predicate/body instead of also completing an outer expression tree.
     (prec right 1 (reference choose-expression))
     (prec right 1 (reference quantified-expression))
     (prec right 1 (reference let-expression))
     (reference case-expression) (reference function-constructor)
     (reference record-expression) (reference set-filter-expression)
     (reference set-map-expression)
     (reference temporal-subscript-expression)
     (reference grouped-expression) (reference tuple-expression)
     (reference set-expression) (reference name-expression)
     (reference junction-expression)
     (reference qualified-name-expression)
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
   (set-map-expression
    (alias SetMapExpression
     (seq (literal "{") (field body (reference expression)) (literal ":")
          (field name (token identifier)) (literal "\\in")
          (field domain (reference expression)) (literal "}"))))
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
   (qualified-name-expression
    (alias QualifiedNameExpression
     (seq (field module (token identifier))
          (repeat1 (seq (literal "!") (field name (token identifier)))))))
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
            (assume "ASSUME") (assumption "ASSUMPTION") (axiom "AXIOM")
            (proposition "PROPOSITION") (use "USE") (hide "HIDE"))

  (recoveries (module "GERBIL-PARSER-TLA-PLUS-V1" preserve-source))
  (conflicts selective-glr)
  (case-insensitive #f)
  (flow (source lexical) (lexical tla-plus-module) (tla-plus-module cst))
  (catalog
   (syntax-kinds
    (SourceFile node (module)) (Module node (name item))
    (ExtendsDeclaration node (module)) (ConstantDeclaration node (name))
    (VariableDeclaration node (name))
    (OperatorDefinition node (name parameter body))
    (RecursiveDeclaration node (name parameter))
    (InstanceDeclaration node (module substitution))
    (InstanceExpression node (module substitution))
    (QualifiedNameExpression node (module name))
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
    (FunctionConstructor node (name domain body))
    (RecordExpression node (name value))
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
    (separator-line SeparatorLine) (punctuation Punctuation))))

(deflanguage tla-plus-sany-candidate
  (identity "tla-plus" "p4-draft" "tla-plus.native-sany-candidate.p4")

  (root source-file)
  (lex
   (module-text ModuleText (module-text "----" "MODULE" "====" "(*" "*)" "\\*" "_"))
   (proof-step ProofStepName (text-profile (ref tla-proof-start)))
   (proof-reference ProofReference (text-profile (ref tla-proof-reference)))
   (horizontal-whitespace HorizontalWhitespace (horizontal-whitespace+))
   (newline Newline (newline+))
   (comment Comment
    (choice (line-comment "\\*") (nested-block-comment "(*" "*)")))
   (string String (quoted-string "\""))
   (module-end ModuleEnd (character-run "=" 4))
   (module-border ModuleBorder (character-run "-" 4))
   (number Number (number))
   (identifier Identifier (text-profile (ref tla-identifier)))
   (punctuation Punctuation
    (literals "\\land" "\\lor" "\\cdot" "\\equiv" "-+->" "\\times" "..." "|" "||" "&&" "&" "$$" "$" "??" "%%" "##" "++" "--" "**" "//" "^^" "!!" "|-" "|=" "-|" "=|" "<:" ":=" "::=" "\\oplus" "\\ominus" "\\odot" "\\oslash" "\\otimes" "\\uplus" "\\sqcap" "\\sqcup" "\\wr" "\\star" "\\bigcirc" "\\bullet" "\\prec" "\\succ" "\\preceq" "\\succeq" "\\sim" "\\simeq" "\\ll" "\\gg" "\\asymp" "\\subset" "\\supset" "\\supseteq" "\\approx" "\\cong" "\\sqsubset" "\\sqsubseteq" "\\sqsupset" "\\sqsupseteq" "\\doteq" "\\propto" "\\mod" "(+)" "(-)" "(.)" "(/)" "(\\X)" "-." "^+" "^*" "^#" "\\lnot" "\\neg" "<=>" "|->" "[]" "<>" "=>" "~>" "==" "/\\" "\\/"
              "\\AA" "\\EE" "\\A" "\\E" "\\notin" "\\intersect"
              "\\union" "\\cap" "\\cup" "\\subseteq" "\\X" "\\in" "\\"
              "\\div" "\\leq" "\\geq" "\\o" "\\circ" "@@" ":>"
              ".." "<<" ">>" "]_" ">>_" "<=" ">=" "=<" "/=" "->" "<-"
              "#" "=" "<" ">" "+" "-" "*" "/" "%" "^" "'" "~" "@"
              "[]ASSUME" "[]PROVE" "::" "(" ")" "[" "]" "{" "}" "," ":" "!" "_" ".")))
  (rules
   (source-file (alias SourceFile
     (seq (optional (field text (token module-text)))
          (field module (reference module))
          (repeat (seq (optional (field text (token module-text)))
                       (field module (reference module))))
          (optional (field text (token module-text))))))
   (module
    (alias Module
      (seq (token module-border) (literal "MODULE")
           (field name (token identifier)) (token module-border)
           (repeat (field item (reference module-item)))
           (token module-end))))
   (module-item
    (choice (reference extends-declaration) (reference constant-declaration)
            (reference variable-declaration) (reference operator-definition)
            (reference function-definition)
            (reference infix-operator-definition) (reference prefix-operator-definition) (reference postfix-operator-definition)
            (reference recursive-declaration) (reference instance-declaration)
            (reference assumption-declaration) (reference theorem-declaration)
            (reference use-hide-declaration) (reference separator) (reference module)))
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
            (seq (literal "(") (field parameter (reference formal-parameter))
                 (repeat (seq (literal ",") (field parameter (reference formal-parameter))))
                 (literal ")")))
           (literal "==")
           (field body (choice (reference instance-expression)
                               (reference expression))))))
   (formal-parameter
    (choice
     (alias FormalParameter
      (seq (field name (choice (token identifier) (literal "_")))
           (optional (seq (literal "(") (field parameter (literal "_"))
                          (repeat (seq (literal ",") (field parameter (literal "_"))))
                          (literal ")")))))
     (alias FormalParameter
      (seq (literal "_") (field name (reference infix-symbol)) (literal "_")))))
   (lambda-expression
    (prec right 1
     (alias LambdaExpression
      (seq (literal "LAMBDA") (field name (token identifier))
           (repeat (seq (literal ",") (field name (token identifier))))
           (literal ":") (field body (reference expression))))))
   (label-expression
    (prec right 1
     (alias LabelExpression
      (seq (field name (reference expression))
           (literal "::") (field body (reference expression))))))
   (function-definition
    (alias FunctionDefinition
     (seq (optional (literal "LOCAL")) (field name (token identifier))
          (literal "[") (field binding (reference domain-binding))
          (repeat (seq (literal ",")
                       (field binding (reference domain-binding))))
          (literal "]") (literal "==")
          (field body (reference expression)))))
   (domain-binding
    (choice
     (alias DomainBinding
      (seq (field name (token identifier))
           (repeat (seq (literal ",") (field name (token identifier))))
           (literal "\\in") (field domain (reference expression))))
     (reference tuple-domain-binding)))
   (tuple-domain-binding
    (alias TupleBinding
     (seq (literal "<<") (field name (token identifier))
          (repeat (seq (literal ",") (field name (token identifier))))
          (literal ">>") (literal "\\in")
          (field domain (reference expression)))))
   (single-domain-binding
    (choice
     (alias DomainBinding
      (seq (field name (token identifier)) (literal "\\in")
           (field domain (reference expression))))
     (reference tuple-domain-binding)))
   (infix-operator-definition
    (alias OperatorDefinition
     (seq (optional (literal "LOCAL"))
          (field parameter (reference formal-parameter))
          (field name (reference infix-symbol))
          (field parameter (reference formal-parameter))
          (literal "==") (field body (reference expression)))))
   (infix-symbol (choice (literal "^") (literal "/") (literal "*") (literal "-") (literal "+") (literal "=") (literal "\\land") (literal "\\lor") (literal "~>") (literal "=>") (literal "\\cdot") (literal "\\equiv") (literal "-+->") (literal "/=") (literal "\\subseteq") (literal "\\in") (literal "<") (literal "\\leq") (literal ">") (literal "\\geq") (literal "\\times") (literal "\\") (literal "\\intersect") (literal "\\union") (literal "...") (literal "..") (literal "|") (literal "||") (literal "&&") (literal "&") (literal "$$") (literal "$") (literal "??") (literal "%%") (literal "%") (literal "##") (literal "++") (literal "--") (literal "**") (literal "//") (literal "^^") (literal "@@") (literal "!!") (literal "|-") (literal "|=") (literal "-|") (literal "=|") (literal "<:") (literal ":>") (literal ":=") (literal "::=") (literal "\\oplus") (literal "\\ominus") (literal "\\odot") (literal "\\oslash") (literal "\\otimes") (literal "\\uplus") (literal "\\sqcap") (literal "\\sqcup") (literal "\\div") (literal "\\wr") (literal "\\star") (literal "\\o") (literal "\\bigcirc") (literal "\\bullet") (literal "\\prec") (literal "\\succ") (literal "\\preceq") (literal "\\succeq") (literal "\\sim") (literal "\\simeq") (literal "\\ll") (literal "\\gg") (literal "\\asymp") (literal "\\subset") (literal "\\supset") (literal "\\supseteq") (literal "\\approx") (literal "\\cong") (literal "\\sqsubset") (literal "\\sqsubseteq") (literal "\\sqsupset") (literal "\\sqsupseteq") (literal "\\doteq") (literal "\\propto") (literal "/\\") (literal "\\/") (literal "<=>") (literal "#") (literal "<=") (literal "=<") (literal ">=") (literal "\\X") (literal "\\cap") (literal "\\cup") (literal "\\mod") (literal "(+)") (literal "(-)") (literal "(.)") (literal "(/)") (literal "(\\X)") (literal "\\circ")))
   (prefix-operator-definition
    (alias OperatorDefinition
     (seq (optional (literal "LOCAL")) (field name (literal "-."))
          (field parameter (reference formal-parameter)) (literal "==")
          (field body (reference expression)))))
   (postfix-operator-definition
    (alias OperatorDefinition
     (seq (optional (literal "LOCAL")) (field parameter (reference formal-parameter))
          (field name (choice (literal "^+") (literal "^*") (literal "^#")))
          (literal "==") (field body (reference expression)))))
   (recursive-declaration
    (alias RecursiveDeclaration
     (seq (literal "RECURSIVE")
          (field name (token identifier))
          (optional
           (seq (literal "(") (field parameter (reference formal-parameter))
                (repeat (seq (literal ",")
                             (field parameter (reference formal-parameter))))
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
           (layout-end ")" "]" "}" ">>" "]_" ">>_"
                       "," ":" "->" "|->" "THEN" "ELSE" "IN")))
     (alias JunctionExpression
      (seq (field operator (layout-start "\\/"))
           (field body (reference expression))
           (repeat (seq (field operator (layout-next "\\/"))
                        (field body (reference expression))))
           (layout-end ")" "]" "}" ">>" "]_" ">>_"
                       "," ":" "->" "|->" "THEN" "ELSE" "IN")))))
   (assumption-declaration
    (alias AssumptionDeclaration
     (seq (choice (literal "ASSUME") (literal "ASSUMPTION")
                  (literal "AXIOM"))
          (optional
           (seq (field name (token identifier)) (literal "==")))
          (field body (reference expression)))))
   (theorem-declaration
    (alias TheoremDeclaration
      (seq (choice (literal "THEOREM") (literal "PROPOSITION") (literal "LEMMA") (literal "COROLLARY"))
           (optional
            (seq (field name (token identifier)) (literal "==")))
           (field body (choice (reference expression) (reference assume-prove)))
           (optional (field proof (reference proof))))))
   (fact-list
    (seq (field item (reference proof-fact))
         (repeat (seq (literal ",") (field item (reference proof-fact))))))
   (proof-fact
    (choice (reference expression) (reference infix-symbol)
            (seq (literal "MODULE") (token identifier))))
   (use-hide-declaration
    (alias UseHideDeclaration
     (seq (choice (literal "USE") (literal "HIDE"))
          (optional (literal "ONLY")) (optional (reference fact-list))
          (optional (seq (choice (literal "DEF") (literal "DEFS"))
                         (reference fact-list))))))
   (terminal-proof
    (alias TerminalProof
     (seq (optional (literal "PROOF"))
          (choice (literal "OBVIOUS") (literal "OMITTED")
           (seq (literal "BY") (optional (literal "ONLY"))
                (optional (reference fact-list))
                (optional (seq (choice (literal "DEF") (literal "DEFS"))
                               (reference fact-list))))))))
   (proof
    (choice (reference terminal-proof)
     (alias Proof
      (seq (optional (literal "PROOF"))
           (repeat1 (field step (reference proof-step)))))))
   (proof-step
    (alias ProofStep
     (seq (field name (choice (token proof-step) (token proof-reference)))
          (field body
           (choice (literal "QED") (reference use-hide-declaration)
                   (reference instance-declaration)
                   (seq (optional (literal "DEFINE"))
                        (repeat1 (reference let-definition)))
                   (reference proof-command)
                   (seq (optional (literal "SUFFICES"))
                        (choice (reference assume-prove) (reference expression)))))
          (optional (field proof (reference terminal-proof))))))
   (proof-command
    (alias ProofCommand
     (choice
      (seq (field operator (choice (literal "HAVE") (literal "CASE")))
           (field item (reference expression)))
      (seq (field operator (literal "WITNESS")) (reference fact-list))
      (seq (field operator (literal "TAKE")) (reference proof-bindings))
      (seq (field operator (literal "PICK")) (reference proof-bindings)
           (literal ":") (field item (reference expression))))))
   (proof-bindings
    (choice
     (seq (reference domain-binding)
          (repeat (seq (literal ",") (reference domain-binding))))
     (seq (token identifier) (repeat (seq (literal ",") (token identifier))))))
   (assume-prove
    (alias AssumeProve
     (seq (choice (literal "ASSUME") (literal "[]ASSUME"))
          (field assumption (reference proof-assumption))
          (repeat (seq (literal ",") (field assumption (reference proof-assumption))))
          (choice (literal "PROVE") (literal "[]PROVE"))
          (field conclusion (reference expression)))))
   (proof-assumption
    (choice (reference expression) (reference assume-prove) (reference new-symbol)))
   (new-symbol
    (alias NewSymbol
     (seq
      (choice (seq (literal "NEW") (optional (literal "CONSTANT")))
              (literal "CONSTANT")
              (seq (optional (literal "NEW"))
                   (choice (literal "VARIABLE") (literal "STATE")
                           (literal "ACTION") (literal "TEMPORAL"))))
      (field name (reference formal-parameter))
      (optional (seq (literal "\\in") (field domain (reference expression)))))))
   (separator (alias Separator (token module-border)))
   (expression
    (choice
     (prec left 30
      (alias Expression
       (seq (field left (reference expression))
            (field operator (choice (literal "\\land") (literal "\\lor")))
            (field right (reference expression)))))
     (prec left 40
      (alias Expression
       (seq (field left (reference expression))
            (field operator (choice (literal "\\cdot")))
            (field right (reference expression)))))
     (prec none 15
      (alias Expression
       (seq (field left (reference expression))
            (field operator (choice (literal "\\equiv") (literal "-+->")))
            (field right (reference expression)))))
     (prec left 60
      (alias Expression
       (seq (field left (reference expression))
            (field operator (choice (literal "\\times") (literal "|") (literal "||") (literal "%%") (literal "++") (literal "\\oplus") (literal "(+)")))
            (field right (reference expression)))))
     (prec none 50
      (alias Expression
       (seq (field left (reference expression))
            (field operator (choice (literal "...") (literal "!!") (literal "\\wr")))
            (field right (reference expression)))))
     (prec left 70
      (alias Expression
       (seq (field left (reference expression))
            (field operator (choice (literal "&&") (literal "&") (literal "**") (literal "\\odot") (literal "\\otimes") (literal "\\star") (literal "\\bigcirc") (literal "\\bullet") (literal "(.)") (literal "(\\X)")))
            (field right (reference expression)))))
     (prec left 50
      (alias Expression
       (seq (field left (reference expression))
            (field operator (choice (literal "$$") (literal "$") (literal "??") (literal "##") (literal "\\uplus") (literal "\\sqcap") (literal "\\sqcup")))
            (field right (reference expression)))))
     (prec left 65
      (alias Expression
       (seq (field left (reference expression))
            (field operator (choice (literal "--") (literal "\\ominus") (literal "(-)")))
            (field right (reference expression)))))
     (prec none 70
      (alias Expression
       (seq (field left (reference expression))
            (field operator (choice (literal "//") (literal "\\oslash") (literal "(/)")))
            (field right (reference expression)))))
     (prec none 80
      (alias Expression
       (seq (field left (reference expression))
            (field operator (choice (literal "^^")))
            (field right (reference expression)))))
     (prec none 40
      (alias Expression
       (seq (field left (reference expression))
            (field operator (choice (literal "|-") (literal "|=") (literal "-|") (literal "=|") (literal ":=") (literal "::=") (literal "\\prec") (literal "\\succ") (literal "\\preceq") (literal "\\succeq") (literal "\\sim") (literal "\\simeq") (literal "\\ll") (literal "\\gg") (literal "\\asymp") (literal "\\subset") (literal "\\supset") (literal "\\supseteq") (literal "\\approx") (literal "\\cong") (literal "\\sqsubset") (literal "\\sqsubseteq") (literal "\\sqsupset") (literal "\\sqsupseteq") (literal "\\doteq") (literal "\\propto")))
            (field right (reference expression)))))
     (prec none 43
      (alias Expression
       (seq (field left (reference expression))
            (field operator (choice (literal "<:")))
            (field right (reference expression)))))
     (prec none 60
      (alias Expression
       (seq (field left (reference expression))
            (field operator (choice (literal "\\mod")))
            (field right (reference expression)))))
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
     (prec left 42
      (alias Expression
       (seq (field left (reference expression)) (field operator (literal "@@"))
            (field right (reference expression)))))
     (prec none 43
      (alias Expression
       (seq (field left (reference expression)) (field operator (literal ":>"))
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
            (field operator (choice (literal "*") (literal "\\o")
                                    (literal "\\circ") (literal "%")
                                    (literal "\\X")))
            (field right (reference expression)))))
     (prec none 70
      (alias Expression
       (seq (field left (reference expression))
            (field operator (choice (literal "/") (literal "\\div")))
            (field right (reference expression)))))
     (prec none 80
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
     (prec right 1 (reference case-expression)) (reference function-constructor)
     (reference function-set-expression)
     (reference except-expression)
     (reference record-expression) (reference set-filter-expression)
     (reference record-set-expression)
     (reference set-map-expression)
     (reference temporal-subscript-expression)
     (reference angle-action-expression)
     (reference fairness-expression)
     (reference grouped-expression) (reference tuple-expression)
     (reference set-expression) (reference name-expression)
     (reference at-expression) (prec right 1 (reference label-expression))
     (reference junction-expression)
     (reference qualified-name-expression)
     (prec left 120 (reference instance-qualified-expression))
     (reference number-expression) (reference string-expression) (prec right 1 (reference lambda-expression))))
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
    (choice
     (prec right 1
      (alias QuantifiedExpression
       (seq (field quantifier (choice (literal "\\A") (literal "\\E")))
            (field binding (reference domain-binding))
            (repeat (seq (literal ",") (field binding (reference domain-binding))))
            (literal ":") (field predicate (reference expression)))))
     (prec right 1
      (alias QuantifiedExpression
       (seq (field quantifier
                   (choice (literal "\\A") (literal "\\E")
                           (literal "\\AA") (literal "\\EE")))
            (field name (token identifier))
            (repeat (seq (literal ",") (field name (token identifier))))
            (literal ":") (field predicate (reference expression)))))))
   (let-expression
    (prec right 1
     (alias LetExpression
      (seq (literal "LET") (field definition (reference let-definition))
           (repeat (field definition (reference let-definition)))
           (literal "IN") (field body (reference expression))))))
   (let-definition
    (choice (reference local-definition)
            (reference local-function-definition) (reference infix-operator-definition) (reference prefix-operator-definition) (reference postfix-operator-definition) (reference recursive-declaration) (reference instance-declaration)))
   (local-function-definition
    (alias LocalFunctionDefinition
     (seq (field name (token identifier))
          (literal "[") (field binding (reference domain-binding))
          (repeat (seq (literal ",")
                       (field binding (reference domain-binding))))
          (literal "]") (literal "==")
          (field body (reference expression)))))
   (local-definition
    (alias LocalDefinition
     (seq (field name (token identifier))
          (optional
           (seq (literal "(") (field parameter (reference formal-parameter))
                (repeat (seq (literal ",")
                             (field parameter (reference formal-parameter))))
                (literal ")")))
          (literal "==") (field body (choice (reference expression) (reference instance-expression))))))
   (case-expression
    (prec right 1
     (alias CaseExpression
     (seq (literal "CASE") (field arm (reference case-arm))
          (repeat (seq (literal "[]") (field arm (reference case-arm))))
          (optional
           (prec right 1 (seq (literal "[]") (literal "OTHER") (literal "->")
                (field other (reference expression)))))))))
   (case-arm
    (prec right 1
     (alias CaseArm
      (seq (field condition (reference expression)) (literal "->")
           (field result (reference expression))))))
   (prefix-expression
    (prec right 90
     (alias PrefixExpression
      (seq (field operator
                  (choice (literal "~") (literal "\\lnot") (literal "\\neg") (literal "-.") (literal "-") (literal "[]")
                          (literal "<>") (literal "ENABLED")
                          (literal "UNCHANGED") (literal "SUBSET")
                          (literal "UNION") (literal "DOMAIN")))
           (field operand (reference expression))))))
   (postfix-expression
    (prec left 100
     (alias PostfixExpression
      (seq (field operand (reference expression))
           (field operator (choice (literal "'") (literal "^+") (literal "^*") (literal "^#")))))))
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
           (optional (field argument (choice (reference expression) (reference infix-symbol))))
           (repeat (seq (literal ",")
                        (field argument (choice (reference expression) (reference infix-symbol)))))
           (literal ")")))))
   (record-field-expression
    (prec left 110
     (alias RecordFieldExpression
      (seq (field record (reference expression)) (literal ".")
           (field field (token identifier))))))
   (function-constructor
    (alias FunctionConstructor
     (seq (literal "[") (field binding (reference domain-binding))
          (repeat (seq (literal ",") (field binding (reference domain-binding))))
          (literal "|->")
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
           (repeat (seq (literal ",") (field index (reference expression))))
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
    ;; SANY BraceCases/matchFcnConst selects a filter when the opening
    ;; identifier or identifier tuple is followed by \in before the colon.
    ;; A tuple-membership predicate can also form a syntactic set-map branch.
    (prec dynamic 1
     (alias SetFilterExpression
     (seq (literal "{") (field binding (reference single-domain-binding))
          (literal ":")
          (field predicate (reference expression)) (literal "}")))))
   (set-map-expression
    (alias SetMapExpression
     (seq (literal "{") (field body (reference expression)) (literal ":")
          (field binding (reference domain-binding))
          (repeat (seq (literal ",") (field binding (reference domain-binding))))
          (literal "}"))))
   (temporal-subscript-expression
    (alias TemporalSubscriptExpression
     (seq (literal "[") (field action (reference expression)) (literal "]_")
          ;; SANY's ReducedExpression admits delimited expressions and names.
          (field subscript (reference reduced-subscript-expression)))))
   (angle-action-expression
    (alias AngleActionExpression
     (seq (literal "<<") (field action (reference expression)) (literal ">>_")
          (field subscript (reference reduced-subscript-expression)))))
   (reduced-subscript-expression
    (choice (reference name-expression) (reference qualified-name-expression) (reference instance-qualified-expression)
            (reference tuple-expression)
            (reference grouped-expression)))
   (fairness-expression
    (alias FairnessExpression
     (seq (field operator (choice (literal "WF_") (literal "SF_")))
          (field subscript (reference reduced-subscript-expression))
          (literal "(") (field action (reference expression)) (literal ")"))))
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
    (alias NameExpression (field name (choice (token identifier) (token proof-reference)))))
   (selector
    (choice (token identifier) (token number) (reference infix-symbol)
            (literal ":") (literal "@") (literal "<<") (literal ">>")
            (alias SelectorArguments
             (seq (literal "(") (field argument (reference expression))
                  (repeat (seq (literal ",") (field argument (reference expression))))
                  (literal ")")))))
   (qualified-name-expression
    (alias QualifiedNameExpression
     (seq (field module (token identifier))
          (repeat1 (seq (literal "!") (field name (reference selector)))))))
   (instance-qualified-expression
    (prec left 120
     (alias InstanceQualifiedExpression
      (seq (field instance (reference operator-application))
           (literal "!") (field name (reference selector))
           (repeat (seq (literal "!")
                        (field name (reference selector))))))))
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
            (weak-fairness "WF_") (strong-fairness "SF_")
            (subset "SUBSET") (union "UNION") (domain "DOMAIN")
            (local "LOCAL") (recursive "RECURSIVE")
            (instance "INSTANCE") (with "WITH")
            (except "EXCEPT")
            (assume "ASSUME") (assumption "ASSUMPTION") (axiom "AXIOM")
            (proof "PROOF") (by "BY") (only "ONLY") (obvious "OBVIOUS") (omitted "OMITTED") (qed "QED") (suffices "SUFFICES") (have "HAVE") (take "TAKE") (witness "WITNESS") (pick "PICK") (define "DEFINE") (def "DEF") (defs "DEFS") (prove "PROVE") (new "NEW") (state "STATE") (action "ACTION") (temporal "TEMPORAL") (lambda "LAMBDA") (lemma "LEMMA") (corollary "COROLLARY") (proposition "PROPOSITION") (use "USE") (hide "HIDE"))

  (recoveries (module "GERBIL-PARSER-TLA-PLUS-SANY-CANDIDATE" preserve-source))
  (conflicts selective-glr)
  (case-insensitive #f)
  (flow (source lexical) (lexical tla-plus-module) (tla-plus-module cst))
  (catalog
   (syntax-kinds
    (SelectorArguments node (argument))
    (Proof node (step)) (ProofStep node (name body proof))
    (TerminalProof node (fact definition item))
    (ProofCommand node (operator item))
    (AssumeProve node (assumption conclusion))
    (NewSymbol node (name domain))
    (ProofStepName token (text)) (ProofReference token (text))
    (FormalParameter node (name parameter))
    (LambdaExpression node (name body))
    (LabelExpression node (name parameter body))
    (SourceFile node (module text)) (ModuleText token (text)) (Module node (name item))
    (ExtendsDeclaration node (module))
    (ConstantDeclaration node (name parameter))
    (VariableDeclaration node (name))
    (OperatorDefinition node (name parameter body))
    (FunctionDefinition node (name binding body))
    (DomainBinding node (name domain))
    (TupleBinding node (name domain))
    (RecursiveDeclaration node (name parameter))
    (InstanceDeclaration node (module substitution))
    (InstanceExpression node (module substitution))
    (QualifiedNameExpression node (module name))
    (InstanceQualifiedExpression node (instance name))
    (JunctionExpression node (operator body))
    (Substitution node (name value))
    (AssumptionDeclaration node (name body))
    (TheoremDeclaration node (name body proof))
    (UseHideDeclaration node (item))
    (Separator node ()) (EmptyLine node ())
    (Expression node (left operator right))
    (IfExpression node (condition consequent alternative))
    (ChooseExpression node (name domain predicate))
    (QuantifiedExpression node (quantifier name binding predicate))
    (LetExpression node (definition body))
    (LocalDefinition node (name parameter body))
    (LocalFunctionDefinition node (name binding body))
    (CaseExpression node (arm other)) (CaseArm node (condition result))
    (PrefixExpression node (operator operand))
    (PostfixExpression node (operand operator))
    (FunctionApplication node (function argument))
    (OperatorApplication node (operator argument))
    (RecordFieldExpression node (record field))
    (FunctionConstructor node (binding body))
    (FunctionSetExpression node (domain codomain))
    (ExceptExpression node (base update))
    (ExceptUpdate node (path value))
    (ExceptIndex node (index)) (ExceptField node (name))
    (AtExpression node ())
    (RecordExpression node (name value))
    (RecordSetExpression node (name domain))
    (SetFilterExpression node (binding predicate))
    (SetMapExpression node (body binding))
    (TemporalSubscriptExpression node (action subscript))
    (AngleActionExpression node (action subscript))
    (FairnessExpression node (operator subscript action))
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
    (module-text ModuleText) (proof-reference ProofReference) (proof-step ProofStepName) (identifier Identifier) (number Number) (string String)
    (horizontal-whitespace HorizontalWhitespace) (newline Newline)
    (comment Comment) (module-border ModuleBorder) (module-end ModuleEnd)
    (punctuation Punctuation))
   (reserved-token-kinds SeparatorLine)))

(defsyntax-corpus tla-plus-core-fixtures
  (identity (language-grammar-language tla-plus-core-language-grammar)
            (language-grammar-version tla-plus-core-language-grammar)
            (language-grammar-contract tla-plus-core-language-grammar))
  (accepted
   ("tla-plus/examples/hour-clock" tla-plus-hour-clock
    "corpus/tlaplus-examples/HourClock.tla" SourceFile
    (Module ExtendsDeclaration VariableDeclaration OperatorDefinition
            TheoremDeclaration))
   ("tla-plus/core/counter" tla-plus-counter
    "corpus/core/Counter.tla" SourceFile
    (Module ExtendsDeclaration ConstantDeclaration VariableDeclaration
            OperatorDefinition TheoremDeclaration))
   ("tla-plus/core/nested-comment" tla-plus-nested-comment
    "corpus/core/NestedComment.tla" SourceFile
    (Module VariableDeclaration OperatorDefinition))
   ("tla-plus/core/structured-expressions" tla-plus-structured-expressions
    "corpus/core/StructuredExpressions.tla" SourceFile
    (Module OperatorDefinition Expression IfExpression ChooseExpression
            QuantifiedExpression TupleExpression SetExpression
            FunctionConstructor OperatorApplication LetExpression
            CaseExpression RecordExpression SetFilterExpression))
   ("tla-plus/core/module-forms" tla-plus-module-forms
    "corpus/core/ModuleForms.tla" SourceFile
    (Module RecursiveDeclaration InstanceDeclaration AssumptionDeclaration
            TheoremDeclaration OperatorDefinition)))
  (rejected
   ("tla-plus/core/malformed-if" tla-plus-malformed-if
    "corpus/core/MalformedIf.tla")))

(def tla-plus-core-accepted-fixtures
  (filter (lambda (fixture)
            (eq? (syntax-fixture-expected-status fixture) 'accepted))
          tla-plus-core-fixtures))

(def tla-plus-core-rejected-fixtures
  (filter (lambda (fixture)
            (eq? (syntax-fixture-expected-status fixture) 'rejected))
          tla-plus-core-fixtures))
