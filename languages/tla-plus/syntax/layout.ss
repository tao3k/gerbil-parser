;;; -*- Gerbil -*-
;;; TLA+ recognition declarations; kinds, fields and references are DSL-derived.
(import (only-in :gerbil-parser/language-support/grammar deflanguage) ./lexical)
(export tla-plus-layout-syntax)
(deflanguage tla-plus-layout
  (syntax
    (lexical
     (root source-file)
     (lex (horizontal-whitespace HorizontalWhitespace (horizontal-whitespace+))
          (newline Newline (newline+))
          (comment Comment (choice (line-comment "\\*") (nested-block-comment "(*" "*)")))
          (string String (quoted-string "\""))
          (module-end ModuleEnd
                      (literals "==============================================================" "===="))
          (separator-line SeparatorLine
                          (literals "--------------------------------------------------------------"))
          (module-border ModuleBorder (literals "----------------------" "----")) (number Number (number))
          (identifier Identifier (identifier))
          (punctuation Punctuation
                       (literals "<=>" "|->" "[]" "<>" "=>" "==" "/\\" "\\/" "\\AA" "\\EE" "\\A" "\\E" "\\notin"
                                 "\\intersect" "\\union" "\\cap" "\\cup" "\\subseteq" "\\X" "\\in" "\\" ".." "<<" ">>" "<="
                                 ">=" "->" "<-" "#" "=" "<" ">" "+" "-" "*" "/" "^" "'" "~" "(" ")" "[" "]" "{" "}" "," ":"
                                 "!" "_" ".")))
     (extras horizontal-whitespace newline comment)
     (keywords (module "MODULE") (extends "EXTENDS") (constant "CONSTANT") (constants "CONSTANTS")
               (variable "VARIABLE") (variables "VARIABLES") (theorem "THEOREM") (if "IF") (then "THEN")
               (else "ELSE") (choose "CHOOSE") (let "LET") (in "IN") (case "CASE") (other "OTHER")
               (enabled "ENABLED") (unchanged "UNCHANGED") (subset "SUBSET") (union "UNION") (domain "DOMAIN")
               (local "LOCAL") (recursive "RECURSIVE") (instance "INSTANCE") (with "WITH") (assume "ASSUME")
               (assumption "ASSUMPTION") (axiom "AXIOM") (proposition "PROPOSITION") (use "USE") (hide "HIDE"))
     (recoveries (module "GERBIL-PARSER-TLA-PLUS-V1" preserve-source))
     (conflicts selective-glr)
     (case-insensitive #f)))
  (rules (source-file (node SourceFile (field module module)))
         (module
          (node Module module-border "MODULE" (field name identifier) module-border
                (repeat (field item module-item)) module-end))
         (module-item
          (choice extends-declaration constant-declaration variable-declaration operator-definition
                  recursive-declaration instance-declaration assumption-declaration theorem-declaration
                  use-hide-declaration separator))
         (extends-declaration
          (node ExtendsDeclaration "EXTENDS" (field module identifier)
                (repeat (seq "," (field module identifier)))))
         (constant-declaration
          (node ConstantDeclaration (choice "CONSTANT" "CONSTANTS") (field name identifier)
                (repeat (seq "," (field name identifier)))))
         (variable-declaration
          (node VariableDeclaration (choice "VARIABLE" "VARIABLES") (field name identifier)
                (repeat (seq "," (field name identifier)))))
         (operator-definition
          (node OperatorDefinition (optional "LOCAL") (field name identifier)
                (optional
                 (seq "(" (field parameter identifier) (repeat (seq "," (field parameter identifier))) ")"))
                "==" (field body (choice instance-expression expression))))
         (recursive-declaration
          (node RecursiveDeclaration "RECURSIVE" (field name identifier)
                (optional
                 (seq "(" (field parameter identifier) (repeat (seq "," (field parameter identifier))) ")"))
                (repeat (seq "," (field name identifier)))))
         (instance-declaration
          (node InstanceDeclaration (optional "LOCAL") "INSTANCE" (field module identifier)
                (optional
                 (seq "WITH" (field substitution substitution)
                      (repeat (seq "," (field substitution substitution)))))))
         (substitution (node Substitution (field name identifier) "<-" (field value expression)))
         (instance-expression
          (node InstanceExpression "INSTANCE" (field module identifier)
                (optional
                 (seq "WITH" (field substitution substitution)
                      (repeat (seq "," (field substitution substitution)))))))
         (junction-expression
          (choice
           (node JunctionExpression (field operator (layout-start "/\\")) (field body expression)
                 (repeat (seq (field operator (layout-next "/\\")) (field body expression))) (layout-end))
           (node JunctionExpression (field operator (layout-start "\\/")) (field body expression)
                 (repeat (seq (field operator (layout-next "\\/")) (field body expression))) (layout-end))))
         (assumption-declaration
          (node AssumptionDeclaration (choice "ASSUME" "ASSUMPTION" "AXIOM")
                (optional (seq (field name identifier) "==")) (field body expression)))
         (theorem-declaration
          (node TheoremDeclaration (choice "THEOREM" "PROPOSITION")
                (optional (seq (field name identifier) "==")) (field body expression)))
         (use-hide-declaration
          (node UseHideDeclaration (choice "USE" "HIDE") (field item expression)
                (repeat (seq "," (field item expression))))) (separator (node Separator separator-line))
                (expression
                 (choice
                  (prec right 10
                        (node Expression (field left expression) (field operator (choice "<=>" "=>"))
                              (field right expression)))
                  (prec left 20
                        (node Expression (field left expression) (field operator "\\/") (field right expression)))
                  (prec left 30
                        (node Expression (field left expression) (field operator "/\\") (field right expression)))
                  (prec none 40
                        (node Expression (field left expression)
                              (field operator (choice "=" "#" "<" ">" "<=" ">=" "\\in" "\\notin" "\\subseteq"))
                              (field right expression)))
                  (prec left 45
                        (node Expression (field left expression)
                              (field operator (choice "\\cup" "\\union" "\\cap" "\\intersect" "\\"))
                              (field right expression)))
                  (prec left 50
                        (node Expression (field left expression) (field operator "..") (field right expression)))
                  (prec left 60
                        (node Expression (field left expression) (field operator (choice "+" "-"))
                              (field right expression)))
                  (prec left 70
                        (node Expression (field left expression) (field operator (choice "*" "/" "\\X"))
                              (field right expression)))
                  (prec right 80
                        (node Expression (field left expression) (field operator "^") (field right expression)))
                  (prec right 90 prefix-expression) (prec left 100 postfix-expression)
                  (prec left 110 function-application) (prec left 110 operator-application) if-expression
                  (prec right 1 choose-expression) (prec right 1 quantified-expression)
                  (prec right 1 let-expression) case-expression function-constructor record-expression
                  set-filter-expression set-map-expression temporal-subscript-expression grouped-expression
                  tuple-expression set-expression name-expression junction-expression
                  qualified-name-expression number-expression string-expression))
                (if-expression
                 (prec right 1
                       (node IfExpression "IF" (field condition expression) "THEN" (field consequent expression)
                             "ELSE" (field alternative expression))))
                (choose-expression
                 (prec right 1
                       (node ChooseExpression "CHOOSE" (field name identifier)
                             (optional (seq "\\in" (field domain expression))) ":" (field predicate expression))))
                (quantified-expression
                 (prec right 1
                       (node QuantifiedExpression (field quantifier (choice "\\A" "\\E" "\\AA" "\\EE"))
                             (field name identifier) (optional (seq "\\in" (field domain expression)))
                             (repeat
                              (seq "," (field name identifier) (optional (seq "\\in" (field domain expression)))))
                             ":" (field predicate expression))))
                (let-expression
                 (prec right 1
                       (node LetExpression "LET" (field definition local-definition) "IN" (field body expression))))
                (local-definition
                 (node LocalDefinition (field name identifier)
                       (optional
                        (seq "(" (field parameter identifier) (repeat (seq "," (field parameter identifier))) ")"))
                       "==" (field body expression)))
                (case-expression
                 (node CaseExpression "CASE" (field arm case-arm) (repeat (seq "[]" (field arm case-arm)))
                       (optional (seq "[]" "OTHER" "->" (field other expression)))))
                (case-arm (node CaseArm (field condition expression) "->" (field result expression)))
                (prefix-expression
                 (prec right 90
                       (node PrefixExpression
                             (field operator (choice "~" "-" "[]" "<>" "ENABLED" "UNCHANGED" "SUBSET" "UNION" "DOMAIN"))
                             (field operand expression))))
                (postfix-expression
                 (prec left 100 (node PostfixExpression (field operand expression) (field operator "'"))))
                (function-application
                 (prec left 110
                       (node FunctionApplication (field function expression) "[" (field argument expression)
                             (repeat (seq "," (field argument expression))) "]")))
                (operator-application
                 (prec left 110
                       (node OperatorApplication (field operator (choice identifier qualified-name-expression)) "("
                             (optional (field argument expression)) (repeat (seq "," (field argument expression))) ")")))
                (function-constructor
                 (node FunctionConstructor "[" (field name identifier) "\\in" (field domain expression) "|->"
                       (field body expression) "]"))
                (record-expression
                 (node RecordExpression "[" (field name identifier) "|->" (field value expression)
                       (repeat (seq "," (field name identifier) "|->" (field value expression))) "]"))
                (set-filter-expression
                 (node SetFilterExpression "{" (field name identifier) "\\in" (field domain expression) ":"
                       (field predicate expression) "}"))
                (set-map-expression
                 (node SetMapExpression "{" (field body expression) ":" (field name identifier) "\\in"
                       (field domain expression) "}"))
                (temporal-subscript-expression
                 (node TemporalSubscriptExpression "[" (field action expression) "]"
                       (field subscript identifier)))
                (grouped-expression (node GroupedExpression "(" (field expression expression) ")"))
                (tuple-expression
                 (node TupleExpression "<<" (optional (field item expression))
                       (repeat (seq "," (field item expression))) ">>"))
                (set-expression
                 (node SetExpression "{" (optional (field item expression))
                       (repeat (seq "," (field item expression))) "}"))
                (name-expression (node NameExpression (field name identifier)))
                (qualified-name-expression
                 (node QualifiedNameExpression (field module identifier)
                       (repeat1 (seq "!" (field name identifier)))))
                (number-expression (node NumberExpression (field value number)))
                (string-expression (node StringExpression (field value string)))))
