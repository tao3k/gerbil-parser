;;; -*- Gerbil -*-
;;; Native TLA+ module-core syntax compiled by the language-neutral engine.

(import :gerbil-parser/src/language/grammar)
(export +tla-plus-contract-version+ +tla-plus-syntax-contract+
        +tla-plus-syntax-source+ +tla-plus-sany-release+
        +tla-plus-sany-commit+ +tla-plus-sany-grammar-blob+
        +tla-plus-examples-commit+ tla-plus-v1-language-grammar
        tla-plus-v1-grammar tla-plus-v1-parser-ir tla-plus-v1-parser)

(def +tla-plus-contract-version+ "v1")
(def +tla-plus-syntax-contract+ "tla-plus.module-core.v1")
(def +tla-plus-syntax-source+ "Specifying Systems, Chapter 15: TLAPlusGrammar")
(def +tla-plus-sany-release+ "v1.7.4")
(def +tla-plus-sany-commit+ "5a47802b5c391f59ecdd44117981f4ff8c0656ba")
(def +tla-plus-sany-grammar-blob+ "bf9e7acb5337f4b6c2a4d6a973a1a65c95e72f56")
(def +tla-plus-examples-commit+ "ceeaa904140e3e03781cb2a79cd6c6d8b8b08e10")

(deflanguage-grammar tla-plus-v1
  (identity "tla-plus" +tla-plus-contract-version+ +tla-plus-syntax-contract+)
  (syntax-kinds
   (SourceFile node (module)) (Module node (name item))
   (ExtendsDeclaration node (module)) (ConstantDeclaration node (name))
   (VariableDeclaration node (name))
   (OperatorDefinition node (name parameter body))
   (TheoremDeclaration node (body)) (Separator node ()) (EmptyLine node ())
   (ExpressionLine node (token)) (Identifier token (text))
   (Number token (text)) (String token (text))
   (HorizontalWhitespace token (text)) (Newline token (text))
   (Comment token (text)) (ModuleBorder token (text))
   (ModuleEnd token (text)) (SeparatorLine token (text))
   (Punctuation token (text)) (Unknown token (text)))
  (terminals
   (identifier Identifier) (number Number) (string String)
   (horizontal-whitespace HorizontalWhitespace) (newline Newline)
   (comment Comment) (module-border ModuleBorder) (module-end ModuleEnd)
   (separator-line SeparatorLine) (punctuation Punctuation) (unknown Unknown))
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
              "\\notin" "\\in" ".." "<<" ">>" "<=" ">="
              "#" "=" "<" ">" "+" "-" "*" "/" "^" "'"
              "(" ")" "[" "]" "{" "}" "," ":" "!" "_" "."))
   (unknown (fallback)))
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
            (reference theorem-declaration) (reference separator)
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
      (seq (field name (token identifier))
           (optional
            (seq (literal "(") (field parameter (token identifier))
                 (repeat (seq (literal ",") (field parameter (token identifier))))
                 (literal ")")))
           (literal "==") (field body (reference expression-line))
           (token newline))))
   (theorem-declaration
    (alias TheoremDeclaration
      (seq (literal "THEOREM") (field body (reference expression-line))
           (token newline))))
   (separator (alias Separator (seq (token separator-line) (token newline))))
   (empty-line (alias EmptyLine (token newline)))
   (expression-line
    (alias ExpressionLine (repeat1 (field token (reference expression-token)))))
   (expression-token
    (choice (token identifier) (token number) (token string)
            (token punctuation) (token unknown))))
  (extras horizontal-whitespace comment)
  (keywords (module "MODULE") (extends "EXTENDS") (constant "CONSTANT")
            (constants "CONSTANTS") (variable "VARIABLE")
            (variables "VARIABLES") (theorem "THEOREM"))
  (parser-entrypoints (source-file parse pure))
  (recoveries (module "GERBIL-PARSER-TLA-PLUS-V1" preserve-source))
  (flow (source lexical) (lexical tla-plus-module) (tla-plus-module cst)))
