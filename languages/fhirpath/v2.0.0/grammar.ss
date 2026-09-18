;;; -*- Gerbil -*-
;;; SPDX-FileCopyrightText: 2026 tao3k team and Contributors
;;;
;;; SPDX-License-Identifier: Apache-2.0 AND LGPL-2.1-or-later

;;; Native, lossless projection of the pinned normative ANTLR grammar.  This
;;; module owns syntax only; FHIR model navigation and function semantics are
;;; deliberately outside the parser authority.
(import :gerbil-parser/src/language/grammar
        ./scanner
        ./source)
(export (import: ./source)
        +fhirpath-syntax-contract+
        fhirpath-v2-language-grammar
        fhirpath-v2-grammar
        fhirpath-v2-bound-grammar-ir
        fhirpath-v2-parser-ir
        fhirpath-v2-parser)

(def +fhirpath-syntax-contract+ "fhirpath-normative-2.0.0-syntax.v1")

(deflanguage-grammar fhirpath-v2
  (identity "fhirpath" "2.0.0" "fhirpath-normative-2.0.0-syntax.v1")
  (syntax-kinds
   (Expression node (left operator right operand invocation index))
   (Term node (value))
   (Literal node (value))
   (ExternalConstant node (name))
   (Invocation node (value))
   (Function node (name parameter))
   (ParamList node (parameter))
   (Quantity node (value unit))
   (Unit node (value))
   (DateTimePrecision node (value))
   (PluralDateTimePrecision node (value))
   (TypeSpecifier node (name))
   (QualifiedIdentifier node (name))
   (Identifier node (value))
   (IdentifierToken token (text))
   (DelimitedIdentifierToken token (text))
   (StringToken token (text))
   (NumberToken token (text))
   (DateToken token (text))
   (DateTimeToken token (text))
   (TimeToken token (text))
   (WhitespaceTrivia token (text))
   (CommentTrivia token (text))
   (PunctuationToken token (text)))
  (terminals
   (identifier IdentifierToken)
   (delimited-identifier DelimitedIdentifierToken)
   (string StringToken)
   (number NumberToken)
   (date DateToken)
   (datetime DateTimeToken)
   (time TimeToken)
   (whitespace WhitespaceTrivia)
   (comment CommentTrivia)
   (punctuation PunctuationToken))
  (lexical-rules
   (whitespace (whitespace+))
   (comment (choice (line-comment "//") (block-comment "/*" "*/")))
   (datetime (external fhirpath-datetime-v2 scan-fhirpath-datetime))
   (time (external fhirpath-time-v2 scan-fhirpath-time))
   (date (external fhirpath-date-v2 scan-fhirpath-date))
   (delimited-identifier
    (external fhirpath-delimited-identifier-v2
              scan-fhirpath-delimited-identifier))
   (string (external fhirpath-string-v2 scan-fhirpath-string))
   (number (external fhirpath-number-v2 scan-fhirpath-number))
   (identifier (external fhirpath-identifier-v2 scan-fhirpath-identifier))
   (punctuation
    (literals "<=" ">=" "!=" "!~" "$this" "$index" "$total"
              "." "[" "]" "+" "-" "*" "/" "&" "|" "<" ">"
              "=" "~" "%" "(" ")" "{" "}" ",")) )
  (rules
   (expression
    (alias Expression (field operand (reference implies-expression))))
   ;; The normative ANTLR rule is directly left recursive.  Its alternative
   ;; order is projected into an explicit precedence ladder so the native LR
   ;; machine has one CST for contextual keywords and qualified type names.
   (implies-expression
    (alias Expression
     (seq (field left (reference or-expression))
          (optional
           (seq (field operator (literal "implies"))
                (field right (reference implies-expression)))))))
   (or-expression
    (alias Expression
     (seq (field left (reference and-expression))
          (repeat
           (seq (field operator (choice (literal "or") (literal "xor")))
                (field right (reference and-expression)))))))
   (and-expression
    (alias Expression
     (seq (field left (reference membership-expression))
          (repeat
           (seq (field operator (literal "and"))
                (field right (reference membership-expression)))))))
   (membership-expression
    (alias Expression
     (seq (field left (reference equality-expression))
          (repeat
           (seq
            (field operator (choice (literal "in") (literal "contains")))
            (field right (reference equality-expression)))))))
   (equality-expression
    (alias Expression
     (seq (field left (reference inequality-expression))
          (repeat
           (seq
            (field operator
                   (choice (literal "=") (literal "~")
                           (literal "!=") (literal "!~")))
            (field right (reference inequality-expression)))))))
   (inequality-expression
    (alias Expression
     (seq (field left (reference union-expression))
          (repeat
           (seq
            (field operator
                   (choice (literal "<=") (literal "<")
                           (literal ">") (literal ">=")))
            (field right (reference union-expression)))))))
   (union-expression
    (alias Expression
     (seq (field left (reference type-expression))
          (repeat
           (seq (field operator (literal "|"))
                (field right (reference type-expression)))))))
   (type-expression
    (alias Expression
     (seq (field left (reference additive-expression))
          (repeat
           (seq (field operator (choice (literal "is") (literal "as")))
                (field right (reference type-specifier)))))))
   (additive-expression
    (alias Expression
     (seq (field left (reference multiplicative-expression))
          (repeat
           (seq
            (field operator
                   (choice (literal "+") (literal "-") (literal "&")))
            (field right (reference multiplicative-expression)))))))
   (multiplicative-expression
    (alias Expression
     (seq (field left (reference polarity-expression))
          (repeat
           (seq
            (field operator
                   (choice (literal "*") (literal "/")
                           (literal "div") (literal "mod")))
            (field right (reference polarity-expression)))))))
   (polarity-expression
    (alias Expression
     (choice
      (seq (field operator (choice (literal "+") (literal "-")))
           (field operand (reference polarity-expression)))
      (field operand (reference postfix-expression)))))
   (postfix-expression
    (alias Expression
     (seq
      (field operand (reference term))
      (repeat
       (choice
        (seq (literal ".")
             (field invocation (reference invocation)))
        (seq (literal "[") (field index (reference expression))
             (literal "]")))))))
   (term
    (alias Term
     (field value
      (choice (reference invocation) (reference literal)
              (reference external-constant)
              (seq (literal "(") (reference expression) (literal ")"))))))
   (literal
    (alias Literal
     (field value
      (choice (seq (literal "{") (literal "}"))
              (literal "true") (literal "false")
              (token string) (token number) (token date)
              (token datetime) (token time) (reference quantity)))))
   (external-constant
    (alias ExternalConstant
     (seq (literal "%")
          (field name (choice (reference identifier) (token string))))))
   (invocation
    (alias Invocation
     (field value
      (choice (reference function) (reference identifier)
              (literal "$this") (literal "$index") (literal "$total")))))
   (function
    (alias Function
     (seq (field name (reference identifier)) (literal "(")
          (optional (field parameter (reference param-list))) (literal ")"))))
   (param-list
    (alias ParamList
     (seq (field parameter (reference expression))
          (repeat
           (seq (literal ",")
                (field parameter (reference expression)))))))
   (quantity
    (alias Quantity
     (seq (field value (token number))
          ;; The normative grammar writes NUMBER unit?, but its preceding
          ;; NUMBER literal already owns the empty-unit branch.  Requiring the
          ;; unit here preserves the accepted language while removing the two
          ;; distinct CSTs for every ordinary number.
          (field unit (reference unit)))))
   (unit
    (alias Unit
     (field value
      (choice (reference date-time-precision)
              (reference plural-date-time-precision) (token string)))))
   (date-time-precision
    (alias DateTimePrecision
     (field value
      (choice (literal "year") (literal "month") (literal "week")
              (literal "day") (literal "hour") (literal "minute")
              (literal "second") (literal "millisecond")))))
   (plural-date-time-precision
    (alias PluralDateTimePrecision
     (field value
      (choice (literal "years") (literal "months") (literal "weeks")
              (literal "days") (literal "hours") (literal "minutes")
              (literal "seconds") (literal "milliseconds")))))
   (type-specifier
    (alias TypeSpecifier (field name (reference qualified-identifier))))
   (qualified-identifier
    (alias QualifiedIdentifier
     (seq (field name (reference identifier))
          (repeat
           (seq (literal ".") (field name (reference identifier)))))))
   (identifier
    (alias Identifier
     (field value
      (choice (token identifier) (token delimited-identifier)
              (literal "as") (literal "contains")
              (literal "in") (literal "is"))))))
  (extras whitespace comment)
  (keywords)
  (parser-entrypoints (expression parse pure))
  (recoveries (expression "GERBIL-PARSER-FHIRPATH-N2" preserve-source))
  (conflicts reject)
  (case-insensitive #f)
  (flow (source lexical) (lexical fhirpath-expression) (fhirpath-expression cst)))
