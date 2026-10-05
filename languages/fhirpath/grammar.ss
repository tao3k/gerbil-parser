;;; -*- Gerbil -*-
;;; SPDX-FileCopyrightText: 2026 tao3k team and Contributors
;;;
;;; SPDX-License-Identifier: Apache-2.0 AND LGPL-2.1-or-later

;;; Native, lossless projection of the pinned normative ANTLR grammar.  This
;;; module owns syntax only; FHIR model navigation and function semantics are
;;; deliberately outside the parser authority.
(import (only-in :gerbil-parser/language-support deflanguage deftext-profile)
        ./source)
(export (import: ./source)
        +fhirpath-syntax-contract+
        fhirpath-language-grammar
        fhirpath-grammar
        fhirpath-bound-grammar-ir
        fhirpath-parser-ir
        fhirpath-parser)

(def +fhirpath-syntax-contract+ "fhirpath-normative-2.0.0-syntax.v1")

;;; Shared text-profile fragments expand into closed declaration data.
(deftext-profile fhirpath-date-format
  (seq (run (numeric) 4 4)
       (if-next (characters "-")
         (seq (literal "-") (run (numeric) 2 2)
              (if-next (characters "-")
                (seq (literal "-") (run (numeric) 2 2)))))))

(deftext-profile fhirpath-time-format
  (seq (run (numeric) 2 2)
       (if-next (characters ":")
         (seq (literal ":") (run (numeric) 2 2)
              (if-next (characters ":")
                (seq (literal ":") (run (numeric) 2 2)
                     (optional (seq (literal ".") (run (numeric) 1 #f)))))))))

(deftext-profile fhirpath-timezone-format
  (if-next (characters "Z") (literal "Z")
    (if-next (characters "+-")
      (seq (run (characters "+-") 1 1) (run (numeric) 2 2)
           (literal ":") (run (numeric) 2 2)))))

(deflanguage fhirpath
  (identity "fhirpath" "2.0.0" "fhirpath-normative-2.0.0-syntax.v1")
  (root expression)
  (lex
   (whitespace WhitespaceTrivia (whitespace+))
   (comment CommentTrivia (choice (line-comment "//") (block-comment "/*" "*/")))
   (datetime DateTimeToken
    (text-profile
     (seq (literal "@") (ref fhirpath-date-format) (literal "T")
          (if-next (numeric)
            (seq (ref fhirpath-time-format) (ref fhirpath-timezone-format))))))
   (time TimeToken
    (text-profile (seq (literal "@T") (ref fhirpath-time-format))))
   (date DateToken
    (text-profile
     (seq (literal "@") (ref fhirpath-date-format) (not-next (characters "T")))))
   (delimited-identifier DelimitedIdentifierToken
    (quoted-string-profile "`" "`'\\/fnrt" 4))
   (string StringToken (quoted-string-profile "'" "`'\\/fnrt" 4))
   (number NumberToken
    (text-profile
     (seq (run (numeric) 1 #f) (optional (seq (literal ".") (run (numeric) 1 #f))))))
   (identifier IdentifierToken
    (text-profile
     (seq (run (union (ascii-letter) (characters "_")) 1 1)
          (run (union (ascii-letter) (characters "_") (numeric)) 0 #f))))
   (punctuation PunctuationToken
    (literals "<=" ">=" "!=" "!~" "$this" "$index" "$total"
              "." "[" "]" "+" "-" "*" "/" "&" "|" "<" ">"
              "=" "~" "%" "(" ")" "{" "}" ",")))
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
  (recoveries (expression "GERBIL-PARSER-FHIRPATH-N2" preserve-source))
  (conflicts reject)
  (case-insensitive #f)
  (flow (source lexical) (lexical fhirpath-expression) (fhirpath-expression cst)))
