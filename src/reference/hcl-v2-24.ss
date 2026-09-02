;;; -*- Gerbil -*-
;;; Version-pinned HCL native syntax structural-core reference grammar.

(import ../modules/parser/config)
(export +hcl-native-syntax-version+
        +hcl-native-syntax-commit+
        +hcl-syntax-contract-v1+
        hcl-v2-24-grammar
        hcl-v2-24-parser-ir
        hcl-v2-24-parser)

(def +hcl-native-syntax-version+ "v2.24.0")
(def +hcl-native-syntax-commit+
  "6b5068090eef06b1f127f61529db5ba0be7ed343")
(def +hcl-syntax-contract-v1+ "hcl-native-v2.24.0-structural-core.v1")

(defparser-config hcl-v2-24-role
  hcl-v2-24-grammar
  hcl-v2-24-parser-ir
  hcl-v2-24-parser
  (syntax-kinds
   (HclFile node (item))
   (Body node (item))
   (Attribute node (name value))
   (Block node (type label body))
   (TraversalExpression node (root step))
   (StringExpression node (value))
   (NumberExpression node (value))
   (TupleExpression node (element))
   (ObjectExpression node (entry))
   (ObjectEntry node (key value))
   (Identifier token (text))
   (Number token (text))
   (String token (text))
   (HorizontalWhitespace token (text))
   (Newline token (text))
   (Comment token (text))
   (Punctuation token (text))
   (Unknown token (text)))
  (terminals
   (identifier Identifier)
   (number Number)
   (string String)
   (horizontal-whitespace HorizontalWhitespace)
   (newline Newline)
   (comment Comment)
   (block-comment-token Comment)
   (punctuation Punctuation)
   (unknown Unknown))
  (lexical-rules
   (horizontal-whitespace (horizontal-whitespace+))
   (newline (newline+))
   (comment (line-comment "#" "//"))
   (block-comment-token (block-comment "/*" "*/"))
   (string (quoted-string "\""))
   (number (decimal-digit+))
   (identifier (identifier))
   (punctuation (literals "{" "}" "[" "]" "(" ")" "=" "," "." ":"))
   (unknown (fallback)))
  (rules
   (config-file
    (alias HclFile
      (repeat (choice (token newline) (field item (reference body-item))))))
   (body
    (alias Body
      (repeat (choice (token newline) (field item (reference body-item))))))
   (body-item
    (choice (reference block) (reference attribute)))
   (attribute
    (alias Attribute
      (seq
       (field name (token identifier))
       (literal "=")
       (field value (reference expression))
       (optional (token newline)))))
   (block
    (alias Block
      (seq
       (field type (token identifier))
       (repeat
        (field label (choice (token string) (token identifier))))
       (literal "{")
       (optional (token newline))
       (field body (reference body))
       (literal "}")
       (optional (token newline)))))
   (expression
    (choice
     (reference object-expression)
     (reference tuple-expression)
     (reference string-expression)
     (reference number-expression)
     (reference traversal-expression)
     (seq (literal "(") (reference expression) (literal ")"))))
   (traversal-expression
    (alias TraversalExpression
      (seq
       (field root (token identifier))
       (repeat
        (seq (literal ".") (field step (token identifier)))))))
   (string-expression
    (alias StringExpression (field value (token string))))
   (number-expression
    (alias NumberExpression (field value (token number))))
   (tuple-expression
    (alias TupleExpression
      (seq
       (literal "[")
       (repeat (token newline))
       (optional
        (seq
         (field element (reference expression))
         (repeat
          (seq
           (choice (literal ",") (token newline))
           (field element (reference expression))))
         (optional (literal ","))))
       (repeat (token newline))
       (literal "]"))))
   (object-expression
    (alias ObjectExpression
      (seq
       (literal "{")
       (repeat
        (choice
         (token newline)
         (field entry (reference object-entry))))
       (literal "}"))))
   (object-entry
    (alias ObjectEntry
      (seq
       (field key (choice (token identifier) (token string)))
       (choice (literal "=") (literal ":"))
       (field value (reference expression))
       (optional (literal ","))
       (optional (token newline))))))
  (extras horizontal-whitespace comment block-comment-token)
  (keywords)
  (parser-entrypoints
   (config-file parse pure))
  (recoveries
   (config-file "GERBIL-PARSER-HCL-V2-24" preserve-source))
  (flow
   (source lexical)
   (lexical hcl-structural-core)
   (hcl-structural-core cst)))
