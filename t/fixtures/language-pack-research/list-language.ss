;;; -*- Gerbil -*-
(import "list-stage" (only-in "../../../src/language/grammar" deflanguage))
(export list-study-language-grammar list-study-parser list-study-grammar list-study-bound-grammar-ir list-study-parser-ir list-composed-ir list-composition-receipt
        list-control-parser
        list-single-language-grammar list-single-grammar list-single-parser-ir
        list-single-bound-grammar-ir list-single-composed-ir list-single-composition-receipt
        native-list-language-grammar native-list-bound-grammar-ir
        native-single-language-grammar native-single-bound-grammar-ir)
(deflist-study list-study list-composed-ir list-composition-receipt)
(defsingle-list-study list-single list-single-composed-ir list-single-composition-receipt)
(defnative-list-study native-list native-composed-ir native-composition-receipt)
(defnative-single-list-study native-single native-single-composed-ir native-single-composition-receipt)

;;; Inline control has identical public structure, with no list helper rules.
(deflanguage list-control
  (identity "list-control" "v1" "list-control.local.v1")
  (root source-file)
  (lex (identifier Identifier (identifier))
       (punctuation Punctuation (literals "(" ")" "[" "]" ","))
       (whitespace Whitespace (whitespace+)))
  (rules
   (source-file (node SourceFile (field form (choice (reference call) (reference array)))))
   (call
    (node Call
      (seq (field callee (token identifier)) (literal "(")
           (optional
            (seq (field argument (reference name))
                 (repeat (seq (literal ",") (field argument (reference name))))))
           (literal ")"))))
   (array
    (node Array
      (seq (literal "[")
           (optional
            (seq (field element (reference name))
                 (repeat (seq (literal ",") (field element (reference name))))))
           (literal "]"))))
   (name (node Name (field value (token identifier)))))
  (extras whitespace) (keywords) (recoveries)
  (conflicts reject) (case-insensitive #f))
