;;; -*- Gerbil -*-
;;; Runtime-small immutable identity and generated-artifact descriptor.

(export +language-grammar-schema+
        language-grammar?
        make-language-grammar
        language-grammar-language
        language-grammar-version
        language-grammar-contract
        language-grammar-grammar
        language-grammar-ir
        language-grammar-machine)

(def +language-grammar-schema+ "gerbil-parser.language-grammar.v1")

(defstruct language-grammar
  (schema language version contract grammar ir machine)
  transparent: #t)
