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
        language-grammar-machine
        language-grammar-observability
        language-grammar-with-observability)

(def +language-grammar-schema+ "gerbil-parser.language-grammar.v1")

(defstruct language-grammar
  (schema language version contract grammar ir machine observability)
  transparent: #t)

;;; Configuration is immutable and grammar-local. The supplied value is a POO
;;; Flow observability policy; its owner validates it when a phase is invoked.
(def (language-grammar-with-observability grammar observability)
  (make-language-grammar
   (language-grammar-schema grammar)
   (language-grammar-language grammar)
   (language-grammar-version grammar)
   (language-grammar-contract grammar)
   (language-grammar-grammar grammar)
   (language-grammar-ir grammar)
   (language-grammar-machine grammar)
   observability))
