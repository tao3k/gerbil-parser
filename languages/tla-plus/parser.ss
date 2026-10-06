;;; -*- Gerbil -*-
;;; Parser names describe syntax scope; grammar metadata owns version identity.

(import (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/language-support/entry deflanguage-parser-loader LanguageLoader.)
        (only-in :gerbil-parser/src/language/descriptor language-grammar-with-parser-policy)
        :gerbil-parser/language-support/structured
        (rename-in ./grammar (tla-plus-sany-candidate-language-grammar recognition-language-grammar))
        :gerbil-parser/src/language/tlc)
(export (import: ./grammar)
        tla-plus-sany-candidate-language tla-plus-sany-candidate-language-grammar parse-tla-plus-sany-candidate
        +tla-plus-model-qualification-schema+ tla-plus-model-receipt?
        tla-plus-model-receipt-admitted tla-plus-model-receipt-output tla-plus-model-receipt->alist
        qualify-tla-plus-model qualify-tla-plus-core-model
        tla-plus-core-language tla-plus-layout-language
        parse-tla-plus-core parse-tla-plus-layout parse-tla-plus)
(deflanguage-parser-loader (tla-plus-core-language :: self LanguageLoader.)
  (grammar tla-plus-core-language-grammar)
  (parse parse-tla-plus-core)
  (slots metadata: (.o grammar-format: 'concise-dsl)))

(deflanguage-parser-loader (tla-plus-layout-language :: self LanguageLoader.)
  (grammar tla-plus-layout-language-grammar)
  (parse parse-tla-plus-layout)
  (slots metadata: (.o grammar-format: 'concise-dsl)))

(def parse-tla-plus parse-tla-plus-layout)

(deflanguage-model-entry qualify-tla-plus-model (grammar tla-plus-layout-language-grammar))
(deflanguage-model-entry qualify-tla-plus-core-model (grammar tla-plus-core-language-grammar))

(def tla-plus-sany-candidate-language-grammar
  (language-grammar-with-parser-policy
   recognition-language-grammar "tla-plus.sany-proof-policy.v1" 4096 (bind-structured-proof-policy (.o (:: self StructuredProofPolicy.) code: "GERBIL-PARSER-TLA-PLUS-SANY-PROOF"))))

(deflanguage-parser-loader (tla-plus-sany-candidate-language :: self LanguageLoader.)
  (grammar tla-plus-sany-candidate-language-grammar)
  (parse parse-tla-plus-sany-candidate)
  (slots metadata: (.o grammar-format: 'concise-dsl)))
