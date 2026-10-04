;;; -*- Gerbil -*-
;;; Public candidate entry binds recognition budget and proof policy together.
(import (only-in :clan/poo/object .o)
        (only-in ./sany-proof sany-proof-diagnostic)
        (only-in :gerbil-parser/language-support
                 deflanguage-parser-loader LanguageLoader. language-grammar-with-parser-policy)
        (rename-in (only-in ./grammars/sany-candidate
                           tla-plus-sany-candidate-language-grammar
                           tla-plus-sany-candidate-parser)
                   (tla-plus-sany-candidate-language-grammar recognition-language-grammar)))
(export tla-plus-sany-candidate-language tla-plus-sany-candidate-language-grammar
        tla-plus-sany-candidate-parser parse-tla-plus-sany-candidate)

(def tla-plus-sany-candidate-language-grammar
  (language-grammar-with-parser-policy
   recognition-language-grammar "tla-plus.sany-proof-policy.v1" 4096 sany-proof-diagnostic))

(deflanguage-parser-loader (tla-plus-sany-candidate-language :: self LanguageLoader.)
  (grammar tla-plus-sany-candidate-language-grammar)
  (parse parse-tla-plus-sany-candidate)
  (slots metadata: (.o grammar-format: 'concise-dsl)))
