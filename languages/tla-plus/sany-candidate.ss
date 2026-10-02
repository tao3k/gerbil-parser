;;; -*- Gerbil -*-
;;; Candidate syntax entry; grammar metadata owns its independent identity.

(import (only-in :gerbil-parser/src/language/entry deflanguage-parser)
        (only-in ./grammars/sany-candidate
                 tla-plus-sany-candidate-language-grammar
                 tla-plus-sany-candidate-parser))
(export tla-plus-sany-candidate-language
        tla-plus-sany-candidate-parser
        parse-tla-plus-sany-candidate)

(deflanguage-parser tla-plus-sany-candidate-language
  (grammar tla-plus-sany-candidate-language-grammar)
  (parse parse-tla-plus-sany-candidate))
