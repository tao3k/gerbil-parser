;;; -*- Gerbil -*-
;;; Parser names describe syntax scope; parser metadata owns version identity.

(import (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/language-support/entry deflanguage-parser-loader language-parser-entry-ref)
        (only-in :gerbil-parser/language-support/grammar-source defsyntax-javacc-source)
        (only-in :gerbil-parser/src/language/descriptor language-grammar-with-parser-policy)
        :gerbil-parser/language-support/structured
        ./grammar
        :gerbil-parser/src/language/tlc)
(export tla-plus-layout-language-grammar tla-plus-core-language-grammar (import: ./grammar)
        tla-plus-sany-candidate-language tla-plus-sany-candidate-language-grammar parse-tla-plus-sany-candidate
        +tla-plus-model-qualification-schema+ tla-plus-model-receipt?
        tla-plus-model-receipt-admitted tla-plus-model-receipt-output tla-plus-model-receipt->alist
        qualify-tla-plus-model qualify-tla-plus-core-model
        tla-plus-core-language tla-plus-layout-language
        parse-tla-plus-core parse-tla-plus-layout parse-tla-plus)
;;; Parser metadata owns release and pinned-source identity.
(defsyntax-javacc-source tla-plus-sany-source
  (identity "tla-plus" "v1.7.4" "5a47802b5c391f59ecdd44117981f4ff8c0656ba")
  (digest "sha256:15edd079cf16cf91556ba66b496c9ffc16f26ab30856753ed58e60b0d54f2d07")
  (source "grammar-source/tla+.jj"))
(def tla-plus-source-metadata
  (append '((syntax-source . "Specifying Systems, Chapter 15: TLAPlusGrammar")
            (examples-commit . "ceeaa904140e3e03781cb2a79cd6c6d8b8b08e10")
            (sany-release . "v1.7.4")
            (sany-commit . "5a47802b5c391f59ecdd44117981f4ff8c0656ba")
            (sany-grammar-blob . "bf9e7acb5337f4b6c2a4d6a973a1a65c95e72f56")
            (sany-grammar-digest . "sha256:15edd079cf16cf91556ba66b496c9ffc16f26ab30856753ed58e60b0d54f2d07"))
          (list (cons 'source-catalog tla-plus-sany-source))))

(deflanguage-parser-loader tla-plus-core-language
  (grammar tla-plus-core-language-grammar tla-plus-core-syntax)
  (parse parse-tla-plus-core)
  (metadata (append '((language . "tla-plus") (version . "v1")
              (contract . "tla-plus.native-core.v1") (grammar-format . concise-dsl)) tla-plus-source-metadata)))

(deflanguage-parser-loader tla-plus-layout-language
  (grammar tla-plus-layout-language-grammar tla-plus-layout-syntax)
  (parse parse-tla-plus-layout)
  (metadata (append '((language . "tla-plus") (version . "v2")
              (contract . "tla-plus.native-layout.v2") (grammar-format . concise-dsl)) tla-plus-source-metadata)))

(def parse-tla-plus parse-tla-plus-layout)

(deflanguage-model-entry qualify-tla-plus-model (grammar tla-plus-layout-language-grammar))
(deflanguage-model-entry qualify-tla-plus-core-model (grammar tla-plus-core-language-grammar))

(def proof-syntax
  (language-grammar-with-parser-policy
   tla-plus-sany-candidate-syntax "tla-plus.sany-proof-policy.v1" 4096 (bind-structured-proof-policy (.o (:: self StructuredProofPolicy.) code: "GERBIL-PARSER-TLA-PLUS-SANY-PROOF"))))

(deflanguage-parser-loader tla-plus-sany-candidate-language
  (grammar tla-plus-sany-candidate-language-grammar proof-syntax)
  (parse parse-tla-plus-sany-candidate)
  (metadata (append '((language . "tla-plus") (version . "p4-draft")
              (contract . "tla-plus.native-sany-candidate.p4") (grammar-format . concise-dsl)) tla-plus-source-metadata)))
