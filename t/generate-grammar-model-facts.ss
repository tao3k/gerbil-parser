;;; Regenerate typed proof inputs after an intentional native publication change.
(import (only-in :gerbil-parser/t/fixtures/lr1-construction write-native-grammar-facts write-construction-raw-data)
        (only-in :gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process test-child-process-exit!))
(export main)
(def (main)
  (call-with-output-file "formal/ParserEngine/NativeGrammarData.lean" write-native-grammar-facts)
  (call-with-output-file "formal/ParserEngine/RawGrammarData.lean" write-construction-raw-data)
  (test-child-process-exit! 0))
