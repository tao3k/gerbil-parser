;;; Regenerate typed proof inputs after an intentional native publication change.
(import (only-in :gerbil-parser/t/resolved-grammar-test write-native-grammar-facts)
        (only-in :gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process test-child-process-exit!))
(export main)
(def (main)
  (call-with-output-file "formal/ParserEngine/NativeFacts.lean" write-native-grammar-facts)
  (displayln "GRAMMAR-MODEL-FACTS-GENERATED")
  (force-output)
  (test-child-process-exit! 0))
