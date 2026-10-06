#!/usr/bin/env gxi
(import (only-in :gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process test-child-process-exit!)
        (only-in :gerbil-parser/src/compiler/native-language generate-native-language-pack))
(def (main source header)
  (generate-native-language-pack source header "gerbil-parser/t/fixtures/shared-scanner/records-native"
    "gerbil-parser/t/fixtures/shared-scanner/records" 'records-language-grammar "records" 'records-contextual-product)
  (displayln "NATIVE-LANGUAGE-PACK-GENERATED") (force-output) (test-child-process-exit! 0))
