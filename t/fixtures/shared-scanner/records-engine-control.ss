;;; Test-owned former scanner/LR kernels; no production selection switch.
(import (only-in :gerbil-parser/t/fixtures/shared-scanner/records records-language-grammar records-contextual-product)
        (only-in :gerbil-parser/t/fixtures/contextual-result-selection install-reference-contextual-results!)
        (only-in :gerbil-parser/t/fixtures/lr-action-selection reference-lr-action-selector)
        (only-in :gerbil-parser/src/ffi/language-artifact-codec bind-language-abi)
        (only-in :gerbil-parser/src/compiler/machine parser-machine-with-lr-action-selector parser-machine-runtime
                 parser-machine-grammar-digest install-parser-machine-direct-step!)
        (only-in :gerbil-parser/src/runtime/lr-parser lr-runtime-direct-step)
        (only-in :gerbil-parser/src/language/descriptor make-language-grammar
                 +language-grammar-schema+ language-grammar-language language-grammar-version
                 language-grammar-contract language-grammar-grammar language-grammar-ir
                 language-grammar-machine language-grammar-observability))
(export make-records-engine-control)
(def (make-records-engine-control)
  (let* ((base (language-grammar-machine records-language-grammar))
         (machine (parser-machine-with-lr-action-selector base reference-lr-action-selector))
         (step (lr-runtime-direct-step (parser-machine-runtime base))))
    (when step
      (install-parser-machine-direct-step! machine (parser-machine-grammar-digest base) step))
    (install-reference-contextual-results!
      (bind-language-abi
        (make-language-grammar +language-grammar-schema+
          (language-grammar-language records-language-grammar)
          (language-grammar-version records-language-grammar)
          (language-grammar-contract records-language-grammar)
          (language-grammar-grammar records-language-grammar)
          (language-grammar-ir records-language-grammar) machine
          (language-grammar-observability records-language-grammar))
        records-contextual-product))))
