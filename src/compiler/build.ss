;;; Public library roots owned by this Module.
(import (only-in :clan/poo/object .def .o))

(.def Build
  defbuild-script:
  (.o spec:
    '("event-fold-scheme-modules" "event-fold-scheme-build"
      "event-fold-scheme" "fold-graph-catalog" "rust-pure-aot" "result-projection"
      "contextual-parser-ir" "decision-code")))
