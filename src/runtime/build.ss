;;; Public library roots owned by this Module.
(import (only-in :clan/poo/object .def .o))

(.def Build
  defbuild-script:
  (.o spec:
    '("fold-tape" "fold-cost" "machine" "lexical-expression"
      "lexical-lexer" "incremental" "recovery" "source-engines")))
