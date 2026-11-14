;;; Public library roots owned by this Module.
(import (only-in :clan/poo/object .def .o))

(.def Build
  defbuild-script:
  (.o spec:
    '("source-execution" "source-witness" "language-assertions"
      "language-product" "language-strategy" "language-source-assertions")))
