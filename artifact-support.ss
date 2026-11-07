;;; Public backend-neutral ParseArtifact and event protocol.
(import ./src/runtime/artifact)
(export (import: ./src/runtime/artifact))

;;; Runtime machine identity belongs to artifact inspection, not compiler setup.
(import (only-in ./src/runtime/machine parser-machine-grammar-digest))
(export parser-machine-grammar-digest)
