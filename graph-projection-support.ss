;;; Public POO projection values and build-time projection generation.
(import ./graph-projection-support/values
        (only-in ./src/compiler/fold-graph-catalog fold-graph-catalog)
        (only-in ./src/compiler/graph-projection-runtime
                 graph-projection-digest
                 generate-graph-projection-runtime-module))
(export (import: ./graph-projection-support/values)
        graph-projection-digest
        generate-graph-projection-runtime-module fold-graph-catalog)
