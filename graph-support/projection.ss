;;; Public POO projection values and build-time projection generation.
(import ./values
        (only-in ../src/compiler/fold-graph-catalog fold-graph-catalog)
        (only-in ../src/compiler/graph-projection-runtime
                 graph-projection-digest
                 generate-graph-projection-runtime-module))
(export (import: ./values)
        graph-projection-digest
        generate-graph-projection-runtime-module fold-graph-catalog)
