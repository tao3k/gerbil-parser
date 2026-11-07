;;; -*- Gerbil -*-
;;; Public POO/AOT support for language-owned graph projections.

(import (only-in ./src/graph/graph-projection-objects
                 GraphProjection. GraphNode. GraphField.
                 make-graph-projection make-graph-node make-graph-field
                 graph-projection?)
        (only-in ./src/compiler/graph-projection-runtime
                 graph-projection-digest
                 generate-graph-projection-runtime-module))
(export GraphProjection. GraphNode. GraphField.
        make-graph-projection make-graph-node make-graph-field
        graph-projection?
        graph-projection-digest
        generate-graph-projection-runtime-module)

(import (only-in ./src/graph/graph-projection-objects
                 graph-projection-nodes graph-node-syntax-kind graph-node-category
                 graph-node-label graph-node-fields graph-field-token graph-field-name graph-field-mode))
(export graph-projection-nodes graph-node-syntax-kind graph-node-category
        graph-node-label graph-node-fields graph-field-token graph-field-name graph-field-mode)
