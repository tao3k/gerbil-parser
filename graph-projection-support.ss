;;; -*- Gerbil -*-
;;; Public POO/AOT support for language-owned graph projections.

(import (only-in ./src/modules/parser/graph-projection-objects
                 GraphProjection. GraphNode. GraphField.
                 make-graph-projection make-graph-node make-graph-field
                 graph-projection?)
        (only-in ./src/compiler/graph-projection-rowan
                 generate-graph-projection-rowan-module))
(export GraphProjection. GraphNode. GraphField.
        make-graph-projection make-graph-node make-graph-field
        graph-projection?
        generate-graph-projection-rowan-module)
