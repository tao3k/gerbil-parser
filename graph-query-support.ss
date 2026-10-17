;;; -*- Gerbil -*-
;;; Public POO graph-query surface for language-owned graph views.

(import (only-in ./src/modules/parser/graph-query-types
                 GraphQueryViewContract GraphQueryContextContract
                 graph-query-view? graph-query-context?)
        (only-in ./src/modules/parser/graph-query-objects
                 GraphQueryContext. make-graph-query-context)
        (only-in ./src/modules/parser/graph-query-funs
                 graph-query-map graph-query-lineage? graph-query-select))
(export GraphQueryViewContract GraphQueryContextContract GraphQueryContext.
        graph-query-view? graph-query-context?
        make-graph-query-context
        graph-query-map graph-query-lineage? graph-query-select)
