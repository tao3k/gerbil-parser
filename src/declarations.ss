;;; -*- Gerbil -*-
;;; Boundary: stable POO-native parser object facade for authors and compilers.
;;; Invariant: type, object, function, and syntax owners remain explicit while
;;; downstream modules import one reviewed public surface.

(import ./grammar/declaration-types
        ./grammar/declaration-objects
        ./grammar/declaration-funcs
        ./grammar/declaration-syntax
        ./language/source-pattern-funs
        ./language/line-structure-objects
        ./language/source-event-scope-types
        ./language/source-event-scope-objects
        ./language/source-event-scope-funs
        ./language/line-event-funs
        ./language/list-event-funs
        ./language/inline-link-event-funs
        ./language/source-boundary-types
        ./language/source-boundary-objects
        ./language/source-boundary-funs)
(export (import: ./grammar/declaration-types)
        (import: ./grammar/declaration-objects)
        (import: ./grammar/declaration-funcs)
        (import: ./grammar/declaration-syntax)
        (import: ./language/source-pattern-funs)
        (import: ./language/line-structure-objects)
        (import: ./language/source-event-scope-types)
        (import: ./language/source-event-scope-objects)
        (import: ./language/source-event-scope-funs)
        (import: ./language/line-event-funs)
        (import: ./language/list-event-funs)
        (import: ./language/inline-link-event-funs)
        (import: ./language/source-boundary-types)
        (import: ./language/source-boundary-objects)
        (import: ./language/source-boundary-funs))
