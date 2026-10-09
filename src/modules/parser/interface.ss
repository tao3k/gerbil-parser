;;; -*- Gerbil -*-
;;; Boundary: stable POO-native parser object facade for authors and compilers.
;;; Invariant: type, object, function, and syntax owners remain explicit while
;;; downstream modules import one reviewed public surface.

(import ./types
        ./objects
        ./funcs
        ./syntax
        ./line-structure-objects
        ./source-event-scope-types
        ./source-event-scope-objects
        ./source-event-scope-funs
        ./line-event-funs
        ./inline-link-event-funs
        ./source-boundary-types
        ./source-boundary-objects
        ./source-boundary-funs)
(export (import: ./types)
        (import: ./objects)
        (import: ./funcs)
        (import: ./syntax)
        (import: ./line-structure-objects)
        (import: ./source-event-scope-types)
        (import: ./source-event-scope-objects)
        (import: ./source-event-scope-funs)
        (import: ./line-event-funs)
        (import: ./inline-link-event-funs)
        (import: ./source-boundary-types)
        (import: ./source-boundary-objects)
        (import: ./source-boundary-funs))
