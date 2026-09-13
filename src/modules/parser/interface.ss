;;; -*- Gerbil -*-
;;; Boundary: stable POO-native parser object facade for authors and compilers.
;;; Invariant: type, object, function, and syntax owners remain explicit while
;;; downstream modules import one reviewed public surface.

(import ./types
        ./objects
        ./funcs
        ./syntax)
(export (import: ./types)
        (import: ./objects)
        (import: ./funcs)
        (import: ./syntax))
