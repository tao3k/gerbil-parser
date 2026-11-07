;;; Publish existing services without executing them during namespace discovery.
(import (only-in :clan/poo/object .def .get)
        "./types.ss"
        (prefix-in (only-in "./normalize.ss" compile-grammar) api0.))
(export Compiler.)
(.def (Compiler. @ (.get CompilerScopeContract proto))
  compile-grammar: api0.compile-grammar)
