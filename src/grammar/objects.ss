;;; Publish existing services without executing them during namespace discovery.
(import (only-in :clan/poo/object .def .get)
        "./types.ss"
        (prefix-in (only-in "./algebra.ss" grammar-expression?) api0.)
        (prefix-in (only-in "./lexical-algebra.ss" lexical-expression?) api1.))
(export Grammar.)
(.def (Grammar. @ (.get GrammarScopeContract proto))
  grammar-expression?: api0.grammar-expression?
  lexical-expression?: api1.lexical-expression?)
