;;; Publish existing services without executing them during namespace discovery.
(import (only-in :clan/poo/object .def .get)
        "./types.ss"
        (prefix-in (only-in "./parser/graph-query-objects.ss" make-graph-query-context) api0.)
        (prefix-in (only-in "./parser/graph-query-types.ss" graph-query-view?) api1.))
(export ParserModules.)
(.def (ParserModules. @ (.get ParserModulesScopeContract proto))
  make-graph-query-context: api0.make-graph-query-context
  graph-query-view?: api1.graph-query-view?)
