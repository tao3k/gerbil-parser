(import (only-in :clan/poo/object .def .o)
        (only-in "./types.ss" GraphScopeContract)
        (only-in "./objects.ss" Graph.))
(.def Graph
  identity: 'graph
  core: (.o scope-contract: GraphScopeContract exports: Graph.)
  features: (.o))
