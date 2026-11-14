(import (only-in :tao3k/yuanxu/module-system/source/declaration .def)
        (only-in :clan/poo/object .o))
(.def Graph
  identity: 'graph
  core: (.o scope-contract: GraphScopeContract exports: Graph.)
  features: (.o))
