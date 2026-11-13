(import (only-in :tao3k/yuanxu/module-system/source/declaration .def))
(.def Graph
  identity: 'graph
  core: (.o requires: (.o) scope-contract: GraphScopeContract exports: Graph.)
  features: (.o))
