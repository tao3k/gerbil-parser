(import (only-in :tao3k/yuanxu/module-system/source/declaration .def))
(.def Utilities
  identity: 'utilities
  core: (.o requires: (.o) scope-contract: UtilitiesScopeContract exports: Utilities.)
  features: (.o))
