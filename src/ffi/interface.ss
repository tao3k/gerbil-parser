(import (only-in :tao3k/yuanxu/module-system/source/declaration .def))
(.def ForeignInterface
  identity: 'ffi
  core: (.o requires: (.o) scope-contract: ForeignInterfaceScopeContract exports: ForeignInterface. projections: (list Build))
  features: (.o))
