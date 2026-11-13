(import (only-in :tao3k/yuanxu/module-system/source/declaration .def))
(.def Testing
  identity: 'testing
  core: (.o requires: (.o) scope-contract: TestingScopeContract exports: Testing. projections: (list Build))
  features: (.o))
