(import (only-in :tao3k/yuanxu/module-system/source/declaration .def))
(.def Runtime
  identity: 'runtime
  core: (.o requires: (.o) scope-contract: RuntimeScopeContract exports: Runtime. projections: (list Build))
  features: (.o))
