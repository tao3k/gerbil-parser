(import (only-in :tao3k/yuanxu/module-system/source/declaration .def))
(.def BuildSupport
  identity: 'build-support
  core: (.o requires: (.o) scope-contract: BuildSupportScopeContract exports: BuildSupport.)
  features: (.o))
