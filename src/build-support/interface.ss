(import (only-in :tao3k/yuanxu/module-system/source/declaration .def)
        (only-in :clan/poo/object .o))
(.def BuildSupport
  identity: 'build-support
  core: (.o scope-contract: BuildSupportScopeContract exports: BuildSupport.)
  features: (.o))
