(import (only-in :tao3k/yuanxu/module-system/source/declaration .def)
        (only-in :clan/poo/object .o))
(.def Incremental
  identity: 'incremental
  core: (.o scope-contract: IncrementalScopeContract exports: Incremental.)
  features: (.o))
