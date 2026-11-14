(import (only-in :tao3k/yuanxu/module-system/source/declaration .def)
        (only-in :clan/poo/object .o))
(.def Utilities
  identity: 'utilities
  core: (.o scope-contract: UtilitiesScopeContract exports: Utilities.)
  features: (.o))
