(import (only-in :tao3k/yuanxu/module-system/source/declaration .def)
        (only-in :clan/poo/object .o))
(.def ForeignInterface
  identity: 'ffi
  core: (.o scope-contract: ForeignInterfaceScopeContract exports: ForeignInterface.)
  features: (.o))
