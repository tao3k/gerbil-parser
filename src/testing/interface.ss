(import (only-in :tao3k/yuanxu/module-system/source/declaration .def)
        (only-in :clan/poo/object .o))
(.def Testing
  identity: 'testing
  core: (.o scope-contract: TestingScopeContract exports: Testing.)
  features: (.o))
