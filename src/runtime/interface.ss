(import (only-in :tao3k/yuanxu/module-system/source/declaration .def)
        (only-in :clan/poo/object .o))
(.def Runtime
  identity: 'runtime
  core: (.o scope-contract: RuntimeScopeContract exports: Runtime.)
  features: (.o))
