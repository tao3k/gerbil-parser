(import (only-in :clan/poo/object .def .o)
        (only-in "./types.ss" RuntimeScopeContract)
        (only-in "./objects.ss" Runtime.))
(.def Runtime
  identity: 'runtime
  core: (.o scope-contract: RuntimeScopeContract exports: Runtime.)
  features: (.o))
