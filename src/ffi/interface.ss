(import (only-in :clan/poo/object .def .o)
        (only-in "./types.ss" ForeignInterfaceScopeContract)
        (only-in "./objects.ss" ForeignInterface.))
(.def ForeignInterface
  identity: 'ffi
  core: (.o scope-contract: ForeignInterfaceScopeContract exports: ForeignInterface.)
  features: (.o))
