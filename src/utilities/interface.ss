(import (only-in :clan/poo/object .def .o)
        (only-in "./types.ss" UtilitiesScopeContract)
        (only-in "./objects.ss" Utilities.))
(.def Utilities
  identity: 'utilities
  core: (.o scope-contract: UtilitiesScopeContract exports: Utilities.)
  features: (.o))
