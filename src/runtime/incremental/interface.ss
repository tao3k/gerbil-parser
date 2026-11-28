(import (only-in :clan/poo/object .def .o)
        (only-in "./types.ss" IncrementalScopeContract)
        (only-in "./objects.ss" Incremental.))
(.def Incremental
  identity: 'incremental
  core: (.o scope-contract: IncrementalScopeContract exports: Incremental.)
  features: (.o))
