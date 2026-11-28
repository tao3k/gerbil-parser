(import (only-in :clan/poo/object .def .o)
        (only-in "./types.ss" EventFoldScopeContract)
        (only-in "./objects.ss" EventFold.))
(.def EventFold
  identity: 'event-fold
  core: (.o scope-contract: EventFoldScopeContract exports: EventFold.)
  features: (.o))
