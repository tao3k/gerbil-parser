(import (only-in :clan/poo/object .def .o)
        (only-in "./types.ss" BuildSupportScopeContract)
        (only-in "./objects.ss" BuildSupport.))
(.def BuildSupport
  identity: 'build-support
  core: (.o scope-contract: BuildSupportScopeContract exports: BuildSupport.)
  features: (.o))
