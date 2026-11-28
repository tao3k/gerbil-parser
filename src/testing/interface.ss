(import (only-in :clan/poo/object .def .o)
        (only-in "./types.ss" TestingScopeContract)
        (only-in "./objects.ss" Testing.))
(.def Testing
  identity: 'testing
  core: (.o scope-contract: TestingScopeContract exports: Testing.)
  features: (.o))
