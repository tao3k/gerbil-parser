(import (only-in :clan/poo/object .def .o)
        (only-in "./types.ss" ScannerScopeContract)
        (only-in "./objects.ss" Scanner.))
(.def Scanner
  identity: 'scan
  core: (.o scope-contract: ScannerScopeContract exports: Scanner.)
  features: (.o))
