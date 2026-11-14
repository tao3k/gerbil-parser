(import (only-in :tao3k/yuanxu/module-system/source/declaration .def)
        (only-in :clan/poo/object .o))
(.def Scanner
  identity: 'scan
  core: (.o scope-contract: ScannerScopeContract exports: Scanner.)
  features: (.o))
