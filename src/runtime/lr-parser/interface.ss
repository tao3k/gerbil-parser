(import (only-in :tao3k/yuanxu/module-system/source/declaration .def)
        (only-in :clan/poo/object .o))
(.def LrParser
  identity: 'lr-parser
  core: (.o scope-contract: LrParserScopeContract exports: LrParser.)
  features: (.o))
