(import (only-in :clan/poo/object .def .o)
        (only-in "./types.ss" LrParserScopeContract)
        (only-in "./objects.ss" LrParser.))
(.def LrParser
  identity: 'lr-parser
  core: (.o scope-contract: LrParserScopeContract exports: LrParser.)
  features: (.o))
