(import (only-in :clan/poo/object .def .o)
        (only-in "./types.ss" GrammarScopeContract)
        (only-in "./objects.ss" Grammar.))
(.def Grammar
  identity: 'grammar
  core: (.o scope-contract: GrammarScopeContract exports: Grammar.)
  features: (.o))
