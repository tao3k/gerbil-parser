(import (only-in :tao3k/yuanxu/module-system/source/declaration .def)
        (only-in :clan/poo/object .o))
(.def Grammar
  identity: 'grammar
  core: (.o scope-contract: GrammarScopeContract exports: Grammar.)
  features: (.o))
