(import (only-in :tao3k/yuanxu/module-system/source/declaration .def))
(.def Grammar
  identity: 'grammar
  core: (.o requires: (.o) scope-contract: GrammarScopeContract exports: Grammar.)
  features: (.o))
