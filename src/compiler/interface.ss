(import (only-in :tao3k/yuanxu/module-system/source/declaration .def))
(.def Compiler
  identity: 'compiler
  core: (.o requires: (.o) scope-contract: CompilerScopeContract exports: Compiler. projections: (list Build))
  features: (.o))
