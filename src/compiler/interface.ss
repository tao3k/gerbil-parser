(import (only-in :tao3k/yuanxu/module-system/source/declaration .def)
        (only-in :clan/poo/object .o))
(.def Compiler
  identity: 'compiler
  core: (.o scope-contract: CompilerScopeContract exports: Compiler.)
  features: (.o))
