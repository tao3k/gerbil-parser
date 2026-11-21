(import (only-in :clan/poo/object .def .o)
        (only-in "./types.ss" CompilerScopeContract)
        (only-in "./objects.ss" Compiler.))
(.def Compiler
  identity: 'compiler
  core: (.o scope-contract: CompilerScopeContract exports: Compiler.)
  features: (.o))
