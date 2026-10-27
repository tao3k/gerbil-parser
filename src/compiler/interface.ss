(.def Compiler
  identity: 'compiler
  core: (.o requires: (.o) scope-contract: CompilerScopeContract exports: Compiler. projections: (list Build))
  features: (.o))
