(.def Compiler
  identity: 'compiler
  core: (.o requires: (.o) scope-contract: CompilerScopeContract exports: Config projections: (list Build))
  features: (.o))
