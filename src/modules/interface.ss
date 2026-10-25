(.def ParserModules
  identity: 'modules
  core: (.o requires: (.o) scope-contract: ParserModulesScopeContract exports: Config projections: (list Build))
  features: (.o))
