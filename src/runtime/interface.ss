(.def Runtime
  identity: 'runtime
  core: (.o requires: (.o) scope-contract: RuntimeScopeContract exports: Config projections: (list Build))
  features: (.o))
