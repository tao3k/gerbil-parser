(.def Runtime
  identity: 'runtime
  core: (.o requires: (.o) scope-contract: RuntimeScopeContract exports: Runtime. projections: (list Build))
  features: (.o))
