(.def ForeignInterface
  identity: 'ffi
  core: (.o requires: (.o) scope-contract: ForeignInterfaceScopeContract exports: Config projections: (list Build))
  features: (.o))
