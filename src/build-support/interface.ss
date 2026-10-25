(.def BuildSupport
  identity: 'build-support
  core: (.o requires: (.o) scope-contract: BuildSupportScopeContract exports: Config)
  features: (.o))
