(.def BuildSupport
  identity: 'build-support
  core: (.o requires: (.o) scope-contract: BuildSupportScopeContract exports: BuildSupport.)
  features: (.o))
