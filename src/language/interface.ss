(.def Language
  identity: 'language
  core: (.o requires: (.o) scope-contract: LanguageScopeContract exports: Config projections: (list Build))
  features: (.o))
