(.def Language
  identity: 'language
  core: (.o requires: (.o) scope-contract: LanguageScopeContract exports: Language. projections: (list Build))
  features: (.o))
