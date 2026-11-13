(import (only-in :tao3k/yuanxu/module-system/source/declaration .def))
(.def Language
  identity: 'language
  core: (.o requires: (.o) scope-contract: LanguageScopeContract exports: Language.)
  features: (.o))
