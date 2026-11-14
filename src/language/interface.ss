(import (only-in :tao3k/yuanxu/module-system/source/declaration .def)
        (only-in :clan/poo/object .o))
(.def Language
  identity: 'language
  core: (.o scope-contract: LanguageScopeContract exports: Language.)
  features: (.o))
