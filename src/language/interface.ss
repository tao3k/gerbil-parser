(import (only-in :clan/poo/object .def .o)
        (only-in "./types.ss" LanguageScopeContract)
        (only-in "./objects.ss" Language.))
(.def Language
  identity: 'language
  core: (.o scope-contract: LanguageScopeContract exports: Language.)
  features: (.o))
