(import (only-in :clan/poo/object .def .o)
        (only-in "./types.ss" LanguageModuleScopeContract)
        (only-in "./objects.ss" LanguageScope))
(.def Language
  identity: 'bash
  core: (.o scope-contract: LanguageModuleScopeContract exports: LanguageScope)
  features: (.o))
