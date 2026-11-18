(import (only-in :clan/poo/object .def .o)
        (only-in "./types.ss" SourceFragmentsScopeContract)
        (only-in "./objects.ss" SourceFragments.))
(.def SourceFragments
  identity: 'source-fragments
  core: (.o scope-contract: SourceFragmentsScopeContract exports: SourceFragments.)
  features: (.o))
