(import (only-in :clan/poo/object .def .get) "./types.ss")
(import (only-in ../../src/language/source-pattern-funs
                 source-offset-after source-pattern-end source-pattern-at?
                 source-ascii-ci-pattern-at?)
        (only-in ../../src/language/source-fragment-objects
                 make-source-delimited-fragment make-source-first-split
                 make-source-reference-scan)
        (only-in ../../src/language/source-fragment-types
                 source-delimited-fragment? source-first-split?
                 source-reference-scan?)
        (only-in ../../src/language/source-fragment-funs
                 source-delimited-fragment-initial
                 source-delimited-fragment-forms
                 source-first-split-initial source-first-split-forms
                 source-reference-scan-initial source-reference-scan-forms))
(export SourceFragments. source-offset-after source-pattern-end source-pattern-at?
        source-ascii-ci-pattern-at?
        make-source-delimited-fragment source-delimited-fragment?
        source-delimited-fragment-initial source-delimited-fragment-forms
        make-source-first-split source-first-split?
        source-first-split-initial source-first-split-forms
        make-source-reference-scan source-reference-scan?
        source-reference-scan-initial source-reference-scan-forms)

(.def (SourceFragments. @ (.get SourceFragmentsScopeContract proto))
  delimited-fragment: make-source-delimited-fragment
  first-split: make-source-first-split
  reference-scan: make-source-reference-scan)
