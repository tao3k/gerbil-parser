(import (only-in :clan/poo/object .cc)
        (only-in :clan/poo/type Fun Record String Symbol define-type)
        (only-in ../../src/language/source-fragment-types
                 SourceDelimitedFragment SourceFirstSplit SourceReferenceScan SourceReferenceMarkers))
(export SourceFragmentsScopeContract)

(define-type SourceFragmentsScope
  (Record delimited-fragment: [(Fun SourceDelimitedFragment <- String Symbol Symbol Symbol)]
          first-split: [(Fun SourceFirstSplit <- String Symbol Symbol Symbol)]
          reference-scan: [(Fun SourceReferenceScan <- SourceReferenceMarkers String String Symbol Symbol Symbol)]))

(def SourceFragmentsScopeContract (.cc SourceFragmentsScope identity: 'source-fragments/scope))
