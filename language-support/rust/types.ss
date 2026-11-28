(import (only-in :clan/poo/object .cc)
        (only-in :clan/poo/type Fun Record String Type. define-type)
        (only-in :clan/poo/io module-action))
(export RustScopeContract)

(def compiled-value?
  (module-action "gerbil-parser/language-support/rust/syntax"
                 'gerbil-parser/language-support/rust/syntax#rust-module-form?))

(define-type (RustModule @ Type.)
  .element?: (lambda (value) (compiled-value? (list value))))

(define-type RustScope
  (Record render-module: [(Fun String <- RustModule)]))

(def RustScopeContract (.cc RustScope identity: 'rust/scope))
