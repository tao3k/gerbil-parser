(import (only-in :clan/poo/object .def .get)
        (only-in :clan/poo/io module-action)
        (only-in "./types.ss" RustScopeContract))
(export Rust.)

;; Scope values cross into native callers, so use the compiled owner here.
(def call-render-module
  (module-action "gerbil-parser/language-support/rust/syntax"
                 'gerbil-parser/language-support/rust/syntax#rust-render))

(.def (Rust. @ (.get RustScopeContract proto))
  render-module: (lambda args (call-render-module args)))
