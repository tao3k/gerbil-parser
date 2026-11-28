;;; Compatibility Parser prelude; prefer plain `:gerbil/core` + `(import :gerbil-parser/lang)`.
prelude: :gerbil/core
package: tao3k/parser
(import :gerbil/core
        (phi: +1 :gerbil/core)
        (phi: +2 :gerbil/core)
        "./lang.ss")
(export (import: :gerbil/core)
        (phi: +1 (import: :gerbil/core))
        (phi: +2 (import: :gerbil/core))
        (import: "./lang.ss"))
