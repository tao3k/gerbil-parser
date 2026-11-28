prelude: :gerbil/core
;;; Select existing subsystem scopes and the runtime execution leaves.
(import (only-in :tao3k/yuanxu/lang yuanxu!))

(yuanxu!
  :build-roots ("lang"
    "language-support/build"
    "build-support/compiler"
    "language-support/rust/emit"
    "language-support/event-fold/emit"
    "language-support/event-fold/program"
    "language-support/event-fold/aot"
    "language-support/event-fold/run"
    "text/line-structure"
    "graph-support/projection"
    "execution/artifact"
    "text/source-lines"
    "execution/actor"
    "language-support/testing"
    "graph-support/query"
    "graph-support/edit"
    "graph-support/syntax"
    "src/declarations"
    "language-support/interface"
    "language-support/source"
    "language-support/grammar"
    "language-support/command-grammar"
    "language-support/projection"
    "language-support/shell"
    "language-support/structured"
    "language-support/abi"
    "languages/types")
  :load-modules "./src" mode: explicit mount: Library
  (build-support)
  (compiler)
  (ffi)
  (grammar)
  (graph)
  (language)
  (runtime) (testing) (utilities)
  :load-modules "./src/runtime" mode: explicit mount: Execution
  (scan)
  (lr-parser)
  (incremental)
  :load-modules "./language-support" mode: explicit mount: LanguageSupport
  (rust) (event-fold) (source-fragments))
