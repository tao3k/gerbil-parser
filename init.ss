prelude: :gerbil/core
;;; Select existing subsystem scopes and the runtime execution leaves.
(import (only-in :tao3k/yuanxu/module-system/selection/init-syntax yuanxu!))

(yuanxu!
  :build-roots ("language-build-support"
    "compiler-build-support"
    "language-support/rust/emit"
    "event-fold-build-support"
    "event-fold-program-support"
    "language-support/event-fold/aot"
    "language-support/event-fold/run"
    "line-structure-support"
    "graph-projection-support"
    "artifact-support"
    "source-line-support"
    "parser-actor-support"
    "language-test-support"
    "graph-query-support"
    "graph-syntax-support"
    "src/declarations"
    "language-support"
    "language-source-support"
    "language-support/grammar"
    "language-support/command-grammar"
    "language-support/projection"
    "language-support/shell"
    "language-support/structured"
    "language-abi-support"
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
