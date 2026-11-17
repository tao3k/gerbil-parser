;;; Select existing subsystem scopes and the runtime execution leaves.
(import (only-in :tao3k/yuanxu/module-system/selection/init-syntax yuanxu!))

(yuanxu!
  :load-modules "./src" mode: explicit rename: Library
  (build-support) (compiler) (ffi) (grammar) (graph) (language)
  (runtime) (testing) (utilities)
  :load-modules "./src/runtime" mode: explicit rename: Execution
  (scan) (lr-parser) (incremental))
