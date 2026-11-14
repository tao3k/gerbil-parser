;;; Select subsystem services once; Core derives the native source closure.
(import (only-in :tao3k/yuanxu/module-system/selection/init-syntax yuanxu!))

(yuanxu!
  :load-modules "./src" mode: explicit rename: Components :test-dir "./t"
  (build-support) (compiler) (ffi) (grammar) (graph) (language)
  (runtime) (testing) (utilities))
