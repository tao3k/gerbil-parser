;;; Select subsystem services once; Core derives the native source closure.
(import :yuanxu/module-system/selection/init-syntax)
(yuanxu!
  :load-modules "./src" mode: explicit rename: Components
  (build-support) (compiler) (ffi) (grammar) (language)
  (modules) (runtime) (testing) (utilities))
