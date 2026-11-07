;;; Select subsystem services once; Core derives the native source closure.
(yuanxu!
  :load-modules "./src" mode: explicit rename: Components
  (compiler) (ffi) (grammar) (graph) (language)
  (runtime) (testing) (utilities))
