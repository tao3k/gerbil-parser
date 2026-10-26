;;; Run directly with gerbil test, in its own native harness process.
;;; No Parser test-support import: the first Case owns policy initialization.
(import :std/test)
(export compiler-policy-test)

(def compiler-policy-test
  (test-suite "native compiler policy initialization"
    (test-case "importing policy preserves absent compiler configuration"
      (let ((cores (getenv "GERBIL_BUILD_CORES" #f))
            (flags (getenv "CORCXXFLAGS_GAMBUILD" #f)))
        (try
          (setenv "GERBIL_BUILD_CORES")
          (setenv "CORCXXFLAGS_GAMBUILD")
          (load-module "gerbil-parser/src/build-support/compiler-profile")
          (check (list (getenv "GERBIL_BUILD_CORES" #f)
                       (getenv "CORCXXFLAGS_GAMBUILD" #f)) => '(#f #f))
          (finally
            (if cores (setenv "GERBIL_BUILD_CORES" cores) (setenv "GERBIL_BUILD_CORES"))
            (if flags (setenv "CORCXXFLAGS_GAMBUILD" flags) (setenv "CORCXXFLAGS_GAMBUILD"))))))
    (test-case "explicit compiler search path survives initialization"
      (let ((path (getenv "COMPILER_PATH" #f))
            (flags (getenv "CORCXXFLAGS_GAMBUILD" #f)))
        (try
          (setenv "COMPILER_PATH" "/explicit/compiler-tools")
          (setenv "CORCXXFLAGS_GAMBUILD" "-O2")
          ((eval 'gerbil-parser/src/build-support/compiler-profile#initialize-compiler-build!))
          (check (getenv "COMPILER_PATH" #f) => "/explicit/compiler-tools")
          (finally
            (if path (setenv "COMPILER_PATH" path) (setenv "COMPILER_PATH"))
            (if flags (setenv "CORCXXFLAGS_GAMBUILD" flags) (setenv "CORCXXFLAGS_GAMBUILD"))))))))
