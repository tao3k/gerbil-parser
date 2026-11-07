;;; Run directly with gerbil test in its own process.
(import :std/test)
(export compiler-policy-test)

(def compiler-policy-test
  (test-suite "compiler environment ownership"
    (test-case "loading build observation leaves caller configuration intact"
      (let* ((names '("PATH" "COMPILER_PATH" "CORCXXFLAGS_GAMBUILD" "GERBIL_BUILD_CORES"))
             (before (map (lambda (name) (getenv name #f)) names)))
        (load-module "gerbil-parser/compiler-build-support")
        (check (map (lambda (name) (getenv name #f)) names) => before)))))
