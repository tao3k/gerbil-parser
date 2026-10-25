#!/usr/bin/env gxi
;;; Compile the C ABI consumer with the standard module build owner.
(import (only-in "./src/build-support/compiler-profile.ss" compiler-build-spec))
(import (only-in "./src/build-support/native-links.ss" native-module-link-options)
        (only-in :std/build-script defbuild-script)
        (only-in :std/source this-source-file))
(def current-ffi-linking? (make-parameter #f))
(def (compiled-spec module (objects '()))
  `(gxc: ,module "-cc-options"
          ,(string-append "-I" (path-expand "include" (path-directory (this-source-file)))
                          " -I" (path-directory (this-source-file)))
          ,@(cond-expand
             (darwin '("-ld-options" "-Wl,-undefined,dynamic_lookup"))
             (else (if (or (not (current-ffi-linking?)) (null? objects)) '()
                       (list "-ld-options" (native-module-link-options objects)))))))
;;; SDK build specs evaluate inside their ordinary main. Delay each spec until
;;; its preceding producer phase has finished, under the single publication lock.
(def (ffi-build-phase spec)
  (defbuild-script (compiler-build-spec (spec)))
  main)
(def (producer-spec)
  (list
   ;; ABI parity uses the admitted production generation. Any required native
   ;; rebuild belongs to this writer, never an untracked recipe compilation.
   ;; std/make keeps current production modules; the public native test route
   ;; and exact-byte fixture assertions remain unchanged.
   (compiled-spec "src/ffi/language-abi-context")
   (compiled-spec "src/ffi/event-binary")
   (compiled-spec "src/ffi/language-artifact-codec")
   ;; The native fixture producer imports these declaration values. Publish
   ;; its complete dynamic closure rather than depending on conformance's
   ;; optional static generation or a previous source-only helper load.
   (compiled-spec "languages/arithmetic/parser-test")
   (compiled-spec "languages/bash/parser-test")
   (compiled-spec "languages/cypher/parser-test")
   (compiled-spec "languages/fhirpath/parser-test")
   (compiled-spec "languages/gql/parser-test")
   (compiled-spec "languages/hcl/parser-test")
   (compiled-spec "languages/hl7/parser-test")
   (compiled-spec "languages/tla-plus/parser-test")
   (compiled-spec "t/fixtures/language-entries")
   (compiled-spec "t/generate-language-abi-alignment")
   (compiled-spec "t/language-abi-alignment-test")
   (compiled-spec "t/fixtures/contextual-result-selection")
   (compiled-spec "t/fixtures/lr-action-selection")
   (compiled-spec "t/fixtures/shared-scanner/records")
   (compiled-spec "t/fixtures/shared-scanner/records-engine-control")))
(def (entry-spec)
  (list
   (compiled-spec "t/fixtures/ffi/abi-probe"
    '("gerbil-parser/src/ffi/language-abi~0"
      "gerbil-parser/src/ffi/rust-aot-abi~0"))
   ;; ELF resolves each loadable module's native references independently.
   ;; The generated language entry calls the runtime owner-thread guard.
   (compiled-spec "t/fixtures/shared-scanner/records-abi"
    '("gerbil-parser/src/ffi/language-abi~0"))
   (compiled-spec "t/fixtures/shared-scanner/records-engine-abi"
    '("gerbil-parser/src/ffi/language-abi~0"))
   (compiled-spec "t/benchmarks/source-edits/abi-call"
    '("gerbil-parser/src/ffi/language-abi~0"))))
(def (consumer-spec)
  (list
   (compiled-spec "t/fixtures/ffi/language-probe"
    '("gerbil-parser/src/ffi/language-abi~0"
      "gerbil-parser/t/fixtures/shared-scanner/records-abi~0"))
   (compiled-spec "t/benchmarks/source-edits/benchmark")
   (compiled-spec "t/benchmarks/language-abi/utf8-cpu")))

(def ffi-specs (list producer-spec entry-spec consumer-spec))
;; Keep the SDK's single metadata/spec/clean protocol as well as its builder.
(defbuild-script (compiler-build-spec (append (producer-spec) (entry-spec) (consumer-spec))))
(let (sdk-main main)
 (set! main
  (lambda args
    (let (compile? (or (null? args) (equal? (car args) "compile")))
      (parameterize ((current-ffi-linking?
                      (or compile? (and (pair? args) (equal? (car args) "spec")))))
        (if compile?
          (for-each (lambda (spec) (apply (ffi-build-phase spec) args)) ffi-specs)
          (apply sdk-main args)))))))
