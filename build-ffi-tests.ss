#!/usr/bin/env gxi
;;; Compile the C ABI consumer with the standard module build owner.
(import (only-in "./src/build-support/library-publication.ss" publication-build-main)
        (only-in "./src/build-support/compiler-profile.ss" compiler-build-spec))
(import (only-in :std/build-script defbuild-script)
        (only-in :std/source this-source-file))
(def (compiled-spec module (objects '()))
  `(gxc: ,module "-cc-options"
          ,(string-append "-I" (path-expand "include" (path-directory (this-source-file)))
                          " -I" (path-directory (this-source-file)))
          ,@(cond-expand
             (darwin '("-ld-options" "-Wl,-undefined,dynamic_lookup"))
             (else (if (null? objects) '()
                       (list "-ld-options" (string-join
                         (map (lambda (object) (path-expand object (gerbil-path))) objects) " ")))))))
(defbuild-script
  (compiler-build-spec (list
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
   (compiled-spec "t/fixtures/ffi/abi-probe"
    '("lib/gerbil-parser/src/ffi/language-abi~0.o1"
      "lib/gerbil-parser/src/ffi/rust-aot-abi~0.o1"))
   ;; ELF resolves each loadable module's native references independently.
   ;; The generated language entry calls the runtime owner-thread guard.
   (compiled-spec "t/fixtures/contextual-result-selection")
   (compiled-spec "t/fixtures/lr-action-selection")
   ;; The reusable library must carry the interface producer before its ABI consumers.
   (compiled-spec "t/fixtures/shared-scanner/records")
   (compiled-spec "t/fixtures/shared-scanner/records-engine-control")
   (compiled-spec "t/fixtures/shared-scanner/records-abi"
    '("lib/gerbil-parser/src/ffi/language-abi~0.o1"))
   (compiled-spec "t/fixtures/shared-scanner/records-engine-abi"
    '("lib/gerbil-parser/src/ffi/language-abi~0.o1"))
   (compiled-spec "t/fixtures/ffi/language-probe"
    '("lib/gerbil-parser/src/ffi/language-abi~0.o1"
      "lib/gerbil-parser/t/fixtures/shared-scanner/records-abi~0.o1"))
   (compiled-spec "t/benchmarks/source-edits/abi-call"
    '("lib/gerbil-parser/src/ffi/language-abi~0.o1"))
   (compiled-spec "t/benchmarks/source-edits/benchmark")
   (compiled-spec "t/benchmarks/language-abi/utf8-cpu"))))

(set! main (publication-build-main main))
