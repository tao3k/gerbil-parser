;;; Test FFI targets and their native options belong to the fixture module.
(import (only-in :std/source this-source-file)
        (only-in "./native-links.ss" native-module-link-options))
(export ffi-build-spec ffi-build-phases ffi-test-command)
(def fixture-root (path-normalize (path-expand "../../.." (path-directory (this-source-file)))))
(def current-ffi-linking? (make-parameter #f))
(def (compiled-spec module (objects '()))
  `(gxc: ,module "-cc-options"
          ,(string-append "-I" (path-expand "include" fixture-root)
                          " -I" fixture-root)
          ,@(cond-expand
             (darwin '("-ld-options" "-Wl,-undefined,dynamic_lookup"))
             (else (if (or (not (current-ffi-linking?)) (null? objects)) '()
                       (list "-ld-options" (native-module-link-options objects)))))))
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

(def (ffi-build-phases)
  (map (lambda (spec)
         (lambda ()
           (parameterize ((current-ffi-linking? #t)) (spec))))
       (list producer-spec entry-spec consumer-spec)))
;; Keep the SDK's single metadata/spec/clean protocol as well as its builder.
(def (ffi-build-spec (link? #f))
  (parameterize ((current-ffi-linking? link?))
    (append (producer-spec) (entry-spec) (consumer-spec))))

(def (ffi-test-command command runtime-options)
  (cond-expand
   (linux
    ;; ELF fixture consumers link the installed producers by absolute path.
    ;; Initialize those same instances before the signed overlay can load
    ;; another copy of their owner state. Restore normal lookup for GxTest.
    (def arguments (list-tail command (+ 2 (length runtime-options))))
    (append '("gxi") runtime-options
      (list "-e"
        (object->string
          `(begin
             (import (only-in :gerbil/runtime/loader load-path set-load-path! load-module)
                     (only-in :gerbil/tools/gxtest main))
             (let ((paths (load-path)))
               (dynamic-wind
                 (lambda () (set-load-path! (cons (path-expand "lib" (gerbil-path)) paths)))
                 (lambda ()
                   (for-each (lambda (module) (load-module module))
                     '("gerbil-parser/src/ffi/language-abi"
                       "gerbil-parser/src/ffi/rust-aot-abi"
                       "gerbil-parser/t/fixtures/shared-scanner/records-abi")))
                 (lambda () (set-load-path! paths))))
             (main ,@arguments))))))
   (else command)))
