;;; Native Core projection retains the complete library and foreign options.
(import :gerbil-parser/language-test-support
        (only-in :clan/poo/object .o .cc .get)
        (only-in :std/source this-source-file)
        (only-in :gerbil-parser/src/build-support/compiler-profile run-build-tool)
        (only-in :std/io/tempfile make-temporary-file-name)
        (only-in :clan/poo/mop element?)
        (only-in "../src/utilities/types.ss" UtilitiesScopeContract)
        (only-in "../src/grammar/types.ss" GrammarScopeContract)
        (only-in "../init.ss" gerbil-parser.init.exports)
        (only-in :gerbil-parser/languages/arithmetic/parser parse-arithmetic)
        (only-in :gerbil-parser/languages/arithmetic/grammar arithmetic-grammar)
        (only-in :gerbil-parser/src/schema +language-grammar-schema+)
        (prefix-in (only-in :gerbil-parser/src/compiler/normalize compile-grammar) compiled.))
;; A spec query expands the actual Core/import closure before returning its
;; datum. Report completed SDK imports on the supervised diagnostic stream,
;; keeping spec data separate and the five-second silence deadline unchanged.
(def spec-import-progress
  (object->string
    '(begin
       (import :gerbil/expander)
       (let ((native-import (current-expander-module-import)) (completed 0))
         (current-expander-module-import
           (lambda (path reload?)
             (begin0 (native-import path reload?)
               (set! completed (+ completed 1))
               (display "BUILD-SPEC-IMPORTED " (current-error-port))
               (write completed (current-error-port))
               (newline (current-error-port))
               (force-output (current-error-port)))))))))
(def (query-spec entry)
  ;; Native inherited stderr bypasses the Worker's Scheme output pipe. Keep
  ;; the spec in a private data file and use the existing streaming build-tool
  ;; adapter so actual diagnostics reach the existing idle observer.
  (let (spec-file (make-temporary-file-name "parser-build-spec-"))
    (try
      (run-build-tool
        (list "gxi" "-:max-heap=1G,debug=q" "-e" spec-import-progress
              "-L" (path-expand entry (path-directory (this-source-file)))
              "-e" (object->string
                      `(begin
                         (call-with-output-file ,spec-file
                           (lambda (output)
                             (parameterize ((current-output-port output))
                               (main "spec"))))
                         ;; The datum is closed before the existing disposable
                         ;; test-child exit. Native teardown is not spec work.
                         (load-module "gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process")
                         ((eval 'gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process#test-child-process-exit!) 0))))
        text?: #t)
      (call-with-input-file spec-file
        (lambda (input)
          (let (spec (read input))
            (unless (and (not (eof-object? spec)) (eof-object? (read input)))
              (error "native spec query did not publish one datum" entry))
            spec)))
      (finally (when (file-exists? spec-file) (delete-file spec-file))))))
(deflanguage-parser-tests library-init-test "native Core library projection"
  (engine)
  (scenario "actual service Scopes validate and reject malformed values"
    (sequential
      ((utilities (.get gerbil-parser.init.exports Components utilities scope))
       (grammar (.get gerbil-parser.init.exports Components grammar scope)))
      (equal (element? UtilitiesScopeContract utilities) #t)
      (equal (element? GrammarScopeContract grammar) #t)
      (equal ((.get utilities append-unique) '(1) 2) '(1 2))
      (equal ((.get grammar grammar-expression?) '(empty)) #t)
      (equal (element? UtilitiesScopeContract (.o)) #f)
      (equal (element? UtilitiesScopeContract (.cc utilities append-unique: 42)) #f)
      (equal (element? GrammarScopeContract (.cc grammar lexical-expression?: 42)) #f)))
  (scenario "compiled products interoperate through structural Scopes"
    (sequential
      ((runtime (.get gerbil-parser.init.exports Components runtime scope))
       (compiler (.get gerbil-parser.init.exports Components compiler scope))
       (language (.get gerbil-parser.init.exports Components language scope))
       (artifact (parse-arithmetic "1 + 2"))
       (ir ((.get compiler compile-grammar) arithmetic-grammar)))
      (equal ((.get runtime parse-artifact-valid?) artifact) #t)
      (equal ((.get runtime parse-artifact-success?) artifact) #t)
      (equal ((.get runtime parse-artifact-roundtrip) artifact) "1 + 2")
      (equal ((.get runtime parse-artifact-valid?) '((schema . "invalid"))) #f)
      (equal ir (compiled.compile-grammar arithmetic-grammar))
      (equal (.get language language-grammar-schema) +language-grammar-schema+)))
  (scenario "init projection preserves public entries and ABI boundaries"
    (sequential
      ((spec (query-spec "../build.ss"))
       (names (map cadr spec))
       (seen (make-hash-table)))
      (each (name) (names)
        (equal (hash-key? seen name) #f)
        (act (hash-put! seen name #t)))
      ;; The development CI gate independently checks the entire native inventory.
      ;; This unit checks public compatibility and phase/ABI boundaries once.
      (each (name) ('("language-support" "language-test-support" "graph-query-support"
                     "src/api" "src/runtime/machine" "src/compiler/machine"))
        (equal (and (member name names) #t) #t))
      (each (name) ('("src/ffi/language-abi" "src/ffi/language-abi-context" "src/ffi/rust-aot-abi"))
        (scope ((entry (assoc name (map cdr spec))))
          (equal (and entry #t) #t)
          (equal (and (member "-cc-options" entry) #t) #t)))
      (each (name) ('("modules/parser/types" "modules/parser/objects" "modules/parser/config"
                      "modules/parser/interface" "build-modules"))
        (equal (and (member name names) #t) #f)))))
