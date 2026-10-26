;;; Native Core projection retains the complete library and foreign options.
(import :gerbil-parser/language-test-support
        (only-in :clan/poo/object .o .cc .get)
        (only-in :std/source this-source-file)
        (only-in :gerbil-parser/src/build-support/compiler-profile run-build-tool)
        (only-in :std/io/tempfile make-temporary-file-name)
        (only-in :clan/poo/mop element? validate TypeError? define-type Type.)
        (only-in :clan/poo/type String Symbol TypeValuePair)
        (only-in :clan/poo/number Integer)
        (only-in "../src/utilities/types.ss" UtilitiesScopeContract UtilitiesScope)
        (only-in "../src/grammar/types.ss" GrammarScopeContract)
        (only-in "../src/ffi/types.ss" ForeignInterfaceScopeContract)
        (only-in "../src/language/types.ss" LanguageScopeContract)
        (only-in "../init.ss" gerbil-parser.init.exports)
        (only-in :gerbil-parser/languages/arithmetic/parser parse-arithmetic)
        (only-in :gerbil-parser/languages/arithmetic/grammar arithmetic-grammar)
        (only-in :gerbil-parser/src/modules/parser/objects make-grammar)
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
(def (query-spec entry (noise? #f))
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
                         ,@(if noise?
                             '((def actual-modules-spec modules-spec)
                               (set! modules-spec
                                 (lambda (roots)
                                   (displayln "SPEC-PREPARATION-NOISE")
                                   (actual-modules-spec roots)))) '())
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
            (unless (and (list? spec) (eof-object? (read input)))
              (error "native spec query did not publish one datum" entry))
            spec)))
      (finally (when (file-exists? spec-file) (delete-file spec-file))))))
;; A concrete consumer owns its struct representation and passes its POO Type.
(defstruct scope-test-row (key value) transparent: #t)
(define-type (ScopeTestRow @ Type.)
  .element?: (lambda (value)
    (and (scope-test-row? value)
         (element? Symbol (scope-test-row-key value))
         (element? String (scope-test-row-value value)))))

;; A type failure must be the native admission error, not an arbitrary target bug.
(def (type-rejected? thunk)
  (with-catch (lambda (condition) (TypeError? condition))
    (lambda () (thunk) #f)))
(deflanguage-parser-tests library-init-test "native Core library projection"
  (engine)
  (scenario "actual service Scopes validate and reject malformed values"
    (sequential
      ((utilities (.get gerbil-parser.init.exports Components utilities scope))
       (grammar (.get gerbil-parser.init.exports Components grammar scope)))
      (equal (element? UtilitiesScopeContract utilities) #t)
      (equal (element? GrammarScopeContract grammar) #t)
      (equal ((.get (cdr ((.get utilities list-operations-for) Integer Integer)) append-unique) '(1) 2) '(1 2))
      (equal ((.get grammar grammar-expression?) '(empty)) #t)
      (equal (element? UtilitiesScopeContract (.o)) #f)
      (equal (element? UtilitiesScopeContract (.cc utilities list-operations-for: 42)) #f)
      (equal (element? GrammarScopeContract (.cc grammar lexical-expression?: 42)) #f)))
  (scenario "specific signatures reject input and output mismatches"
    (sequential
      ((raw (.get gerbil-parser.init.exports Components utilities scope))
       (selection ((.get raw list-operations-for) String String))
       (typed (cdr selection))
       (runtime (.get gerbil-parser.init.exports Components runtime scope))
       (testing (.get gerbil-parser.init.exports Components testing scope))
       (build (.get gerbil-parser.init.exports Components build-support scope))
       (modules (.get gerbil-parser.init.exports Components modules scope))
       (compiler (.get gerbil-parser.init.exports Components compiler scope))
       (calls 0))
      (equal (element? TypeValuePair selection) #t)
      (equal (type-rejected? (lambda () ((.get raw list-operations-for) 42 String))) #t)
      (equal (type-rejected? (lambda () ((.get raw list-operations-for)))) #t)
      (equal (type-rejected? (lambda ()
        ((.get (validate UtilitiesScope
           (.cc raw list-operations-for: (lambda (_row _key) (cons String 42)))) list-operations-for) String String))) #t)
      (equal ((.get typed append-unique) '("a") "b") '("a" "b"))
      (equal ((.get raw text-lines) "a\nb") '("a" "b"))
      (equal ((.get raw ascii-whitespace?) #\space) #t)
      (equal ((.get raw string-index-from) "abc" "b" 0) 1)
      (equal (type-rejected? (lambda () ((.get raw string-index-from) "abc" "b" "0"))) #t)
      (equal (type-rejected? (lambda () ((.get typed append-unique) '(42) "a"))) #t)
      (equal (type-rejected? (lambda () ((.get raw ascii-whitespace?) 32))) #t)
      (equal (type-rejected? (lambda () ((.get typed append-unique) '("a") 2))) #t)
      (equal (type-rejected? (lambda ()
        ((.get typed merge-keyed-row) '("a") "b"
          (lambda (row) (set! calls (+ calls 1)) 42) (lambda (_) '())))) #t)
      (equal calls 1)
      (equal ((.get typed merge-keyed-row) '("a") "b" (lambda (row) row) (lambda (_) '())) '("a" "b"))
      (equal (type-rejected? (lambda ()
        ((.get typed merge-keyed-row) '("a") "b" 42 (lambda (_) '())))) #t)
      (equal (type-rejected? (lambda ()
        ((.get (validate UtilitiesScope
                (.cc raw text-lines: (lambda (_) '(42)))) text-lines) "x"))) #t)
      (equal (type-rejected? (lambda () ((.get build native-module-link-options) '(42)))) #t)
      (equal (type-rejected? (lambda () ((.get build compiler-build-spec) 42))) #t)
      (equal (type-rejected? (lambda () ((.get testing parser-test-form-admitted?) '(quote x) '(42)))) #t)
      (equal (type-rejected? (lambda () ((.get modules make-graph-query-context) 42))) #t)
      (equal (type-rejected? (lambda () ((.get compiler compile-grammar) 42))) #t)
      (equal (type-rejected? (lambda () ((.get compiler compile-grammar) (.o)))) #t)
      (equal (type-rejected? (lambda () ((.get compiler compile-grammar) '((schema . "unknown") (grammar . test))))) #t)
      (equal ((.get modules graph-query-view?)
        (.get ((.get modules make-graph-query-context)
          (.o records: '() id-of: (lambda (_) 0) parent-of: (lambda (_) #f) kind-of: (lambda (_) 'node))) view)) #t)
      (equal (element? ForeignInterfaceScopeContract
        (.cc (.get gerbil-parser.init.exports Components ffi scope) language-abi-version: -1)) #f)
      (equal (element? LanguageScopeContract
        (.cc (.get gerbil-parser.init.exports Components language scope) grammar-schema: "unknown")) #f)
      (equal (type-rejected? (lambda () ((.get runtime parse-artifact-roundtrip) '((schema . "invalid"))))) #t)))
  (scenario "struct rows and POO Types share the same explicit service"
    (sequential
      ((utilities (.get gerbil-parser.init.exports Components utilities scope))
       (selection ((.get utilities list-operations-for) ScopeTestRow Symbol))
       (operations (cdr selection))
       (first (make-scope-test-row 'first "a"))
       (second (make-scope-test-row 'second "b")))
      (equal (element? (car selection) operations) #t)
      (equal (element? TypeValuePair selection) #t)
      (equal ((.get operations append-unique) (list first) second) (list first second))
      (equal ((.get operations merge-keyed-row) (list first) second scope-test-row-key
                (lambda (_) '())) (list first second))
      (equal (type-rejected? (lambda () ((.get operations append-unique) (list first) "row"))) #t)
      (equal (type-rejected? (lambda () ((.get operations append-unique) (list first)
                  (make-scope-test-row 42 "value")))) #t)
      (equal (type-rejected? (lambda () ((.get operations append-unique) (list first)
                  (make-scope-test-row 'key 42)))) #t)
      (equal (type-rejected? (lambda ()
        ((.get operations merge-keyed-row) (list first) second scope-test-row-value (lambda (_) '())))) #t)))
  (scenario "compiled products interoperate through structural Scopes"
    (sequential
      ((runtime (.get gerbil-parser.init.exports Components runtime scope))
       (compiler (.get gerbil-parser.init.exports Components compiler scope))
       (language (.get gerbil-parser.init.exports Components language scope))
       (artifact (parse-arithmetic "1 + 2"))
       (ir ((.get compiler compile-grammar) arithmetic-grammar))
       (open-grammar (make-grammar 'scope-test '() '()))
       (open-ir ((.get compiler compile-grammar) open-grammar)))
      (equal ((.get runtime parse-artifact-valid?) artifact) #t)
      (equal ((.get runtime parse-artifact-success?) artifact) #t)
      (equal ((.get runtime parse-artifact-roundtrip) artifact) "1 + 2")
      (equal ((.get runtime parse-artifact-valid?) '((schema . "invalid"))) #f)
      (equal ir (compiled.compile-grammar arithmetic-grammar))
      (equal open-ir (compiled.compile-grammar open-grammar))
      (equal (.get language language-grammar-schema) +language-grammar-schema+)))
  (scenario "spec preparation diagnostics cannot corrupt the data channel"
    (sequential ((spec (query-spec "../build.ss" #t)))
      (equal (and (list? spec) (pair? spec)) #t)
      (equal (and (assoc "src/utilities/types" (map cdr spec)) #t) #t)))
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
