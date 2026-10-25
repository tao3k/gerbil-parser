;;; Native Core projection retains the complete library and foreign options.
(import :gerbil-parser/language-test-support
        (only-in :clan/poo/object .o)
        (only-in :std/source this-source-file)
        (only-in :std/misc/process run-process)
        (only-in :std/io/tempfile make-temporary-file-name)
        (only-in :clan/poo/mop element?)
        (only-in "../modules/parser/types.ss" ParserScopeContract)
        (only-in "../build-modules.ss" gerbil-parser-library-modules))
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
  ;; the spec in a private data file and forward actual child diagnostics via
  ;; the SDK coprocess so completed imports reach the existing idle observer.
  (let ((status #f) (spec-file (make-temporary-file-name "parser-build-spec-")))
    (try
      (run-process
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
        stdout-redirection: #t stderr-redirection: #t
        coprocess:
          (lambda (process)
            (let loop ()
              (let (line (read-line process))
                (unless (eof-object? line)
                  (display line (current-error-port))
                  (newline (current-error-port))
                  (force-output (current-error-port))
                  (loop)))))
        check-status: (lambda (code _settings) (set! status code)))
      (unless (zero? status) (error "native spec query failed" entry status))
      (call-with-input-file spec-file
        (lambda (input)
          (let (spec (read input))
            (unless (and (not (eof-object? spec)) (eof-object? (read input)))
              (error "native spec query did not publish one datum" entry))
            spec)))
      (finally (when (file-exists? spec-file) (delete-file spec-file))))))
(deflanguage-parser-tests library-init-test "native Core library projection"
  (engine)
  (scenario "the publication requires a real nonempty string catalog"
    (equal (element? ParserScopeContract (.o catalog: gerbil-parser-library-modules)) #t)
    (equal (element? ParserScopeContract (.o)) #f)
    (equal (element? ParserScopeContract (.o catalog: '())) #f)
    (equal (element? ParserScopeContract (.o catalog: '(fake))) #f))
  (scenario "the init-owned projection preserves every library and FFI target"
    (sequential
      ((spec (query-spec "../build.ss"))
       (qualified-spec (query-spec "../build-qualified-library.ss"))
       (names (map cadr spec))
       (seen (make-hash-table)))
      (equal qualified-spec spec)
      (each (name) (names)
        (equal (hash-key? seen name) #f)
        (act (hash-put! seen name #t)))
      (each (file) (gerbil-parser-library-modules)
        (equal (and (member (path-strip-extension file) names) #t) #t))
      (each (name) ('("src/ffi/language-abi" "src/ffi/language-abi-context" "src/ffi/rust-aot-abi"))
        (scope ((entry (assoc name (map cdr spec))))
          (equal (and entry #t) #t)
          (equal (and (member "-cc-options" entry) #t) #t)))
      (each (name) ('("modules/parser/types" "modules/parser/objects" "modules/parser/config"
                      "modules/parser/interface" "build-modules"))
        (equal (and (member name names) #t) #t)))))
