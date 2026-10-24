;;; Native Core projection retains the complete library and foreign options.
(import :gerbil-parser/language-test-support
        (only-in :clan/poo/object .o)
        (only-in :std/source this-source-file)
        (only-in :std/misc/process run-process)
        (only-in :clan/poo/mop element?)
        (only-in "../modules/parser/types.ss" ParserScopeContract)
        (only-in "../build-modules.ss" gerbil-parser-library-modules))
(deflanguage-parser-tests library-init-test "native Core library projection"
  (engine)
  (scenario "the publication requires a real nonempty string catalog"
    (equal (element? ParserScopeContract (.o catalog: gerbil-parser-library-modules)) #t)
    (equal (element? ParserScopeContract (.o)) #f)
    (equal (element? ParserScopeContract (.o catalog: '())) #f)
    (equal (element? ParserScopeContract (.o catalog: '(fake))) #f))
  (scenario "the init-owned projection preserves every library and FFI target"
    (sequential
      ((spec (run-process
               (list "gxi" "-:max-heap=1G,debug=q"
                     (path-expand "../build.ss" (path-directory (this-source-file))) "spec")
               stdout-redirection: #t coprocess: read))
       (qualified-spec
        (run-process
          (list "gxi" "-:max-heap=1G,debug=q"
                (path-expand "../build-qualified-library.ss" (path-directory (this-source-file))) "spec")
          stdout-redirection: #t coprocess: read))
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
