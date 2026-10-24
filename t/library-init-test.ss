;;; Native Core projection retains the complete library and foreign options.
(import :std/test
        (only-in :clan/poo/object .o)
        (only-in :std/source this-source-file)
        (only-in :std/misc/process run-process)
        (only-in :clan/poo/mop element?)
        (only-in "../modules/parser/types.ss" ParserScopeContract)
        (only-in "../build-modules.ss" gerbil-parser-library-modules))
(export library-init-test)
(def library-init-test
  (test-suite "library-init-test"
    (test-case "the publication requires a real nonempty string catalog"
      (check-equal? (element? ParserScopeContract (.o catalog: gerbil-parser-library-modules)) #t)
      (check-equal? (element? ParserScopeContract (.o)) #f)
      (check-equal? (element? ParserScopeContract (.o catalog: '())) #f)
      (check-equal? (element? ParserScopeContract (.o catalog: '(fake))) #f))
    (test-case "the init-owned projection preserves every library and FFI target"
      (def spec
        (run-process
          (list "gxi" (path-expand "../build.ss" (path-directory (this-source-file))) "spec")
          stdout-redirection: #t coprocess: read))
      (def names (map cadr spec))
      (def seen (make-hash-table))
      (for-each (lambda (name)
                  (check-equal? (hash-key? seen name) #f)
                  (hash-put! seen name #t)) names)
      (for-each (lambda (file) (check-equal? (and (member (path-strip-extension file) names) #t) #t))
                gerbil-parser-library-modules)
      (for-each
        (lambda (name)
          (def entry (assoc name (map cdr spec)))
          (check-equal? (and entry #t) #t)
          (check-equal? (and (member "-cc-options" entry) #t) #t))
        '("src/ffi/language-abi" "src/ffi/language-abi-context" "src/ffi/rust-aot-abi"))
      (for-each (lambda (name) (check-equal? (and (member name names) #t) #t))
                '("modules/parser/types" "modules/parser/objects" "modules/parser/config"
                  "modules/parser/interface" "build-modules")))))
