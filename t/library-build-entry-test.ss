;;; The external build entry is a different owner from runtime Scope admission.
(import :gerbil-parser/language-test-support
        (only-in :std/source this-source-file)
        (only-in ./fixtures/package-build-entry query-spec))
(def package-root (path-expand ".." (path-directory (this-source-file))))
(deflanguage-parser-tests library-build-entry-test "native package build entry"
  (engine)
  (scenario "native spec preserves its data channel, public entries and ABI boundaries"
    (sequential
      ((spec (query-spec package-root "build.ss" #t))
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
