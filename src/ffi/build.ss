;;; The ABI owner contributes its three C leaves and their native include/link options.
(import (only-in :std/source this-source-file)
        (only-in :clan/poo/object .o))
(export Build)
(def Build
  (let ((include-option
          (string-append "-I"
            (path-strip-trailing-directory-separator
              (path-normalize (path-expand "../../include" (path-directory (this-source-file)))))))
        (link-options (cond-expand
                        (darwin '("-ld-options" "-Wl,-undefined,dynamic_lookup"))
                        (else '()))))
    (.o kind: 'module/build
        library-roots: '("language-abi-support" "src/ffi/rust-runtime-aot" "src/ffi/schema")
        roots: (map (lambda (module)
                      `(gxc: ,module "-cc-options" ,include-option ,@link-options))
                    '("language-abi" "language-abi-context" "rust-aot-abi")))))
