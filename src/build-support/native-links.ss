;;; Native consumers must bind the same physical generation as the SDK loader.
(import (only-in :gerbil/runtime/loader __find-library-module))
(export native-module-object native-module-link-options)

(def (native-module-object module)
  (unless (and (string? module) (string-prefix? "gerbil-parser/" module)
               (not (string-contains module "/../"))
               (not (string-suffix? "/.." module)))
    (error "native link requires a parser module" module))
  (let* ((selected (__find-library-module module))
         (extension (and selected (path-extension selected)))
         (expected (path-normalize (path-expand module (path-expand "lib" (gerbil-path)))))
         (namespace-prefix
           (path-directory (path-normalize
             (path-expand "lib/gerbil-parser/.native-link" (gerbil-path))))))
    (unless (and (string-prefix? namespace-prefix expected)
                 selected (> (string-length extension) 2)
                 (string-prefix? ".o" extension)
                 (andmap (lambda (digit) (and (char>=? digit #\0) (char<=? digit #\9)))
                         (string->list (substring extension 2 (string-length extension))))
                 (equal? (path-normalize (path-strip-extension selected)) expected))
      (error "native link has no owned SDK-selected object" module selected))
    selected))

(def (native-module-link-options modules)
  (string-join (map native-module-object modules) " "))
