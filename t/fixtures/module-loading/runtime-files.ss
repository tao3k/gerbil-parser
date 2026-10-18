;;; Shared owned runtime files; cleanup restores the official load path.
(import :gerbil-parser/language-test-support
        (only-in :std/io/tempfile make-temporary-file-name))
(export with-module-file with-runtime-file write-program write-datum)

(def (with-module-file extension procedure)
  (let (file (string-append (make-temporary-file-name "parser-module-control-") extension))
    (try (procedure file)
         (finally (when (file-exists? file) (delete-file file))))))
(def (with-runtime-file procedure)
  (with-module-file ".scm"
    (lambda (file)
      (let ((prior (load-path)) (name (path-strip-extension (path-strip-directory file))))
        (try (add-load-path! (path-directory file)) (procedure file name)
             (finally (set-load-path! prior)))))))
(def (write-program file writer value)
  (call-with-output-file file (lambda (port) (writer value port))))
(def write-datum (parser-test-datum-writer values))
