;;; -*- Gerbil -*-
;;; Build-time static products only; requests never import the compiler.
(import (only-in :gerbil/compiler compile-module))
(export compile-event-fold-scheme-units)

(def (compile-event-fold-scheme-units directory names)
  ;; The engine emits names in dependency order. Retain native SSI/SSXI and
  ;; static Scheme inputs, without compiling unused dynamic loading objects.
  (for-each
   (lambda (name)
     (displayln "... compile native fold unit " name)
     (force-output)
     (compile-module
      (path-expand (string-append name ".ss") directory)
      [invoke-gsc: #f keep-scm: #t optimize: #t generate-ssxi: #t
       output-dir: (path-expand "lib" directory)]))
   names))
