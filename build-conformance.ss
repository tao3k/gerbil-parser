#!/usr/bin/env gxi
;;; Native build declaration owns research compilation and executable linking.
(import (only-in "./src/build-support/compiler-profile.ss"
                 call-with-compiler-import-progress))
(def (main)
  (call-with-compiler-import-progress
    (lambda ()
      (load "build-language-conformance.ss")
      (eval '(main "compile")))))
