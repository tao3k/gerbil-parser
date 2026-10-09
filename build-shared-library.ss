#!/usr/bin/env gxi
;;; SDK compiler owns static closure and job execution; no generated C rewriting.
(import (only-in :gerbil/compiler compile-module compile-exe execute-pending-compile-jobs!)
        (only-in :std/os/fcntl __fcntl1 __fcntl2 F_GETFL F_SETFL O_NONBLOCK)
        (only-in :std/os/error do-syscall))
;;; Compiler children inherit these descriptors; use the SDK's native OS API.
(def (blocking-standard-streams!)
  (for-each
    (lambda (fd)
      (let (flags (__fcntl1 fd F_GETFL))
        (unless (fxnegative? flags)
          (do-syscall (__fcntl2 fd F_SETFL (fxand flags (fxnot O_NONBLOCK)))))))
    '(0 1 2))
  0)
(def (standard-streams-blocking?)
  (andmap
    (lambda (fd)
      (let (flags (__fcntl1 fd F_GETFL))
        (and (not (fxnegative? flags)) (fxzero? (fxand flags O_NONBLOCK)))))
    '(1 2)))
(def (main source output cc-options ld-options)
  (displayln "COMPILER-STREAMS before-blocking=" (standard-streams-blocking?))
  (unless (zero? (blocking-standard-streams!)) (error "cannot establish blocking compiler streams"))
  (displayln "COMPILER-STREAMS after-blocking=" (standard-streams-blocking?))
  (displayln "SHARED-LIBRARY-MODULE-COMPILE") (force-output)
  (def output-directory (path-expand "lib" (path-directory source)))
  (compile-module source
    [output-dir: output-directory invoke-gsc: #f optimize: #f generate-ssxi: #f parallel: #t verbose: #t
     gsc-options: ["-cc-options" cc-options]])
  (execute-pending-compile-jobs!)
  (displayln "SHARED-LIBRARY-CLOSURE-LINK") (force-output)
  (compile-exe source
    [output-dir: output-directory invoke-gsc: #t output-file: output parallel: #t verbose: #t
     gsc-options: ["-cc-options" cc-options "-ld-options" ld-options]])
  (execute-pending-compile-jobs!)
  (unless (file-exists? output) (error "native bundle was not published" output))
  (displayln "SHARED-LIBRARY-COMPILED") (force-output)
  (exit 0))
