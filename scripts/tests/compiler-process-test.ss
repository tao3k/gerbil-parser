;;; Exercise the real native compiler process owner, including concurrent jobs.
(displayln "COMPILER-PROCESS-START") (force-output)
(load "t/fixtures/tla-sany-differential/preload.ss")
(prefer-compiled-interfaces!)
(call-with-compiled-interface-trace
 (lambda () (load "build-language-conformance-link.ss")))
(import (only-in :std/misc/process process-error?))
(let* ((root (path-expand ".data/compiler-process-controls"))
       (probe (path-expand "compiler-pipe" root)))
  (create-directory* root)
  (run-compiler [(getenv "GERBIL_GCC" "gcc")
                 "scripts/tests/fixtures/compiler-pipe.c" "-o" probe])
  (def (capture command)
    (call-with-output-u8vector
     (lambda (output)
       (parameterize ((current-output-port output)) (run-compiler command)))))
  (unless (equal? (capture [probe "--burst"]) (make-u8vector 262144 120))
    (error "compiler output was lost under pipe pressure"))
  (unless (equal? (capture ["sh" "-c" "printf fragment; printf diagnostic >&2"])
                 (string->utf8 "fragmentdiagnostic"))
    (error "compiler fragment or stderr bytes were changed"))
  ;; Use the same workgroup and process owner as conformance preparation.
  (run-static-jobs (make-list 8 probe) (lambda (path) (run-compiler [path])))
  (unless (with-catch process-error?
            (lambda () (run-compiler ["sh" "-c" "exit 17"]) #f))
    (error "compiler process failure was not propagated")))
(displayln "COMPILER-PROCESS-OK") (force-output)
(exit 0)
