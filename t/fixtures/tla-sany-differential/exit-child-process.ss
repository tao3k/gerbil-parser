;;; -*- Gerbil -*-
;;; Exit only a completed, disposable test child. Harness status and buffered
;;; receipts are published first; native library teardown is not a test phase.
(import :std/ffi)
(export test-child-process-exit!)
(C-ffi-macrology)
(C-include "<stdlib.h>")
(def-C-lambda _Exit (int) void)
(def (test-child-process-exit! status)
  (unless (and (exact-integer? status) (<= 0 status 255))
    (error "invalid native test exit status" status))
  (force-output (current-output-port))
  (force-output (current-error-port))
  (_Exit status))
