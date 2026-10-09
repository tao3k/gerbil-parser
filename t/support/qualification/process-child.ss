;;; A fresh POSIX session contains the compiler/test child and its descendants.
(import :gerbil-parser/t/support/qualification/process-os)
(export main)
(def (main witness loadpath program . arguments)
  (let (owner (start-session!))
    (when (< (blocking-standard-streams!) 0) (error "cannot establish blocking compiler streams"))
    (when (< owner 0) (error "cannot establish owned process session"))
    (call-with-output-file witness (lambda (port) (write owner port))))
  (if (zero? (string-length loadpath)) (setenv "GERBIL_LOADPATH") (setenv "GERBIL_LOADPATH" loadpath))
  ;; Successful exec never returns. The parent observes the target's actual
  ;; wait status through its existing process port, including signal exits.
  (replace-process! program (cons program arguments))
  (error "cannot replace qualification process" program))
