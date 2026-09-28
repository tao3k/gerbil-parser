;;; -*- Gerbil -*-
;;; Public event-fold AOT surface; execution and IR lowering are separate modules.
(import (only-in ./event-fold-runtime.ss run-event-fold)
        (only-in ./event-fold-ir.ss event-fold-ir-json))
(export define-event-fold-parser run-event-fold event-fold-ir-json)

(defrules define-event-fold-parser ()
  ((_ scheme-name rust-name grammar root source initial (line-form ...) (finish-form ...))
   (begin
     (def (scheme-name source)
       (run-event-fold source 'root 'initial
                       '(line-form ...) '(finish-form ...)))
     (def rust-name
       (event-fold-ir-json 'rust-name grammar 'root 'initial
                           '(line-form ...) '(finish-form ...))))))
