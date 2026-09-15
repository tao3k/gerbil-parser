;;; -*- Gerbil -*-
;;; Grammar-owned adapter to the POO Flow observability framework.
;;;
;;; LanguageGrammar carries either #f or a PooFlowDebugCallPolicy. This module
;;; defines no observation type, timer, sink, receipt, or enablement semantic.

(import (only-in :poo-flow/src/module-system/observability/interface
                 call-with-poo-flow-debug-trace))
(export call-with-parser-observed-phase)

(def (call-with-parser-observed-phase policy phase thunk)
  (if policy
    (call-with-poo-flow-debug-trace
     policy phase thunk '() emit?: #t)
    (thunk)))
