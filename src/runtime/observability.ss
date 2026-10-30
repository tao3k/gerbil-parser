;;; -*- Gerbil -*-
;;; Grammar-owned adapter to the POO Flow observability framework.
;;; Delayed phase bodies execute directly when the policy is absent.
(import (only-in :yuanxu/observability/debug call-with-yuanxu-debug-trace))
(export call-with-parser-observed-phase)

(def (call-with-parser-observed-phase/procedure policy phase thunk)
  (if policy
    (call-with-yuanxu-debug-trace policy phase thunk '() emit?: #t)
    (thunk)))

;;; Follow the SDK's hygienic delayed-body and first-class identifier pattern.
;;; Evaluate policy and phase once; only observed execution needs a thunk.
(defrules call-with-parser-observed-phase (lambda)
  ((_ policy phase (lambda () body ...))
   (let* ((selected-policy policy) (selected-phase phase))
     (if selected-policy
       (call-with-parser-observed-phase/procedure
        selected-policy selected-phase (lambda () body ...))
       (begin body ...))))
  ((_ policy phase thunk)
   (call-with-parser-observed-phase/procedure policy phase thunk))
  (id (identifier? #'id) call-with-parser-observed-phase/procedure))
