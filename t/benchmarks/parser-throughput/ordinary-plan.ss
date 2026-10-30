;;; Inspect admitted common-byte effects before designing a run transition.
(import (only-in :gerbil-parser/src/compiler/event-fold-zero-state fold-ordinary-state-plan)
        (only-in :gerbil-parser/src/compiler/event-fold-analysis fold-statement-analysis))
(export main)

(def (main path)
  (def (walk value)
    (when (pair? value)
      (when (eq? (car value) 'for-line-bytes)
        (let (plan (fold-ordinary-state-plan (list-ref value 4) (cadr value)))
          (when plan
            (write (list 'ordinary-run-admission (cadr value)
                         (vector-ref plan 0) (vector-ref plan 1)
                         (fold-statement-analysis (vector-ref plan 2))
                         (vector-ref plan 2)))
            (newline))))
      (for-each walk value)))
  (walk (call-with-input-file path read)))
