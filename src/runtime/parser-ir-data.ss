;;; -*- Gerbil -*-
;;; Runtime IR access has no grammar construction or LR compiler dependency.
(export parser-ir-ref)
(def (parser-ir-ref ir key)
  (alet (entry (assq key ir)) (cdr entry)))
