;;; -*- Gerbil -*-
;;; lr-parser: entry owner. Private execution state and named algorithms.

(import (only-in "./state.ss"
                 lr-runtime-layout?)
        (only-in "./checkpoint-base.ss"
                 lr-initial-checkpoint)
        (only-in "./prepare.ss"
                 lr-prepare)
        (only-in "./batch.ss"
                 lr-parse/prepared/receipt)
        (only-in "./transfer.ss"
                 lr-checkpoint-resume))
(export lr-parse/prepared
        lr-parse)

(def (lr-parse/prepared runtime tokens (observability #f))
  (if (lr-runtime-layout? runtime)
    (let-values (((root rest _receipt)
                  (lr-parse/prepared/receipt runtime tokens)))
      (values root rest))
    (lr-checkpoint-resume
     (lr-initial-checkpoint runtime tokens) observability)))

(def (lr-parse spec tokens)
  (lr-parse/prepared (lr-prepare spec) tokens))

