;;; -*- Gerbil -*-
;;; lr-parser: checkpoint-base owner. Private execution state and named algorithms.

(import (only-in ../token
                 token-end)
        (only-in "./state.ss"
                 lr-runtime-layout? lr-runtime-lexical-modes lr-runtime-lexical-mode-catalog make-lr-checkpoint lr-checkpoint-runtime lr-checkpoint-states lr-checkpoint-rest current-lr-recognition-observer lr-failure-frontier-checkpoint)
        (only-in "./prepare.ss"
                 lr-runtime-for-current-semantic-backend))
(export require-lr-checkpoint-runtime!
        lr-initial-checkpoint
        lr-initial-checkpoint/selected
        lr-checkpoint-remaining-token-count
        lr-checkpoint-lexical-mode
        lr-checkpoint-interned-lexical-mode-count
        lr-failure-frontier-remaining-tokens
        lr-rejection-condition?
        lr-checkpoint-state)

;;; Checkpoints own all continuation state. Layout execution also needs source
;;; columns and branch frames, which this record does not capture.
(def (require-lr-checkpoint-runtime! runtime)
  (when (lr-runtime-layout? runtime)
    (error "LR checkpoints require a runtime without layout")))

(def (lr-initial-checkpoint runtime tokens)
  (require-lr-checkpoint-runtime! runtime)
  (lr-initial-checkpoint/selected (lr-runtime-for-current-semantic-backend runtime) tokens))

(def (lr-initial-checkpoint/selected runtime tokens)
  (make-lr-checkpoint
   runtime tokens
   (fold (lambda (input-token offset)
           (max offset (token-end input-token)))
         0 tokens)
   '(0) '() tokens 0 0
   (and (current-lr-recognition-observer)
        (not (lr-runtime-layout? runtime)) '())))

(def (lr-checkpoint-remaining-token-count checkpoint)
  (length (lr-checkpoint-rest checkpoint)))

(def (lr-checkpoint-lexical-mode checkpoint)
  (vector-ref
   (lr-runtime-lexical-modes (lr-checkpoint-runtime checkpoint))
   (car (lr-checkpoint-states checkpoint))))

(def (lr-checkpoint-interned-lexical-mode-count checkpoint)
  (vector-length
   (lr-runtime-lexical-mode-catalog (lr-checkpoint-runtime checkpoint))))

(def (lr-failure-frontier-remaining-tokens frontier)
  (lr-checkpoint-rest (lr-failure-frontier-checkpoint frontier)))

;;; Distinguishes an expected typed parser rejection from an implementation or
;;; contract exception. Recovery probes may discard the former only.
(def (lr-rejection-condition? condition)
  (any (lambda (irritant)
         (and (list? irritant) (assq 'failureKind irritant)))
       (error-irritants condition)))

(def (lr-checkpoint-state checkpoint) (car (lr-checkpoint-states checkpoint)))

