;;; Measure complete publication with unchanged source admission and digest work.
(import (only-in ../../fixtures/artifact-window window-tokens window-artifact)
        (only-in ../parser-stage-cost/benchmark measure-parser-component)
        (only-in :gerbil-parser/src/runtime/artifact
                 make-certified-window-artifact make-failure-parse-artifact
                 parse-artifact-valid-for-source? sha256-text))
(export benchmark-artifact-window)

(def (benchmark-artifact-window (samples 20) (batch-count 5) (width 20000))
  (let* ((source (make-string width #\a)) (tokens (window-tokens source))
         (base (window-artifact source tokens)) (assigned (map list tokens))
         (grammar (sha256-text "window-failure"))
         (diagnostic '((message . "expected another token")))
         (rejected (make-failure-parse-artifact grammar source tokens diagnostic)))
    (unless (and (parse-artifact-valid-for-source? base source)
                 (parse-artifact-valid-for-source? rejected source))
      (error "window benchmark requires admitted complete products"))
    (measure-parser-component 'certified-window-publication samples batch-count
      (lambda ()
        (let-values (((product shared)
                      (make-certified-window-artifact base source 0 tokens tokens 0 assigned)))
          (unless (= shared 2) (error "window benchmark changed shared event accounting"))
          product)) base)
    (measure-parser-component 'rejected-token-publication samples batch-count
      (lambda () (make-failure-parse-artifact grammar source tokens diagnostic)) rejected)
    (displayln "ARTIFACT-WINDOW-BENCHMARK-OK")))
