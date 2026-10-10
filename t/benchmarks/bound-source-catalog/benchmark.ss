;;; Complete binding, reference closure and canonical digest publication.
(import (only-in ../../fixtures/bound-source-catalog bound-source-catalog)
        (only-in ../parser-stage-cost/benchmark measure-parser-component measure-parser-cpu-pairs)
        (only-in :gerbil-parser/src/compiler/bound-ir bind-grammar-ir))
(export benchmark-bound-source-catalog compare-bound-source-catalog)

(def (benchmark-bound-source-catalog (samples 20) (batch-count 5) (width 512)
                                    (bind bind-grammar-ir))
  (let-values (((grammar sources) (bound-source-catalog width)))
    (let (expected (bind-grammar-ir grammar "bound-source-benchmark" '(author) sources))
      ;; Every baseline/candidate result must equal the entire production sidecar.
      (measure-parser-component (list 'bound-source-catalog width) samples batch-count
        (lambda () (bind grammar "bound-source-benchmark" '(author) sources)) expected)
      (displayln "BOUND-SOURCE-CATALOG-BENCHMARK-OK"))))

;;; The original constructor is an experiment input, never an engine fallback.
(def (compare-bound-source-catalog original (groups 20) (calls 2) (width 512))
  (let-values (((grammar sources) (bound-source-catalog width)))
    (let* ((left (lambda () (original grammar "bound-source-benchmark" '(author) sources)))
           (right (lambda () (bind-grammar-ir grammar "bound-source-benchmark" '(author) sources)))
           (expected (right)))
      (let (ratios (measure-parser-cpu-pairs 'bound-source-catalog width groups calls expected left right))
        (write (list 'BOUND-SOURCE-CPU-RATIOS width ratios)) (newline) (force-output)
        (displayln "BOUND-SOURCE-COMPARISON-OK")
        ratios))))
