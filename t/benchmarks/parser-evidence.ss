;;; Evidence workload composition; statistics and semantic admission stay upstream.
(import (only-in ./gql/runtime/matched-stages main)
        (only-in ./lr-request-stack/benchmark benchmark-lr-request-backends))
(export (rename: evidence-main main))
(def (evidence-main)
  (main "40" "100")
  (benchmark-lr-request-backends 20 20)
  ;; Completion does not imply that the sampler admitted a performance benefit.
  (displayln "PARSER-EVIDENCE-REQUESTS-OK") (force-output))
