;;; -*- Gerbil -*-
;;; Real completed-work receipts for watchdogs, independent of assertions.
(export report-test-progress! report-parser-batch!)
(def (report-test-progress! . parts)
  (for-each display parts)
  (newline)
  (force-output))
(def (report-parser-batch! count)
  (report-test-progress! "BENCHMARK-BATCH-OK parses=" count))
