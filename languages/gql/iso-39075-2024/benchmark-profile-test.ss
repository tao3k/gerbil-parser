;;; -*- Gerbil -*-
;;; Profiling must preserve lossless Unicode artifacts and LR token admission.
(import :std/test
        (only-in "./benchmarks/runtime/matched-stages" profile-gql-stages))
(export gql-benchmark-profile-test)
(def gql-benchmark-profile-test
  (test-suite "GQL language performance diagnostics"
    (test-case "Unicode and trivia retain exact artifacts through all stages"
      (let (reports (profile-gql-stages "RETURN '你好' /* retained */\n" 2 2))
        (check (map (lambda (row) (cdr (assq 'stage row))) reports)
               => '(global-lexing prepared-lr artifact-publication full-source))
        (for-each
         (lambda (row)
           (check (cdr (assq 'sampleCount row)) => 2)
           (check (cdr (assq 'parsesPerSample row)) => 2)
           (check (length (cdr (assq 'samples row))) => 2)) reports)))
    (test-case "rejected inputs cannot publish misleading stage receipts"
      (check-exception (profile-gql-stages "RETURN @" 2 2) true))
    (test-case "invalid measurement sizes fail before work"
      (check-exception (profile-gql-stages "RETURN 1" 0 2) true))))
