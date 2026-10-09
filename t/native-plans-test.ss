(import :std/test :gerbil-parser/t/support/qualification/plans)
(export native-plans-test)
(def native-plans-test
  (test-suite "native qualification plans"
    (test-case "diagnostic modules are unique and ordered"
      (check (diagnostic-modules '("gql-profile" "gql-actors" "gql-profile"))
        => '("reduction-counts" "execution-counts" "matched-stages" "actors")))
    (test-case "one driver and one diagnostic build"
      (let (plan (local-diagnostic-plan '("gql-profile" "gql-actors") #f))
        (check (length plan) => 2)
        (check (length (qualification-stage-command (cadr plan))) => 6)))
    (test-case "prepared diagnostics do not rebuild"
      (check (local-diagnostic-plan '("gql-profile" "gql-actors") #t) => '()))
    (test-case "invalid prepared scope and unknown suites are rejected"
      (check-exception (local-diagnostic-plan '("ffi") #t) true)
      (check-exception (local-diagnostic-plan '("unknown") #f) true))))
