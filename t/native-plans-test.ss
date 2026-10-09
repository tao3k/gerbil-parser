(import :std/test :gerbil-parser/t/support/qualification/plans
        (only-in :gerbil-parser/t/support/qualification/process process-child-arguments))
(export native-plans-test)
(def native-plans-test
  (test-suite "native qualification plans"
    (test-case "native child argv is quoted data, without source imports"
      (let (args (process-child-arguments "session" "" '("fixture" "λ a;$(not-a-command)")))
        (check (car args) => "-e")
        (check (cadr args)
          => "(load-module \"gerbil-parser/t/support/qualification/process-child\")")
        (check (read (open-input-string (list-ref args 3)))
          => '(apply gerbil-parser/t/support/qualification/process-child#main
                     (quote ("session" "" "fixture" "λ a;$(not-a-command)"))))))
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
