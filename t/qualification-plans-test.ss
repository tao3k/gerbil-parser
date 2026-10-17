(import :gerbil-parser/language-test-support)
(import :std/test :gerbil-parser/t/support/qualification/plans
        (only-in :gerbil-parser/t/support/qualification/process process-child-arguments))

(deflanguage-parser-tests qualification-plans-test "qualification plans"
  (engine)
    (scenario "child argv is quoted data, without source imports"
      (scope ((args (process-child-arguments "session" "" '("fixture" "λ a;$(not-a-command)"))))
        (equal (car args)  "-e")
        (equal (cadr args)
           "(load-module \"gerbil-parser/t/support/qualification/process-child\")")
        (equal (read (open-input-string (list-ref args 3)))
           '(apply gerbil-parser/t/support/qualification/process-child#main
                     (quote ("session" "" "fixture" "λ a;$(not-a-command)"))))))
    (property "diagnostic modules are unique and ordered"
      (bindings)
      (equal (diagnostic-modules '("gql-profile" "gql-actors" "gql-profile"))
         '("reduction-counts" "execution-counts" "matched-stages" "actors")))
    (property "one driver and one diagnostic build"
      (bindings (plan (local-diagnostic-plan '("gql-profile" "gql-actors") #f)))
      (equal (length plan)  2)
      (equal (length (qualification-stage-command (cadr plan)))  7)
      (equal (list-ref (qualification-stage-command (cadr plan)) 2)
           "t/benchmarks/parser-stage-cost/benchmark.ss"))
    (property "prepared diagnostics do not rebuild"
      (bindings)
      (equal (local-diagnostic-plan '("gql-profile" "gql-actors") #t)  '()))
    (scenario "invalid prepared scope and unknown suites are rejected"
      (raises (local-diagnostic-plan '("ffi") #t) true)
      (raises (local-diagnostic-plan '("unknown") #f) true)))
