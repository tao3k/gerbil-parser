(import :gerbil-parser/language-test-support)
(import :std/test :std/text/pregexp :gerbil-parser/t/support/qualification/plans
        (only-in :gerbil-parser/t/support/qualification/process process-child-arguments process-error-prefix))

(defparser-check (error-prefix-splits text)
  (scope ((bytes (string->utf8 text)))
    (each (split) ((iota (+ 1 (u8vector-length bytes))))
      (scope ((state (process-error-prefix
                      (process-error-prefix "" (subu8vector bytes 0 split))
                      (subu8vector bytes split (u8vector-length bytes)))))
        (equal (or (eq? state 'error) (equal? state "ERROR"))
               (and (pregexp-match "(^|\\n)(\\*\\*\\* ERROR|ERROR([^A-Za-z0-9_]|$))" text) #t))))))

(deflanguage-parser-tests qualification-plans-test "qualification plans"
  (engine)
    (matrix "error recognition is invariant under every byte split"
      (setup) ((text '("" "ERROR" "ERROR\n" "ERROR: failed" "ERRORλ" "ERROR_ignored"
                       "ERROR2 ignored" "*** ERROR failed" "*** ERR" "prefix ERROR\n"
                       "PIPE-PROGRESS λλ\nERROR failed" "\n*** ERROR" "\x00;ERROR\nOK")))
      (bindings)
      (equal (parser-check-result error-prefix-splits text) (void)))
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
