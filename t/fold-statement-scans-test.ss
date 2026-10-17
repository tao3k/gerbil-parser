(import :gerbil-parser/language-test-support)
(import :std/test ../src/compiler/event-fold-analysis)

(def scan '(line-skip-horizontal start))
(def first `(set-uint at (offset ,scan)))
(def last `(token Text ,scan end))
(deflanguage-parser-tests fold-statement-scans-test "Cross-statement dominating source scans"
  (engine)
    (property "source scan spans independent state writes and ordered events"
      (bindings (body (list first '(set-bool ready (bool #t)) '(start-node Text) last)))
      (equal (fold-share-statement-scans body)
                (list (list '%shared-scans (list scan) body))))
    (property "conditionally reached first scan cannot be hoisted"
      (bindings (body `((if (state ready) (,first) ()) ,last)))
      (equal (fold-share-statement-scans body)  body))
    (scenario "source frame helper opaque and loop boundaries block sharing"
      (each (barrier) ('((with-source-bounds start end ()) (call-source-helper nested start end)
         (unknown-transition) (for-line-bytes cursor start end ())))
         (scope ((body (list first barrier last)))
           (equal (fold-share-statement-scans body)  body))))
    (property "ordinary short circuit keeps queries and errors ordered"
      (bindings (body `((if (and (future-heading-title? "*" " " 1 "END")
                            (offset-less? ,scan end)) () ()) ,last)))
      (equal (fold-share-statement-scans body)  body))
    (scenario "state-dependent ranges and unique scans are not reused"
      (scope ((body '((set-uint at (offset (line-skip-horizontal (state-offset before))))
                   (token Text (line-skip-horizontal (state-offset before)) end))))
        (equal (fold-share-statement-scans body)  body))
      (equal (fold-share-statement-scans (list first))  (list first))
      (sequential ((delimiter '(line-scan-until start "]"))
             (body `((set-uint at (offset ,delimiter)) (token Text start ,delimiter))))
        (equal (fold-share-statement-scans body #t)
                (list (list '%shared-scans (list delimiter) body))))))
