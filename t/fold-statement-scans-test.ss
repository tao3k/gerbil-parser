(import :std/test ../src/compiler/event-fold-analysis)
(export fold-statement-scans-test)
(def scan '(line-skip-horizontal start))
(def first `(set-uint at (offset ,scan)))
(def last `(token Text ,scan end))
(def fold-statement-scans-test
  (test-suite "Cross-statement dominating source scans"
    (test-case "source scan spans independent state writes and ordered events"
      (let (body (list first '(set-bool ready (bool #t)) '(start-node Text) last))
        (check (fold-share-statement-scans body)
               => (list (list '%shared-scans (list scan) body)))))
    (test-case "conditionally reached first scan cannot be hoisted"
      (let (body `((if (state ready) (,first) ()) ,last))
        (check (fold-share-statement-scans body) => body)))
    (test-case "source frame helper opaque and loop boundaries block sharing"
      (for-each
       (lambda (barrier)
         (let (body (list first barrier last))
           (check (fold-share-statement-scans body) => body)))
       '((with-source-bounds start end ()) (call-source-helper nested start end)
         (unknown-transition) (for-line-bytes cursor start end ()))))
    (test-case "ordinary short circuit keeps queries and errors ordered"
      (let (body `((if (and (future-heading-title? "*" " " 1 "END")
                            (offset-less? ,scan end)) () ()) ,last))
        (check (fold-share-statement-scans body) => body)))
    (test-case "state-dependent ranges and unique scans are not reused"
      (let (body '((set-uint at (offset (line-skip-horizontal (state-offset before))))
                   (token Text (line-skip-horizontal (state-offset before)) end)))
        (check (fold-share-statement-scans body) => body))
      (check (fold-share-statement-scans (list first)) => (list first))
      (let* ((delimiter '(line-scan-until start "]"))
             (body `((set-uint at (offset ,delimiter)) (token Text start ,delimiter))))
        (check (fold-share-statement-scans body #t)
               => (list (list '%shared-scans (list delimiter) body)))))))
