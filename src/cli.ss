;;; Thin command behavior over the pure Scheme library; option parsing lives in
;;; main.ss and parser implementation remains in runtime/parser.ss.

(import ./compiler/parser-ir
        ./runtime/artifact
        ./runtime/parser
        ./reference/arithmetic-v1)
(export gparse-build
        gparse-inspect
        gparse-check
        gparse-test)

(def (write-line value)
  (write value)
  (newline))

(def (gparse-build)
  (write-line arithmetic-parser-ir)
  0)

(def (gparse-inspect)
  (displayln (parser-ir-canonical arithmetic-parser-ir))
  0)

(def (gparse-check source)
  (let (artifact (parse-source arithmetic-parser source))
    (write-line artifact)
    (if (parse-artifact-success? artifact) 0 1)))

(def (gparse-test)
  (gparse-check "1 + 2 * value"))
