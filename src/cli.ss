;;; Thin command behavior over the pure Scheme library; option parsing lives in
;;; main.ss and parser implementation remains in runtime/parser.ss.

(import (only-in ./compiler/parser-ir parser-ir-canonical)
        (only-in ./runtime/artifact parse-artifact-success?)
        (only-in ../languages/arithmetic/v1/parser
                 arithmetic-parser-ir parse-arithmetic-v1))
(export gparse-build
        gparse-inspect
        gparse-check
        gparse-test)

;; write-line
;; : (-> Datum Void)
(def (write-line value)
  (write value)
  (newline))

;; : (-> Fixnum)
(def (gparse-build)
  (write-line arithmetic-parser-ir)
  0)

;; : (-> Fixnum)
(def (gparse-inspect)
  (displayln (parser-ir-canonical arithmetic-parser-ir))
  0)

;; : (-> String Fixnum)
(def (gparse-check source)
  (let (artifact (parse-arithmetic-v1 source))
    (write-line artifact)
    (if (parse-artifact-success? artifact) 0 1)))

;; : (-> Fixnum)
(def (gparse-test)
  (gparse-check "1 + 2 * value"))
