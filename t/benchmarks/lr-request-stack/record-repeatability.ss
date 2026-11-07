;;; Controlled attribution only; these extra phases cannot replace admission.
(import (rename-in ./record-access (main record-request-main))
        (only-in ./sdk-control call-with-record-sdk-control require-record-sdk-code-identity!)
        (only-in ../parser-stage-cost/benchmark measure-parser-component))
(export main)

(def (main)
  (call-with-record-sdk-control
    (lambda (encode expected)
      (measure-parser-component '(sdk-native-before utf8) 40 1000 encode expected)
      (let (ordinary-encode (record-request-main))
        (require-record-sdk-code-identity! encode ordinary-encode))
      (measure-parser-component '(sdk-immediate-a utf8) 40 1000 encode expected)
      (measure-parser-component '(sdk-deferred utf8) 40 1000 encode expected sample-output: 'deferred)
      (measure-parser-component '(sdk-immediate-b utf8) 40 1000 encode expected)))
  (displayln "SDK-REPEATABILITY-DIAGNOSTIC-OK")
  (force-output))
