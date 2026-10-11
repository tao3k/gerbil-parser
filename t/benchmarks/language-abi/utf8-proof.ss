#!/usr/bin/env gxi
;;; Fresh Scheme VM, compiled complete ABI requests and strict CPU admission.
(port-settings-set! (current-output-port) '(buffering: #f))
(displayln "ABI-UTF8-PROOF-BOOTSTRAP") (force-output)
(load "t/fixtures/tla-sany-differential/preload.ss")
(prefer-compiled-interfaces!)
(call-with-compiled-interface-trace
 (lambda ()
   (preload-runtime-module "gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process")
   (preload-runtime-module "gerbil-parser/t/benchmarks/language-abi/utf8-cpu")))
(def leave (eval 'gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process#test-child-process-exit!))
(def (count text)
  (let (value (string->number text))
    (unless (and (exact-integer? value) (positive? value))
      (error "ABI CPU proof requires positive exact counts" text))
    value))
(def (main (groups "20") (calls "128") (lines "128") (shape "identifier") (unicode "false") (admission "benefit"))
  (with-catch
    (lambda (condition)
      (write (list 'ABI-UTF8-CPU-REJECTED (error-message condition) (error-irritants condition)))
      (newline) (force-output) (leave 1))
    (lambda ()
      (unless (member unicode '("true" "false")) (error "expected true or false" unicode))
      (unless (member admission '("benefit" "regression")) (error "unknown CPU admission" admission))
      ((eval (if (equal? admission "benefit")
                 'gerbil-parser/t/benchmarks/language-abi/utf8-cpu#qualify-abi-utf8-cpu
                 'gerbil-parser/t/benchmarks/language-abi/utf8-cpu#qualify-abi-utf8-regression))
       (count groups) (count calls) (count lines) (equal? unicode "true") (string->symbol shape))
      (leave 0))))
