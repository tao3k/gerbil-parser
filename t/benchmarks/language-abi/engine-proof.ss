#!/usr/bin/env gxi
;;; Complete requests compare independent scanner/LR preparation kernels.
(port-settings-set! (current-output-port) '(buffering: #f))
(displayln "ENGINE-CPU-BOOTSTRAP") (force-output)


(begin (load-module "gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process")
   (load-module "gerbil-parser/t/benchmarks/language-abi/utf8-cpu"))
(def leave (eval 'gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process#test-child-process-exit!))
(def (main (mode "combined") (unicode "false"))
  (with-catch
   (lambda (condition)
     (write (list 'ENGINE-CPU-REJECTED (error-message condition) (error-irritants condition)))
     (newline) (force-output) (leave 1))
   (lambda ()
     (unless (and (member mode '("scanner" "lr" "combined"))
                  (member unicode '("false" "true"))) (error "invalid engine CPU workload"))
     (let (summary ((eval 'gerbil-parser/t/benchmarks/language-abi/utf8-cpu#benchmark-abi-utf8-cpu)
                    20 256 64 (equal? unicode "true") 'records #f (string->symbol mode)))
       (unless (cdr (assq 'cpu-benefit (cdr summary)))
         (error "complete engine request CPU gate failed" summary)))
     (displayln "ENGINE-CPU-PROOF-OK") (force-output) (leave 0))))
