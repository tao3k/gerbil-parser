;;; Use the existing disposable native-child contract after all ABI assertions.
(displayln "MODULE-COLD-HOST-START") (force-output)
(load "t/fixtures/module-loading/observations.ss")

(preload-module "gerbil-parser/t/fixtures/ffi/abi-probe")
(preload-module "gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process")
(eval '(import
         (rename-in (only-in :gerbil-parser/t/fixtures/ffi/abi-probe main)
                    (main run-abi-probe))
         (only-in :gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process
                  test-child-process-exit!)))
(eval '(run-abi-probe))
(displayln "MODULE-COLD-HOST-OK") (force-output)
(eval '(test-child-process-exit! 0))
