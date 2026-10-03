#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Native gxtest loads modules before emitting HARNESS/MODULE notices.
;;; Report real import/eval operations so an external silence watchdog can
;;; distinguish dependency loading from a stalled check. No timer heartbeat.
(import "native-load-progress" "native-benchmark-progress" :gerbil/expander
        (rename-in :gerbil/tools/gxtest (main native-test-main)))
(def (main . args)
  (install-native-load-progress!)
  (install-native-benchmark-progress!)
  (let ((importer (current-expander-module-import))
        (evaluator (current-expander-module-eval)))
    (parameterize
        ((current-expander-module-import
          (lambda (path reload?)
            (displayln "IMPORT " path) (force-output)
            (let (result (importer path reload?))
              (displayln "IMPORT-OK " path) (force-output)
              result)))
         (current-expander-module-eval
          (lambda (context)
            (displayln "EVAL " (module-context-id context)) (force-output)
            (let (result (evaluator context))
              (displayln "EVAL-OK " (module-context-id context)) (force-output)
              result))))
      ;; Propagate the native harness status instead of returning into gxi.
      ;; This also makes completion explicit for the silence watchdog.
      (exit (apply native-test-main args)))))
