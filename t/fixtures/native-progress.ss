#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Native gxtest loads modules before emitting HARNESS/MODULE notices.
;;; Report real import/eval operations so an external silence watchdog can
;;; distinguish dependency loading from a stalled check. No timer heartbeat.
(import :gerbil/expander
        (rename-in :gerbil/tools/gxtest (main native-test-main)))
(def (main . args)
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
      (apply native-test-main args))))
