;;; Engine checkpoints are owned by the creating worker, including shared text.
(import :std/test
        (only-in :gerbil-parser/src/runtime/source-scanner
                 make-source-scanner source-scanner-initial-state source-scanner-step
                 source-scan-state-with-context source-scan-state-byte-offset)
        (only-in :gerbil-parser/src/runtime/token token-start token-end))
(def (rejected? thunk)
  (with-catch (lambda (_) #t) (lambda () (thunk) #f)))
(def source-scanner-test
  (test-suite "source scanner worker ownership"
    (test-case "another worker cannot use a checkpoint over the exact same string"
      (let* ((source "αx") (calls 0)
             (step (lambda (_source offset context _mode)
                     (set! calls (+ calls 1))
                     (values 'character (+ offset 1) context)))
             (left (make-source-scanner source 'left step))
             (right (make-source-scanner source 'right step))
             (initial (source-scanner-initial-state left)))
        (check (rejected? (lambda () (source-scanner-step right initial 'test))) => #t)
        (check calls => 0)
        (check (rejected? (lambda ()
                           (source-scanner-step right (source-scan-state-with-context initial 'changed) 'test))) => #t)
        (check calls => 0)))
    (test-case "owned checkpoint replay and context updates preserve UTF-8 spans"
      (let* ((worker (make-source-scanner "αx" 'initial
                      (lambda (_source offset context _mode)
                        (if (= offset 2) (values #f offset context)
                          (values 'character (+ offset 1) context)))))
             (initial (source-scanner-initial-state worker)))
        (let-values (((first next) (source-scanner-step worker initial 'test)))
          (check (list (token-start first) (token-end first)) => '(0 2))
          (check (source-scan-state-byte-offset initial) => 0)
          (let-values (((again replay) (source-scanner-step worker initial 'test)))
            (check (list (token-start again) (token-end again)) => '(0 2))
            (check (source-scan-state-byte-offset replay) => 2))
          (let-values (((second end) (source-scanner-step worker (source-scan-state-with-context next 'updated) 'test)))
            (check (list (token-start second) (token-end second)) => '(2 3))
            (let-values (((eof final) (source-scanner-step worker end 'test)))
              (check eof => #f)
              (check (source-scan-state-byte-offset final) => 3))))))))
(export source-scanner-test)
