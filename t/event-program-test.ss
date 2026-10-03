#!/usr/bin/env gxi
;;; Direct event lowering must match the independent original-production replay.
(import :std/test
        (only-in :gerbil-parser/languages/arithmetic/v1/parser arithmetic-parser parse-arithmetic-v1)
        (only-in :gerbil-parser/languages/hcl/v2-24/parser hcl-v2-24-parser parse-hcl-v2-24)
        :gerbil-parser/src/runtime/event-program
        :gerbil-parser/src/runtime/event-reduce
        :gerbil-parser/src/runtime/recognition
        (only-in :gerbil-parser/src/runtime/reduce recognition-children-alias)
        :gerbil-parser/src/runtime/funcs
        (only-in :gerbil-parser/src/runtime/token make-token token-kind)
        (only-in :gerbil-parser/src/runtime/artifact make-success-parse-artifact)
        (only-in :gerbil-parser/src/runtime/lr-parser
                 current-lr-event-program-enabled? lr-runtime-event-program?
                 lr-recognition-fragment-runtime lr-recognition-fragment-value
                 lr-recognition-view? lr-recognition-view-base lr-recognition-fragment?
                 lr-recognition-fragment-children lr-recognition-fragment-end
                 lr-recognition-fragment-token-count lr-recognition-fragment-executor)
        (only-in :gerbil-parser/languages/hcl/v2-24/direct-step direct-event-step)
        (only-in :gerbil-parser/src/runtime/incremental
                 make-incremental-session incremental-session-artifact
                 incremental-session-recognition-root incremental-session-project-artifact
                 parse-incremental-session make-edit apply-edit)
        "./scenarios/performance/selective-glr/scenario")

(def (base-root root)
  (if (lr-recognition-view? root) (base-root (lr-recognition-view-base root)) root))
(def (program-root session)
  (recognition-child-value
   (car (recognition-sequence->list
         (lr-recognition-fragment-value (base-root (incremental-session-recognition-root session)))))))
(def (find-program-piece root bound)
  (let loop ((pending (list root)))
    (and (pair? pending)
         (let (piece (base-root (car pending)))
           (if (lr-recognition-fragment? piece)
             (let (values (recognition-sequence->list (lr-recognition-fragment-value piece)))
               (if (and (< (lr-recognition-fragment-end piece) bound)
                        (> (lr-recognition-fragment-token-count piece) 2)
                        (pair? values) (null? (cdr values))
                        (event-program-value? (recognition-child-value (car values))))
                 piece
                 (loop (append (lr-recognition-fragment-children piece) (cdr pending)))))
             (loop (cdr pending)))))))
(def (contains-piece? root expected)
  (let loop ((pending (list root)))
    (and (pair? pending)
         (let (piece (base-root (car pending)))
           (or (eq? piece expected)
               (loop (append (if (lr-recognition-fragment? piece)
                               (lr-recognition-fragment-children piece) '())
                             (cdr pending))))))))
(def (artifact-for source tokens root)
  (make-success-parse-artifact "event-test" source tokens root
                              (lambda (token) #f)))
(def event-program-test
  (test-suite "persistent LR event programs"
    (test-case "deep programs walk in source order and compose views without recursion"
      (let* ((code (let loop ((n 0) (code #f))
                     (if (= n 10000) code
                       (loop (+ n 1) (event-program-append code
                                      (event-program-token (make-token 'number "1" n (+ n 1))))))))
             (moved (event-program-relocate (event-program-relocate code 13) -5))
             (count 0) (valid? #t))
        (event-program-walk
         (lambda (operation token offset delta moved?)
           (set! valid? (and valid? (eq? operation 'token)
                             (= (+ offset delta) (+ count 8)) moved?))
           (set! count (+ count 1))) moved)
        (check count => 10000)
        (check valid? => #t)))
    (test-case "LR aliases retain the sole semantic sequence with cached boundaries"
      (let* ((sequence (let loop ((n 0) (sequence #f))
                         (if (= n 10000) sequence
                           (loop (+ n 1) (event-program-append sequence
                             (make-token 'number "1" n (+ n 1)))))))
             (program (event-children-alias 'Root sequence 0))
             (view (recognition-sequence-relocate
                     (recognition-sequence-relocate sequence 17) -17)))
        (check (eq? (event-program-value-body program) sequence) => #t)
        (check (event-program-sequence-arity sequence) => 2)
        (check (recognition-sequence-start sequence 99) => 0)
        (check (recognition-sequence-end sequence 99) => 10000)
        (check (recognition-sequence-start view 99) => 0)
        (check (recognition-sequence-end view 99) => 10000)
        (check (length (recognition-sequence->list view)) => 10000)))
    (test-case "lowering snapshots field names rather than caching mutable children"
      (let* ((token (make-token 'number "1" 0 1))
             (child (make-recognition-child #f token))
             (program (recognition-child-value (car (recognition-sequence->list (event-children-alias 'Root (list child) 0)))))
             (expected (artifact-for "1" (list token)
                         (recognition-child-value
                          (car (recognition-children-alias 'Root (list (make-recognition-child #f token)) 0))))))
        (recognition-child-field-set! child 'mutated)
        (check (artifact-for "1" (list token) program) => expected)))
    (test-case "named singleton fields and cancelled moves preserve canonical output and token proof"
      (let* ((old (make-token 'number "1" 0 1))
             (fresh (make-token 'number "1" 0 1))
             (children (list (make-recognition-child 'inner old)))
             (program-children (event-children-field 'outer children 0))
             (ordinary-children
              (list (make-recognition-child 'outer
                      (make-recognition-fragment 0 1 children))))
             (program-root (recognition-child-value
                            (car (recognition-sequence->list (event-children-alias 'Root program-children 0)))))
             (moved-program (recognition-child-value
                              (car (recognition-sequence->list (event-children-alias 'Root
                                     (recognition-sequence-relocate
                                      (recognition-sequence-relocate program-children 7) -7) 0)))))
             (moved-ordinary (recognition-child-value
                               (car (recognition-children-alias 'Root
                                      (recognition-sequence->list
                                       (recognition-sequence-relocate
                                        (recognition-sequence-relocate ordinary-children 7) -7)) 0)))))
        (check (artifact-for "1" (list old) program-root)
               => (artifact-for "1" (list old)
                    (recognition-child-value (car (recognition-children-alias 'Root ordinary-children 0)))))
        (check (artifact-for "1" (list fresh) moved-program)
               => (artifact-for "1" (list fresh) moved-ordinary))
        (check-exception (artifact-for "1" (list fresh) program-root) true)))
    (test-case "relocated program roots retain token proof and root clamps"
      (let* ((old (make-token 'number "1" 0 1))
             (fresh (make-token 'number "1" 1 2))
             (tokens (list (make-token 'ws " " 0 1) fresh))
             (children (list (make-recognition-child #f old)))
             (program (recognition-child-value (car (recognition-sequence->list (event-children-alias 'Root children 0)))))
             (ordinary (recognition-child-value (car (recognition-children-alias 'Root children 0))))
             (trivia? (lambda (token) (eq? (token-kind token) 'ws))))
        (check (make-success-parse-artifact "event-test" " 1" tokens
                 (relocate-recognition-value program 1) trivia?)
               => (make-success-parse-artifact "event-test" " 1" tokens
                    (relocate-recognition-value ordinary 1) trivia?))
        (check (make-success-parse-artifact "event-test" " 1" tokens
                 (relocate-recognition-value (relocate-recognition-value program 8) -7) trivia?)
               => (make-success-parse-artifact "event-test" " 1" tokens
                    (relocate-recognition-value ordinary 1) trivia?))))
    (test-case "HCL events actually execute generated steps across topology Unicode and history edits"
      (parameterize ((current-lr-event-program-enabled? #t))
        (let* ((source (string-append "# 前置\n" (string-join (make-list 100 "a = 1 # 中\n") "") "# 尾部\n"))
               (session (make-incremental-session hcl-v2-24-parser source #t)))
          (check (event-program-value? (program-root session)) => #t)
          (check
           (let loop ((pending (list (incremental-session-recognition-root session))))
             (if (null? pending) #t
               (let (piece (base-root (car pending)))
                 (and (or (not (lr-recognition-fragment? piece))
                          (null? (lr-recognition-fragment-value piece))
                          (event-program-sequence? (lr-recognition-fragment-value piece)))
                      (loop (append (if (lr-recognition-fragment? piece)
                                      (lr-recognition-fragment-children piece) '())
                                    (cdr pending))))))) => #t)
          (check (lr-runtime-event-program?
                  (lr-recognition-fragment-runtime (base-root (incremental-session-recognition-root session)))) => #t)
          (check (incremental-session-artifact session) => (parse-hcl-v2-24 source))
          (check (eq? (lr-recognition-fragment-executor
                       (base-root (incremental-session-recognition-root session))) direct-event-step) => #t)
          (let* ((piece (find-program-piece (incremental-session-recognition-root session) 500))
                 (code (and piece (event-program-value-code
                         (recognition-child-value (car (recognition-sequence->list
                           (lr-recognition-fragment-value piece)))))))
                 (edit (make-edit (u8vector-length (string->utf8 source)) 0 "last = 2\n")))
            (check (not (not piece)) => #t)
            (let-values (((next receipt) (parse-incremental-session session edit)))
              (set! source (apply-edit source edit)) (set! session next)
              (check (contains-piece? (incremental-session-recognition-root session) piece) => #t)
              (check (eq? code (event-program-value-code
                        (recognition-child-value (car (recognition-sequence->list
                          (lr-recognition-fragment-value piece)))))) => #t)
              (check (incremental-session-artifact session) => (parse-hcl-v2-24 source))
              (check (incremental-session-project-artifact session) => (incremental-session-artifact session))))
          (for-each
           (lambda (edit)
             (let-values (((next receipt) (parse-incremental-session session edit)))
               (set! source (apply-edit source edit)) (set! session next)
               (check (event-program-value? (program-root session)) => #t)
               (check (incremental-session-artifact session) => (parse-hcl-v2-24 source))
               (check (incremental-session-project-artifact session) => (incremental-session-artifact session))))
           (list (make-edit 0 0 "x = 2\n") (make-edit 0 6 "")
                 (make-edit 9 0 "y = 3\n") (make-edit 9 6 "")
                 (make-edit 0 0 "# é\n") (make-edit 0 5 ""))))))
    (test-case "selected machine preserves certified transfers and backend ownership across flag changes"
      (let* ((source (string-join (make-list 100 "a = 1\n") ""))
             (control (parameterize ((current-lr-event-program-enabled? #f))
                        (make-incremental-session hcl-v2-24-parser source #t)))
             (candidate (parameterize ((current-lr-event-program-enabled? #t))
                          (make-incremental-session hcl-v2-24-parser source #t)))
             (edit (make-edit 0 0 "b = 2\n")))
        (let-values (((control-next control-receipt)
                      (parameterize ((current-lr-event-program-enabled? #f))
                        (parse-incremental-session control edit)))
                     ((candidate-next candidate-receipt)
                      (parameterize ((current-lr-event-program-enabled? #t))
                        (parse-incremental-session candidate edit))))
          (check (> (cdr (assq 'reusedRecognitionFragmentCount candidate-receipt)) 0) => #t)
          (for-each
           (lambda (key)
             (check (assq key candidate-receipt) => (assq key control-receipt)))
           '(reusedRecognitionFragmentCount fragmentCertificateProbeByteCount
             fragmentRejectedProbeCount fragmentControlProbeCount
             fragmentProbeReuseTokenCount fragmentProbeReuseByteCount fragmentCursorVisitCount
             remainingSignificantTokenCount relexedByteCount reusedSignificantTokenCount))
          (check (incremental-session-artifact candidate-next)
                 => (incremental-session-artifact control-next))
          (check (incremental-session-project-artifact candidate-next)
                 => (incremental-session-artifact candidate-next))
          (let-values (((restored receipt)
                        (parameterize ((current-lr-event-program-enabled? #f))
                          (parse-incremental-session candidate-next (make-edit 0 6 "")))))
            (check (event-program-value? (program-root restored)) => #t)
            (check (lr-runtime-event-program? (lr-recognition-fragment-runtime
                    (base-root (incremental-session-recognition-root restored)))) => #t)
            (check (incremental-session-artifact restored) => (parse-hcl-v2-24 source))
            (check (incremental-session-project-artifact restored)
                   => (incremental-session-artifact restored))))))
    (test-case "generic deterministic LR lowers events without a generated executor"
      (parameterize ((current-lr-event-program-enabled? #t))
        (let* ((source "1+2*3")
               (session (make-incremental-session arithmetic-parser source #t))
               (root (base-root (incremental-session-recognition-root session))))
          (check (event-program-value? (program-root session)) => #t)
          (check (lr-runtime-event-program? (lr-recognition-fragment-runtime root)) => #t)
          (check (lr-recognition-fragment-executor root) => #f)
          (check (incremental-session-artifact session) => (parse-arithmetic-v1 source))
          (let-values (((next receipt) (parse-incremental-session session (make-edit 0 0 "0+"))))
            (check (event-program-value? (program-root next)) => #t)
            (check (incremental-session-artifact next) => (parse-arithmetic-v1 "0+1+2*3"))
            (check (incremental-session-project-artifact next) => (incremental-session-artifact next))))))
    (test-case "unsupported selective GLR remains on the canonical backend"
      (parameterize ((current-lr-event-program-enabled? #t))
        (check (selective-glr-scenario-pass? (selective-glr-scenario)) => #t)))))
(export event-program-test)
