#!/usr/bin/env gxi
;;; Direct event lowering must match the independent original-production replay.
(import :std/test
        (only-in :gerbil-parser/languages/arithmetic/parser arithmetic-parser parse-arithmetic)
        (only-in :gerbil-parser/languages/hcl/parser hcl-parser parse-hcl)
        :gerbil-parser/src/runtime/event-program
        :gerbil-parser/src/runtime/event-reduce
        :gerbil-parser/src/runtime/recognition
        (only-in :gerbil-parser/src/runtime/reduce recognition-children-alias)
        :gerbil-parser/src/runtime/funcs
        (only-in :gerbil-parser/src/runtime/token make-token token-kind)
        (only-in :gerbil-parser/src/runtime/artifact make-success-parse-artifact
                 parse-artifact-valid? parse-artifact-events sha256-text)
        (only-in :gerbil-parser/src/runtime/lr-parser
                 current-lr-event-program-enabled? lr-runtime-event-program?
                 lr-recognition-fragment-runtime lr-recognition-fragment-value
                 lr-recognition-view? lr-recognition-view-base lr-recognition-fragment?
                 lr-recognition-fragment-children lr-recognition-fragment-end
                 lr-recognition-fragment-token-count lr-recognition-fragment-executor)
        (only-in :gerbil-parser/src/compiler/hcl-reductions direct-event-step)
        (only-in :gerbil-parser/src/runtime/incremental
                 make-incremental-session incremental-session-artifact
                 incremental-session-recognition-root incremental-session-project-artifact
                 incremental-session-publication-comparison
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
        (check (<= (event-program-sequence-height sequence) 12) => #t)
        (check (recognition-sequence-start sequence 99) => 0)
        (check (recognition-sequence-end sequence 99) => 10000)
        (check (recognition-sequence-start view 99) => 0)
        (check (recognition-sequence-end view 99) => 10000)
        (check (length (recognition-sequence->list view)) => 10000)))
    (test-case "bounded block append preserves frozen prefixes and composed moves"
      (let* ((prefix (let loop ((n 0) (value #f))
                       (if (= n 33) value
                         (loop (+ n 1) (event-program-append value
                           (make-token 'number "1" n (+ n 1)))))))
             (prefix-list (recognition-sequence->list prefix))
             (next (event-program-append prefix (make-token 'number "1" 33 34)))
             (moved (event-program-append
                      (event-program-relocate prefix 10) (make-token 'number "1" 43 44)))
             (count 0) (valid? #t))
        (check (recognition-sequence->list prefix) => prefix-list)
        (check (length (recognition-sequence->list next)) => 34)
        (check (recognition-sequence-start moved 99) => 10)
        (check (recognition-sequence-end moved 99) => 44)
        (event-program-walk
         (lambda (operation token offset delta moved?)
           (set! valid? (and valid? (eq? operation 'token)
                            (= (+ offset delta) (+ count 10))))
           (set! count (+ count 1))) moved)
        (check count => 34)
        (check valid? => #t)))
    (test-case "deep node publication retains opening IDs through the bounded fallback"
      (let* ((token (make-token 'number "1" 0 1))
             (program (let loop ((n 0) (body token))
                        (if (= n 10000) body
                          (loop (+ n 1) (event-program-node-value 'Node 0 1 body)))))
             (artifact (make-success-parse-artifact (sha256-text "event-test") "1"
                         (list token) program (lambda (token) #f))))
        (check (parse-artifact-valid? artifact) => #t)
        (check (length (parse-artifact-events artifact)) => 20001)
        (check (car (parse-artifact-events artifact)) => (vector 'start-node 0 'Node 0))
        (check (last (parse-artifact-events artifact)) => (vector 'finish-node 0 'Node 1))))
    (test-case "asymmetric balanced joins preserve complete source order and old operands"
      (def (range start count)
        (let loop ((n 0) (value #f))
          (if (= n count) value
            (loop (+ n 1) (event-program-append value
              (make-token 'number "1" (+ start n) (+ start n 1)))))))
      (for-each
       (lambda (sizes)
         (let* ((left-size (car sizes)) (right-size (cadr sizes))
                (left (range 0 left-size)) (right (range left-size right-size))
                (joined (event-program-append left right)) (seen 0) (valid? #t))
           (event-program-walk
            (lambda (operation token offset delta moved?)
              (set! valid? (and valid? (eq? operation 'token) (= offset seen)))
              (set! seen (+ seen 1))) joined)
           (check valid? => #t)
           (check seen => (+ left-size right-size))
           (check (<= (event-program-sequence-height joined) 12) => #t)
           (check (length (recognition-sequence->list left)) => left-size)
           (check (length (recognition-sequence->list right)) => right-size)))
       '((1 5000) (5000 1) (257 4096) (4096 257) (33 33))))
    (test-case "binary block carries preserve branching roots and deep fallback order"
      (let* ((tokens (list->vector (map (lambda (i) (make-token 'number "1" i (+ i 1))) (iota 1025))))
             (retained '())
             (sequence (let loop ((i 0) (value #f))
                         (when (memv i '(16 17 32 33 255 256 257 1024))
                           (set! retained (cons (cons i value) retained)))
                         (if (= i 1025) value
                           (loop (+ i 1) (event-program-append value (vector-ref tokens i))))))
             (control (let loop ((i 0) (value #f))
                        (if (= i 1025) value
                          (loop (+ i 1) (event-program-append/avl value (vector-ref tokens i))))))
             (root (let loop ((i 0) (value sequence))
                     (if (= i 100) value
                       (loop (+ i 1) (event-program-node-value 'Root 0 1025 value)))))
             (count 0) (opens 0) (closes 0))
        (check (recognition-sequence->list sequence) => (recognition-sequence->list control))
        (for-each
         (lambda (entry)
           (let* ((n (car entry)) (old (cdr entry))
                  (a (event-program-append old (vector-ref tokens n)))
                  (b (event-program-append old (vector-ref tokens n))))
             (check (length (recognition-sequence->list old)) => n)
             (check (recognition-sequence->list a) => (recognition-sequence->list b))
             (check (recognition-sequence-end a 0) => (+ n 1)))) retained)
        (event-program-walk
         (lambda (operation value offset delta moved?)
           (case operation
             ((token) (check offset => count) (set! count (+ count 1)))
             ((open-node) (set! opens (+ opens 1)))
             ((close-node) (set! closes (+ closes 1))))) root)
        (check count => 1025) (check opens => 100) (check closes => 100)))
    (test-case "alternating singleton and multi-sequence joins retain bounded rope height"
      (let* ((tokens (list->vector (map (lambda (i) (make-token 'number "1" i (+ i 1))) (iota 3000))))
             (sequence (let loop ((i 0) (value #f))
                         (if (= i 3000) value
                           (let* ((one (event-program-append value (vector-ref tokens i)))
                                  (two (event-program-append (vector-ref tokens (+ i 1))
                                                            (vector-ref tokens (+ i 2)))))
                             (loop (+ i 3) (event-program-append one two))))))
             (count 0))
        (check (<= (event-program-sequence-height sequence) 12) => #t)
        (event-program-sequence-for-each
         (lambda (field value delta moved?)
           (check (eq? value (vector-ref tokens count)) => #t)
           (set! count (+ count 1))) sequence)
        (check count => 3000)))
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
               (session (make-incremental-session hcl-parser source #t)))
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
          (check (incremental-session-artifact session) => (parse-hcl source))
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
              (check (incremental-session-artifact session) => (parse-hcl source))
              (check (incremental-session-project-artifact session) => (incremental-session-artifact session))))
          (for-each
           (lambda (edit)
             (let-values (((next receipt) (parse-incremental-session session edit)))
               (set! source (apply-edit source edit)) (set! session next)
               (check (event-program-value? (program-root session)) => #t)
               (check (incremental-session-artifact session) => (parse-hcl source))
               (check (incremental-session-project-artifact session) => (incremental-session-artifact session))))
           (list (make-edit 0 0 "x = 2\n") (make-edit 0 6 "")
                 (make-edit 9 0 "y = 3\n") (make-edit 9 6 "")
                 (make-edit 0 0 "# é\n") (make-edit 0 5 ""))))))
    (test-case "selected machine preserves certified transfers and backend ownership across flag changes"
      (let* ((source (string-join (make-list 100 "a = 1\n") ""))
             (control (parameterize ((current-lr-event-program-enabled? #f))
                        (make-incremental-session hcl-parser source #t)))
             (candidate (parameterize ((current-lr-event-program-enabled? #t))
                          (make-incremental-session hcl-parser source #t)))
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
            (check (incremental-session-artifact restored) => (parse-hcl source))
            (check (incremental-session-project-artifact restored)
                   => (incremental-session-artifact restored))))))
    (test-case "publication closures retain edited source and token binding across inverse edits"
      (let* ((source (string-join (make-list 100 "a = 1\n") ""))
             (session (parameterize ((current-lr-event-program-enabled? #t))
                        (make-incremental-session hcl-parser source #t))))
        (let-values (((next receipt) (parse-incremental-session session (make-edit 0 0 "b = 2\n"))))
          (let-values (((control candidate code) (incremental-session-publication-comparison next)))
            (let ((expected (parse-hcl (string-append "b = 2\n" source))) (moved 0))
              (event-program-walk
               (lambda (op value offset delta moved?)
                 (when (and (eq? op 'token) moved?) (set! moved (+ moved 1)))) code)
              (check (> moved 0) => #t)
              (parameterize ((current-lr-event-program-enabled? #f))
                (let-values (((restored receipt) (parse-incremental-session next (make-edit 0 6 ""))))
                  (check (incremental-session-artifact restored) => (parse-hcl source))
                  (check (control) => expected)
                  (check (candidate) => expected)
                  (check (incremental-session-artifact session) => (parse-hcl source)))))))))
    (test-case "generic deterministic LR lowers events without a generated executor"
      (parameterize ((current-lr-event-program-enabled? #t))
        (let* ((source "1+2*3")
               (session (make-incremental-session arithmetic-parser source #t))
               (root (base-root (incremental-session-recognition-root session))))
          (check (event-program-value? (program-root session)) => #t)
          (check (lr-runtime-event-program? (lr-recognition-fragment-runtime root)) => #t)
          (check (lr-recognition-fragment-executor root) => #f)
          (check (incremental-session-artifact session) => (parse-arithmetic source))
          (let-values (((next receipt) (parse-incremental-session session (make-edit 0 0 "0+"))))
            (check (event-program-value? (program-root next)) => #t)
            (check (incremental-session-artifact next) => (parse-arithmetic "0+1+2*3"))
            (check (incremental-session-project-artifact next) => (incremental-session-artifact next))))))
    (test-case "unsupported selective GLR remains on the canonical backend"
      (parameterize ((current-lr-event-program-enabled? #t))
        (check (selective-glr-scenario-pass? (selective-glr-scenario)) => #t)))))
(export event-program-test)
