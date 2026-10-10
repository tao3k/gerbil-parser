;;; Consecutive decorated unary reductions versus independent GLR and pauses.
(import :std/test
        (only-in :gerbil-parser/src/compiler/lr-compiler compile-lr-spec)
        (only-in :gerbil-parser/src/runtime/token make-token)
        (only-in :gerbil-parser/src/runtime/lr-parser
                 lr-prepare lr-parse/prepared lr-parse/prepared/receipt
                 lr-initial-checkpoint lr-checkpoint-drive lr-checkpoint-advance
                 lr-checkpoint-deterministic-actions lr-checkpoint-deterministic-shifts
                 current-lr-event-program-enabled? lr-runtime-for-current-semantic-backend)
        (only-in :gerbil-parser/src/runtime/artifact make-success-parse-artifact))
(export lr-unary-corridor-test)

(def (check-unary-chain depth events? (decorated? #t))
  (def (name n) (string->symbol (string-append "layer" (number->string n))))
  (let* ((rules
          (cons '(source-file (alias SourceFile (sequence (reference layer0) (token end))))
                (map (lambda (n)
                       (list (name n)
                             (let (operand (if (= n depth) '(token word)
                                             (list 'reference (name (+ n 1)))))
                               (if decorated?
                                 (list 'field (name n) (list 'alias (name n) operand))
                                 operand))))
                     (iota (+ depth 1)))))
         (runtime (lr-prepare (compile-lr-spec rules 'source-file)))
         (tokens (list (make-token 'word "a" 0 1) (make-token 'end ";" 1 2)))
         (digest (string-append "sha256:" (make-string 64 #\0))))
    (let-values (((root rest receipt) (lr-parse/prepared/receipt runtime tokens)))
      (check rest => '())
      (let (expected (make-success-parse-artifact digest "a;" tokens root false))
        (parameterize ((current-lr-event-program-enabled? events?))
          (let* ((selected (lr-runtime-for-current-semantic-backend runtime))
                 (initial (lr-initial-checkpoint selected tokens))
                 (second-shift-actions #f))
            (let-values (((status result)
                          (lr-checkpoint-drive initial
                            (lambda (_mode) #f)
                            (lambda (_token _states _values actions shifts)
                              (when (= shifts 2) (set! second-shift-actions actions))))))
              (check status => 'accepted)
              (check (make-success-parse-artifact digest "a;" tokens (car result) false)
                     => expected))
            (let pause ((checkpoint initial))
              (let-values (((status result) (lr-checkpoint-advance checkpoint 1)))
                (case status
                  ((checkpoint)
                   (check (lr-checkpoint-deterministic-actions result)
                          => (+ 1 (lr-checkpoint-deterministic-actions checkpoint)))
                   (if (= (lr-checkpoint-deterministic-shifts result) 2)
                     (check (lr-checkpoint-deterministic-actions result) => second-shift-actions)
                     (pause result)))
                  (else (error "bounded chain failed before second shift" status)))))))))))

(def lr-unary-corridor-test
  (test-suite "shared unary reduction corridors"
    (test-case "decorated chains retain canonical artifacts and every action count"
      (for-each (lambda (depth)
                  (for-each (lambda (events?) (check-unary-chain depth events?)) '(#f #t)))
                '(0 1 4 16)))
    (test-case "identity chains preserve artifacts and every paused action"
      (for-each (lambda (depth)
                  (for-each (lambda (events?) (check-unary-chain depth events? #f)) '(#f #t)))
                '(0 1 4 16)))))
