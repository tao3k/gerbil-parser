(import :gerbil-parser/language-test-support)
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


(defparser-check (check-unary-chain depth events? (decorated? #t))
(locals ((name (lambda ( n) (string->symbol (string-append "layer" (number->string n))))))

  (sequential ((rules
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
    (scope-values (((root rest receipt) (lr-parse/prepared/receipt runtime tokens)))
      (equal rest  '())
      (scope ((expected (make-success-parse-artifact digest "a;" tokens root false)))
        (parameters ((current-lr-event-program-enabled? events?))
          (sequential ((selected (lr-runtime-for-current-semantic-backend runtime))
                 (initial (lr-initial-checkpoint selected tokens))
                 (second-shift-actions #f))
            (scope-values (((status result)
                          (lr-checkpoint-drive initial
                            (lambda (_mode) #f)
                            (lambda (_token _states _values actions shifts)
                              (when (= shifts 2) (set! second-shift-actions actions))))))
              (equal status  'accepted)
              (equal (make-success-parse-artifact digest "a;" tokens (car result) false)
                      expected))
            (iterate pause ((checkpoint initial))
              (scope-values (((status result) (lr-checkpoint-advance checkpoint 1)))
                (choose status ((checkpoint)
                   (equal (lr-checkpoint-deterministic-actions result)
                           (+ 1 (lr-checkpoint-deterministic-actions checkpoint)))
                   (branch (= (lr-checkpoint-deterministic-shifts result) 2)
                     (equal (lr-checkpoint-deterministic-actions result)  second-shift-actions)
                     (continue pause result))) (else (act (error "bounded chain failed before second shift" status)))))))))))))

(deflanguage-parser-tests lr-unary-corridor-test "shared unary reduction corridors"
  (engine)
    (scenario "decorated chains retain canonical artifacts and every action count"
      (each (depth) ('(0 1 4 16))
                  (each (events?) ('(#f #t)) (verify check-unary-chain depth events?))))
    (scenario "identity chains preserve artifacts and every paused action"
      (each (depth) ('(0 1 4 16))
                  (each (events?) ('(#f #t)) (verify check-unary-chain depth events? #f)))))
