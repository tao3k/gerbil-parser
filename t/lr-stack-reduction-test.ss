;;; Shared deterministic stack reduction versus independent materialized GLR.
(import :std/test
        (only-in :gerbil-parser/src/compiler/lr-compiler compile-lr-spec)
        (only-in :gerbil-parser/src/runtime/token make-token)
        (only-in :gerbil-parser/src/runtime/lr-parser
                 lr-prepare lr-parse/prepared lr-parse/prepared/receipt
                 current-lr-event-program-enabled? lr-runtime-for-current-semantic-backend)
        (only-in :gerbil-parser/src/runtime/artifact
                 make-success-parse-artifact parse-artifact-valid? parse-artifact-roundtrip))
(export lr-stack-reduction-test)

(def (check-stack-family width decorated?)
  (let* ((operand (if decorated?
                   '(field outer (alias Renamed (field inner (token word))))
                   '(token word)))
         (rules (list (list 'source-file
                       (list 'alias 'SourceFile
                             (cons 'sequence (make-list width operand))))))
         (runtime (lr-prepare (compile-lr-spec rules 'source-file)))
         (source (make-string width #\a))
         (tokens (map (lambda (index) (make-token 'word "a" index (+ index 1)))
                      (iota width))))
    (let-values (((reference rest receipt) (lr-parse/prepared/receipt runtime tokens)))
      (check rest => '())
      (let* ((digest (string-append "sha256:" (make-string 64 #\0)))
             (expected (make-success-parse-artifact digest source tokens reference false)))
        (check (parse-artifact-valid? expected) => #t)
        (check (parse-artifact-roundtrip expected) => source)
        (for-each
         (lambda (events?)
           (parameterize ((current-lr-event-program-enabled? events?))
             (let (selected (lr-runtime-for-current-semantic-backend runtime))
               (let-values (((root rest) (lr-parse/prepared selected tokens)))
                 (check rest => '())
                 (check (make-success-parse-artifact digest source tokens root false)
                        => expected)))))
         '(#f #t))))))

(def lr-stack-reduction-test
  (test-suite "shared LR stack reduction"
    (test-case "prepared runtimes reject unsupported actions before execution"
      (def (prepare action operand-actions)
        (lr-prepare
         (list (cons 'productions
                     (list (list 0 'source-file
                                 (list (list 'marked '(terminal word) operand-actions))
                                 action #f)))
               (cons 'actions (vector '()))
               (cons 'gotos (vector '()))
               (cons 'case-insensitive? #f))))
      (check-exception (prepare 'callback '()) true)
      (check-exception (prepare 'concat '((callback user))) true)
      (check-exception (prepare 'concat '((field))) true)
      (check-exception (prepare 'concat '((alias Name extra))) true)
      (check-exception (prepare 'concat '((field Name) . invalid)) true)
      (check (not (not (prepare 'concat '((field inner) (alias Renamed))))) => #t))
    (test-case "empty, unary and wide identity operands agree with GLR"
      (for-each (lambda (width) (check-stack-family width #f)) '(0 1 2 8 32)))
    (test-case "wide ordered field and alias chains agree with GLR"
      (for-each (lambda (width) (check-stack-family width #t)) '(1 2 8 32)))))
