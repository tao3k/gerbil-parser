(import (only-in :gerbil-parser/t/fixtures/fixture-release bind-fixture-grammar-release))
;;; -*- Gerbil -*-
;;; Empty reductions and identity operands must agree across semantic backends.
(import :std/test
        :gerbil-parser/src/grammar/algebra
        (only-in :gerbil-parser/src/language/grammar deflanguage)
        (only-in :gerbil-parser/src/runtime/parser parse-source)
        (only-in :gerbil-parser/src/runtime/lr-parser current-lr-event-program-enabled?)
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-success? parse-artifact-valid? parse-artifact-roundtrip
                 parse-artifact-events event-kind token-event? token-event-lexeme)
        (only-in :gerbil-parser/src/runtime/incremental
                 make-incremental-session incremental-session-artifact
                 incremental-session-project-artifact parse-incremental-session
                 make-edit apply-edit))
(export lr-empty-action-test)

(begin
 (deflanguage empty-action-probe
  (syntax
   (lexical
    (root source-file)
    (lex (word Word (identifier))
       (punctuation Punctuation (literals ","))
       (space Space (whitespace+)))
    (extras space)
    (keywords)
    (recoveries)
    (conflicts selective-glr)
    (case-insensitive #f)))
  (rules
   (source-file
    (node SourceFile
     (seq (optional (field left word))
          (optional (seq "," (field right (node Item word)))))))))
 (bind-fixture-grammar-release empty-action-probe "empty-action-probe" "v1" "empty-action-probe.v1") )

(begin
 (deflanguage ordered-action-probe
  (syntax
   (lexical
    (root source-file)
    (lex (word Word (identifier))
       (punctuation Punctuation (literals ","))
       (space Space (whitespace+)))
    (extras space)
    (keywords)
    (recoveries)
    (conflicts reject)
    (case-insensitive #f)))
  (rules
   (source-file
    (node SourceFile
     (seq (field first (node First word)) ","
          (optional (field middle (node Middle word))) ","
          (field last (node Last word)))))))
 (bind-fixture-grammar-release ordered-action-probe "ordered-action-probe" "v1" "ordered-action-probe.v1") )

(begin
 (deflanguage chained-action-probe
  (syntax
   (lexical
    (root source-file)
    (lex (word Word (identifier))
       (space Space (whitespace+)))
    (extras space)
    (keywords)
    (recoveries)
    (conflicts reject)
    (case-insensitive #f)))
  (rules
   (source-file
    (node SourceFile
     (field outer (node Renamed (field inner (node Original word))))))))
 (bind-fixture-grammar-release chained-action-probe "chained-action-probe" "v1" "chained-action-probe.v1") )

(begin
 (deflanguage unary-stack-probe
  (syntax
   (lexical
    (root source-file)
    (lex (word Word (identifier))
       (punctuation Punctuation (literals ";"))
       (space Space (whitespace+)))
    (extras space)
    (keywords)
    (recoveries)
    (conflicts reject)
    (case-insensitive #f)))
  (rules
   (source-file
    (node SourceFile
     (seq (field head (node Head word)) ";"
          (field tail (node Renamed (field inner (node Original word)))))))))
 (bind-fixture-grammar-release unary-stack-probe "unary-stack-probe" "v1" "unary-stack-probe.v1") )

(def (parse-probe source events?)
  (parameterize ((current-lr-event-program-enabled? events?))
    (parse-source empty-action-probe-parser source)))

(def lr-empty-action-test
  (test-suite "LR identity operands and empty reductions"
    (test-case "nullable and decorated operands retain identical artifacts"
      (for-each
       (lambda (source)
         (let (artifact (parse-probe source #f))
           (check (parse-artifact-success? artifact) => #t)
           (check (parse-artifact-valid? artifact) => #t)
           (check (parse-artifact-roundtrip artifact) => source)
           (check (parse-probe source #t) => artifact)))
       '("" " " "a" ",b" "a, b"))
      (for-each
       (lambda (source)
         (let (artifact (parse-probe source #f))
           (check (parse-artifact-success? artifact) => #f)
           (check (parse-probe source #t) => artifact)))
       '("a," "a,b,c")))
    (test-case "multi-operand concatenation retains field and token order"
      (for-each
       (lambda (entry)
         (let* ((source (car entry))
                (artifact (parse-source ordered-action-probe-parser source))
                (events (parse-artifact-events artifact)))
           (check (parse-artifact-success? artifact) => #t)
           (check (parse-artifact-valid? artifact) => #t)
           (check (parse-artifact-roundtrip artifact) => source)
           (check (map (lambda (event) (vector-ref event 1))
                       (filter (lambda (event) (eq? (event-kind event) 'start-field)) events))
                  => (cadr entry))
           (check (map token-event-lexeme (filter token-event? events)) => (caddr entry))
           (check (parameterize ((current-lr-event-program-enabled? #t))
                    (parse-source ordered-action-probe-parser source)) => artifact)))
       '(("a,b,c" (first middle last) ("a" "," "b" "," "c"))
         ("a,,c" (first last) ("a" "," "," "c"))
         ("a, b, c" (first middle last) ("a" "," " " "b" "," " " "c")))))
    (test-case "chained fields and aliases retain their nesting order"
      (for-each
       (lambda (source)
         (let* ((artifact (parameterize ((current-lr-event-program-enabled? #f))
                           (parse-source chained-action-probe-parser source)))
                (events (parse-artifact-events artifact)))
           (check (parse-artifact-success? artifact) => #t)
           (check (parse-artifact-valid? artifact) => #t)
           (check (parse-artifact-roundtrip artifact) => source)
           (check (map (lambda (event) (vector-ref event 1))
                       (filter (lambda (event) (eq? (event-kind event) 'start-field)) events))
                  => '(outer inner))
           (check (map (lambda (event) (vector-ref event 2))
                       (filter (lambda (event) (eq? (event-kind event) 'start-node)) events))
                  => '(SourceFile Renamed Original))
           (check (parameterize ((current-lr-event-program-enabled? #t))
                    (parse-source chained-action-probe-parser source)) => artifact)))
       '("a" " a ")))
    (test-case "unary actions consume only the top value above retained siblings"
      (for-each
       (lambda (source)
         (for-each
          (lambda (events?)
            (parameterize ((current-lr-event-program-enabled? events?))
              (let* ((artifact (parse-source unary-stack-probe-parser source))
                     (events (parse-artifact-events artifact))
                     (session (make-incremental-session unary-stack-probe-parser source #t))
                     (edit (make-edit 0 1 "alpha")))
                (check (parse-artifact-success? artifact) => #t)
                (check (parse-artifact-valid? artifact) => #t)
                (check (parse-artifact-roundtrip artifact) => source)
                (check (map (lambda (event) (vector-ref event 1))
                            (filter (lambda (event) (eq? (event-kind event) 'start-field)) events))
                       => '(head tail inner))
                (check (map (lambda (event) (vector-ref event 2))
                            (filter (lambda (event) (eq? (event-kind event) 'start-node)) events))
                       => '(SourceFile Head Renamed Original))
                (check (parameterize ((current-lr-event-program-enabled? (not events?)))
                         (parse-source unary-stack-probe-parser source)) => artifact)
                (let-values (((next receipt) (parse-incremental-session session edit)))
                  (check (incremental-session-artifact next)
                         => (parse-source unary-stack-probe-parser (apply-edit source edit)))
                  ;; Two successors must retain independent semantics while the
                  ;; source session and its previously published artifact stay valid.
                  (let (other-edit (make-edit 0 1 "gamma"))
                    (let-values (((other other-receipt)
                                  (parse-incremental-session session other-edit)))
                      (check (incremental-session-artifact other)
                             => (parse-source unary-stack-probe-parser
                                              (apply-edit source other-edit)))
                      (check (incremental-session-artifact next)
                             => (parse-source unary-stack-probe-parser
                                              (apply-edit source edit)))
                      (check (incremental-session-artifact session) => artifact)))))))
          '(#f #t)))
       '("a;b" "a ; β")))
    (test-case "editing through empty source matches independent production replay"
      (for-each
       (lambda (events?)
         (parameterize ((current-lr-event-program-enabled? events?))
           (let loop ((source "a, b")
                      (session (make-incremental-session empty-action-probe-parser "a, b" #t))
                      (edits (list (make-edit 0 4 "") (make-edit 0 0 "a, b"))))
             (check (incremental-session-artifact session) => (parse-probe source events?))
             (check (incremental-session-project-artifact session) => (incremental-session-artifact session))
             (when (pair? edits)
               (let-values (((next receipt) (parse-incremental-session session (car edits))))
                 (loop (apply-edit source (car edits)) next (cdr edits)))))))
       '(#f #t)))))
