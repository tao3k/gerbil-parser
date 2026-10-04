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

(deflanguage empty-action-probe
  (identity "empty-action-probe" "v1" "empty-action-probe.v1")
  (root source-file)
  (lex (word Word (identifier))
       (punctuation Punctuation (literals ","))
       (space Space (whitespace+)))
  (rules
   (source-file
    (node SourceFile
     (seq (optional (field left word))
          (optional (seq "," (field right (node Item word))))))))
  (extras space)
  (keywords)
  (recoveries)
  (conflicts selective-glr)
  (case-insensitive #f))

(deflanguage ordered-action-probe
  (identity "ordered-action-probe" "v1" "ordered-action-probe.v1")
  (root source-file)
  (lex (word Word (identifier))
       (punctuation Punctuation (literals ","))
       (space Space (whitespace+)))
  (rules
   (source-file
    (node SourceFile
     (seq (field first (node First word)) ","
          (optional (field middle (node Middle word))) ","
          (field last (node Last word))))))
  (extras space)
  (keywords)
  (recoveries)
  (conflicts reject)
  (case-insensitive #f))

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
