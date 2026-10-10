#!/usr/bin/env gxi
(import (only-in :gerbil-parser/t/fixtures/fixture-release bind-fixture-grammar-release))
;;; -*- Gerbil -*-
;;; Explicit closing boundaries belong to the declaring language grammar.

(import :std/test
        :gerbil-parser/src/grammar/algebra
        (only-in :gerbil-parser/src/language/grammar deflanguage)
        (only-in :gerbil-parser/src/runtime/parser
                 parse-source parse-source/checkpoints
                 prepare-contextual-parser parse-source/contextual)
        (only-in :gerbil-parser/src/compiler/machine
                 parser-machine-runtime parser-machine-grammar-digest parser-machine-ir)
        (only-in :gerbil-parser/src/modules/parser/contextual-objects
                 make-contextual-method make-contextual-role make-contextual-scan-rule)
        (only-in :gerbil-parser/src/compiler/contextual-parser-ir compile-contextual-parser)
        (only-in :gerbil-parser/src/compiler/lr-compiler compile-lr-spec)
        (only-in :gerbil-parser/src/runtime/lr-parser
                 lr-prepare lr-parse/prepared lr-parse/prepared/receipt
                 current-lr-event-program-enabled? lr-runtime-for-current-semantic-backend
                 lr-initial-checkpoint lr-checkpoint-drive)
        (only-in :gerbil-parser/src/runtime/lr-action-index index-action-row)
        (only-in :gerbil-parser/src/runtime/artifact
                 make-success-parse-artifact
                 parse-artifact-success? parse-artifact-valid? parse-artifact-roundtrip)
        (only-in :gerbil-parser/src/runtime/token make-token)
        (only-in :gerbil-parser/src/runtime/layout
                 make-layout-columns current-layout-columns current-layout-frames
                 layout-after-end layout-current-action-row))
(export layout-boundary-test)

(begin
 (deflanguage closing-boundary-probe
  (syntax
   (lexical
    (root source-file)
    (lex (word Word (identifier))
       (punctuation Punctuation (literals "(" "|>"))
       (space Space (whitespace+)))
    (extras space)
    (keywords)
    (recoveries)
    (conflicts selective-glr)
    (case-insensitive #f)))
  (rules
   (source-file (node SourceFile (seq "(" (field items (reference items)) "END")))
   (items
    (node Items
      (seq (layout-start "|>") (field item word)
           (repeat (seq (layout-next "|>") (field item word)))
           (layout-end "END"))))))
 (bind-fixture-grammar-release closing-boundary-probe "closing-boundary-probe" "v1" "closing-boundary-probe.v1") )

(def (layout-contextual-product machine)
  (compile-contextual-parser
   (parser-machine-ir machine) (parser-machine-grammar-digest machine)
   (list (make-contextual-role
          'layout-probe
          (list (make-contextual-method 'word 'any 'any 'word 'word))))
   '(normal) '(body) '(word)
   (list (make-contextual-scan-rule 'word 'normal 'word '(identifier) 0 'keep))
   'normal '((body () ()))))

(def layout-boundary-test
  (test-suite "Grammar-owned layout closing boundaries"
    (test-case "layout continuations are rejected before contextual preparation"
      (let* ((machine closing-boundary-probe-parser)
             (runtime (parser-machine-runtime machine))
             (product (layout-contextual-product machine)))
        (for-each
         (lambda (events?)
           (parameterize ((current-lr-event-program-enabled? events?))
             (for-each
              (lambda (create)
                (check (with-catch (lambda (condition) (error-message condition)) create)
                       => "LR checkpoints require a runtime without layout"))
              (list (lambda () (lr-initial-checkpoint runtime '()) #f)
                    (lambda () (prepare-contextual-parser machine product) #f)
                    (lambda () (parse-source/contextual machine product "( |> x END") #f)))))
         '(#f #t))))
    (test-case "layout checkpoint source requests retain complete GLR artifacts"
      (for-each
       (lambda (source)
         (let-values (((artifact tokens modes snapshots)
                       (parse-source/checkpoints closing-boundary-probe-parser source 1)))
           (check artifact => (parse-source closing-boundary-probe-parser source))
           (check (parse-artifact-valid? artifact) => #t)
           (check (parse-artifact-roundtrip artifact) => source)
           (check tokens => #f)
           (check modes => #f)
           (check snapshots => '#())))
       '("( |> x END" "( |> x\n  |> y END" "( |>")))
    (test-case "plain prepared runtimes ignore outer layout columns and frames"
      (for-each
       (lambda (rules)
         (let ((runtime (lr-prepare (compile-lr-spec rules 'root 'selective-glr)))
               (tokens (list (make-token 'word "x" 0 1)))
               (columns (make-layout-columns "x"))
               (frames '((100 . "outer"))))
           (for-each
            (lambda (events?)
              (parameterize ((current-lr-event-program-enabled? events?))
                (let (selected (lr-runtime-for-current-semantic-backend runtime))
                  (let-values (((expected rest) (lr-parse/prepared selected tokens)))
                    (check rest => '())
                    (def (artifact root)
                      (make-success-parse-artifact
                       (string-append "sha256:" (make-string 64 #\0)) "x" tokens root #f))
                    (parameterize ((current-layout-columns columns) (current-layout-frames frames))
                      (let-values (((root rest) (lr-parse/prepared selected tokens)))
                        (check rest => '())
                        (check (artifact root) => (artifact expected)))
                      ;; The independent materialized GLR entry has its own
                      ;; fast and memoized action lookups, even for plain IR.
                      (let-values (((root rest receipt) (lr-parse/prepared/receipt selected tokens)))
                        (check rest => '())
                        (check (artifact root) => (artifact expected)))
                      (check (eq? columns (current-layout-columns)) => #t)
                      (check (eq? frames (current-layout-frames)) => #t))))))
            '(#f #t))))
       '(((root (alias Root (token word))))
         ((root (alias Root (choice (reference first) (reference second))))
          (first (precedence dynamic 2 (token word)))
          (second (precedence dynamic 1 (token word)))))))
    (test-case "streaming LR callbacks can parse inside an active outer layout request"
      (let* ((runtime (lr-prepare (compile-lr-spec '((root (alias Root (token word)))) 'root)))
             (input (make-token 'word "x" 0 1))
             (pending (list input)) (shifted '())
             (columns (make-layout-columns "x")) (frames '((100 . "outer"))))
        (parameterize ((current-layout-columns columns) (current-layout-frames frames))
          (let-values (((status payload)
                        (lr-checkpoint-drive
                         (lr-initial-checkpoint runtime '())
                         (lambda (_mode)
                           (and (pair? pending)
                                (let (token (car pending))
                                  (set! pending (cdr pending)) token)))
                         (lambda (token _states _values _actions _shifts)
                           (set! shifted (cons token shifted))))))
            (check status => 'accepted)
            (check (cadr payload) => '())
            (check shifted => (list input)))
          (check (eq? columns (current-layout-columns)) => #t)
          (check (eq? frames (current-layout-frames)) => #t))))
    (test-case "layout casefold lookup retains the declared row and guard decision"
      (def entries
        '(((terminal literal "END") layout-guard (shift 1 #f) (reduce 0))
          ((terminal literal "É") shift 2 #f)
          ((terminal token word) shift 3 #f)))
      (def row (index-action-row entries))
      (parameterize ((current-layout-columns (make-layout-columns "end é"))
                     (current-layout-frames '()))
        (check (layout-current-action-row row (make-token 'word "end" 0 3) #t)
               => '((terminal literal "END") shift 1 #f))
        (check (eq? (layout-current-action-row row (make-token 'word "é" 4 6) #t)
                    (cadr entries)) => #t)
        (check (eq? (layout-current-action-row row (make-token 'word "end" 0 3) #f)
                    (caddr entries)) => #t)
        (parameterize ((current-layout-frames '((100 . "outer"))))
          (check (layout-current-action-row row (make-token 'word "end" 0 3) #t)
                 => '((terminal literal "END") reduce 0)))))
    (test-case "nested source success and rejection restore the outer request context"
      (let ((columns (make-layout-columns "outer")) (frames '((100 . "outer"))))
        (parameterize ((current-layout-columns columns) (current-layout-frames frames))
          (for-each
           (lambda (source)
             (let ((expected (equal? source "( |> x END"))
                   (artifact (parse-source closing-boundary-probe-parser source)))
               (check (parse-artifact-success? artifact) => expected)
               (check (parse-artifact-valid? artifact) => #t)
               (check (parse-artifact-roundtrip artifact) => source)
               (check (eq? columns (current-layout-columns)) => #t)
               (check (eq? frames (current-layout-frames)) => #t)))
           '("( |> x END" "( |>" "( |> x END")))))
    (test-case "closing boundaries are validated nullable grammar values"
      (check (grammar-expression? '(layout-end ")" "END")) => #t)
      (check (grammar-expression-nullable? (grammar-expression (layout-end "END"))) => #t)
      (check (grammar-expression? '(layout-end "")) => #f)
      (check (grammar-expression? '(layout-end END)) => #f))
    (test-case "an arbitrary declared word closes a list to the right of its marker"
      (for-each
       (lambda (source)
         (let (artifact (parse-source closing-boundary-probe-parser source))
           (check (parse-artifact-success? artifact) => #t)
           (check (parse-artifact-valid? artifact) => #t)
           (check (parse-artifact-roundtrip artifact) => source)))
       '("( |> x END" "( |> x\n  |> y END")))
    (test-case "undeclared right-hand boundaries retain the column restriction"
      (parameterize ((current-layout-columns (make-layout-columns "( |> x END"))
                     (current-layout-frames '((3 . "|>"))))
        (let (end (make-token 'word "END" 7 10))
          (check (layout-after-end end) => #f)
          (check (layout-after-end end '("END")) => '()))))
    (test-case "closing a nested list pops exactly one reference"
      (parameterize ((current-layout-columns (make-layout-columns "( |> x END"))
                     (current-layout-frames '((3 . "|>") (1 . "outer"))))
        (check (layout-after-end (make-token 'word "END" 7 10) '("END"))
               => '((1 . "outer")))))
    (test-case "an aligned continuation remains in its list"
      (parameterize ((current-layout-columns (make-layout-columns "  |> x\n  |> y"))
                     (current-layout-frames '((3 . "|>"))))
        (check (layout-after-end (make-token 'punctuation "|>" 9 11) '("|>"))
               => #f)))))
