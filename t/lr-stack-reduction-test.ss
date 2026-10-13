;;; Shared deterministic stack reduction versus independent materialized GLR.
(import :std/test
        (only-in :std/test/base TestCase test-case-add!)
        (only-in :gerbil-parser/src/compiler/lr-compiler compile-lr-spec)
        (only-in :gerbil-parser/src/compiler/lr operand-actions-valid? validate-production-semantics)
        (only-in :gerbil-parser/src/runtime/token make-token)
        (only-in :gerbil-parser/src/runtime/recognition
                 make-recognition-child make-recognition-node make-recognition-fragment
                 recognition-child-value recognition-value-start recognition-value-end
                 recognition-value-bounds with-recognition-value-bounds recognition-node-children relocate-recognition-value)
        (only-in :gerbil-parser/src/runtime/event-program event-program-node-value event-program-fragment-value)
        (only-in :gerbil-parser/src/runtime/reduce
                 recognition-children-field recognition-children-alias
                 recognition-action-field recognition-action-alias
                 make-recognition-field-kernel make-recognition-alias-kernel)
        (only-in :gerbil-parser/src/runtime/lr-parser
                 lr-prepare lr-parse/prepared lr-parse/prepared/receipt
                 lr-initial-checkpoint lr-checkpoint-frontier lr-checkpoint-state
                 lr-checkpoint-rebind-suffix lr-checkpoint-resume lr-checkpoint-drive
                 lr-checkpoint-deterministic-actions lr-checkpoint-deterministic-shifts
                 lr-checkpoint-remaining-token-count
                 install-lr-runtime-direct-step! lr-rejection-condition?
                 current-lr-event-program-enabled? lr-runtime-for-current-semantic-backend
                 current-lr-recognition-observer lr-runtime-with-action-selector)
        (only-in :gerbil-parser/src/runtime/funcs association-row-index-ref
                 recognition-sequence-append recognition-sequence-relocate
                 recognition-sequence-for-action)
        (only-in :gerbil-parser/src/runtime/artifact
                 make-success-parse-artifact parse-artifact-valid? parse-artifact-roundtrip)
        (only-in :gerbil-parser/t/benchmarks/lr-request-stack/derivation
                 prepare-lr-semantic-replay replay-lr-semantics replay-lr-semantic-control
                 lr-semantic-replay-count profile-lr-semantic-actions))
(import (only-in :gerbil-parser/src/runtime/lr-stack-storage make-lr-token-observer))
(import (only-in :gerbil-parser/src/runtime/lr-parser/state lr-failure-frontier-checkpoint))
(import (only-in :gerbil-parser/t/fixtures/lr-action-selection reference-lr-action-selector))
(export lr-stack-reduction-test)

;;; Installed procedures are arbitrary callbacks. They may change the current
;;; lookahead during a reduction, so their executor must not cache its class.
(def (installed-step-lookahead-case)
  (let* ((runtime (lr-prepare
                   (compile-lr-spec '((start (sequence (reference first) (literal "b")))
                                      (first (literal "a"))) 'start)))
         (tokens (list (make-token 'literal "a" 0 1) (make-token 'literal "b" 1 2)))
         (calls 0))
    (install-lr-runtime-direct-step! runtime
      (lambda (_production states semantic-values rest _end gotos)
        (set! calls (+ calls 1))
        (let (target (cdr (association-row-index-ref gotos (cadr states) 'first)))
          (vector-set! (car rest) 1 "c")
          (values target (cons target (cdr states)) semantic-values))))
    (check-exception (lr-parse/prepared runtime tokens) lr-rejection-condition?)
    (check calls => 1)))

(def (production-spec productions)
  ;; Isolate production admission with an already accepting table graph.
  (list (cons 'productions productions)
        (cons 'actions (vector '(((terminal eof) accept)))) (cons 'gotos (vector '()))
        (cons 'case-insensitive? #f)))

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
                      (iota width)))
         ;; This tree is the declaration's semantic oracle, independent of
         ;; either LR executor and its prepared action representation.
         (expected-root
          (make-recognition-node 'SourceFile 0 width
            (map (lambda (token index)
                   (if decorated?
                     (make-recognition-child 'outer
                       (make-recognition-node 'Renamed index (+ index 1)
                         (list (make-recognition-child 'inner token))))
                     (make-recognition-child #f token)))
                 tokens (iota width)))))
    (let-values (((reference rest receipt) (lr-parse/prepared/receipt runtime tokens)))
      (check rest => '())
      (let* ((digest (string-append "sha256:" (make-string 64 #\0)))
             (expected (make-success-parse-artifact digest source tokens expected-root false)))
        (check (make-success-parse-artifact digest source tokens reference false) => expected)
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
    (test-case "owned failure frontiers remain immutable across later requests and resumption"
      (let* ((runtime (lr-prepare (compile-lr-spec
                                   '((start (alias Source (sequence (literal "a") (literal "b"))))) 'start)))
             (a (make-token 'literal "a" 0 1)) (b (make-token 'literal "b" 1 2))
             (wrong (make-token 'literal "x" 1 2)) (good (list a b)))
        (let-values (((status frontier) (lr-checkpoint-frontier (lr-initial-checkpoint runtime (list a wrong)))))
          (check status => 'failure)
          (let* ((checkpoint (lr-failure-frontier-checkpoint frontier))
                 (before (list (lr-checkpoint-state checkpoint) (lr-checkpoint-deterministic-actions checkpoint)
                               (lr-checkpoint-deterministic-shifts checkpoint) (lr-checkpoint-remaining-token-count checkpoint)))
                 (expected (call-with-values (lambda () (lr-parse/prepared runtime good)) list)))
            (check (lr-checkpoint-deterministic-shifts checkpoint) => 1)
            (let-values (((root rest) (lr-checkpoint-resume (lr-checkpoint-rebind-suffix checkpoint good (list b)))))
              (check (list root rest) => expected))
            (check (list (lr-checkpoint-state checkpoint) (lr-checkpoint-deterministic-actions checkpoint)
                         (lr-checkpoint-deterministic-shifts checkpoint) (lr-checkpoint-remaining-token-count checkpoint))
                   => before)))))
    (test-case "token-only and arbitrary stack observers preserve their distinct contracts"
      (let* ((runtime (lr-prepare (compile-lr-spec
                                   '((start (alias Source (sequence (literal "a") (literal "b"))))) 'start)))
             (tokens (list (make-token 'literal "a" 0 1) (make-token 'literal "b" 1 2)))
             (expected (call-with-values (lambda () (lr-parse/prepared runtime tokens)) list)))
        (for-each
         (lambda (owned?)
           (let ((remaining tokens) (seen '()))
             (def (next-input _mode)
               (and (pair? remaining)
                    (let (token (car remaining)) (set! remaining (cdr remaining)) token)))
             (def observer
               (if owned?
                 (make-lr-token-observer (lambda (token) (set! seen (cons token seen))))
                 (lambda (token states values _actions _shifts)
                   (check (list? states) => #t) (check (list? values) => #t)
                   (set! seen (cons token seen)))))
             (let-values (((status actual) (lr-checkpoint-drive (lr-initial-checkpoint runtime '()) next-input observer)))
               (check status => 'accepted) (check actual => expected)
               (check (reverse seen) => tokens)))) '(#f #t))))
    (test-case "owned unary chains preserve decorated ASTs, boundaries and failure handoff"
      (for-each
       (lambda (depth)
         (def (name index) (string->symbol (string-append "unit" (number->string index))))
         (let* ((rules
                 (cons '(start (alias Root (sequence (reference unit0) (literal "b"))))
                   (map (lambda (index)
                          (let (child (if (= index depth) '(token word)
                                        (list 'reference (name (+ index 1)))))
                            (list (name index)
                              (if (even? index) child
                                (list 'alias (name index) (list 'field 'value child))))))
                        (iota (+ depth 1)))))
                (selections 0)
                (runtime (lr-runtime-with-action-selector
                           (lr-prepare (compile-lr-spec rules 'start))
                           (lambda (rows index casefold? layout?)
                             (let (select (reference-lr-action-selector rows index casefold? layout?))
                               (lambda (state tokens)
                                 (set! selections (+ selections 1))
                                 (select state tokens)))))))
           (for-each
            (lambda (offset)
              (let* ((tokens (list (make-token 'word "a" offset (+ offset 1))
                                   (make-token 'literal "b" (+ offset 1) (+ offset 2))))
                     (expected (call-with-values
                                (lambda () (lr-parse/prepared/receipt runtime tokens)) list))
                     (owned (begin (set! selections 0)
                              (call-with-values (lambda () (lr-parse/prepared runtime tokens)) list)))
                     (owned-selections selections)
                     (persistent
                      (begin (set! selections 0)
                       (parameterize ((current-lr-recognition-observer (lambda (_) (void))))
                        (call-with-values (lambda () (lr-parse/prepared runtime tokens)) list)))))
                (check owned => (list (car expected) (cadr expected)))
                (check persistent => owned)
                (check selections => owned-selections)
                (check-exception
                 (lr-parse/prepared runtime
                   (list (car tokens) (make-token 'literal "c" (+ offset 1) (+ offset 2))))
                 lr-rejection-condition?))) '(0 37)))) '(1 16 96)))
    (test-case "constructor bounds continuation retains scalar endpoints and evaluates once"
      (for-each
       (lambda (value)
         (let ((evaluations 0) (current 'caller-binding) (delta 999))
           (check
            (with-recognition-value-bounds
              (begin (set! evaluations (+ evaluations 1)) value)
              (start end) (list start end current delta))
            => (list (recognition-value-start value) (recognition-value-end value)
                     'caller-binding 999))
           (check evaluations => 1)))
       (let ((token (make-token 'word "a" 5 6))
             (node (make-recognition-node 'Node 9 13 '())))
         (list token node (make-recognition-fragment 17 19 '())
               (event-program-node-value 'Event 21 23 #f)
               (event-program-fragment-value 25 26 #f)
               (relocate-recognition-value token 37)
               (relocate-recognition-value (relocate-recognition-value node 11) -4))))
      (for-each
       (lambda (value)
         (check-exception (with-recognition-value-bounds value (start end) (list start end)) true))
       (list #f 0 '() '#())))
    (test-case "action boundaries precede normalization without changing child ownership"
      (let* ((a (list (make-recognition-child #f (make-token 'word "a" 5 6))))
             (b (list (make-recognition-child 'inner (make-token 'word "b" 9 10))))
             (branch (recognition-sequence-append a b))
             (deep (foldl recognition-sequence-append '() (make-list 2048 a))))
        (check (eq? a (recognition-node-children
                       (recognition-child-value (car (recognition-action-alias 'Node a 0))))) => #t)
        (for-each
         (lambda (children)
           (for-each
            (lambda (offset)
              (let ((ready (recognition-sequence-for-action children)) (calls '()))
                (def (construct start end values)
                  (check (list start end)
                         => (if (null? ready) (list offset offset)
                              (list (recognition-value-start (recognition-child-value (car ready)))
                                    (recognition-value-end (recognition-child-value (last ready))))))
                  (set! calls (cons (list start end values) calls))
                  (make-recognition-fragment start end values))
                (check (recognition-action-alias 'Node children offset)
                       => (recognition-children-alias 'Node ready offset))
                (check (recognition-action-field 'outer children offset construct)
                       => (recognition-children-field 'outer ready offset construct))
                (when (pair? calls)
                  (check (car calls) => (cadr calls)))
                (check ((make-recognition-alias-kernel 'Node) children offset construct)
                       => (recognition-action-alias 'Node children offset))
                (check ((make-recognition-field-kernel 'outer) children offset construct)
                       => (recognition-action-field 'outer children offset construct))
                (when (pair? calls)
                  (check (car calls) => (cadr calls)))
                (check (recognition-sequence-for-action children) => ready)))
            '(0 37)))
         (list '() a b branch deep
               (recognition-sequence-relocate a 11)
               (recognition-sequence-relocate b -3)
               (recognition-sequence-relocate branch 7)
               (recognition-sequence-append (recognition-sequence-relocate a 11)
                                            (recognition-sequence-relocate b -3))))))
    (test-case "combined recognition endpoints retain checked value admission"
      (for-each
       (lambda (value)
         (check (call-with-values (lambda () (recognition-value-bounds value)) list)
                => (list (recognition-value-start value) (recognition-value-end value))))
       (list (make-token 'word "a" 5 6)
             (make-recognition-node 'Node 7 9 '())
             (make-recognition-fragment 11 11 '())
             (event-program-node-value 'Event 17 19 #f)
             (event-program-fragment-value 21 22 #f)
             (relocate-recognition-value (make-recognition-node 'Moved 31 33 '()) -7)))
      (for-each
       (lambda (value) (check-exception (recognition-value-bounds value) true))
       (list #f #() (make-token 'word "a" 6 5))))
    (test-case "semantic replay retains decorated operands and request boundaries"
      (let* ((spec (compile-lr-spec
                    '((source-file
                       (alias SourceFile
                         (sequence (alias Before (empty))
                                   (field outer (alias Word (field inner (token word))))
                                   (alias After (empty)))))) 'source-file))
             (runtime (lr-prepare spec)))
        (for-each
         (lambda (offset)
           (let* ((tokens (list (make-token 'word "a" offset (+ offset 1))))
                  (replay (prepare-lr-semantic-replay runtime spec tokens)))
             (check (positive? (lr-semantic-replay-count replay)) => #t)
             (let (work (profile-lr-semantic-actions replay))
               (check (cdr (assq 'fieldActions work)) => 2)
               (check (cdr (assq 'aliasActions work)) => 4)
               (check (cdr (assq 'widestActionInput work)) => 3))
             (let-values (((expected rest) (lr-parse/prepared runtime tokens)))
               (check rest => '())
               (check (replay-lr-semantics replay) => expected)
               (check (replay-lr-semantic-control replay) => expected)
               (check (replay-lr-semantics replay) => expected))))
         '(0 17 3))))
    (test-case-add! (TestCase "installed reduction callbacks retain fresh lookahead queries"
                      installed-step-lookahead-case))
    (test-case "prepared nullable aliases receive each request's actual boundaries"
      (let (runtime
            (lr-prepare
             (compile-lr-spec
              '((source-file
                 (alias SourceFile
                   (sequence (alias Before (empty)) (token word)
                             (alias After (empty)))))) 'source-file)))
        (for-each
         (lambda (offset)
           (let* ((end (+ offset 1)) (token (make-token 'word "a" offset end))
                  (expected-root
                   (make-recognition-node 'SourceFile offset end
                     (list
                      (make-recognition-child #f (make-recognition-node 'Before offset offset '()))
                      (make-recognition-child #f token)
                      (make-recognition-child #f (make-recognition-node 'After end end '())))))
                  (source (string-append (make-string offset #\space) "a"))
                  (digest (string-append "sha256:" (make-string 64 #\0)))
                  (publish (lambda (root)
                             (make-success-parse-artifact digest source (list token) root false)))
                  (expected (publish expected-root)))
             (let-values (((root rest) (lr-parse/prepared runtime (list token))))
               (check rest => '())
               (check (publish root) => expected))
             (let-values (((root rest receipt) (lr-parse/prepared/receipt runtime (list token))))
               (check rest => '())
               (check (publish root) => expected))))
         '(0 17 3))))
    (test-case "canonical productions validate numbering, width and scalar metadata"
      (for-each
       (lambda (production)
         (let (productions (list production))
           (check (eq? productions (validate-production-semantics productions)) => #t)
           (check (not (not (lr-prepare (production-spec productions)))) => #t)))
       '((0 root ((terminal token word)) pass #f)
         (0 root () concat (left 10))
         (0 root () layout-end #f)
         (0 root () (layout-end "END" ")") (dynamic -1))
         (0 root ((nonterminal root) (terminal literal "+")) concat (none 0))
         (0 root ((terminal layout-start "|>") (terminal layout-next "|>")) concat (right 2)))))
    (test-case "malformed canonical productions fail before backend preparation"
      (for-each
       (lambda (productions)
         (for-each
          (lambda (events?)
            (parameterize ((current-lr-event-program-enabled? events?))
              (check-exception (lr-prepare (production-spec productions))
                               (lambda (condition)
                                 (string-prefix? "invalid canonical LR" (error-message condition))))))
          '(#f #t)))
       '(#f #() ((0 root () concat #f) . invalid)
         ((1 root () concat #f))
         ((0 root () concat #f) (0 other () concat #f))
         ((0 "root" () concat #f)) ((0 root () concat #f extra))
         ((0 root () pass #f))
         ((0 root ((terminal token word) (terminal token word)) pass #f))
         ((0 root ((terminal token word)) layout-end #f))
         ((0 root () (layout-end) #f)) ((0 root () (layout-end "") #f))
         ((0 root () (layout-end END) #f)) ((0 root () (layout-end . "END") #f))
         ((0 root () concat (callback 1))) ((0 root () concat (left "10")))
         ((0 root () concat (left 1 extra))) ((0 root () concat (dynamic . 1)))
         ((0 root ((terminal word)) concat #f))
         ((0 root ((terminal literal "")) concat #f))
         ((0 root ((marked (terminal token word))) concat #f))
         ((0 root ((marked (terminal token word) () extra)) concat #f))
         ((0 root ((nonterminal "item")) concat #f)))))
    (test-case "reference admission resolves complete owners without pruning dead cycles"
      (def productions
        '((0 root ((marked (nonterminal later) ((field value)))) pass #f)
          (1 later ((terminal token word)) pass #f)
          (2 unused ((nonterminal unused)) pass #f)))
      (check (eq? productions (validate-production-semantics productions)) => #t)
      (for-each
       (lambda (events?)
         (parameterize ((current-lr-event-program-enabled? events?))
           (check (not (not (lr-prepare (production-spec productions)))) => #t)
           (for-each
            (lambda (invalid)
              (check-exception
               (lr-prepare (production-spec invalid))
               (lambda (condition)
                 (equal? (error-message condition) "invalid canonical LR unresolved reference"))))
            '(((0 root ((nonterminal missing)) pass #f))
              ((0 root ((terminal token word)) pass #f)
               (1 unused ((marked (nonterminal missing) ((alias Item)))) pass #f))))))
       '(#f #t)))
    (test-case "all LR construction routes reject undeclared references including unused rules"
      (for-each
       (lambda (construction)
         (for-each
          (lambda (rules)
            (check-exception
             (compile-lr-spec rules 'root 'reject #f construction)
             (lambda (condition)
               (equal? (error-message condition) "invalid canonical LR unresolved reference"))))
          '(((root (reference missing)))
            ((root (token word)) (unused (field value (reference missing)))))))
       '(lalr canonical-lr1 partitioned-lr1 follow-partition-lr1 lalr-then-follow)))
    (test-case "standalone lowering cannot publish invalid canonical operands"
      (for-each
       (lambda (construction)
         (check-exception
          (compile-lr-spec '((root (field "name" (token word)))) 'root 'reject #f construction)
          (lambda (condition) (equal? (error-message condition) "invalid LR operand actions")))
         (check-exception
          (compile-lr-spec '((root (layout-end ""))) 'root 'reject #f construction)
          (lambda (condition) (equal? (error-message condition) "invalid canonical LR production"))))
       '(lalr canonical-lr1 partitioned-lr1 follow-partition-lr1 lalr-then-follow)))
    (test-case "canonical action admission requires proper symbolic declarations"
      (for-each (lambda (actions) (check (operand-actions-valid? actions) => #t))
                '(() ((field left)) ((alias Item)) ((alias Item) (field left))))
      (for-each (lambda (actions) (check (operand-actions-valid? actions) => #f))
                '(#f #() ((field)) ((alias Item extra)) ((callback Item))
                  ((field #f)) ((field "left")) ((alias 1)) ((alias (Item)))
                  ((field . left)) ((field left) . invalid))))
    (test-case "prepared runtimes reject unsupported actions before execution"
      (def (prepare action operand-actions)
        (lr-prepare
         (production-spec
          (list (list 0 'source-file
                      (list (list 'marked '(terminal token word) operand-actions))
                      action #f)))))
      (check-exception (prepare 'callback '()) true)
      (check-exception (prepare 'concat '((callback user))) true)
      (check-exception (prepare 'concat '((field))) true)
      (check-exception (prepare 'concat '((alias Name extra))) true)
      (check-exception (prepare 'concat '((field Name) . invalid)) true)
      (for-each
       (lambda (events?)
         (parameterize ((current-lr-event-program-enabled? events?))
           (for-each (lambda (actions)
                       (check-exception (prepare 'concat actions)
                                        (lambda (condition)
                                          (and (equal? (error-message condition) "invalid LR operand actions")
                                               (equal? (error-irritants condition) (list actions))))))
                     '(((field #f)) ((field "left")) ((alias 1)) ((alias (Item)))))))
       '(#f #t))
      (check (not (not (prepare 'concat '((field inner) (alias Renamed))))) => #t))
    (test-case "unselected productions cannot bypass semantic name admission"
      ;; Immediate acceptance does not select either production for reduction.
      ;; Preparation still owns admission of the whole canonical production set.
      (for-each
       (lambda (events?)
         (parameterize ((current-lr-event-program-enabled? events?))
           (check-exception
            (lr-prepare
             (production-spec
              '((0 source-file ((terminal token word)) pass #f)
                (1 unused ((marked (terminal token word) ((alias "Item")))) concat #f))))
            (lambda (condition)
              (equal? (error-message condition) "invalid LR operand actions")))))
       '(#f #t)))
    (test-case "empty, unary and wide identity operands agree with GLR"
      (for-each (lambda (width) (check-stack-family width #f)) '(0 1 2 8 32 128)))
    (test-case "wide ordered field and alias chains agree with GLR"
      (for-each (lambda (width) (check-stack-family width #t)) '(1 2 8 32 128)))))
