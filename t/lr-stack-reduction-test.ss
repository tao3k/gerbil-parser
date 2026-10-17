(import :gerbil-parser/language-test-support)
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


;;; Installed procedures are arbitrary callbacks. They may change the current
;;; lookahead during a reduction, so their executor must not cache its class.
(defparser-check (installed-step-lookahead-case)
  (sequential ((runtime (lr-prepare
                   (compile-lr-spec '((start (sequence (reference first) (literal "b")))
                                      (first (literal "a"))) 'start)))
         (tokens (list (make-token 'literal "a" 0 1) (make-token 'literal "b" 1 2)))
         (calls 0))
    (act (install-lr-runtime-direct-step! runtime
      (lambda (_production states semantic-values rest _end gotos)
        (set! calls (+ calls 1))
        (let (target (cdr (association-row-index-ref gotos (cadr states) 'first)))
          (vector-set! (car rest) 1 "c")
          (values target (cons target (cdr states)) semantic-values)))))
    (raises (lr-parse/prepared runtime tokens) lr-rejection-condition?)
    (equal calls  1)))

(def (production-spec productions)
  ;; Isolate production admission with an already accepting table graph.
  (list (cons 'productions productions)
        (cons 'actions (vector '(((terminal eof) accept)))) (cons 'gotos (vector '()))
        (cons 'case-insensitive? #f)))

(defparser-check (check-stack-family width decorated?)
  (sequential ((operand (if decorated?
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
    (scope-values (((reference rest receipt) (lr-parse/prepared/receipt runtime tokens)))
      (equal rest  '())
      (sequential ((digest (string-append "sha256:" (make-string 64 #\0)))
             (expected (make-success-parse-artifact digest source tokens expected-root false)))
        (equal (make-success-parse-artifact digest source tokens reference false)  expected)
        (equal (parse-artifact-valid? expected)  #t)
        (equal (parse-artifact-roundtrip expected)  source)
        (each (events?) ('(#f #t))
           (parameters ((current-lr-event-program-enabled? events?))
             (scope ((selected (lr-runtime-for-current-semantic-backend runtime)))
               (scope-values (((root rest) (lr-parse/prepared selected tokens)))
                 (equal rest  '())
                 (equal (make-success-parse-artifact digest source tokens root false)
                         expected)))))))))

(deflanguage-parser-tests lr-stack-reduction-test "shared LR stack reduction"
  (engine)
    (scenario "owned failure frontiers remain immutable across later requests and resumption"
      (sequential ((runtime (lr-prepare (compile-lr-spec
                                   '((start (alias Source (sequence (literal "a") (literal "b"))))) 'start)))
             (a (make-token 'literal "a" 0 1)) (b (make-token 'literal "b" 1 2))
             (wrong (make-token 'literal "x" 1 2)) (good (list a b)))
        (scope-values (((status frontier) (lr-checkpoint-frontier (lr-initial-checkpoint runtime (list a wrong)))))
          (equal status  'failure)
          (sequential ((checkpoint (lr-failure-frontier-checkpoint frontier))
                 (before (list (lr-checkpoint-state checkpoint) (lr-checkpoint-deterministic-actions checkpoint)
                               (lr-checkpoint-deterministic-shifts checkpoint) (lr-checkpoint-remaining-token-count checkpoint)))
                 (expected (call-with-values (lambda () (lr-parse/prepared runtime good)) list)))
            (equal (lr-checkpoint-deterministic-shifts checkpoint)  1)
            (scope-values (((root rest) (lr-checkpoint-resume (lr-checkpoint-rebind-suffix checkpoint good (list b)))))
              (equal (list root rest)  expected))
            (equal (list (lr-checkpoint-state checkpoint) (lr-checkpoint-deterministic-actions checkpoint)
                         (lr-checkpoint-deterministic-shifts checkpoint) (lr-checkpoint-remaining-token-count checkpoint))
                    before)))))
    (scenario "token-only and arbitrary stack observers preserve their distinct contracts"
      (sequential ((runtime (lr-prepare (compile-lr-spec
                                   '((start (alias Source (sequence (literal "a") (literal "b"))))) 'start)))
             (tokens (list (make-token 'literal "a" 0 1) (make-token 'literal "b" 1 2)))
             (expected (call-with-values (lambda () (lr-parse/prepared runtime tokens)) list)))
        (each (owned?) ('(#f #t))
           (scope ((remaining tokens) (seen '()))
(locals ((next-input (lambda ( _mode)
               (and (pair? remaining)
                    (let (token (car remaining)) (set! remaining (cdr remaining)) token))))
(observer (if owned?
                 (make-lr-token-observer (lambda (token) (set! seen (cons token seen))))
                 (parser-check-procedure (parser-check (token states values _actions _shifts)
                   (equal (list? states)  #t) (equal (list? values)  #t)
                   (act (set! seen (cons token seen))))))))


             (scope-values (((status actual) (lr-checkpoint-drive (lr-initial-checkpoint runtime '()) next-input observer)))
               (equal status  'accepted) (equal actual  expected)
               (equal (reverse seen)  tokens)))))))
    (scenario "owned unary chains preserve decorated ASTs, boundaries and failure handoff"
      (each (depth) ('(1 16 96))
(locals ((name (lambda ( index) (string->symbol (string-append "unit" (number->string index))))))

         (sequential ((rules
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
           (each (offset) ('(0 37))
              (sequential ((tokens (list (make-token 'word "a" offset (+ offset 1))
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
                (equal owned  (list (car expected) (cadr expected)))
                (equal persistent  owned)
                (equal selections  owned-selections)
                (raises
                 (lr-parse/prepared runtime
                   (list (car tokens) (make-token 'literal "c" (+ offset 1) (+ offset 2))))
                 lr-rejection-condition?)))))))
    (scenario "constructor bounds continuation retains scalar endpoints and evaluates once"
      (each (value) ((let ((token (make-token 'word "a" 5 6))
             (node (make-recognition-node 'Node 9 13 '())))
         (list token node (make-recognition-fragment 17 19 '())
               (event-program-node-value 'Event 21 23 #f)
               (event-program-fragment-value 25 26 #f)
               (relocate-recognition-value token 37)
               (relocate-recognition-value (relocate-recognition-value node 11) -4))))
         (scope ((evaluations 0) (current 'caller-binding) (delta 999))
           (equal
            (with-recognition-value-bounds
              (begin (set! evaluations (+ evaluations 1)) value)
              (start end) (list start end current delta))
             (list (recognition-value-start value) (recognition-value-end value)
                     'caller-binding 999))
           (equal evaluations  1)))
      (each (value) ((list #f 0 '() '#()))
         (raises (with-recognition-value-bounds value (start end) (list start end)) true)))
    (scenario "action boundaries precede normalization without changing child ownership"
      (sequential ((a (list (make-recognition-child #f (make-token 'word "a" 5 6))))
             (b (list (make-recognition-child 'inner (make-token 'word "b" 9 10))))
             (branch (recognition-sequence-append a b))
             (deep (foldl recognition-sequence-append '() (make-list 2048 a))))
        (equal (eq? a (recognition-node-children
                       (recognition-child-value (car (recognition-action-alias 'Node a 0)))))  #t)
        (each (children) ((list '() a b branch deep
               (recognition-sequence-relocate a 11)
               (recognition-sequence-relocate b -3)
               (recognition-sequence-relocate branch 7)
               (recognition-sequence-append (recognition-sequence-relocate a 11)
                                            (recognition-sequence-relocate b -3))))
           (each (offset) ('(0 37))
              (scope ((ready (recognition-sequence-for-action children)) (calls '()))
(locals ((construct (parser-check ( start end values)
                  (equal (list start end)
                          (if (null? ready) (list offset offset)
                              (list (recognition-value-start (recognition-child-value (car ready)))
                                    (recognition-value-end (recognition-child-value (last ready))))))
                  (act (set! calls (cons (list start end values) calls)))
                  (act (make-recognition-fragment start end values)))))

                (equal (recognition-action-alias 'Node children offset)
                        (recognition-children-alias 'Node ready offset))
                (equal (recognition-action-field 'outer children offset (parser-check-procedure construct))
                        (recognition-children-field 'outer ready offset (parser-check-procedure construct)))
                (when (pair? calls)
                  (equal (car calls)  (cadr calls)))
                (equal ((make-recognition-alias-kernel 'Node) children offset (parser-check-procedure construct))
                        (recognition-action-alias 'Node children offset))
                (equal ((make-recognition-field-kernel 'outer) children offset (parser-check-procedure construct))
                        (recognition-action-field 'outer children offset (parser-check-procedure construct)))
                (when (pair? calls)
                  (equal (car calls)  (cadr calls)))
                (equal (recognition-sequence-for-action children)  ready)))))))
    (scenario "combined recognition endpoints retain checked value admission"
      (each (value) ((list (make-token 'word "a" 5 6)
             (make-recognition-node 'Node 7 9 '())
             (make-recognition-fragment 11 11 '())
             (event-program-node-value 'Event 17 19 #f)
             (event-program-fragment-value 21 22 #f)
             (relocate-recognition-value (make-recognition-node 'Moved 31 33 '()) -7)))
         (equal (call-with-values (lambda () (recognition-value-bounds value)) list)
                 (list (recognition-value-start value) (recognition-value-end value))))
      (each (value) ((list #f #() (make-token 'word "a" 6 5))) (raises (recognition-value-bounds value) true)))
    (scenario "semantic replay retains decorated operands and request boundaries"
      (sequential ((spec (compile-lr-spec
                    '((source-file
                       (alias SourceFile
                         (sequence (alias Before (empty))
                                   (field outer (alias Word (field inner (token word))))
                                   (alias After (empty)))))) 'source-file))
             (runtime (lr-prepare spec)))
        (each (offset) ('(0 17 3))
           (sequential ((tokens (list (make-token 'word "a" offset (+ offset 1))))
                  (replay (prepare-lr-semantic-replay runtime spec tokens)))
             (equal (positive? (lr-semantic-replay-count replay))  #t)
             (scope ((work (profile-lr-semantic-actions replay)))
               (equal (cdr (assq 'fieldActions work))  2)
               (equal (cdr (assq 'aliasActions work))  4)
               (equal (cdr (assq 'widestActionInput work))  3))
             (scope-values (((expected rest) (lr-parse/prepared runtime tokens)))
               (equal rest  '())
               (equal (replay-lr-semantics replay)  expected)
               (equal (replay-lr-semantic-control replay)  expected)
               (equal (replay-lr-semantics replay)  expected))))))
    (scenario "installed reduction callbacks retain fresh lookahead queries" (verify installed-step-lookahead-case))
    (scenario "prepared nullable aliases receive each request's actual boundaries"
      (scope ((runtime
            (lr-prepare
             (compile-lr-spec
              '((source-file
                 (alias SourceFile
                   (sequence (alias Before (empty)) (token word)
                             (alias After (empty)))))) 'source-file))))
        (each (offset) ('(0 17 3))
           (sequential ((end (+ offset 1)) (token (make-token 'word "a" offset end))
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
             (scope-values (((root rest) (lr-parse/prepared runtime (list token))))
               (equal rest  '())
               (equal (publish root)  expected))
             (scope-values (((root rest receipt) (lr-parse/prepared/receipt runtime (list token))))
               (equal rest  '())
               (equal (publish root)  expected))))))
    (scenario "canonical productions validate numbering, width and scalar metadata"
      (each (production) ('((0 root ((terminal token word)) pass #f)
         (0 root () concat (left 10))
         (0 root () layout-end #f)
         (0 root () (layout-end "END" ")") (dynamic -1))
         (0 root ((nonterminal root) (terminal literal "+")) concat (none 0))
         (0 root ((terminal layout-start "|>") (terminal layout-next "|>")) concat (right 2))))
         (scope ((productions (list production)))
           (equal (eq? productions (validate-production-semantics productions))  #t)
           (equal (not (not (lr-prepare (production-spec productions))))  #t))))
    (scenario "malformed canonical productions fail before backend preparation"
      (each (productions) ('(#f #() ((0 root () concat #f) . invalid)
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
         ((0 root ((nonterminal "item")) concat #f))))
         (each (events?) ('(#f #t))
            (parameters ((current-lr-event-program-enabled? events?))
              (raises (lr-prepare (production-spec productions))
                               (lambda (condition)
                                 (string-prefix? "invalid canonical LR" (error-message condition))))))))
    (scenario "reference admission resolves complete owners without pruning dead cycles"
(locals ((productions '((0 root ((marked (nonterminal later) ((field value)))) pass #f)
          (1 later ((terminal token word)) pass #f)
          (2 unused ((nonterminal unused)) pass #f))))

      (equal (eq? productions (validate-production-semantics productions))  #t)
      (each (events?) ('(#f #t))
         (parameters ((current-lr-event-program-enabled? events?))
           (equal (not (not (lr-prepare (production-spec productions))))  #t)
           (each (invalid) ('(((0 root ((nonterminal missing)) pass #f))
              ((0 root ((terminal token word)) pass #f)
               (1 unused ((marked (nonterminal missing) ((alias Item)))) pass #f))))
              (raises
               (lr-prepare (production-spec invalid))
               (lambda (condition)
                 (equal? (error-message condition) "invalid canonical LR unresolved reference"))))))))
    (scenario "all LR construction routes reject undeclared references including unused rules"
      (each (construction) ('(lalr canonical-lr1 partitioned-lr1 follow-partition-lr1 lalr-then-follow))
         (each (rules) ('(((root (reference missing)))
            ((root (token word)) (unused (field value (reference missing))))))
            (raises
             (compile-lr-spec rules 'root 'reject #f construction)
             (lambda (condition)
               (equal? (error-message condition) "invalid canonical LR unresolved reference"))))))
    (scenario "standalone lowering cannot publish invalid canonical operands"
      (each (construction) ('(lalr canonical-lr1 partitioned-lr1 follow-partition-lr1 lalr-then-follow))
         (raises
          (compile-lr-spec '((root (field "name" (token word)))) 'root 'reject #f construction)
          (lambda (condition) (equal? (error-message condition) "invalid LR operand actions")))
         (raises
          (compile-lr-spec '((root (layout-end ""))) 'root 'reject #f construction)
          (lambda (condition) (equal? (error-message condition) "invalid canonical LR production")))))
    (scenario "canonical action admission requires proper symbolic declarations"
      (each (actions) ('(() ((field left)) ((alias Item)) ((alias Item) (field left)))) (equal (operand-actions-valid? actions)  #t))
      (each (actions) ('(#f #() ((field)) ((alias Item extra)) ((callback Item))
                  ((field #f)) ((field "left")) ((alias 1)) ((alias (Item)))
                  ((field . left)) ((field left) . invalid))) (equal (operand-actions-valid? actions)  #f)))
    (scenario "prepared runtimes reject unsupported actions before execution"
(locals ((prepare (lambda ( action operand-actions)
        (lr-prepare
         (production-spec
          (list (list 0 'source-file
                      (list (list 'marked '(terminal token word) operand-actions))
                      action #f)))))))

      (raises (prepare 'callback '()) true)
      (raises (prepare 'concat '((callback user))) true)
      (raises (prepare 'concat '((field))) true)
      (raises (prepare 'concat '((alias Name extra))) true)
      (raises (prepare 'concat '((field Name) . invalid)) true)
      (each (events?) ('(#f #t))
         (parameters ((current-lr-event-program-enabled? events?))
           (each (actions) ('(((field #f)) ((field "left")) ((alias 1)) ((alias (Item)))))
                       (raises (prepare 'concat actions)
                                        (lambda (condition)
                                          (and (equal? (error-message condition) "invalid LR operand actions")
                                               (equal? (error-irritants condition) (list actions))))))))
      (equal (not (not (prepare 'concat '((field inner) (alias Renamed)))))  #t)))
    (scenario "unselected productions cannot bypass semantic name admission"
      ;; Immediate acceptance does not select either production for reduction.
      ;; Preparation still owns admission of the whole canonical production set.
      (each (events?) ('(#f #t))
         (parameters ((current-lr-event-program-enabled? events?))
           (raises
            (lr-prepare
             (production-spec
              '((0 source-file ((terminal token word)) pass #f)
                (1 unused ((marked (terminal token word) ((alias "Item")))) concat #f))))
            (lambda (condition)
              (equal? (error-message condition) "invalid LR operand actions"))))))
    (scenario "empty, unary and wide identity operands agree with GLR"
      (each (width) ('(0 1 2 8 32 128)) (verify check-stack-family width #f)))
    (scenario "wide ordered field and alias chains agree with GLR"
      (each (width) ('(1 2 8 32 128)) (verify check-stack-family width #t))))
