;;; Shared deterministic stack reduction versus independent materialized GLR.
(import :std/test
        (only-in :std/test/base TestCase test-case-add!)
        (only-in :gerbil-parser/src/compiler/lr-compiler compile-lr-spec)
        (only-in :gerbil-parser/src/compiler/lr operand-actions-valid? validate-production-semantics)
        (only-in :gerbil-parser/src/runtime/token make-token)
        (only-in :gerbil-parser/src/runtime/recognition
                 make-recognition-child make-recognition-node)
        (only-in :gerbil-parser/src/runtime/lr-parser
                 lr-prepare lr-parse/prepared lr-parse/prepared/receipt
                 install-lr-runtime-direct-step! lr-rejection-condition?
                 current-lr-event-program-enabled? lr-runtime-for-current-semantic-backend)
        (only-in :gerbil-parser/src/runtime/funcs association-row-index-ref)
        (only-in :gerbil-parser/src/runtime/artifact
                 make-success-parse-artifact parse-artifact-valid? parse-artifact-roundtrip))
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
