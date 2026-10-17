;;; Engine admission controls for the public language-test DSL.
(import (only-in :gerbil-parser/languages/arithmetic/parser-test arithmetic-test-language)
        (only-in :gerbil-parser/languages/hcl/parser-test hcl-test-language)
        (only-in :gerbil-parser/languages/arithmetic/parser-test arithmetic-basic-fixture)
        :std/test
        (for-syntax (only-in :gerbil/expander core-expand))
        :gerbil-parser/language-test-support
        (only-in :clan/poo/object .cc .ref)
)

(defsyntax (invalid-test-declarations stx)
  (def (rejects form)
    (with-catch (lambda (_) #t) (lambda () (core-expand form) #f)))
  (datum->syntax #'invalid-test-declarations
    (list 'quote
      (list
        (rejects #'(deflanguage-parser-tests bad "bad" (engine) (scenario "bad" (continue missing 1))))
        (rejects #'(deflanguage-parser-tests bad "bad" (engine (exclusive 1)) (property "bad" (bindings) (equal 1 1))))
        (rejects #'(deflanguage-parser-tests bad "bad" (engine (affinity 1)) (property "bad" (bindings) (equal 1 1))))
        (rejects #'(deflanguage-parser-tests bad "bad" (engine (exclusive #t) (exclusive #f)) (property "bad" (bindings) (equal 1 1))))
        (rejects #'(deflanguage-parser-tests bad "bad" (engine (workers 99)) (property "bad" (bindings) (equal 1 1))))
        (rejects #'(deflanguage-parser-tests bad "bad" (engine) (scenario "bad" (inline-callback (subject) (x) (equal x 1)))))
        (rejects #'(deflanguage-parser-tests bad "bad" (engine) (scenario "bad" (iterate loop ((x 0)) (continue loop 1 2) (equal x 0)))))
        (rejects #'(deflanguage-parser-tests bad "bad" (engine) (scenario "bad" (iterate loop ((x 0)) (continue absent 1) (equal x 0)))))
        (rejects #'(deflanguage-parser-tests bad "bad" (engine) (scenario "bad" (protect (body (equal 1 1)) (cleanup)))))
        (rejects #'(deflanguage-parser-tests bad "bad" (engine) (scenario "bad" (wind (before (act (void))) (body (equal 1 1)) (wrong (act (void)))))))
        (rejects #'(deflanguage-parser-tests bad "bad" (engine) (scenario "bad" (choose x ((1) (begin #t))))))
        (rejects #'(deflanguage-parser-tests bad "bad" (engine) (profiled "bad" (begin #t))))
        (rejects #'(deflanguage-parser-tests bad "bad" (engine) (scenario "bad" (verify))))
        (rejects #'(deflanguage-parser-tests bad "bad" (engine) (scenario "bad" (act (void)))))
        (rejects #'(deflanguage-parser-tests bad "bad" (engine) (scenario "bad" (equal #t))))
        (rejects #'(deflanguage-parser-tests bad "bad" (engine) (scenario "bad" (raises (error "x")))))
        (rejects #'(deflanguage-parser-tests bad "bad" (engine) (scenario "bad" (scope ((x)) (equal x 1)))))
        (rejects #'(deflanguage-parser-tests bad "bad" (engine) (scenario "bad" (scope ((x 1))))))
        (rejects #'(deflanguage-parser-tests bad "bad" (engine) (scenario "bad" (scope-values (((x x) (values 1 2))) (equal x 1)))))
        (rejects #'(deflanguage-parser-tests bad "bad" (engine) (scenario "bad" (each () () (equal 1 1)))))
        (rejects #'(deflanguage-parser-tests bad "bad" (engine) (scenario "bad" (each (x y) ('(1)) (equal x y)))))
        (rejects #'(deflanguage-parser-tests bad "bad" (engine) (scenario "bad" (each (x x) ('(1) '(2)) (equal x x)))))
        (rejects #'(deflanguage-parser-tests bad "bad" (engine) (scenario "bad" (when #t (begin #t)))))
        (rejects #'(deflanguage-parser-tests bad "bad" (engine) (scenario "bad" (branch #t (equal 1 1)))))
        (rejects #'(deflanguage-parser-tests bad "bad" (engine) (scenario "bad" (act (display "x")) (equal 1 1))))
        (rejects #'(deflanguage-parser-tests bad "bad" (engine) (scenario "bad" (act (begin (void))) (equal 1 1))))
        (rejects #'(deflanguage-parser-tests bad "bad" (engine) (scenario "bad" (act (let ((x #t)) x)) (equal 1 1))))
        (rejects #'(deflanguage-parser-tests bad "bad" (engine) (scenario "bad" (act (map (lambda (x) (check x => #t)) '(#t))) (equal 1 1))))
        (rejects #'(deflanguage-parser-tests bad "bad" (engine) (property "bad" (bindings) (equal (begin (check #t => #t) #t) #t))))
        (rejects #'(deflanguage-parser-tests bad "bad" (engine) (property "bad" (bindings) (equal (quasiquote ((unquote (display "bad")))) #t))))
        (rejects #'(deflanguage-parser-tests bad "bad" (engine) (property "bad" (bindings) (equal (quasisyntax ((unsyntax (displayln "bad")))) #t))))
        (rejects #'(deflanguage-parser-tests bad "bad" (engine) (accepted "bad" "1")))
        (rejects #'(deflanguage-parser-tests bad "bad" (engine) (test-case "bad" #t)))
        (rejects #'(deflanguage-parser-tests bad "bad" (engine) (property "bad" (bindings) (equal (display "bad") #t))))
        (rejects #'(deflanguage-parser-tests bad "bad" (engine) (property "bad" (bindings (x (lambda () (displayln "bad")))) (equal x #t))))
        (rejects #'(deflanguage-parser-tests bad "bad" (engine) (matrix "bad" (setup) ((x '(1)) (x '(2))) (bindings) (equal x 1))))
        (rejects #'(deflanguage-parser-tests bad "bad" (engine) (matrix "bad" (setup) () (bindings) (equal 1 1))))
        (rejects #'(deflanguage-parser-tests bad "bad" (engine) (matrix "bad" (setup) ((x '(1))) (bindings) (begin #t))))
        (rejects #'(deflanguage-parser-tests bad "bad" (engine) (property "same" (bindings) (equal 1 1)) (property "same" (bindings) (equal 2 2))))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f)))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (arbitrary "bad" #t)))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (accepted "bad" "1" (nodes))))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (accepted "bad" "1" (unknown x))))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (accepted "bad" "1" (counts (Number -1)))))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (identity "bad" (unknown #t))))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (property "bad" (bindings))))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (property "bad" (bindings) (begin #t))))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (accepted "same" "1") (rejected "same" "2")))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (accepted 17 "1")))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (accepted "bad" "1" (subtree (node X (lexemes))))))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (accepted "bad" "1" (subtree (node X (unknown 1))))))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (accepted "bad" "1" (field-counts X value (-1)))))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (parser-ir "bad" #f (unknown 1))))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (bound-rules "bad" #f (rule (unknown 1)))))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (lr "bad" #f (initial-shifts))))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (lr-receipt "bad" #f "x" 0 (rest '()))))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (parallel "bad" 0 '("1"))))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (incremental "bad" "1" (replace "" "2") (schema "x"))))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (recovery-rejected "bad" "x")))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (installed "bad" arbitrary)))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (strategy-parity "bad" (sources "1") (routes unknown entry))))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (strategy-parity "bad" (sources "1") (routes entry entry))))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (strategy-parity "bad" (sources) (routes entry indexed))))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (strategy-parity "bad" (indexed 0 "x") (routes entry indexed))))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (strategy-parity "bad" (seeded -1 1 "x") (routes entry indexed))))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (strategy-parity "bad" (seeded 1 0 "x") (routes entry indexed))))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (strategy-parity "bad" (seeded 1 1 (character "")) (routes entry indexed))))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (strategy-parity "bad" (seeded 1 1 (integer 0 0)) (routes entry indexed))))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (strategy-parity "bad" (seeded 1 1 foreign) (routes entry indexed))))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (strategy-parity "bad" (seeded 1 1 (bind ((x "1") (x "2")) x)) (routes entry indexed))))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (strategy-parity "bad" (sources "1") (routes entry indexed) (accepted "true"))))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (strategy-parity "bad" (sources "1") (routes entry indexed) (coverage 0 0))))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (fixture-parity "bad" arbitrary (routes entry indexed))))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (fixture-parity "bad" accepted (routes entry indexed) (diagnostics -1))))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (accepted "bad" "1" (fields X))))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (accepted "bad" "1" (token-counts (number -1)))))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (accepted-fixture "bad" -1)))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (fixture-group "bad" unknown)))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (fixture-catalog "bad" (unknown 1))))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (syntax-kind "bad" X unknown ())))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (distinct-contract "bad")))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (native-entry "bad" "1" unknown)))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (portable-rejected "bad" 1 "message")))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (model-receipt "bad" "../Escape" "source" "config" (unresolved) 1 (admitted #f))))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (model-receipt "bad" "Model" "source" "config" (callback #t) 1 (admitted #f))))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (model-receipt "bad" "Model" "source" "config" (stdout "data" 256) 1 (admitted #f))))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (model-receipt "bad" "Model" "source" "config" (stdout "data" 0) 0 (admitted #f))))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (model-receipt "bad" "Model" "source" "config" (unresolved) 1 (unknown #f))))
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (model-receipt "bad" "Model" "source" "config" (unresolved) 1)))))))
(deflanguage-parser-tests language-test-syntax-test "language parser test declarations"
  (engine (exclusive #f))
    (scenario "unknown, empty, malformed and duplicate declarations reject during expansion"
      (equal (invalid-test-declarations)  (make-list 89 #t))
      (equal test-worker-exclusive? #f)))

(def owner-calls 0)
(def source-calls 0)
(def parse-calls 0)
(def counting-loader
  (.cc arithmetic-test-language '.parse
       (lambda (source)
         (set! parse-calls (+ parse-calls 1))
         ((.ref arithmetic-test-language '.parse) source))))
(deflanguage-parser-tests language-test-evaluation-test "language-test single evaluation"
  (loader (begin (set! owner-calls (+ owner-calls 1)) counting-loader))
  (accepted "source and parser execute once"
    (begin (set! source-calls (+ source-calls 1)) "1")
    (nodes NumberExpression) (tokens number) (counts (NumberExpression 1)))
  (property "loader, source and parse are not repeated per expectation"
    (bindings (owner 'caller-binding) (artifact 'caller-artifact))
    (equal owner-calls 1) (equal source-calls 1) (equal parse-calls 1)
    (equal owner 'caller-binding) (equal artifact 'caller-artifact)))

(deflanguage-parser-tests language-test-fixture-override-test "inherited fixture override"
  (loader (.cc arithmetic-test-language 'fixtures (list arithmetic-basic-fixture)))
  (fixtures "fixture service receives the effective loader"))

(deflanguage-parser-tests language-test-structure-test "generic structural test semantics"
  (loader arithmetic-test-language)
  (accepted "queries observe real node text and field cardinality" "1"
    (root SourceFile) (subtree (node NumberExpression (lexemes "1")))
    (without-subtree (node NumberExpression (lexemes "2")))
    (without-subtree (node MissingKind))
    (field-counts SourceFile expression (1)) (field-counts NumberExpression value (1)))
  (parallel "parallel results retain their own sources" 5 '("1" "2" "1+2")))

(deflanguage-parser-tests language-test-byte-edit-test "incremental test byte offsets"
  (loader hcl-test-language)
  (incremental "Unicode before and inside the replaced text" "名称 = \"λ中😀\"\n"
    (replace "λ中" "μ文") (schema "gerbil-parser.incremental-receipt.v1")))


(def engine-setup-calls 0)
(def engine-row-calls 0)
(deflanguage-parser-tests language-test-engine-test "shared parser engine declarations"
  (engine)
  (matrix "engine matrix shares setup and checks every Cartesian row"
    (setup (source (begin (set! engine-setup-calls (+ engine-setup-calls 1)) '(10 20))))
    ((x source) (y '(1 2 3)))
    (bindings (pair (begin (set! engine-row-calls (+ engine-row-calls 1)) (cons x y))))
    (equal (car pair) x) (equal (cdr pair) y))
  (property "setup is not repeated for each expectation or row"
    (bindings) (equal engine-setup-calls 1) (equal engine-row-calls 6))
  (property "quoted negative fixtures remain literal data"
    (bindings)
    (equal '(display fixture) '(display fixture))
    (equal (quasiquote (display fixture)) '(display fixture))
    (equal (quasiquote ((unquote (+ 1 1)))) '(2))))

(deflanguage-parser-tests language-test-scenario-test "closed parser engine scenarios"
  (engine)
  (scenario "lexical scope, multiple values and action order"
    (scope ((x 7) (count 0) (setting (make-parameter 'outer)))
      (scope ((x 9) (previous x)) (equal x 9) (equal previous 7))
      (sequential ((x 9) (next x)) (equal next 9))
      (scope-values (((left right) (values 3 4)))
        (equal (+ left right) 7))
      (sequential-values (((left right) (values 3 4)) ((sum) (values (+ left right))))
        (equal sum 7))
      (parameters ((setting 'inner)) (equal (setting) 'inner))
      (equal (setting) 'outer)
      (each (left right) ('(1 2 3) '(10 20 30))
        (act (set! count (+ count left right))))
      (equal count 66)
      (when #t (equal x 7))
      (unless #f (equal x 7))
      (branch #f (equal x 99) (group (equal x 7) (raises (error "subject") true))))))

(defparser-check (declared-sum left right (expected 7))
  (equal (+ left right) expected)
  (result (+ left right)))

(deflanguage-parser-tests language-test-scenario-ownership-test "declared check and control ownership"
  (engine)
  (scenario "DSL tags cannot be captured by subject bindings"
    (scope ((receipt '(receipt-data)) (group 'group-data) (equal 'equal-data)
            (scope 'scope-data) (branch 'branch-data))
      (equal receipt '(receipt-data))
      (equal group 'group-data)
      (equal equal 'equal-data)
      (equal scope 'scope-data)
      (branch #t (equal branch 'branch-data) (equal branch 'wrong))))
  (scenario "declared checks retain return values and optional arguments"
    (verify declared-sum 3 4)
    (equal (parser-check-result declared-sum 2 3 5) 5)
    (raises (parser-check-result (lambda () #t)) true)
    (raises (parser-check-procedure (lambda () #t)) true)
    (scope ((callback (parser-check-procedure declared-sum)))
      (equal (callback 3 4) 7)))
  (scenario "loop transfers retain lexical arguments and every assertion"
    (scope ((visits 0))
      (iterate walk ((remaining 5))
        (branch (zero? remaining)
          (equal visits 5)
          (group
            (matches remaining positive?)
            (act (set! visits (+ visits 1)))
            (continue walk (- remaining 1)))))
      (equal visits 5)))
  (scenario "resource cleanup survives a subject exception"
    (scope ((closed? #f))
      (locals ((guarded
                 (parser-check ()
                   (protect
                     (body (equal closed? #f) (act (error "subject failure")))
                     (cleanup (act (set! closed? #t)))))))
        (raises (parser-check-result guarded) true))
      (equal closed? #t)))
  (scenario "wind callbacks retain order and source contexts"
    (scope ((trace '()))
      (wind
        (before (act (set! trace (cons 'enter trace))))
        (body (equal trace '(enter)) (act (set! trace (cons 'body trace))))
        (after (act (set! trace (cons 'leave trace)))))
      (equal trace '(leave body enter))))
  (scenario "callback receivers preserve multiple values and branch selection"
    (scope-values (((left right) (values 2 3)))
      (choose left
        ((0 1) (equal right 'wrong))
        ((2) (equal right 3))
        (else (equal right 'wrong)))))
  (profiled "POO case admission still owns the closed scenario"
    (equal (parser-check-result declared-sum 3 4) 7)))

(deflanguage-parser-tests language-test-receiver-test "receiver delivery and owned fixture output"
  (engine)
  (scenario "synchronous receiver arguments and body execute once"
    (scope ((trace '()) (arguments 0))
      (locals ((with-subject
                 (lambda (value receive)
                   (set! trace (cons 'enter trace))
                   (receive value)
                   (set! trace (cons 'leave trace)))))
        (using (with-subject (begin (set! arguments (+ arguments 1)) 3)) (value)
          (equal value 3)
          (act (set! trace (cons 'body trace)))))
      (equal arguments 1)
      (equal trace '(leave body enter))))
  (scenario "a skipped receiver cannot publish a successful test"
    (locals ((without-receiver (lambda (receive) 'skipped))
             (missing (parser-check ()
                        (using (without-receiver) () (equal 1 1)))))
      (raises (parser-check-result missing) true)))
  (scenario "fixture serializers own their ports and cannot print snapshots"
    (scope ((text (call-with-output-string (lambda (port) (display "input" port))))
            (datum (call-with-output-string (lambda (port) (write '(input 1) port)))))
      (equal text "input")
      (equal datum "(input 1)"))))

;;; These are subject values, not declaration keywords. Expansion must retain
;;; their lexical identity while dispatching the surrounding DSL by datum.
(def bindings 'subject-bindings)
(def setup 'subject-setup)
(deflanguage-parser-tests language-test-keyword-test "subject keyword bindings"
  (engine)
  (property "property keywords do not capture subject values"
    (bindings (actual bindings))
    (equal actual 'subject-bindings))
  (matrix "matrix setup and bindings remain datum keywords"
    (setup (actual setup))
    ((row '(1 2)))
    (bindings (subject bindings))
    (equal (list actual subject row)
           (list 'subject-setup 'subject-bindings row))))

(defrules invoke-literal-callback (lambda)
  ((_ (lambda (value) body ...) actual)
   (let (value actual) body ...)))
(deflanguage-parser-tests language-test-inline-callback-test "literal callback syntax"
  (engine)
  (scenario "inline subjects receive a literal lambda with checked steps"
    (scope ((calls 0))
      (inline-callback (invoke-literal-callback 7) (value)
        (equal value 7)
        (act (set! calls (+ calls 1))))
      (equal calls 1))))
