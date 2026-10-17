(import :gerbil-parser/language-test-support)
;;; Generic POO generation and both semantic backends use two independent packs.
(import (only-in :gerbil-parser/languages/arithmetic/parser-test arithmetic-test-language)
        (only-in :gerbil-parser/languages/hcl/parser-test hcl-test-language)
        (only-in :gerbil-parser/languages/hcl/parser-test hcl-official-fixtures)
        :std/test
        :gerbil-parser/src/grammar/algebra
        (only-in :gerbil-parser/src/language/grammar deflanguage)
        (only-in :gerbil-parser/t/fixtures/fixture-release bind-fixture-grammar-release)
        (only-in :std/misc/ports read-all-as-string)
        (only-in :clan/poo/object .o .cc .ref)
        (only-in :clan/poo/mop validate)
        (only-in :gerbil-parser/language-build-support
                 FusedReductionStrategy. FusedReductionStrategyContract
                 make-fused-reduction-strategy emit-build-strategy
                 emit-language-build-strategy)
        (only-in :gerbil-parser/src/compiler/machine parser-machine-for-current-semantic-backend)
        (only-in :gerbil-parser/src/runtime/lr-parser current-lr-event-program-enabled? lr-runtime-event-program?)
        (only-in :gerbil-parser/src/compiler/machine parser-machine-runtime)
        (only-in :gerbil-parser/src/testing/language-strategy parse-indexed-reference)
        (only-in :gerbil-parser/src/language/descriptor language-grammar-machine)
        (only-in :gerbil-parser/src/language/descriptor language-grammar-ir)
        (only-in :gerbil-parser/src/language/descriptor
                 +language-grammar-schema+ make-language-grammar language-grammar-language language-grammar-version
                 language-grammar-contract language-grammar-grammar language-grammar-observability)
        (only-in :gerbil-parser/src/compiler/lr lr-spec-ref production-table production-rhs)
        (only-in :gerbil-parser/src/runtime/artifact make-success-parse-artifact
                 parse-artifact-valid? parse-artifact-roundtrip)
        (only-in :gerbil-parser/src/runtime/token make-token)
        (only-in :gerbil-parser/src/runtime/recognition make-recognition-node make-recognition-child)
        (only-in :gerbil-parser/src/compiler/machine parser-machine-grammar-digest parser-machine-trivia)
        (only-in :gerbil-parser/language-support/fixture syntax-fixture-source syntax-fixture-expected-status)
        (only-in :gerbil-parser/languages/hcl/parser  hcl-language-grammar)
        (only-in :gerbil-parser/languages/arithmetic/parser  arithmetic-language-grammar)
        (only-in :gerbil-parser/language-support/development LanguageDevelopmentLoaderContract)
        (prefix-in :gerbil-parser/src/compiler/hcl-reductions hcl-)
        (prefix-in "fixtures/arithmetic-reductions.ss" arithmetic-))


(deflanguage nullable-boundary-probe
  (syntax
   (lexical
    (root source-file)
    (lex (word Word (identifier)))
    (extras) (keywords) (recoveries) (conflicts selective-glr)
    (case-insensitive #f)))
  (rules
   (source-file (node SourceFile (seq (node Before (empty)) word (node After (empty)))))))
(bind-fixture-grammar-release nullable-boundary-probe
  "nullable-boundary-probe" "test" "nullable-boundary-probe.test")

(def (emitted strategy)
  (call-with-output-string (lambda (port) (emit-build-strategy strategy port))))

(def (cdr-expression-count form)
  (if (pair? form)
    (+ (if (eq? (car form) 'cdr) 1 0)
       (cdr-expression-count (car form)) (cdr-expression-count (cdr form)))
    0))

(defparser-check (check-linear-stack-reads descriptor)
  (sequential ((form (call-with-input-string (emitted (make-fused-reduction-strategy descriptor)) read))
         (table (production-table (lr-spec-ref (cdr (assq 'lr-spec (language-grammar-ir descriptor))) 'productions)))
         (steps (filter (lambda (definition)
                          (and (pair? definition) (eq? (car definition) 'def)
                               (pair? (cadr definition)))) (cdr form))))
    (equal (length steps)  2)
    (each (step) (steps)
       (each (clause) ((cddr (caddr step)))
          (when (pair? (car clause))
            (sequential ((id (caar clause))
                   (width (length (production-rhs (vector-ref table id))))
                   (bindings (cadr (cadr clause))))
              (iterate loop ((remaining bindings) (reads 0))
                (branch (eq? (caar remaining) 'offset)
                  (equal reads  (* 2 width))
                  (continue loop (cdr remaining) (+ reads (cdr-expression-count (cadar remaining))))))))))))
(defparser-check (check-reductions descriptor sources step event-step)
  (scope ((machine (language-grammar-machine descriptor)))
    (each (source) (sources)
       (scope ((baseline (parse-indexed-reference machine source))
             (fused (parse-indexed-reference machine source step)))
         (equal fused  baseline)
         (equal (parse-artifact-valid? fused)  #t)
         (equal (parse-artifact-roundtrip fused)  source)
         (parameters ((current-lr-event-program-enabled? #t))
           (sequential ((events-machine (parser-machine-for-current-semantic-backend machine))
                  (event-artifact (parse-indexed-reference events-machine source event-step)))
             (equal (lr-runtime-event-program? (parser-machine-runtime events-machine))  #t)
             (equal event-artifact  baseline)))))))

(deflanguage-parser-tests fused-reduction-test "generic POO fused reduction strategy"
  (engine)
    (scenario "generated recognition and event reducers preserve nullable sibling origins"
      (sequential ((descriptor nullable-boundary-probe-language-grammar)
             (machine (language-grammar-machine descriptor))
             (token (make-token 'word "a" 0 1))
             (root (make-recognition-node 'SourceFile 0 1
                     (list (make-recognition-child #f (make-recognition-node 'Before 0 0 '()))
                           (make-recognition-child #f token)
                           (make-recognition-child #f (make-recognition-node 'After 1 1 '())))))
             (expected (make-success-parse-artifact (parser-machine-grammar-digest machine)
                         "a" (list token) root (parser-machine-trivia machine)))
             (strategy (.cc (make-fused-reduction-strategy descriptor)
                            'step-name 'nullable-step 'event-name 'nullable-event-step
                            'digest-name 'nullable-digest)))
        (act (eval (call-with-input-string (emitted strategy) read)))
        (equal (parse-artifact-valid? expected)  #t)
        (equal (parse-indexed-reference machine "a")  expected)
        (equal (parse-indexed-reference machine "a" (eval 'nullable-step))  expected)
        (parameters ((current-lr-event-program-enabled? #t))
          (equal (parse-indexed-reference (parser-machine-for-current-semantic-backend machine)
                                         "a" (eval 'nullable-event-step))  expected))))
    (scenario "both emitted backends traverse each stack suffix once"
      (each (subject) ((list arithmetic-language-grammar hcl-language-grammar)) (verify check-linear-stack-reads subject)))
    (property "two descriptors generate independently and deterministically"
      (bindings (hcl (make-fused-reduction-strategy hcl-language-grammar)) (arithmetic (make-fused-reduction-strategy arithmetic-language-grammar)) (first (emitted hcl)))
      (equal first  (call-with-input-file "src/compiler/hcl-reductions.ss" read-all-as-string))
      (equal (emitted arithmetic)  (call-with-input-file "t/fixtures/arithmetic-reductions.ss" read-all-as-string))
      (equal (equal? first (emitted arithmetic))  #f)
      (equal (emitted hcl)  first))
    (property "POO inherited slots rename output and carry extension metadata"
      (bindings (prototype (.o (:: self FusedReductionStrategy.)
                            step-name: 'reduce-step event-name: 'reduce-events digest-name: 'reduce-digest
                            metadata: (.o consumer: 'downstream))) (strategy (make-fused-reduction-strategy arithmetic-language-grammar prototype)) (form (call-with-input-string (emitted strategy) read)))
      (equal (.ref (.ref strategy 'metadata) 'consumer)  'downstream)
      (equal (and (member '(export reduce-step reduce-events reduce-digest) (cdr form)) #t)  #t)
      (equal (equal? (emitted strategy) (emitted (make-fused-reduction-strategy arithmetic-language-grammar)))  #f))
    (scenario "effective overrides reject stale identity and malformed names before emission"
      (scope ((hcl (make-fused-reduction-strategy hcl-language-grammar))
            (port (open-output-string)))
        (each (invalid) ((list (.cc hcl 'digest "sha256:stale")
               (.cc hcl 'descriptor arithmetic-language-grammar 'digest hcl-direct-grammar-digest)
               (.cc hcl 'event-name 'direct-step)
               (.cc hcl 'step-name 'if)
               (.cc hcl 'step-name 'values)
               (.cc hcl 'digest-name 'token-start)
               (.cc hcl 'step-name '|bad name|)
               (.cc hcl 'metadata #f)))
           (raises (validate FusedReductionStrategyContract invalid) true)
           (raises (emit-build-strategy invalid port) true))
        (equal (get-output-string port)  "")))
    (scenario "unresolved reference overrides reject before either fused backend emits"
(locals ((original arithmetic-language-grammar)
(ir (map (lambda (entry)
               (if (eq? (car entry) 'lr-spec)
                 (cons 'lr-spec
                       (map (lambda (row)
                              (if (eq? (car row) 'productions)
                                (cons 'productions
                                      (append (cdr row)
                                              (list (list (length (cdr row)) 'unused
                                                          '((nonterminal missing)) 'pass #f))))
                                row)) (cdr entry)))
                 entry)) (language-grammar-ir original)))
(descriptor (make-language-grammar +language-grammar-schema+
                               (language-grammar-language original) (language-grammar-version original)
                               (language-grammar-contract original) (language-grammar-grammar original)
                               ir (language-grammar-machine original) (language-grammar-observability original)))
(candidate (.cc (make-fused-reduction-strategy original) 'descriptor descriptor))
(port (open-output-string)))


      (raises (validate FusedReductionStrategyContract candidate) true)
      (raises (emit-build-strategy candidate port) true)
      (equal (get-output-string port)  "")))
    (scenario "Loader slots admit declared strategies and reject foreign or duplicated bindings"
      (scope ((output (call-with-output-string (lambda (port) (emit-language-build-strategy hcl-test-language 'fused-reductions port)))))
        (equal output  (emitted (make-fused-reduction-strategy hcl-language-grammar)))
        (equal (length (.ref arithmetic-test-language 'build-strategies))  2)
        (raises (validate LanguageDevelopmentLoaderContract
                         (.cc hcl-test-language 'build-strategies (.ref arithmetic-test-language 'build-strategies))) true)
        (raises (validate LanguageDevelopmentLoaderContract
                         (.cc hcl-test-language 'build-strategies
                              (append (.ref hcl-test-language 'build-strategies) (.ref hcl-test-language 'build-strategies)))) true)
        (raises (emit-language-build-strategy hcl-test-language 'missing (open-output-string)) true)))
    (scenario "HCL field and alias reductions preserve both native semantic backends"
      (verify check-reductions hcl-language-grammar (map syntax-fixture-source (filter (lambda (fixture) (eq? (syntax-fixture-expected-status fixture) 'accepted)) hcl-official-fixtures)) hcl-direct-step hcl-direct-event-step))
    (scenario "Arithmetic precedence and zero-width reductions preserve both backends"
      (verify check-reductions arithmetic-language-grammar '("1" "a" "1 + 2 * 3" "(1 + 2) * 3" "-1 + +2" "a / (b - c)" "1 - 2 - 3" "α + 2" "1\r\n + 2") arithmetic-direct-step arithmetic-direct-event-step))
    (scenario "malformed sources reject with generic and generated reductions"
      (each (row) ((list (list hcl-language-grammar hcl-direct-step "x =\n" "b { x = 1\n")
             (list arithmetic-language-grammar arithmetic-direct-step "1 +" "(1 + 2")))
         (scope ((machine (language-grammar-machine (car row))) (step (cadr row)))
           (each (source) ((cddr row))
                       (raises (parse-indexed-reference machine source) true)
                       (raises (parse-indexed-reference machine source step) true))))))
