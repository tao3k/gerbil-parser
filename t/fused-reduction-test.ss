;;; Generic POO generation and both semantic backends use two independent packs.
(import (only-in :gerbil-parser/languages/arithmetic/parser-test arithmetic-test-language)
        (only-in :gerbil-parser/languages/hcl/parser-test hcl-test-language)
        (only-in :gerbil-parser/languages/hcl/parser-test hcl-official-fixtures)
        :std/test
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
        (only-in :gerbil-parser/src/runtime/artifact parse-artifact-valid? parse-artifact-roundtrip)
        (only-in :gerbil-parser/language-support/fixture syntax-fixture-source syntax-fixture-expected-status)
        (only-in :gerbil-parser/languages/hcl/parser  hcl-language-grammar)
        (only-in :gerbil-parser/languages/arithmetic/parser  arithmetic-language-grammar)
        (only-in :gerbil-parser/language-support/development LanguageDevelopmentLoaderContract)
        (prefix-in :gerbil-parser/src/compiler/hcl-reductions hcl-)
        (prefix-in "fixtures/arithmetic-reductions.ss" arithmetic-))
(export fused-reduction-test)

(def (emitted strategy)
  (call-with-output-string (lambda (port) (emit-build-strategy strategy port))))

(def (cdr-expression-count form)
  (if (pair? form)
    (+ (if (eq? (car form) 'cdr) 1 0)
       (cdr-expression-count (car form)) (cdr-expression-count (cdr form)))
    0))

(def (check-linear-stack-reads descriptor)
  (let* ((form (call-with-input-string (emitted (make-fused-reduction-strategy descriptor)) read))
         (table (production-table (lr-spec-ref (cdr (assq 'lr-spec (language-grammar-ir descriptor))) 'productions)))
         (steps (filter (lambda (definition)
                          (and (pair? definition) (eq? (car definition) 'def)
                               (pair? (cadr definition)))) (cdr form))))
    (check (length steps) => 2)
    (for-each
     (lambda (step)
       (for-each
        (lambda (clause)
          (when (pair? (car clause))
            (let* ((id (caar clause))
                   (width (length (production-rhs (vector-ref table id))))
                   (bindings (cadr (cadr clause))))
              (let loop ((remaining bindings) (reads 0))
                (if (eq? (caar remaining) 'offset)
                  (check reads => (* 2 width))
                  (loop (cdr remaining) (+ reads (cdr-expression-count (cadar remaining)))))))))
        (cddr (caddr step)))) steps)))
(def (check-reductions descriptor sources step event-step)
  (let (machine (language-grammar-machine descriptor))
    (for-each
     (lambda (source)
       (let ((baseline (parse-indexed-reference machine source))
             (fused (parse-indexed-reference machine source step)))
         (check fused => baseline)
         (check (parse-artifact-valid? fused) => #t)
         (check (parse-artifact-roundtrip fused) => source)
         (parameterize ((current-lr-event-program-enabled? #t))
           (let* ((events-machine (parser-machine-for-current-semantic-backend machine))
                  (event-artifact (parse-indexed-reference events-machine source event-step)))
             (check (lr-runtime-event-program? (parser-machine-runtime events-machine)) => #t)
             (check event-artifact => baseline)))))
     sources)))

(def fused-reduction-test
  (test-suite "generic POO fused reduction strategy"
    (test-case "both emitted backends traverse each stack suffix once"
      (for-each check-linear-stack-reads (list arithmetic-language-grammar hcl-language-grammar)))
    (test-case "two descriptors generate independently and deterministically"
      (let* ((hcl (make-fused-reduction-strategy hcl-language-grammar))
             (arithmetic (make-fused-reduction-strategy arithmetic-language-grammar))
             (first (emitted hcl)))
        (check first => (call-with-input-file "src/compiler/hcl-reductions.ss" read-all-as-string))
        (check (emitted arithmetic) => (call-with-input-file "t/fixtures/arithmetic-reductions.ss" read-all-as-string))
        (check (equal? first (emitted arithmetic)) => #f)
        (check (emitted hcl) => first)))
    (test-case "POO inherited slots rename output and carry extension metadata"
      (let* ((prototype (.o (:: self FusedReductionStrategy.)
                            step-name: 'reduce-step event-name: 'reduce-events digest-name: 'reduce-digest
                            metadata: (.o consumer: 'downstream)))
             (strategy (make-fused-reduction-strategy arithmetic-language-grammar prototype))
             (form (call-with-input-string (emitted strategy) read)))
        (check (.ref (.ref strategy 'metadata) 'consumer) => 'downstream)
        (check (and (member '(export reduce-step reduce-events reduce-digest) (cdr form)) #t) => #t)
        (check (equal? (emitted strategy) (emitted (make-fused-reduction-strategy arithmetic-language-grammar))) => #f)))
    (test-case "effective overrides reject stale identity and malformed names before emission"
      (let ((hcl (make-fused-reduction-strategy hcl-language-grammar))
            (port (open-output-string)))
        (for-each
         (lambda (invalid)
           (check-exception (validate FusedReductionStrategyContract invalid) true)
           (check-exception (emit-build-strategy invalid port) true))
         (list (.cc hcl 'digest "sha256:stale")
               (.cc hcl 'descriptor arithmetic-language-grammar 'digest hcl-direct-grammar-digest)
               (.cc hcl 'event-name 'direct-step)
               (.cc hcl 'step-name 'if)
               (.cc hcl 'step-name 'values)
               (.cc hcl 'digest-name 'token-start)
               (.cc hcl 'step-name '|bad name|)
               (.cc hcl 'metadata #f)))
        (check (get-output-string port) => "")))
    (test-case "unresolved reference overrides reject before either fused backend emits"
      (def original arithmetic-language-grammar)
      (def ir
        (map (lambda (entry)
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
      (def descriptor
        (make-language-grammar +language-grammar-schema+
                               (language-grammar-language original) (language-grammar-version original)
                               (language-grammar-contract original) (language-grammar-grammar original)
                               ir (language-grammar-machine original) (language-grammar-observability original)))
      (def candidate (.cc (make-fused-reduction-strategy original) 'descriptor descriptor))
      (def port (open-output-string))
      (check-exception (validate FusedReductionStrategyContract candidate) true)
      (check-exception (emit-build-strategy candidate port) true)
      (check (get-output-string port) => ""))
    (test-case "Loader slots admit declared strategies and reject foreign or duplicated bindings"
      (let (output (call-with-output-string (lambda (port) (emit-language-build-strategy hcl-test-language 'fused-reductions port))))
        (check output => (emitted (make-fused-reduction-strategy hcl-language-grammar)))
        (check (length (.ref arithmetic-test-language 'build-strategies)) => 2)
        (check-exception (validate LanguageDevelopmentLoaderContract
                         (.cc hcl-test-language 'build-strategies (.ref arithmetic-test-language 'build-strategies))) true)
        (check-exception (validate LanguageDevelopmentLoaderContract
                         (.cc hcl-test-language 'build-strategies
                              (append (.ref hcl-test-language 'build-strategies) (.ref hcl-test-language 'build-strategies)))) true)
        (check-exception (emit-language-build-strategy hcl-test-language 'missing (open-output-string)) true)))
    (test-case "HCL field and alias reductions preserve both native semantic backends"
      (check-reductions hcl-language-grammar
        (map syntax-fixture-source (filter (lambda (fixture) (eq? (syntax-fixture-expected-status fixture) 'accepted)) hcl-official-fixtures))
        hcl-direct-step hcl-direct-event-step))
    (test-case "Arithmetic precedence and zero-width reductions preserve both backends"
      (check-reductions arithmetic-language-grammar
        '("1" "a" "1 + 2 * 3" "(1 + 2) * 3" "-1 + +2" "a / (b - c)" "1 - 2 - 3" "α + 2" "1\r\n + 2")
        arithmetic-direct-step arithmetic-direct-event-step))
    (test-case "malformed sources reject with generic and generated reductions"
      (for-each
       (lambda (row)
         (let ((machine (language-grammar-machine (car row))) (step (cadr row)))
           (for-each (lambda (source)
                       (check-exception (parse-indexed-reference machine source) true)
                       (check-exception (parse-indexed-reference machine source step) true))
                     (cddr row))))
       (list (list hcl-language-grammar hcl-direct-step "x =\n" "b { x = 1\n")
             (list arithmetic-language-grammar arithmetic-direct-step "1 +" "(1 + 2"))))))
