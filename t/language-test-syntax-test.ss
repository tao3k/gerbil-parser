;;; Engine admission controls for the public language-test DSL.
(import :std/test
        (for-syntax (only-in :gerbil/expander core-expand))
        :gerbil-parser/language-test-support
        (only-in :clan/poo/object .cc .ref)
        (only-in :gerbil-parser/languages/arithmetic/parser arithmetic-language arithmetic-basic-fixture)
        (only-in :gerbil-parser/languages/hcl/parser hcl-language))
(export language-test-syntax-test)
(defsyntax (invalid-test-declarations stx)
  (def (rejects form)
    (with-catch (lambda (_) #t) (lambda () (core-expand form) #f)))
  (datum->syntax #'invalid-test-declarations
    (list 'quote
      (list
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
(def language-test-syntax-test
  (test-suite "language parser test declarations"
    (test-case "unknown, empty, malformed and duplicate declarations reject during expansion"
      (check (invalid-test-declarations) => (make-list 50 #t)))))

(def owner-calls 0)
(def source-calls 0)
(def parse-calls 0)
(def counting-loader
  (.cc arithmetic-language '.parse
       (lambda (source)
         (set! parse-calls (+ parse-calls 1))
         ((.ref arithmetic-language '.parse) source))))
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
  (loader (.cc arithmetic-language 'fixtures (list arithmetic-basic-fixture)))
  (fixtures "fixture service receives the effective loader"))

(deflanguage-parser-tests language-test-structure-test "generic structural test semantics"
  (loader arithmetic-language)
  (accepted "queries observe real node text and field cardinality" "1"
    (root SourceFile) (subtree (node NumberExpression (lexemes "1")))
    (without-subtree (node NumberExpression (lexemes "2")))
    (without-subtree (node MissingKind))
    (field-counts SourceFile expression (1)) (field-counts NumberExpression value (1)))
  (parallel "parallel results retain their own sources" 5 '("1" "2" "1+2")))

(deflanguage-parser-tests language-test-byte-edit-test "incremental test byte offsets"
  (loader hcl-language)
  (incremental "Unicode before and inside the replaced text" "名称 = \"λ中😀\"\n"
    (replace "λ中" "μ文") (schema "gerbil-parser.incremental-receipt.v1")))
