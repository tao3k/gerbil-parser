;;; Engine admission controls for the public language-test DSL.
(import :std/test
        (for-syntax (only-in :gerbil/expander core-expand))
        :gerbil-parser/language-test-support
        (only-in :clan/poo/object .cc .ref)
        (only-in :gerbil-parser/languages/arithmetic/parser arithmetic-language arithmetic-basic-fixture))
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
        (rejects #'(deflanguage-parser-tests bad "bad" (loader #f) (accepted 17 "1")))))))
(def language-test-syntax-test
  (test-suite "language parser test declarations"
    (test-case "unknown, empty, malformed and duplicate declarations reject during expansion"
      (check (invalid-test-declarations) => (make-list 10 #t)))))

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
