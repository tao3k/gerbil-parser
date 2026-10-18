;;; Prepare after loader-controls: authoring explicitly activates the compiler.
(import :gerbil-parser/language-test-support
        (only-in :gerbil-parser/language-support/grammar deflanguage defgrammar-syntax)
        (only-in :gerbil-parser/src/runtime/parser parse-source)
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-success? parse-artifact-roundtrip))

(defgrammar-syntax (number-leaf operand)
  (node NumberExpression (field value operand)))
(deflanguage phase-control
  (syntax (lexical (root document)
           (lex (number Number (decimal-digit+)) (unknown Unknown (fallback)))
           (conflicts reject)))
  (rules (document (node Document (number-leaf number)))))

(def +compiler-loaded+
  (and (assoc "gerbil-parser/src/compiler/language-declaration" (list-modules)) #t))
(deflanguage-parser-tests test-macro-author-test "author declaration phase"
  (engine (exclusive #t))
  (property "source declaration activates its service and retains parsing semantics"
    (bindings (artifact (parse-source phase-control-parser "123")))
    (equal +compiler-loaded+ #t)
    (equal (parse-artifact-success? artifact) #t)
    (equal (parse-artifact-roundtrip artifact) "123"))
  (property "source declaration retains rejection"
    (bindings (artifact (parse-source phase-control-parser "x")))
    (equal (parse-artifact-success? artifact) #f))
  (scenario "invalid source declarations still fail at expansion"
    (raises
     (eval '(begin
              (import (only-in :gerbil-parser/language-support/grammar deflanguage))
              (deflanguage invalid-phase-control (syntax (lexical)) (rules))))
     syntax-error?)))
