#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Explicit closing boundaries belong to the declaring language grammar.

(import :std/test
        :gerbil-parser/src/grammar/algebra
        (only-in :gerbil-parser/src/language/grammar deflanguage)
        (only-in :gerbil-parser/src/runtime/parser parse-source)
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-success? parse-artifact-valid? parse-artifact-roundtrip)
        (only-in :gerbil-parser/src/runtime/token make-token)
        (only-in :gerbil-parser/src/runtime/layout
                 make-layout-columns current-layout-columns current-layout-frames
                 layout-after-end))
(export layout-boundary-test)

(deflanguage closing-boundary-probe
  (identity "closing-boundary-probe" "v1" "closing-boundary-probe.v1")
  (root source-file)
  (lex (word Word (identifier))
       (punctuation Punctuation (literals "(" "|>"))
       (space Space (whitespace+)))
  (rules
   (source-file (node SourceFile (seq "(" (field items (reference items)) "END")))
   (items
    (node Items
      (seq (layout-start "|>") (field item word)
           (repeat (seq (layout-next "|>") (field item word)))
           (layout-end "END")))))
  (extras space)
  (keywords)
  (recoveries)
  (conflicts selective-glr)
  (case-insensitive #f))

(def layout-boundary-test
  (test-suite "Grammar-owned layout closing boundaries"
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
