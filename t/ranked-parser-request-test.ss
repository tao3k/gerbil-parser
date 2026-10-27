(import :gerbil-parser/language-test-support)
;;; Scanner substitution retains the canonical public parser request graph.
(import :std/test
        (only-in :gerbil-parser/src/runtime/machine
                 parser-machine-with-ranked-scanner parser-machine-with-lr-action-selector parser-machine-lex
                 parser-machine-runtime parser-machine-grammar-digest)
        (only-in :gerbil-parser/src/runtime/scan make-ranked-lexical-scanner-factory)
        (only-in :gerbil-parser/src/runtime/parser parse-source)
        (only-in :gerbil-parser/src/runtime/artifact parse-artifact-success? parse-artifact-valid-for-source?)
        (only-in :gerbil-parser/src/language/descriptor language-grammar-machine)
        (only-in :gerbil-parser/languages/tla-plus/parser tla-plus-layout-language-grammar tla-plus-core-language-grammar)
        (only-in ./fixtures/ranked-publication reference-ranked-lexical-scanner-factory))

(import (only-in ./fixtures/lr-action-selection reference-lr-action-selector))

(deflanguage-parser-tests ranked-parser-request-test "ranked scanner public parser requests"
  (engine)
    (scenario "complete fresh artifacts retain source and runtime ownership"
      (sequential ((machine (language-grammar-machine tla-plus-layout-language-grammar))
             (lexer (parser-machine-lex machine))
             (old-lexical (parser-machine-with-ranked-scanner machine reference-ranked-lexical-scanner-factory))
             (reference (parser-machine-with-lr-action-selector old-lexical reference-lr-action-selector))
             (candidate (parser-machine-with-ranked-scanner machine make-ranked-lexical-scanner-factory)))
        (equal (eq? lexer (parser-machine-lex machine))  #t)
        (equal (eq? (parser-machine-runtime candidate) (parser-machine-runtime machine))  #t)
        (equal (eq? (parser-machine-runtime old-lexical) (parser-machine-runtime machine))  #t)
        (equal (parser-machine-grammar-digest candidate)  (parser-machine-grammar-digest machine))
        (each (source) ('("---- MODULE Ranked ----\nInit ==\n  /\\ TRUE\n  /\\ FALSE\n====\n"
            "---- MODULE Unicode ----\n\\* é中😀\nInit == <<1, 2>>\n====\n"))
          (scope ((expected (parse-source machine source)))
            (equal (parse-artifact-success? expected)  #t)
            (equal (parse-artifact-valid-for-source? expected source)  #t)
            (equal (parse-source reference source)  expected)
            (equal (parse-source candidate source)  expected)
            (equal (parse-source (parser-machine-with-ranked-scanner reference make-ranked-lexical-scanner-factory) source)  expected)))))
    (scenario "ordinary LR retains complete source artifacts after mode specialization"
      (sequential ((machine (language-grammar-machine tla-plus-core-language-grammar))
             (reference (parser-machine-with-ranked-scanner machine reference-ranked-lexical-scanner-factory))
             (candidate (parser-machine-with-ranked-scanner machine make-ranked-lexical-scanner-factory)))
        (each (source) ('("---- MODULE Ranked ----\nInit == TRUE /\\ FALSE\n====\n"
            "---- MODULE Unicode ----\n\\* é中😀\nInit == <<1, 2>>\n====\n"))
          (scope ((expected (parse-source machine source)))
            (equal (parse-artifact-success? expected)  #t)
            (equal (parse-artifact-valid-for-source? expected source)  #t)
            (equal (parse-source reference source)  expected)
            (equal (parse-source candidate source)  expected)))))
    (scenario "invalid factories fail before lexical publication"
      (scope ((machine (language-grammar-machine tla-plus-layout-language-grammar)))
        (raises (parser-machine-with-ranked-scanner machine #f) true)
        (raises (parser-machine-with-lr-action-selector machine #f) true)
        (raises (parser-machine-with-lr-action-selector machine (lambda (_rows _index _casefold _layout) 42)) true))))
