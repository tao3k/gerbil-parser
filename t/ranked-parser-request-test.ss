;;; Scanner substitution retains the canonical public parser request graph.
(import :std/test
        (only-in :gerbil-parser/src/compiler/machine
                 parser-machine-with-ranked-scanner parser-machine-with-lr-action-selector parser-machine-lex
                 parser-machine-runtime parser-machine-grammar-digest)
        (only-in :gerbil-parser/src/runtime/scan make-ranked-lexical-scanner-factory)
        (only-in :gerbil-parser/src/runtime/parser parse-source)
        (only-in :gerbil-parser/src/runtime/artifact parse-artifact-success? parse-artifact-valid-for-source?)
        (only-in :gerbil-parser/src/language/descriptor language-grammar-machine)
        (only-in :gerbil-parser/languages/tla-plus/parser tla-plus-layout-language-grammar tla-plus-core-language-grammar)
        (only-in ./fixtures/ranked-publication reference-ranked-lexical-scanner-factory))

(import (only-in ./fixtures/lr-action-selection reference-lr-action-selector))

(def ranked-parser-request-test
  (test-suite "ranked scanner public parser requests"
    (test-case "complete fresh artifacts retain source and runtime ownership"
      (let* ((machine (language-grammar-machine tla-plus-layout-language-grammar))
             (lexer (parser-machine-lex machine))
             (old-lexical (parser-machine-with-ranked-scanner machine reference-ranked-lexical-scanner-factory))
             (reference (parser-machine-with-lr-action-selector old-lexical reference-lr-action-selector))
             (candidate (parser-machine-with-ranked-scanner machine make-ranked-lexical-scanner-factory)))
        (check (eq? lexer (parser-machine-lex machine)) => #t)
        (check (eq? (parser-machine-runtime candidate) (parser-machine-runtime machine)) => #t)
        (check (eq? (parser-machine-runtime old-lexical) (parser-machine-runtime machine)) => #t)
        (check (parser-machine-grammar-digest candidate) => (parser-machine-grammar-digest machine))
        (for-each (lambda (source)
          (let (expected (parse-source machine source))
            (check (parse-artifact-success? expected) => #t)
            (check (parse-artifact-valid-for-source? expected source) => #t)
            (check (parse-source reference source) => expected)
            (check (parse-source candidate source) => expected)
            (check (parse-source (parser-machine-with-ranked-scanner reference make-ranked-lexical-scanner-factory) source) => expected)))
          '("---- MODULE Ranked ----\nInit ==\n  /\\ TRUE\n  /\\ FALSE\n====\n"
            "---- MODULE Unicode ----\n\\* é中😀\nInit == <<1, 2>>\n====\n"))))
    (test-case "ordinary LR retains complete source artifacts after mode specialization"
      (let* ((machine (language-grammar-machine tla-plus-core-language-grammar))
             (reference (parser-machine-with-ranked-scanner machine reference-ranked-lexical-scanner-factory))
             (candidate (parser-machine-with-ranked-scanner machine make-ranked-lexical-scanner-factory)))
        (for-each (lambda (source)
          (let (expected (parse-source machine source))
            (check (parse-artifact-success? expected) => #t)
            (check (parse-artifact-valid-for-source? expected source) => #t)
            (check (parse-source reference source) => expected)
            (check (parse-source candidate source) => expected)))
          '("---- MODULE Ranked ----\nInit == TRUE /\\ FALSE\n====\n"
            "---- MODULE Unicode ----\n\\* é中😀\nInit == <<1, 2>>\n====\n"))))
    (test-case "invalid factories fail before lexical publication"
      (let (machine (language-grammar-machine tla-plus-layout-language-grammar))
        (check-exception (parser-machine-with-ranked-scanner machine #f) true)
        (check-exception (parser-machine-with-lr-action-selector machine #f) true)
        (check-exception (parser-machine-with-lr-action-selector machine (lambda (_rows _index _casefold _layout) 42)) true)))))
(export ranked-parser-request-test)
