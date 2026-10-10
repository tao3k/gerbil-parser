;;; Source preparation and complete GLR products use the shared batch sampler.
(import (only-in ../../fixtures/layout-columns reference-layout-columns)
        (only-in ../parser-stage-cost/benchmark measure-parser-component)
        (only-in :gerbil-parser/src/runtime/layout
                 make-layout-columns current-layout-columns current-layout-frames layout-token-column)
        (only-in :gerbil-parser/src/runtime/token make-token)
        (only-in :gerbil-parser/src/runtime/lexer lex-source)
        (only-in :gerbil-parser/src/runtime/significant parser-significant-tokens)
        (only-in :gerbil-parser/src/runtime/lr-parser lr-parse/prepared/receipt)
        (only-in :gerbil-parser/src/compiler/machine
                 parser-machine-runtime parser-machine-grammar-digest parser-machine-trivia)
        (only-in :gerbil-parser/src/runtime/artifact
                 make-success-parse-artifact parse-artifact-valid-for-source?)
        (only-in :gerbil-parser/languages/tla-plus/parser tla-plus-layout-language-grammar)
        (only-in :gerbil-parser/src/language/descriptor language-grammar-machine))
(export benchmark-layout-columns)

(def (prepared-layout-product machine source columns)
  (let* ((tokens (lex-source machine source))
         (significant (parser-significant-tokens machine tokens)))
    (parameterize ((current-layout-columns (columns source)) (current-layout-frames '()))
      (let-values (((root rest _receipt)
                    (lr-parse/prepared/receipt (parser-machine-runtime machine) significant)))
        (unless (null? rest) (error "layout benchmark left trailing tokens"))
        (make-success-parse-artifact (parser-machine-grammar-digest machine)
                                    source tokens root (parser-machine-trivia machine))))))

(def (benchmark-layout-columns (samples 20) (batch-count 10) (padding 32768))
  (let* ((machine (language-grammar-machine tla-plus-layout-language-grammar))
         (source (string-append "---- MODULE J ----\nInit ==\n  /\\ TRUE"
                                (make-string padding #\space) "\n  /\\ FALSE\n====\n"))
         (expected (prepared-layout-product machine source reference-layout-columns)))
    (unless (parse-artifact-valid-for-source? expected source)
      (error "layout benchmark requires a complete admitted product"))
    (for-each
     (lambda (variant)
       (let* ((name (car variant)) (constructor (cdr variant))
              (end (- (vector-length (reference-layout-columns source)) 1))
              (probe (make-token 'probe "" end end))
              (column (vector-ref (reference-layout-columns source) end)))
         (measure-parser-component (list name 'columns) samples batch-count
          (lambda () (parameterize ((current-layout-columns (constructor source)))
                       (layout-token-column probe))) column)
         (measure-parser-component (list name 'prepared-glr-product) samples batch-count
          (lambda () (prepared-layout-product machine source constructor)) expected)))
     (list (cons 'reference reference-layout-columns) (cons 'production make-layout-columns)))
    (displayln "LAYOUT-COLUMNS-BENCHMARK-OK")))
