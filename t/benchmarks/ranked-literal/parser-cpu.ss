;;; Complete public parser CPU assertions load language owners only here.
(import (only-in ./benchmark ranked-cpu-admission)
        (only-in ../../fixtures/ranked-publication reference-ranked-lexical-scanner-factory)
        (only-in :gerbil-parser/src/runtime/scan make-ranked-lexical-scanner-factory))
(import (only-in ../../fixtures/lr-action-selection reference-lr-action-selector))
(export benchmark-ranked-parser-cpu qualify-ranked-parser-cpu)

(import (only-in :gerbil-parser/src/compiler/machine parser-machine-with-ranked-scanner parser-machine-with-lr-action-selector)
        (only-in :gerbil-parser/src/language/descriptor language-grammar-machine)
        (only-in :gerbil-parser/src/runtime/parser parse-source)
        (only-in :gerbil-parser/src/runtime/artifact parse-artifact-valid-for-source? parse-artifact-success?)
        (only-in :gerbil-parser/languages/tla-plus/parser tla-plus-layout-language-grammar tla-plus-core-language-grammar))

;;; Both variants execute the public fresh parse entry: source traversal,
;;; lexical modes, GLR/layout and complete artifact publication stay shared.
;;; Preparation substitutes ranked scanning and, for layout, the former
;;; action projection primitive. Timed requests keep the public pipeline.
(def (benchmark-ranked-parser-cpu (groups 20) (batch-count 256) (scenario 'layout) (clauses 12))
  (unless (and (memq scenario '(layout core assumptions)) (exact-integer? clauses) (positive? clauses))
    (error "ranked parser CPU workload requires positive exact clauses"))
  (let* ((machine (language-grammar-machine
                   (if (eq? scenario 'layout) tla-plus-layout-language-grammar
                       tla-plus-core-language-grammar)))
         (old-lexical (parser-machine-with-ranked-scanner machine reference-ranked-lexical-scanner-factory))
         (old (if (eq? scenario 'layout)
                (parser-machine-with-lr-action-selector old-lexical reference-lr-action-selector)
                old-lexical))
         (new (parser-machine-with-ranked-scanner machine make-ranked-lexical-scanner-factory))
         (source
          (string-append "---- MODULE Ranked ----\n"
            (case scenario
              ((layout) (string-append "Init ==\n"
                           (apply string-append (map (lambda (_) "  /\\ TRUE\n") (iota clauses)))))
              ((core) (string-append "Init == TRUE"
                         (apply string-append (map (lambda (_) " /\\ TRUE") (iota clauses))) "\n"))
              ((assumptions) (apply string-append (map (lambda (_) "ASSUME TRUE\n") (iota clauses)))))
            "====\n"))
         (expected (parse-source old source)))
    (unless (and (parse-artifact-success? expected)
                 (parse-artifact-valid-for-source? expected source)
                 (equal? expected (parse-source new source))
                 (equal? expected (parse-source machine source)))
      (error "ranked parser variants differ from complete production artifact"))
    (list (ranked-cpu-admission (list 'public-tla-plus-parser scenario clauses) groups batch-count expected
            (lambda () (parse-source old source)) (lambda () (parse-source new source))))))

(def (qualify-ranked-parser-cpu (groups 20) (batch-count 256) (scenario 'layout) (clauses 12))
  (let (summaries (benchmark-ranked-parser-cpu groups batch-count scenario clauses))
    (unless (andmap (lambda (entry) (cdr (assq 'cpu-benefit (cdr entry)))) summaries)
      (error "complete public parser CPU controls or benefit gate failed" summaries))
    (displayln "RANKED-PARSER-CPU-PROOF-OK") (force-output)
    summaries))
