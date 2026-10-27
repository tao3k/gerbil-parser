;;; Standalone research programs; Suite execution belongs to gerbil test.
(import (prefix-in (only-in :gerbil-parser/t/benchmarks/versioned-languages/all-languages main) benchmark-)
        (prefix-in (only-in :gerbil-parser/t/fixtures/language-pack-research/package-expression-runtime main) concise-runtime-)
        (prefix-in (only-in :gerbil-parser/t/fixtures/language-pack-research/composition-history main) history-)
        (prefix-in (only-in :gerbil-parser/t/fixtures/language-pack-research/list-runtime main) native-poo-runtime-)
        (prefix-in (only-in :gerbil-parser/t/fixtures/language-pack-research/provenance-language main) provenance-))
(export main)
(def research-programs
  (list (cons "concise-runtime" concise-runtime-main)
        (cons "history" history-main)
        (cons "native-poo-runtime" native-poo-runtime-main)
        (cons "provenance" provenance-main)
        (cons "all-language-benchmark"
          (lambda args (apply benchmark-main (if (null? args) ["1"] args))))))
(def (main name . args)
  (if (equal? name "--list")
    (begin
      (unless (null? args) (error "unexpected catalogue arguments" args))
      (for-each (lambda (program) (displayln (car program))) research-programs))
    (let (program (assoc name research-programs))
      (unless program (error "unknown research program" name))
      (apply (cdr program) args))))
