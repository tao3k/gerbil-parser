;;; Engine ownership: source-contract admission and UTF-8 scanner checkpoints.
(import (only-in :std/test check test-case test-suite)
        (only-in :gerbil-parser/src/language/source declare-source-language parse-source-language)
        (only-in "fixtures/source-strategies.ss" test-source-strategy)
        (only-in :gerbil-parser/languages/bash/parser parse-bash)
        (only-in :gerbil-parser/src/runtime/source-scanner
                 make-source-scanner source-scanner-initial-state
                 source-scanner-step source-scan-state-byte-offset)
        (only-in :gerbil-parser/src/runtime/token token-end token-kind token-start))

(export language-source-contract-test)
(def language-source-contract-test
  (test-suite "language source contracts"
    (test-case "source language rejects an artifact with another digest"
      (let (other
            (declare-source-language
             "bash" "5.3" "different-contract"
             (test-source-strategy (lambda (_) #f) (lambda (_) '())
              (lambda (source _scanner _digest) (parse-bash source)))))
        (check
         (with-catch
          (lambda (_condition) #t)
          (lambda ()
            (parse-source-language other "echo hi\n")
            #f))
         => #t)))
    (test-case "scanner checkpoints retain byte offsets"
      (let* ((scanner
              (make-source-scanner
               "αx" #f
               (lambda (_source offset context _mode)
                 (if (= offset 2)
                   (values #f offset context)
                   (values 'character (fx+ offset 1) context)))))
             (initial (source-scanner-initial-state scanner)))
        (let-values (((first after-first)
                      (source-scanner-step scanner initial 'test)))
          (check (token-start first) => 0)
          (check (token-end first) => 2)
          (check (source-scan-state-byte-offset initial) => 0)
          (check (source-scan-state-byte-offset after-first) => 2)
          (let-values (((second after-second)
                        (source-scanner-step scanner after-first 'test)))
            (check (token-kind second) => 'character)
            (check (token-start second) => 2)
            (check (token-end second) => 3)
            (check (source-scan-state-byte-offset after-second) => 3)))))
))
