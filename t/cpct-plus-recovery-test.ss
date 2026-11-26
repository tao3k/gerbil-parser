;;; -*- Gerbil -*-
;;; Bounded CPCT+ LR error recovery regression suite (Diekmann & Tratt 2020):
;;; verifies multi-edit minimum-cost repair (`cost <= 3`) and fail-closed
;;; uniqueness (`'unique-candidate` vs `'ambiguous-tie` vs `'unrecovered`).

(import :std/test
        :gerbil-parser/languages/arithmetic/parser
        :gerbil-parser/src/runtime/recovery)
(export cpct-plus-recovery-test)

(def (receipt-ref receipt key)
  (let ((entry (assq key receipt)))
    (and entry (cdr entry))))

(def cpct-plus-recovery-test
  (test-suite "gerbil-parser bounded CPCT+ minimum-cost LR recovery"

    (test-case "Valid input takes zero-cost fast path"
      (let-values (((_artifact receipt)
                    (parse-source/recover-cpct+ arithmetic-parser "(1 + 2)")))
        (check (receipt-ref receipt 'outcome) => 'not-needed)
        (check (receipt-ref receipt 'uniqueMinimal) => #t)
        (check (receipt-ref receipt 'totalCost) => 0)))

    (test-case "Cost-1 unique missing closing paren succeeds"
      (let-values (((_artifact receipt)
                    (parse-source/recover-cpct+ arithmetic-parser "(1 + 2")))
        (check (receipt-ref receipt 'outcome) => 'unique-candidate)
        (check (receipt-ref receipt 'uniqueMinimal) => #t)
        (check (receipt-ref receipt 'totalCost) => 1)
        (check (length (receipt-ref receipt 'operations)) => 1)
        (check (receipt-ref (car (receipt-ref receipt 'operations)) 'kind) => 'MISSING)
        (check (receipt-ref (car (receipt-ref receipt 'operations)) 'literal) => ")")))

    (test-case "Cost-2 double missing closing parens succeeds (beyond 1-edit recovery)"
      (let-values (((_artifact receipt)
                    (parse-source/recover-cpct+ arithmetic-parser "((1 + 2")))
        (check (receipt-ref receipt 'outcome) => 'unique-candidate)
        (check (receipt-ref receipt 'uniqueMinimal) => #t)
        (check (receipt-ref receipt 'totalCost) => 2)
        (check (length (receipt-ref receipt 'operations)) => 2)))

    (test-case "Cost-3 triple missing closing parens succeeds within budget"
      (let-values (((_artifact receipt)
                    (parse-source/recover-cpct+ arithmetic-parser "(((1 + 2")))
        (check (receipt-ref receipt 'outcome) => 'unique-candidate)
        (check (receipt-ref receipt 'uniqueMinimal) => #t)
        (check (receipt-ref receipt 'totalCost) => 3)
        (check (length (receipt-ref receipt 'operations)) => 3)))

    (test-case "Exceeding max-cost budget fails closed without guessing"
      (let-values (((_artifact receipt)
                    (parse-source/recover-cpct+ arithmetic-parser "((((1 + 2" 3)))
        (check (receipt-ref receipt 'outcome) => 'unrecovered)
        (check (receipt-ref receipt 'uniqueMinimal) => #f)
        (check (receipt-ref receipt 'operations) => '())))))
