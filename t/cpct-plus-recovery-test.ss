;;; -*- Gerbil -*-
;;; Bounded CPCT+ LR error recovery regression suite (Diekmann & Tratt 2020):
;;; verifies multi-edit minimum-cost repair (`cost <= 3`) and fail-closed
;;; uniqueness (`'unique-candidate` vs `'ambiguous-tie` vs `'unrecovered`).

(import :gerbil-parser/language-support/testing
        :gerbil-parser/languages/arithmetic/parser
        :gerbil-parser/src/runtime/recovery)

(def (receipt-ref receipt key)
  (let ((entry (assq key receipt)))
    (and entry (cdr entry))))

(def (cpct+-receipt-summary source (max-cost 3))
  (let-values (((_artifact receipt)
                (parse-source/recover-cpct+ arithmetic-parser source max-cost)))
    (let ((ops (receipt-ref receipt 'operations)))
      (list (receipt-ref receipt 'outcome)
            (receipt-ref receipt 'uniqueMinimal)
            (receipt-ref receipt 'totalCost)
            (length ops)
            (if (pair? ops)
              (list (receipt-ref (car ops) 'kind)
                    (receipt-ref (car ops) 'literal))
              #f)))))

(deflanguage-parser-tests cpct-plus-recovery-test "gerbil-parser bounded CPCT+ minimum-cost LR recovery"
  (engine)
    (property "valid input takes zero-cost fast path"
      (bindings)
      (equal (cpct+-receipt-summary "(1 + 2)")
             '(not-needed #t 0 0 #f)))
    (property "cost-1 unique missing closing paren succeeds"
      (bindings)
      (equal (cpct+-receipt-summary "(1 + 2")
             '(unique-candidate #t 1 1 (MISSING ")"))))
    (property "cost-2 double missing closing parens succeeds beyond 1-edit recovery"
      (bindings)
      (equal (cpct+-receipt-summary "((1 + 2")
             '(unique-candidate #t 2 2 (MISSING ")"))))
    (property "cost-3 triple missing closing parens succeeds within budget"
      (bindings)
      (equal (cpct+-receipt-summary "(((1 + 2")
             '(unique-candidate #t 3 3 (MISSING ")"))))
    (property "exceeding max-cost budget fails closed without guessing"
      (bindings)
      (equal (cpct+-receipt-summary "((((1 + 2" 3)
             '(unrecovered #f 0 0 #f))))
