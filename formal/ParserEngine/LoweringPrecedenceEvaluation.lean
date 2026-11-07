import ParserEngine.LoweringDispatch

/-! Execute the native precedence prefix, including parallel initializer scope
and dynamic admission. Only the recursive child call remains an induction
hypothesis; the selected whole branch is not assumed to execute. -/
namespace ParserEngine.LoweringPrecedenceEvaluation
open WalkSource SchemeTableEvaluation

private def entered (env : Environment) (direction : String) (rank : Datum) : Environment :=
  extend (extend env "direction" (.symbol direction)) "rank" (datumValue rank)

def forward : Datum := app "lower-to" [s "lhs", s "owner", app "cadddr" [s "expression"],
  app "list" [s "direction", s "rank"], app "child-path" [s "path", n 0], s "isolated?", s "context", s "recurrence"]

/-- Both field reads use env, not the environment extended by the first local.
Metadata can have either sign; raw data is retained rather than evaluated again. -/
theorem initializers
    (read : resolve env before "expression" = some (datumValue (.list [s "precedence", s direction, rank, child])))
    (cadrPrimitive : env "cadr" = none) (caddrPrimitive : env "caddr" = none) :
    Arguments env before [app "cadr" [s "expression"], app "caddr" [s "expression"]]
      [.symbol direction, datumValue rank] before := by
  apply Arguments.cons
  · exact .apply (.cons (.variable read) .nil) rfl cadrPrimitive
  · apply Arguments.cons
    · exact .apply (.cons (.variable read) .nil) rfl caddrPrimitive
    · exact .nil

private theorem admission
    (eqPrimitive : env "eq?" = none) (notPrimitive : env "not" = none)
    (allowed : direction ≠ "dynamic" ∨ resolve env before "allow-dynamic?" = some (.boolean true)) :
    Eval (entered env direction rank) before
      (app "and" [app "eq?" [s "direction", quoted "dynamic"], app "not" [s "allow-dynamic?"]])
      (.boolean false) before := by
  have eqRun : Eval (entered env direction rank) before
      (app "eq?" [s "direction", quoted "dynamic"]) (.boolean (direction == "dynamic")) before := by
    apply Eval.apply (values := [.symbol direction, .symbol "dynamic"]) (.cons (.variable (by simp [entered, extend, resolve])) (.cons .quoteSymbol .nil)) rfl
    simpa [entered, extend] using eqPrimitive
  by_cases dynamic : direction = "dynamic"
  · subst direction
    have permitted : resolve env before "allow-dynamic?" = some (.boolean true) := allowed.resolve_left (by simp)
    apply Eval.conjunctionTrue eqRun rfl
    apply Eval.apply (values := [.boolean true])
    · refine .cons (.variable ?_) .nil
      simpa [entered, extend, resolve] using permitted
    · rfl
    · simpa [entered, extend] using notPrimitive
  · have stopped : Eval (entered env direction rank) before
        (app "eq?" [s "direction", quoted "dynamic"]) (.boolean false) before := by
      simpa [show (direction == "dynamic") = false from beq_eq_false_iff_ne.mpr dynamic] using eqRun
    exact .conjunctionFalse stopped rfl

/-- Arbitrary parent scope, heap, metadata and recursive result. The source
initializer ordering, local bindings and rejection guard are derived here;
no Sequence premise for the complete branch or post-state oracle is supplied. -/
theorem branch
    (read : resolve env before "expression" = some (datumValue (.list [s "precedence", s direction, rank, child])))
    (cadrPrimitive : env "cadr" = none) (caddrPrimitive : env "caddr" = none)
    (eqPrimitive : env "eq?" = none) (notPrimitive : env "not" = none)
    (allowed : direction ≠ "dynamic" ∨ resolve env before "allow-dynamic?" = some (.boolean true))
    (descend : Eval (entered env direction rank) before forward result after) :
    SchemeTableEvaluation.Sequence env before (LoweringDispatchSyntax.body LoweringBranchSource.precedenceBranch) result after := by
  apply SchemeTableEvaluation.Sequence.single
  apply Eval.parallelLet (names := ["direction", "rank"])
    (initial := [app "cadr" [s "expression"], app "caddr" [s "expression"]])
    (values := [.symbol direction, datumValue rank]) (entered := entered env direction rank) rfl (by decide)
    (initializers read cadrPrimitive caddrPrimitive) rfl
  apply SchemeTableEvaluation.Sequence.cons (Eval.whenFalse (admission eqPrimitive notPrimitive allowed) rfl)
  exact .single descend

end ParserEngine.LoweringPrecedenceEvaluation
