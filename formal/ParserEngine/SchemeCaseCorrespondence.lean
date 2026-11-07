import ParserEngine.SchemeEvaluationDeterminism

/-! Case executes one selector and one selected sequence in its resulting heap.
Inversion excludes a normal procedure call masquerading as special syntax. -/
namespace ParserEngine.SchemeTableEvaluation
open WalkSource

theorem Eval.case_inversion
    (actual : Eval env before (app "case" (selectorExpression :: clauses)) result after) :
    ∃ key middle statements,
      Eval env before selectorExpression (.symbol key) middle ∧
      selectCase key clauses = some statements ∧
      Sequence env middle statements result after := by
  generalize codeEq : app "case" (selectorExpression :: clauses) = code at actual
  cases actual
  case caseSymbol test selected body =>
    simp only [app, Datum.list.injEq, List.cons.injEq, true_and] at codeEq
    obtain ⟨rfl, rfl⟩ := codeEq
    exact ⟨_, _, _, selected, test, body⟩
  all_goals grind [app, callable, primitive_callable]

/-- Any effectful selector is threaded once. After it returns the clause body
runs in that heap; its later writes cannot cause the selector to run again. -/
theorem Eval.selected_sequence
    (actual : Eval env before (app "case" (selectorExpression :: clauses)) result after)
    (selectorRun : Eval env before selectorExpression (.symbol key) middle) :
    ∃ statements, selectCase key clauses = some statements ∧ Sequence env middle statements result after := by
  obtain ⟨other, intermediate, statements, test, selected, body⟩ := actual.case_inversion
  obtain ⟨same, rfl⟩ := test.deterministic selectorRun
  cases Value.symbol.inj same
  exact ⟨statements, selected, body⟩

theorem first_match (key : String) (rest : List Datum) (first following : Datum) :
    selectCase key (.list [.list [s key], first] :: .list [.list [s key], following] :: rest) = some [first] := by
  simp [selectCase]

theorem malformed_not_a_default (key : String) :
    selectCase key [.list [s "broken", app "void" []], .list [s "else", app "void" []]] = none := rfl

end ParserEngine.SchemeTableEvaluation
