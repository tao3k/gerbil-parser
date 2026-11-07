import ParserEngine.SourceDependencyExecution

/-! Complete project preparation guard. The compiler supplies one expanded
syntax and its visitor tree; lexical closure alone is not procedure admission.
Reader, hygienic expansion and syntax stability remain external premises. -/
namespace ParserEngine.SourceProcedureAdmission
open WalkSource LeanPoo.Functional

structure Core where
  form : Datum
  references : SourceDependencies.Tree String

def procedureLiteral : Datum → Bool
  | .list (.symbol tag :: _) => tag == "%#lambda" || tag == "%#case-lambda"
  | _ => false

/-- The native order visits first, then tests the literal and missing list.
A token retains the complete checked syntax/tree, never a reconstructed core. -/
def check (core : Core) : Option Core := do
  let missing ← SourceDependencies.dependencies core.references
  if procedureLiteral core.form && missing.isEmpty then some core else none

/-- Both actual rejection conditions, for arbitrary syntax and visitor trees. -/
theorem check_iff (core retained : Core) :
    check core = some retained ↔
      retained = core ∧ procedureLiteral core.form = true ∧
      SourceDependencies.admit core.references = some core.references := by
  unfold check SourceDependencies.admit
  cases visit : SourceDependencies.dependencies core.references with
  | none => simp
  | some missing =>
    cases shape : procedureLiteral core.form <;>
      cases empty : missing.isEmpty <;> simp_all
    exact eq_comm

/-- The dependency-only model admits closed data. That is a model-composition
counterexample, not evidence that the actual Scheme guard is defective. -/
theorem closed_data_is_not_a_procedure :
    SourceDependencies.admit (.data : SourceDependencies.Tree String) = some .data ∧
    check ⟨.integer 42, .data⟩ = none := by
  simp [SourceDependencies.admit, SourceDependencies.dependencies, check, procedureLiteral]

/-- No assumed successful lexical preparation: native admission itself proves
that there is no unresolved obligation requiring an extra source capability. -/
theorem checked_closed (accepted : check core = some retained) :
    retained = core ∧ procedureLiteral retained.form = true ∧
      ∀ name, ¬ SourceDependencies.Requires name retained.references := by
  obtain ⟨rfl, shape, admitted⟩ := (check_iff core retained).mp accepted
  exact ⟨rfl, shape, (SourceDependencies.admitted_closed _ _ admitted).2⟩

def executeChecked {Result : Type} (evaluate : Core → Option Result)
    (isProcedure : Result → Bool) (core : Core) : Option Result := do
  let retained ← check core
  let result ← evaluate retained
  if isProcedure result then some result else none

/-- Success uses the retained checked core and the real final procedure test.
An evaluator for a replacement expansion cannot supply this request's result. -/
theorem execution_checked {Result : Type} (evaluate : Core → Option Result)
    (isProcedure : Result → Bool) (core : Core) (result : Result)
    (success : executeChecked evaluate isProcedure core = some result) :
    check core = some core ∧ evaluate core = some result ∧ isProcedure result = true := by
  unfold executeChecked at success
  cases checked : check core with
  | none => simp [checked] at success
  | some retained =>
    have same := ((check_iff core retained).mp checked).1
    subst retained
    cases evaluated : evaluate core with
    | none => simp [checked, evaluated] at success
    | some value =>
      have actual : (if isProcedure value then some value else none) = some result := by
        simpa [checked, evaluated] using success
      cases procedure : isProcedure value with
      | false => simp [procedure] at actual
      | true =>
        have sameValue : value = result := by simpa [procedure] using actual
        subst result
        exact ⟨rfl, rfl, procedure⟩

theorem execution_ready {Result : Type} (evaluate : Core → Option Result)
    (isProcedure : Result → Bool) (core : Core) (result : Result)
    (checked : check core = some core) (evaluated : evaluate core = some result)
    (procedure : isProcedure result = true) :
    executeChecked evaluate isProcedure core = some result := by
  simp [executeChecked, checked, evaluated, procedure]

/-- Even a closed literal cannot publish a non-procedure evaluator result. -/
theorem nonprocedure_result_rejected {Result : Type} (evaluate : Core → Option Result)
    (isProcedure : Result → Bool) (core : Core) (result : Result)
    (checked : check core = some core) (evaluated : evaluate core = some result)
    (notProcedure : isProcedure result = false) :
    executeChecked evaluate isProcedure core = none := by
  simp [executeChecked, checked, evaluated, notProcedure]

/-- Reuse LeanPoo's exact named source capability factories at the complete
preparation boundary. Empty residual dependencies follows from native guards;
the source provider remains responsible for supplied lexical parameter names. -/
theorem checked_supplied_coverage (form : Datum) (tree : SourceDependencies.Tree String)
    (supplied : List String)
    (accepted : check ⟨form, SourceDependencies.supply supplied tree⟩ =
      some ⟨form, SourceDependencies.supply supplied tree⟩)
    {Context : Type} {Value : Context → String → Type}
    (provider : Provider Context String Value)
    {factories : Requirements.Factories Context Value supplied}
    (ready : Requirements.prepare provider supplied = .ok factories) :
    ∀ name, SourceDependencies.Requires name tree → ∃ factory, provider name = some factory := by
  have closed := (checked_closed accepted).2.2
  intro name required
  have present : name ∈ supplied := by
    by_cases present : name ∈ supplied
    · exact present
    · exact False.elim (closed name ((SourceDependencies.supplied_requires tree supplied).mpr ⟨required, present⟩))
  exact Requirements.Selected.available provider supplied factories
    ((Requirements.prepare_ok_iff _ _ _).mp ready) name present

/-- The native guard syntax is sourced from the complete real preparation
function, whose ordering and same-core evaluator are already source-witnessed. -/
theorem native_literal_guard : SourceDependencyExecution.literalGuard = app "unless" [
    app "and" [app "stx-pair?" [s "core"],
      app "memq" [app "stx-e" [app "stx-car" [s "core"]],
        app "quote" [l [s "%#lambda", s "%#case-lambda"]]]],
    app "error" [.text "source witness must expand to a procedure literal"]] := rfl

end ParserEngine.SourceProcedureAdmission
