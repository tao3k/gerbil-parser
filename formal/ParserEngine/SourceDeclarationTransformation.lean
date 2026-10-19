import ParserEngine.SourceDeclarationPredicate
import ParserEngine.SourceDependencyAdapter
import ParserEngine.SourceDependencies
import ParserEngine.SourceProcedureAdmission

/-! A transform is an actual Scheme closure call with a threaded heap, not an
uninterpreted declaration value. Interface preservation and complete dependency
admission are distinct from semantic equivalence of instrumentation callbacks. -/
namespace ParserEngine.SourceDeclarationTransformation
open WalkSource SchemeTableEvaluation LeanPoo.Functional SourceDeclarationPredicate

/-- A successful guard keeps the name and complete parameter interface. -/
def interface : Datum → Option Datum
  | .list (.symbol "def" :: .list (.symbol owner :: parameters) :: _ :: _) =>
    some (.list (.symbol owner :: parameters))
  | _ => none

def accept (original transformed : Datum) : Bool :=
  match interface original, interface transformed with
  | some left, some right => left == right
  | _, _ => false

theorem accepted_interface (accepted : accept original transformed = true) :
    ∃ header, interface original = some header ∧ interface transformed = some header := by
  unfold accept at accepted
  split at accepted
  · rename_i left right a b
    have same : left = right := by simpa using accepted
    exact ⟨left, a, same ▸ b⟩
  · contradiction

/-- Prefix validation shared by the original and returned declaration guards. -/
def originalTest : Test := .all [.proper, .lengthAtLeast 3, .definition,
  .pair .header, .symbol .owner, .properView .parameters]

def resultTest (header : Datum) : Test :=
  .all [.proper, .lengthAtLeast 3, .definition, .sameHeader header]

def candidateHeader : Datum → Option Datum
  | .list (.symbol "def" :: header :: _ :: _) => some header
  | _ => none

theorem original_test_corresponds (datum : Datum) :
    evaluate "" datum originalTest = (interface datum).isSome := by
  cases datum with
  | symbol _ | keyword _ | text _ | integer _ | negative _ | boolean _ => rfl
  | list items =>
    cases items with
    | nil => rfl
    | cons tag rest =>
      cases rest with
      | nil => cases tag <;> simp [evaluate, evaluateAll, originalTest, interface]
      | cons header body =>
        cases body with
        | nil => cases tag <;> simp [evaluate, evaluateAll, originalTest, interface]
        | cons first trailing =>
          cases tag with
          | symbol tag =>
            by_cases definition : tag = "def"
            · subst tag
              cases header with
              | list parts => cases parts with
                | nil => rfl
                | cons owner parameters => cases owner <;> simp [evaluate, evaluateAll, originalTest, interface, view]
              | symbol _ | keyword _ | text _ | integer _ | negative _ | boolean _ => rfl
            · simp [evaluate, evaluateAll, originalTest, interface, definition]
          | keyword _ | text _ | integer _ | negative _ | boolean _ | list _ => rfl

theorem result_test_corresponds (datum header : Datum) :
    evaluate "" datum (resultTest header) = decide (candidateHeader datum = some header) := by
  cases datum with
  | symbol _ | keyword _ | text _ | integer _ | negative _ | boolean _ => rfl
  | list items =>
    cases items with
    | nil => rfl
    | cons tag rest =>
      cases rest with
      | nil => cases tag <;> simp [evaluate, evaluateAll, resultTest, candidateHeader]
      | cons actual body =>
        cases body with
        | nil => cases tag <;> simp [evaluate, evaluateAll, resultTest, candidateHeader]
        | cons first trailing =>
          cases tag with
          | symbol tag =>
            by_cases definition : tag = "def"
            · subst tag; simp [evaluate, evaluateAll, resultTest, candidateHeader, view]
            · simp [evaluate, evaluateAll, resultTest, candidateHeader, definition]
          | keyword _ | text _ | integer _ | negative _ | boolean _ | list _ => rfl

theorem interface_projection (found : interface datum = some header) :
    view .header datum = some header ∧ candidateHeader datum = some header ∧
      ∃ owner parameters, header = l (s owner :: parameters) := by
  unfold interface at found
  split at found
  · cases found; exact ⟨rfl, rfl, _, _, rfl⟩
  · contradiction

theorem candidate_promotes (found : candidateHeader datum = some (l (s owner :: parameters))) :
    interface datum = some (l (s owner :: parameters)) := by
  unfold candidateHeader at found
  split at found
  · cases found; rfl
  · contradiction

/-- Actual first-order guard execution uses the protected pre-call header.
The SDK codec supplies its unchanged value in independently owned storage. -/
def guardsAccept (original transformed : Datum) : Bool :=
  if evaluate "" original originalTest then
    match view .header original with
    | some header => evaluate "" transformed (resultTest header)
    | none => false
  else false

/-- Native source guards derive the acceptance fact for arbitrary declarations;
it is no longer an independent admission premise supplied by a caller. -/
theorem guards_accept_iff (original transformed : Datum) :
    guardsAccept original transformed = true ↔ accept original transformed = true := by
  unfold guardsAccept
  rw [original_test_corresponds]
  cases first : interface original with
  | none => simp [first, accept]
  | some header =>
    obtain ⟨projection, _, owner, parameters, rfl⟩ := interface_projection first
    simp only [Option.isSome_some, ↓reduceIte, projection]
    rw [result_test_corresponds]
    constructor
    · intro result
      have selected := candidate_promotes (of_decide_eq_true result)
      simp [accept, first, selected]
    · intro accepted
      cases second : interface transformed with
      | none => simp [accept, first, second] at accepted
      | some right =>
        have same : l (s owner :: parameters) = right := by simpa [accept, first, second] using accepted
        have current : interface transformed = some (l (s owner :: parameters)) := by simpa [same] using second
        exact decide_eq_true (interface_projection current).2.1

/-- Successful guards provide the complete procedure envelope required by
the project lambda constructor; malformed data cannot reach compiler input. -/
theorem accepted_lambda_shape (accepted : accept original transformed = true) (dependencies : List String) :
    ∃ parameters body, SourceDependencyAdapter.lambdaData transformed dependencies =
      l (s "lambda" :: l parameters :: body) := by
  obtain ⟨header, _, result⟩ := accepted_interface accepted
  unfold interface at result
  split at result
  · exact ⟨_, _, rfl⟩
  · contradiction

/-- A header-changing transformation cannot turn a free name into a new
parameter and thereby hide that obligation from the dependency collector. -/
theorem changed_interface_rejects
    (left : interface original = some originalHeader)
    (right : interface transformed = some transformedHeader)
    (changed : originalHeader ≠ transformedHeader) : accept original transformed = false := by
  simp [accept, left, right, changed]

/-- If the guard snapshot aliases callback storage, a write destroys the
old guard value. This holds for any value and any original heap. -/
theorem aliased_snapshot_overwritten (heap : Heap) (address : Nat)
    (changed : SchemeTableEvaluation.Value) :
    write heap address 0 changed address 0 = some changed := by simp [write]

/-- Physical separation is the required host-copy contract. Threading a
callback write preserves every independently owned snapshot cell. -/
theorem distinct_snapshot_retained (heap : Heap) (snapshot callback : Nat)
    (changed : SchemeTableEvaluation.Value) (distinct : snapshot ≠ callback) :
    write heap callback 0 changed snapshot 0 = heap snapshot 0 := by simp [write, distinct]

/-- The literal call extracted from the real adapter is evaluated exactly once. -/
def invocation : Datum := app "transform" [s "declaration"]

def invocationFromSource : Datum :=
  match SourceDependencyWitness.transformSource with
  | .list [.symbol "def", _, _, .list [.symbol "let*", _,
      .list [.symbol "let*", .list [.list [_, call], _], _, _]]] => call
  | _ => s "unsupported"

theorem extracted_invocation : invocationFromSource = invocation := rfl

def originalGuard : Datum := app "unless" [app "and" [
  app "list?" [s "declaration"], app ">=" [app "length" [s "declaration"], n 3],
  app "eq?" [app "car" [s "declaration"], quoted "def"],
  app "pair?" [app "cadr" [s "declaration"]],
  app "symbol?" [app "caadr" [s "declaration"]],
  app "list?" [app "cdadr" [s "declaration"]]],
  app "error" [.text "source witness requires a procedure declaration"]]

def transformedGuard : Datum := app "unless" [app "and" [
  app "list?" [s "transformed"], app ">=" [app "length" [s "transformed"], n 3],
  app "eq?" [app "car" [s "transformed"], quoted "def"],
  app "equal?" [app "cadr" [s "transformed"], s "header"]],
  app "error" [.text "source transformation changed the declaration interface"]]

/-- The generic guard denotations emit the complete real source tests. -/
theorem emitted_original_guard : originalGuard = app "unless" [emit "declaration" originalTest,
    app "error" [.text "source witness requires a procedure declaration"]] := rfl

theorem emitted_result_guard (header : Datum) : transformedGuard = app "unless" [
    emit "transformed" (resultTest header),
    app "error" [.text "source transformation changed the declaration interface"]] := rfl

/-- Both guards surround the single invocation in the entire real procedure. -/
theorem extracted_transform : SourceDependencyWitness.transformSource =
    app "def" [l [s "transform-source-declaration", s "declaration", s "transform"],
      originalGuard, app "let*" [l [
        l [s "header", app "copy-source-data" [app "cadr" [s "declaration"]]],
        l [s "declaration", app "copy-source-data" [s "declaration"]]],
        app "let*" [l [l [s "output", invocation],
          l [s "transformed", app "copy-source-data" [s "output"]]],
          transformedGuard, s "transformed"]]] := rfl

/-- Project use of the installed host's object-graph codec is checked. Codec
round-trip and storage separation are host premises, not invented copy code. -/
theorem extracted_snapshot : SourceDependencyWitness.snapshotSource =
    app "def" [l [s "copy-source-data", s "value"],
      app "u8vector->object" [app "object->u8vector" [s "value"]]] := rfl

/-- Full closure bodies may mutate captured storage. Successful evaluation of
that body generates the one call; the resulting heap goes to later admission. -/
theorem invoke_closure (env captured entered : Environment) (before after : Heap)
    (parameters body : List Datum) (original transformed : Value)
    (transformRead : resolve env before "transform" = some (.closure parameters body captured))
    (sourceRead : resolve env before "declaration" = some original)
    (bound : bind parameters [original] captured = some entered)
    (execution : SchemeTableEvaluation.Sequence entered before body transformed after) :
    Eval env before invocation transformed after := by
  exact .call rfl (.variable transformRead) (.cons (.variable sourceRead) .nil)
    (.closure bound execution)

/-- Once retained, a closure result is unique even with effects. Re-evaluating
against the updated heap is not justified by this theorem. -/
theorem unique_execution (first : Eval env before invocation value after)
    (second : Eval env before invocation other last) : value = other ∧ after = last := by
  exact Eval.deterministic first second

/-- A successful project guard retains the result of the actual call. Syntax
conversion is the same literal representation used by the source evaluator. -/
structure GuardedCall (env : Environment) (before after : Heap)
    (original transformed : Datum) where
  originalValue : SchemeTableEvaluation.Value
  transformedValue : SchemeTableEvaluation.Value
  input : literal original = some originalValue
  output : literal transformed = some transformedValue
  sourceRead : resolve env before "declaration" = some originalValue
  execution : Eval env before invocation transformedValue after
  accepted : accept original transformed = true

/-- The effectful closure body and syntax conversion construct a guarded call;
there is no function from an assumed transform result to a pretend execution. -/
def checked_closure (env captured entered : Environment) (before after : Heap)
    (parameters body : List Datum) (original transformed : Datum)
    (transformRead : resolve env before "transform" = some (.closure parameters body captured))
    (sourceRead : resolve env before "declaration" = some (datumValue original))
    (bound : bind parameters [datumValue original] captured = some entered)
    (bodyExecution : SchemeTableEvaluation.Sequence entered before body (datumValue transformed) after) :
    Option (GuardedCall env before after original transformed) :=
  if validated : guardsAccept original transformed = true then
    some ⟨datumValue original, datumValue transformed, literal_total original, literal_total transformed, sourceRead,
      invoke_closure env captured entered before after parameters body (datumValue original) (datumValue transformed)
        transformRead sourceRead bound bodyExecution, (guards_accept_iff original transformed).mp validated⟩
  else none

/-- Complete native request composition: whole-source selection, actual
closure execution, project-owned lambda construction, one external expansion,
and the complete computed preparation checks. There is no independent caller
certificate claiming that the expanded tree was already admitted. -/
theorem read_transform_prepare {Result : Type}
    (name : String) (forms : List Datum) (dependencies : List String)
    (original transformed : Datum) (selected : SourceDeclarationReading.readDeclaration name forms = some original)
    (env : Environment) (before after : Heap)
    (called : GuardedCall env before after original transformed)
    (expand : Datum → Option SourceProcedureAdmission.Core) (core : SourceProcedureAdmission.Core)
    (expanded : expand (SourceDependencyAdapter.lambdaData transformed dependencies) = some core)
    (evaluate : SourceProcedureAdmission.Core → Option Result) (isProcedure : Result → Bool)
    (result : Result)
    (completed : SourceProcedureAdmission.executeChecked evaluate isProcedure core = some result) :
    (original ∈ forms ∧ SourceDeclarationReading.selects name original = true) ∧
    (∃ header, interface original = some header ∧ interface transformed = some header) ∧
    Eval env before invocation called.transformedValue after ∧
    expand (SourceDependencyAdapter.lambdaData transformed dependencies) = some core ∧
    SourceProcedureAdmission.procedureLiteral core.form = true ∧
    (∀ key, ¬ SourceDependencies.Requires key core.references) ∧
    evaluate core = some result ∧ isProcedure result = true := by
  obtain ⟨checked, evaluated, procedure⟩ :=
    SourceProcedureAdmission.execution_checked evaluate isProcedure core result completed
  have closed := SourceProcedureAdmission.checked_closed checked
  exact ⟨SourceDeclarationReading.selected_occurs selected, accepted_interface called.accepted,
    called.execution, expanded, closed.2.1, closed.2.2, evaluated, procedure⟩

end ParserEngine.SourceDeclarationTransformation
