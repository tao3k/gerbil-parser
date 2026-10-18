import ParserEngine.SchemeTableEvaluation

/-! Determinism of the supported source fragment, independent of rule indexing. -/
namespace ParserEngine.SchemeTableEvaluation
open WalkSource

def EvalClaim (env : Environment) (before : Heap) (expression : Datum) (value : Value) (after : Heap) : Prop :=
  ∀ code other last, expression = code → Eval env before code other last → value = other ∧ after = last

def InvokeClaim (before : Heap) (procedure : Value) (values : List Value) (value : Value) (after : Heap) : Prop :=
  ∀ other last, Invoke before procedure values other last → value = other ∧ after = last

def ArgsClaim (env : Environment) (before : Heap) (expressions : List Datum) (values : List Value) (after : Heap) : Prop :=
  ∀ others last, Arguments env before expressions others last → values = others ∧ after = last

def SeqClaim (env : Environment) (before : Heap) (expressions : List Datum) (value : Value) (after : Heap) : Prop :=
  ∀ other last, Sequence env before expressions other last → value = other ∧ after = last

def EachClaim (env : Environment) (name : String) (expressions : List Datum) (before : Heap)
    (values : List Value) (after : Heap) : Prop :=
  ∀ last, Each env name expressions before values last → after = last

theorem Sequence.nonempty (evaluated : Sequence env before expressions value after) : expressions ≠ [] := by
  cases evaluated <;> simp

theorem Eval.variable_result (evaluated : Eval env before (.symbol name) value after) :
    resolve env before name = some value ∧ after = before := by
  cases evaluated with
  | «variable» found => exact ⟨found, rfl⟩

theorem Eval.no_unbound_call (absent : env name = none)
    (evaluated : Eval env before callee value after)
    (shape : Datum.list (callee :: args) = app name expressions) : False := by
  simp only [app, Datum.list.injEq, List.cons.injEq] at shape
  rcases shape with ⟨rfl, _⟩
  have found := evaluated.variable_result.1
  simp [resolve, absent] at found

theorem Eval.unique (first : Eval env before expression value after) : EvalClaim env before expression value after := by
  induction first using Eval.rec
      (motive_2 := fun before procedure values value after _ => InvokeClaim before procedure values value after)
      (motive_3 := fun env before expressions values after _ => ArgsClaim env before expressions values after)
      (motive_4 := fun env before expressions value after _ => SeqClaim env before expressions value after)
      (motive_5 := fun env name expressions before values after _ => EachClaim env name expressions before values after) <;>
    (dsimp only [EvalClaim, InvokeClaim, ArgsClaim, SeqClaim, EachClaim] at *
     intros
     rename_i second
     cases second <;> grind [app, primitive, callable, resolve, Sequence.nonempty, Eval.no_unbound_call])

theorem Eval.deterministic (first : Eval env before expression value after)
    (second : Eval env before expression other last) : value = other ∧ after = last :=
  first.unique expression other last rfl second
end ParserEngine.SchemeTableEvaluation
