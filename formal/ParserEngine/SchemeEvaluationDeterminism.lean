import ParserEngine.SchemeTableEvaluation

/-! Determinism of the supported source fragment, independent of rule indexing. -/
namespace ParserEngine.SchemeTableEvaluation
open WalkSource

def EvalClaim (env : Environment) (before : Heap) (expression : Datum) (value : Value) (after : Heap) : Prop :=
  ∀ code other last, expression = code → Eval env before code other last → value = other ∧ after = last

def ArgsClaim (env : Environment) (before : Heap) (expressions : List Datum) (values : List Value) (after : Heap) : Prop :=
  ∀ others last, Arguments env before expressions others last → values = others ∧ after = last

def SeqClaim (env : Environment) (before : Heap) (expressions : List Datum) (value : Value) (after : Heap) : Prop :=
  ∀ other last, Sequence env before expressions other last → value = other ∧ after = last

def EachClaim (env : Environment) (name : String) (expressions : List Datum) (before : Heap)
    (values : List Value) (after : Heap) : Prop :=
  ∀ last, Each env name expressions before values last → after = last

theorem Sequence.nonempty (evaluated : Sequence env before expressions value after) : expressions ≠ [] := by
  cases evaluated <;> simp

theorem Eval.unique (first : Eval env before expression value after) : EvalClaim env before expression value after := by
  induction first using Eval.rec
      (motive_2 := fun env before expressions values after _ => ArgsClaim env before expressions values after)
      (motive_3 := fun env before expressions value after _ => SeqClaim env before expressions value after)
      (motive_4 := fun env name expressions before values after _ => EachClaim env name expressions before values after) <;>
    (dsimp only [EvalClaim, ArgsClaim, SeqClaim, EachClaim] at *
     intros
     rename_i second
     cases second <;> grind [app, primitive, Sequence.nonempty])

theorem Eval.deterministic (first : Eval env before expression value after)
    (second : Eval env before expression other last) : value = other ∧ after = last :=
  first.unique expression other last rfl second
end ParserEngine.SchemeTableEvaluation
