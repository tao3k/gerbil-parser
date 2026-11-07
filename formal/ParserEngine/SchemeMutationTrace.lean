import ParserEngine.SchemeEvaluationDeterminism

/-! Generate ordered physical mutations from actual evaluation, including all
recursive calls, selected branches, local declarations and callback executions.
No supplied event list or syntax-envelope equality substitutes for evaluation. -/
namespace ParserEngine.SchemeMutationTrace
open WalkSource SchemeTableEvaluation

inductive Mutation where
  | index (address key : Nat) (value : Value)
  | symbol (address : Nat) (key : String) (value : Value)

def Mutation.apply : Mutation → Heap → Heap
  | .index address key value, heap => write heap address key value
  | .symbol address key value, heap => writeSymbol heap address key value

def Mutation.touches : Mutation → Nat → Key → Prop
  | .index address key _, other, slot => other = address ∧ slot = .index key
  | .symbol address key _, other, slot => other = address ∧ slot = .symbol key

inductive Writes : Heap → List Mutation → Heap → Prop where
  | nil : Writes heap [] heap
  | cons (tail : Writes (mutation.apply before) rest after) : Writes before (mutation :: rest) after

theorem Writes.append (first : Writes before updates middle) (last : Writes middle suffix after) :
    Writes before (updates ++ suffix) after := by
  induction first with
  | nil => exact last
  | cons tail ih => exact .cons (ih last)

def Generated (before after : Heap) : Prop := ∃ updates, Writes before updates after

theorem Generated.pure (heap : Heap) : Generated heap heap := ⟨[], .nil⟩

theorem Generated.append (first : Generated before middle) (last : Generated middle after) : Generated before after := by
  obtain ⟨updates, first⟩ := first
  obtain ⟨suffix, last⟩ := last
  exact ⟨updates ++ suffix, first.append last⟩

theorem Generated.assigned (value : Generated before middle) :
    Generated before (write middle address key result) :=
  value.append ⟨[.index address key result], .cons .nil⟩

/-- Every installed primitive in this restricted evaluator either preserves
its heap or performs the actual single table write named by its arguments. -/
theorem primitive_generated (returned : primitive operation argv before = some (value, after)) :
    Generated before after := by
  unfold primitive at returned
  split at returned <;> cases returned
  all_goals first
    | exact Generated.pure _
    | exact ⟨[.index _ _ _], .cons .nil⟩
    | exact ⟨[.symbol _ _ _], .cons .nil⟩

theorem Generated.primitive (arguments : Generated before middle)
    (returned : primitive operation argv middle = some (value, after)) : Generated before after :=
  arguments.append (primitive_generated returned)

/-- A mutual induction over actual execution generates the whole trace. The
recursive cases thread the same intermediate heaps in source order. -/
theorem eval_generated (execution : Eval env before expression value after) : Generated before after := by
  induction execution using Eval.rec
      (motive_2 := fun before _ _ _ after _ => Generated before after)
      (motive_3 := fun _ before _ _ after _ => Generated before after)
      (motive_4 := fun _ before _ _ after _ => Generated before after)
      (motive_5 := fun _ _ _ before _ after _ => Generated before after)
      (motive_6 := fun _ _ _ before _ _ after _ => Generated before after) <;>
    first
    | assumption
    | exact Generated.pure _
    | apply Generated.assigned; assumption
    | apply Generated.primitive <;> assumption
    | solve | apply Generated.append <;> assumption
    | solve | apply Generated.append <;> first | assumption | apply Generated.append <;> assumption

theorem invoke_generated (execution : Invoke before function argv value after) : Generated before after := by
  cases execution with
  | closure _ body => exact eval_generated (.begin body)
  | recursive _ _ body => exact eval_generated (.begin body)

theorem Mutation.frame (mutation : Mutation) (before : Heap) (untouched : ¬ mutation.touches address key) :
    mutation.apply before address key = before address key := by
  cases mutation <;> simp_all [Mutation.touches, Mutation.apply, write, writeSymbol]

theorem Writes.frame (run : Writes before updates after)
    (untouched : ∀ update ∈ updates, ¬ update.touches address key) : after address key = before address key := by
  induction run with
  | nil => rfl
  | @cons rest after before mutation tail ih =>
      exact (ih (fun update member => untouched update (by simp [member]))).trans
        (Mutation.frame mutation before (untouched mutation (by simp)))

/-- Callback noninterference is checked against its generated footprint, not
an arbitrary assertion that its final store happens to match the first one. -/
theorem evaluation_frame (execution : Eval env before expression value after) :
    ∃ updates, Writes before updates after ∧
      ∀ address key, (∀ update ∈ updates, ¬ update.touches address key) → after address key = before address key := by
  obtain ⟨updates, run⟩ := eval_generated execution
  exact ⟨updates, run, fun _ _ untouched => run.frame untouched⟩

end ParserEngine.SchemeMutationTrace
