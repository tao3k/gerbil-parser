import LeanPoo.Functional.Requirements
import ParserEngine.Recognition

namespace ParserEngine


inductive Action where
  | field (name : String)
  | alias (kind : String)
  deriving DecidableEq, Repr

structure Context where
  children : Children
  offset : Nat

def executeAction : Action → Context → Children
  | .field name, ctx => field name ctx.children ctx.offset
  | .alias kind, ctx => alias kind ctx.children ctx.offset

def provider : LeanPoo.Functional.Provider Context Action (fun _ _ => Children) :=
  fun action => some (executeAction action)

def interpretActions : List Action → Context → Children
  | [], ctx => ctx.children
  | action :: rest, ctx =>
    interpretActions rest { ctx with children := executeAction action ctx }

abbrev Plan (actions : List Action) := LeanPoo.Functional.Requirements.Factories Context (fun _ _ => Children) actions

/-- Compose selected factories in declaration order, preserving the request offset. -/
def executePlan : {actions : List Action} → Plan actions → Context → Children
  | [], _, ctx => ctx.children
  | _ :: _, (factory, tail), ctx =>
    executePlan tail { ctx with children := factory ctx }

theorem selected_plan_equivalent (actions : List Action) (plan : Plan actions)
    (selected : LeanPoo.Functional.Requirements.Selected provider actions plan) (ctx : Context) :
    executePlan plan ctx = interpretActions actions ctx := by
  induction actions generalizing ctx with
  | nil => rfl
  | cons action rest ih =>
    rcases plan with ⟨factory, tail⟩
    rcases selected with ⟨head, selectedTail⟩
    have same : executeAction action = factory := Option.some.inj head
    subst factory
    exact ih tail selectedTail { ctx with children := executeAction action ctx }

/-- Reuse LeanPoo's preparation provenance theorem, rather than postulating a plan. -/
theorem prepared_plan_equivalent (actions : List Action) (plan : Plan actions)
    (ready : LeanPoo.Functional.Requirements.prepare provider actions = .ok plan) (ctx : Context) :
    executePlan plan ctx = interpretActions actions ctx :=
  selected_plan_equivalent actions plan ((LeanPoo.Functional.Requirements.prepare_ok_iff _ _ _).mp ready) ctx

theorem preparation_total (actions : List Action) :
    ∃ plan : Plan actions, LeanPoo.Functional.Requirements.prepare provider actions = .ok plan := by
  induction actions with
  | nil => exact ⟨PUnit.unit, rfl⟩
  | cons action rest ih =>
    obtain ⟨tail, ready⟩ := ih
    exact ⟨(executeAction action, tail), by simp [LeanPoo.Functional.Requirements.prepare, provider, ready]⟩

/-- Count dynamic action-kind selections, not CPU instructions or factory-body work. -/
def interpretedSelections : List Action → Nat
  | [] => 0
  | _ :: rest => 1 + interpretedSelections rest

theorem interpreted_selections_exact (actions : List Action) :
    interpretedSelections actions = actions.length := by
  induction actions with
  | nil => rfl
  | cons action rest ih => simp [interpretedSelections, ih, Nat.add_comm]

/-- The retained executor calls factories directly; it performs no provider selection. -/
def retainedSelections {actions : List Action} (_plan : Plan actions) : Nat := 0

theorem retained_selections_zero {actions : List Action} (plan : Plan actions) :
    retainedSelections plan = 0 := rfl

end ParserEngine
