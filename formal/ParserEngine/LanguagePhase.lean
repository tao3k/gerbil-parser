import LeanPoo.Functional.Requirements

/-! Descriptor lookup and request execution must not request declaration
constructors. This is a capability-selection contract, not a claim that
arbitrary Scheme initializers may be reordered or suppressed. The host owns
module evaluation; source tests must check the actual requested closure. -/
namespace ParserEngine.LanguagePhase
open LeanPoo.Functional
universe u v w x
variable {Context : Type u} {Key : Type v} {Value : Context → Key → Type w}

/-- Providers agree on exactly the requested capabilities, including failures.
No assumption is made about capabilities outside this list. -/
def Agrees (left right : Provider Context Key Value) (keys : List Key) : Prop :=
  ∀ key ∈ keys, left key = right key

theorem prepare_preserved (left right : Provider Context Key Value) (keys : List Key)
    (same : Agrees left right keys) :
    Requirements.prepare left keys = Requirements.prepare right keys := by
  apply Requirements.prepare_congr
  exact same

/-- The consumer retains identical factories, errors and execution bodies at
arbitrary contexts. This does not prove a compiler's source extraction. -/
theorem consumer_preserved (left right : Provider Context Key Value) (keys : List Key)
    (same : Agrees left right keys) {Output : Context → Type x}
    (consume : ∀ context, Requirements.Results Value context keys → Output context) :
    Requirements.prepareWith left keys consume = Requirements.prepareWith right keys consume := by
  unfold Requirements.prepareWith
  rw [prepare_preserved left right keys same]

/-- Removing a constructor capability outside the request is unobservable to
preparation. A missing requested capability still produces the same error. -/
theorem outside_request (provider : Provider Context Key Value) (keys : List Key)
    (constructor : Key) [DecidableEq Key] (absent : constructor ∉ keys) :
    Requirements.prepare (fun key => if key = constructor then none else provider key) keys =
      Requirements.prepare provider keys := by
  apply prepare_preserved
  intro key member
  have different : key ≠ constructor := by
    intro same
    subst key
    exact absent member
  simp [different]

end ParserEngine.LanguagePhase
