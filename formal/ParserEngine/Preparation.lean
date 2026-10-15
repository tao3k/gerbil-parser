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


/-- Stage the action kind and declared name into a retained semantic closure. -/
def stagedAction : Action → Context → Children
  | .field name => fun ctx => field name ctx.children ctx.offset
  | .alias kind => fun ctx => alias kind ctx.children ctx.offset

/-- This is semantic staging, not a theorem about native call cost or allocation. -/
theorem staged_action_equivalent (action : Action) (ctx : Context) :
    stagedAction action ctx = executeAction action ctx := by
  cases action <;> rfl

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

/-- Singleton prepending preserves ordered concatenation without copying its suffix.
    Native tail sharing and allocation are checked separately in Scheme. -/
theorem singleton_prepend_equivalent {α : Type} (child : α) (suffix : List α) :
    [child] ++ suffix = child :: suffix := rfl

variable {Key Target State : Type}

/-- Transpose admitted goto rows once per distinct LHS. The source row remains
first-match authoritative, represented here as an already admitted function. -/
def gotoColumn (rows : List (Key → Option Target)) (key : Key) : List (Option Target) :=
  rows.map (fun row => row key)

theorem goto_column_exact (rows : List (Key → Option Target)) (key : Key) (state : Nat) :
    (gotoColumn rows key)[state]? = (rows[state]?).map (fun row => row key) := by
  simp [gotoColumn, List.getElem?_map]

/-- Reject dense preparation before allocating columns when the product exceeds
its fixed cell budget. A rejected plan retains the original sparse lookup. -/
def admitGotoColumns (states distinctKeys budget : Nat) : Bool :=
  states * distinctKeys ≤ budget

theorem goto_plan_within_budget (states keys budget : Nat)
    (accepted : admitGotoColumns states keys budget = true) : states * keys ≤ budget := by
  simpa [admitGotoColumns] using accepted

/-- Scalar matrix reads replace dynamic key search only under the actual
producer premise. A matching grammar key alone does not establish the cell. -/
theorem retained_goto_cell (source retained : State → Key → Option Target)
    (producer : ∀ state key, retained state key = source state key) (state : State) (key : Key) :
    retained state key = source state key := producer state key

/-- Lexical matches are ordered by exclusive end, precedence and declaration.
A pure fallback consumes one character; a certificate guarantees an admitted
nonfallback match of at least that width. No scanner callback is speculated. -/
structure RankedMatch where
  finish : Nat
  rank : Int
  ordinal : Nat
  deriving DecidableEq, Repr

def rankedBeats (winner loser : RankedMatch) : Prop :=
  loser.finish < winner.finish ∨
  (loser.finish = winner.finish ∧
    (loser.rank < winner.rank ∨
      (loser.rank = winner.rank ∧ winner.ordinal < loser.ordinal)))

theorem certified_fallback_loses (start : Nat) (winner fallback : RankedMatch)
    (fallbackWidth : fallback.finish = start + 1)
    (guaranteed : start + 1 ≤ winner.finish)
    (priority : fallback.rank < winner.rank ∨
      (fallback.rank = winner.rank ∧ winner.ordinal < fallback.ordinal)) :
    rankedBeats winner fallback := by
  unfold rankedBeats
  by_cases longer : fallback.finish < winner.finish
  · exact Or.inl longer
  · exact Or.inr ⟨by omega, priority⟩

/-- Selecting a subset preserves the winner only with an actual admitted
witness for each omitted candidate, not a first-character hint alone. -/
theorem omitted_fallback_has_witness (retained : List RankedMatch) (winner fallback : RankedMatch)
    (admitted : winner ∈ retained) (beats : rankedBeats winner fallback) :
    ∃ candidate ∈ retained, rankedBeats candidate fallback := ⟨winner, admitted, beats⟩

/-- In a singleton monotone DFA, every successful transition retains the
same state. Prepare the continuation predicate and winning rule once; only
source position remains dynamic. Numeric rollback states are excluded. -/
def singletonRun (accept : Char → Bool) : List Char → Nat
  | [] => 0
  | ch :: rest => if accept ch then 1 + singletonRun accept rest else 0

def monotoneSingletonRun (accept : Char → Bool) (active : Bool) : List Char → Nat
  | [] => 0
  | ch :: rest =>
    if active && accept ch then 1 + monotoneSingletonRun accept true rest else 0

theorem singleton_state_elimination (accept : Char → Bool) (source : List Char) :
    monotoneSingletonRun accept true source = singletonRun accept source := by
  induction source with
  | nil => rfl
  | cons ch rest ih =>
    simp only [monotoneSingletonRun, Bool.true_and, singletonRun]
    split <;> simp_all

/-- A prepared ASCII class may replace the generic predicate only with exact
per-character membership. Unknown characters keep the original predicate. -/
theorem prepared_class_membership (original prepared : Char → Bool)
    (producer : ∀ ch, prepared ch = original ch) (source : List Char) :
    singletonRun prepared source = singletonRun original source := by
  have same : prepared = original := funext producer
  rw [same]

/-- Prepared initial product metadata stores candidate identities, not a cached
priority result. The query may read live candidate payloads through `consume`. -/
structure ScanStart (Node Candidate : Type) where
  node : Option Node
  active : Nat
  literal : Option Candidate
  regular : Option Candidate

def prepareScanStart (classify : Char → ScanStart Node Candidate) (ch : Char) :
    (ScanStart Node Candidate → Query → Value) → Query → Value :=
  fun consume query => consume (classify ch) query

theorem prepared_start_query_exact (classify : Char → ScanStart Node Candidate)
    (ch : Char) (consume : ScanStart Node Candidate → Query → Value) (query : Query) :
    prepareScanStart classify ch consume query = consume (classify ch) query := rfl

/-- Eliminating an impossible literal probe needs a source-topology premise.
A missing first trie edge is stronger than losing a priority comparison. -/
theorem absent_literal_probe (regular literal : Query → Option Candidate)
    (choose : Option Candidate → Option Candidate → Option Candidate)
    (leftOnly : ∀ value, choose value none = value)
    (impossible : ∀ query, literal query = none) (query : Query) :
    choose (regular query) (literal query) = regular query := by
  rw [impossible, leftOnly]

/-- A grammar-owned immutable empty-reference datum has one retained digest.
Every caller still receives an independent mutable string in Scheme; this
logical equality concerns contents, not native string aliasing or SHA proof. -/
theorem retained_empty_digest (digest : List α → Value) (consume : Value → Result) :
    consume (digest []) = consume (let retained := digest []; retained) := rfl

/-- A corpus sweep retains one semantic preparation basis per immutable
 grammar/capability pair. Fresh per-input state remains the executor's premise;
 this equality does not authorize reuse of mutable token vectors or parse state. -/
theorem prepared_corpus_preserves (prepare : Grammar → Runtime)
    (execute : Runtime → Input → Output) (grammar : Grammar) (inputs : List Input) :
    inputs.map (fun input => execute (prepare grammar) input) =
      (let runtime := prepare grammar; inputs.map (execute runtime)) := rfl

/-- Counts only the two parser preparations in an equivalence sweep. -/
theorem corpus_preparations_saved (requests : Nat) (multiple : 1 < requests) :
    2 < 2 * requests := by omega

end ParserEngine
