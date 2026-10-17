import ParserEngine.LookaheadExecution.GraphMachine
import ParserEngine.ProducerAdmission

/-! Native graph generation and FIFO lookahead execution. Admitted LR state
coordinates, primitive storage/hash/queue operations and host behavior are
explicit boundaries. Static FIRST evidence is activation guarded. -/
namespace ParserEngine.LookaheadExecution.Machine
/-- The extraction witness is only the actual dequeue-ID stream. The checker
generates every mask/state transition and rejects a wrong ID or extra dequeue. -/
def replay (graph : Graph) : State → List Nat → Option State
  | state, [] => some state
  | state, node :: rest => match state.queue.work with
    | [] => none
    | expected :: tail => if node = expected then replay graph (step graph state node tail) rest else none

theorem replay_trace (graph : Graph) (seedNode seedMask : Nat) (witness : List Nat) (before after : State)
    (history : Trace graph seedNode seedMask before) (checked : replay graph before witness = some after) :
    Trace graph seedNode seedMask after := by
  induction witness generalizing before with
  | nil => cases checked; exact history
  | cons node rest ih =>
    cases front : before.queue.work with
    | nil => simp [replay, front] at checked
    | cons expected tail =>
      by_cases same : node = expected
      · subst expected
        simp only [replay, front, ↓reduceIte] at checked
        exact ih (step graph before node tail) (Trace.next history front) checked
      · simp [replay, front, same] at checked

theorem checked_quiescent (graph : Graph) (seedNode seedMask : Nat) (witness : List Nat) (after : State)
    (checked : replay graph (initial seedNode seedMask) witness = some after) (finished : after.queue.work = []) :
    ∀ node bit, (after.queue.cells node).known.testBit bit = true ↔ Reach graph seedNode seedMask node bit :=
  quiescent_exact graph seedNode seedMask after (replay_trace graph seedNode seedMask witness _ after Trace.start checked) finished
#print axioms replay_trace
#print axioms checked_quiescent
end ParserEngine.LookaheadExecution.Machine

namespace ParserEngine.LookaheadExecution.Termination
open Machine Encoding

theorem filter_le (values : List Value) (p q : Value → Bool)
    (monotone : ∀ value ∈ values, p value = true → q value = true) :
    (values.filter p).length ≤ (values.filter q).length := by
  induction values with
  | nil => simp
  | cons head rest ih =>
    have tail := ih (fun value member => monotone value (by simp [member]))
    have first := monotone head (by simp)
    cases a : p head <;> cases b : q head <;> simp_all <;> omega

theorem filter_lt (values : List Value) (p q : Value → Bool)
    (monotone : ∀ value ∈ values, p value = true → q value = true)
    (value : Value) (member : value ∈ values) (before : q value = true) (after : p value = false) :
    (values.filter p).length < (values.filter q).length := by
  induction values with
  | nil => simp at member
  | cons head rest ih =>
    have tailMono : ∀ value ∈ rest, p value = true → q value = true :=
      fun value present => monotone value (by simp [present])
    rcases List.mem_cons.mp member with same | present
    · subst value
      have tail := filter_le rest p q tailMono
      simp [before, after]
      omega
    · have tail := ih tailMono present
      have first := monotone head (by simp)
      cases a : p head <;> cases b : q head <;> simp_all <;> omega

def holes (queue : Queue) (domain : List (Nat × Nat)) : List (Nat × Nat) :=
  domain.filter fun coordinate => !(queue.cells coordinate.1).known.testBit coordinate.2
def rank (state : State) (domain : List (Nat × Nat)) : Nat :=
  2 * (holes state.queue domain).length + state.queue.work.length

theorem enqueue_holes (queue : Queue) (owner evidence : Nat) (coordinate : Nat × Nat) :
    (!((enqueue queue owner evidence).cells coordinate.1).known.testBit coordinate.2) = true →
      (!(queue.cells coordinate.1).known.testBit coordinate.2) = true := by
  intro unknown
  cases old : (queue.cells coordinate.1).known.testBit coordinate.2 with
  | false => rfl
  | true =>
    have known := enqueue_monotone queue owner evidence coordinate.1 coordinate.2 old
    simp [known] at unknown

theorem enqueue_length (queue : Queue) (owner evidence : Nat) :
    (enqueue queue owner evidence).work.length ≤ queue.work.length + 1 := by
  unfold enqueue
  dsimp only
  split
  · omega
  · split <;> simp <;> omega

theorem publish_rank (state : State) (domain : List (Nat × Nat)) (owner evidence : Nat)
    (bounded : ∀ bit, evidence.testBit bit = true → (owner, bit) ∈ domain) :
    rank (publish state owner evidence) domain ≤ rank state domain := by
  by_cases empty : difference evidence (state.queue.cells owner).known = 0
  · simp [Machine.publish, enqueue, empty]
  · obtain ⟨bit, fresh⟩ := nonzero_bit _ empty
    obtain ⟨incoming, absent⟩ := (difference_fresh _ _ _).mp fresh
    have strict := filter_lt domain
      (fun coordinate => !((enqueue state.queue owner evidence).cells coordinate.1).known.testBit coordinate.2)
      (fun coordinate => !(state.queue.cells coordinate.1).known.testBit coordinate.2)
      (fun coordinate _ => enqueue_holes state.queue owner evidence coordinate)
      (owner, bit) (bounded bit incoming) (by simp [absent]) (by simp [enqueue_known, incoming])
    have length := enqueue_length state.queue owner evidence
    simp only [rank, Machine.publish, holes]
    omega

theorem publications_rank (state : State) (domain : List (Nat × Nat)) (events : List (Nat × Nat))
    (bounded : ∀ event ∈ events, ∀ bit, event.2.testBit bit = true → (event.1, bit) ∈ domain) :
    rank (publications state events) domain ≤ rank state domain := by
  induction events generalizing state with
  | nil => exact Nat.le_refl _
  | cons event rest ih =>
    exact Nat.le_trans (ih (publish state event.1 event.2) (fun other member => bounded other (by simp [member])))
      (publish_rank state domain event.1 event.2 (bounded event (by simp)))

theorem drain_rank (graph : Machine.Graph) (state : State) (node : Nat) (tail : List Nat)
    (front : state.queue.work = node :: tail) (domain : List (Nat × Nat)) :
    rank (drained graph state node tail) domain < rank state domain := by
  have equal : holes (drained graph state node tail).queue domain = holes state.queue domain := by
    unfold holes
    congr 1
    funext coordinate
    by_cases same : coordinate.1 = node <;> simp [drained, drain, clear, same]
  change 2 * (holes (drained graph state node tail).queue domain).length + tail.length <
    2 * (holes state.queue domain).length + state.queue.work.length
  rw [equal, front]
  simp only [List.length_cons]
  omega

theorem step_rank (graph : Machine.Graph) (seedNode seedMask : Nat) (state : State) (node : Nat) (tail : List Nat)
    (valid : Machine.Valid graph seedNode seedMask state) (front : state.queue.work = node :: tail)
    (domain : List (Nat × Nat))
    (bounded : ∀ node bit, Reach graph seedNode seedMask node bit → (node, bit) ∈ domain) :
    rank (step graph state node tail) domain < rank state domain := by
  apply Nat.lt_of_le_of_lt (publications_rank (drained graph state node tail) domain _ ?_) (drain_rank graph state node tail front domain)
  intro event member bit evidence
  exact bounded event.1 bit (callbacks_origin graph state node (state.queue.cells node).pending seedNode seedMask
    ((valid.weak.queue.support node).mp (by simp [front]))
    (fun bit pending => valid.weak.sound node bit (valid.weak.pending node bit pending)) event member bit evidence)
end ParserEngine.LookaheadExecution.Termination

namespace ParserEngine.LookaheadExecution.Termination
open Machine
structure Complete (graph : Machine.Graph) (seedNode seedMask : Nat) where
  state : State
  finished : state.queue.work = []
  history : Trace graph seedNode seedMask state

/-- Finite-domain coverage follows from seed and generated-edge admission;
it is not a final fixed-point/termination certificate. -/
theorem reach_bounded (graph : Machine.Graph) (seedNode seedMask : Nat) (domain : List (Nat × Nat))
    (seeds : ∀ bit, seedMask.testBit bit = true → (seedNode, bit) ∈ domain)
    (edges : ∀ source bit, (source, bit) ∈ domain → ∀ target terminal,
      Emits graph source bit target terminal → (target, terminal) ∈ domain) :
    ∀ node bit, Reach graph seedNode seedMask node bit → (node, bit) ∈ domain := by
  intro node bit reached
  induction reached with
  | seed bit => exact seeds _ bit
  | emit before edge ih => exact edges _ _ ih _ _ edge

def finish (graph : Machine.Graph) (seedNode seedMask : Nat) (domain : List (Nat × Nat))
    (bounded : ∀ node bit, Reach graph seedNode seedMask node bit → (node, bit) ∈ domain)
    (state : State) (valid : Machine.Valid graph seedNode seedMask state) (history : Trace graph seedNode seedMask state) :
    Complete graph seedNode seedMask :=
  match front : state.queue.work with
  | [] => ⟨state, front, history⟩
  | node :: tail => finish graph seedNode seedMask domain bounded (step graph state node tail)
      (step_valid graph state seedNode seedMask node tail valid front) (Trace.next history front)
termination_by rank state domain
decreasing_by exact step_rank graph seedNode seedMask state node tail valid front domain bounded

def run (graph : Machine.Graph) (seedNode seedMask : Nat) (domain : List (Nat × Nat))
    (seeds : ∀ bit, seedMask.testBit bit = true → (seedNode, bit) ∈ domain)
    (edges : ∀ source bit, (source, bit) ∈ domain → ∀ target terminal,
      Emits graph source bit target terminal → (target, terminal) ∈ domain) : Complete graph seedNode seedMask :=
  finish graph seedNode seedMask domain (reach_bounded graph seedNode seedMask domain seeds edges)
    (initial seedNode seedMask) (initial_valid graph seedNode seedMask) Trace.start

theorem run_exact (graph : Machine.Graph) (seedNode seedMask : Nat) (domain : List (Nat × Nat))
    (seeds : ∀ bit, seedMask.testBit bit = true → (seedNode, bit) ∈ domain)
    (edges : ∀ source bit, (source, bit) ∈ domain → ∀ target terminal,
      Emits graph source bit target terminal → (target, terminal) ∈ domain) :
    ∀ node bit, ((run graph seedNode seedMask domain seeds edges).state.queue.cells node).known.testBit bit = true ↔
      Reach graph seedNode seedMask node bit :=
  Machine.quiescent_exact graph seedNode seedMask _ (run graph seedNode seedMask domain seeds edges).history
    (run graph seedNode seedMask domain seeds edges).finished
#print axioms step_rank
#print axioms finish
#print axioms run_exact
end ParserEngine.LookaheadExecution.Termination

namespace ParserEngine.LookaheadExecution.Graph
/-- The graph builder's lookup callback is the actual admitted half-open
binary search, rather than an assumed correct abstract node oracle. -/
def searched (input : Input) (offsets items : Nat → Nat) : Input :=
  {input with nodeIndex := fun state core => Search.find items core (offsets state) (offsets (state + 1))}

theorem closure_exact (input : Input) (state core name : Nat) (row : Edges)
    (afterDot : input.symbol core = some (.nonterminal name)) (built : build input state core = some row) :
    predictions input state name = some row.closure := by
  unfold build at built
  obtain ⟨closure, generated, rest⟩ := Option.bind_eq_some_iff.mp built
  obtain ⟨shift, _, result⟩ := Option.bind_eq_some_iff.mp rest
  cases result
  simpa [closureRows, afterDot] using generated

theorem searched_prediction (input : Input) (offsets items : Nat → Nat) (state core name target : Nat) (row : Edges)
    (afterDot : input.symbol core = some (.nonterminal name))
    (built : build (searched input offsets items) state core = some row) (member : target ∈ row.closure) :
    ∃ production ∈ input.productions name,
      offsets state ≤ target ∧ target < offsets (state + 1) ∧ items target = input.initialCore production := by
  have generated := closure_exact (searched input offsets items) state core name row afterDot built
  obtain ⟨production, belongs, indexed⟩ := (predictions_members (searched input offsets items) state name row.closure generated target).mp member
  exact ⟨production, belongs, Search.sound items (input.initialCore production) _ _ target indexed⟩

theorem searched_shift (input : Input) (offsets items : Nat → Nat) (state core : Nat) (symbol : Grammar.Symbol) (row : Edges)
    (afterDot : input.symbol core = some symbol) (built : build (searched input offsets items) state core = some row) :
    ∃ targetState target, input.transition state symbol = some targetState ∧ row.shift = some target ∧
      offsets targetState ≤ target ∧ target < offsets (targetState + 1) ∧ items target = core + 1 := by
  obtain ⟨targetState, target, transition, indexed, shifted⟩ := shift_exact (searched input offsets items) state core row built symbol afterDot
  exact ⟨targetState, target, transition, shifted, Search.sound items (core + 1) _ _ target indexed⟩
#print axioms searched_prediction
#print axioms searched_shift
end ParserEngine.LookaheadExecution.Graph

namespace ParserEngine.LookaheadExecution.Graph
/-- Convert generated immutable adjacency rows and suffix catalogues to the
machine graph. FIRST/nullable addresses use core+1, not the dense node ID. -/
def compile (input : Input) (suffixFirst : Nat → Nat) (suffixNullable : Nat → Bool) : Option Machine.Graph :=
  (generate input).map fun rows =>
    { closure := fun node => ((rows[node]?).getD ⟨[], none⟩).closure
      shift := fun node => ((rows[node]?).getD ⟨[], none⟩).shift
      first := fun node => suffixFirst (((input.nodes[node]?).getD (0, 0)).2 + 1)
      nullable := fun node => suffixNullable (((input.nodes[node]?).getD (0, 0)).2 + 1) }

theorem compiled_node (input : Input) (suffixFirst : Nat → Nat) (suffixNullable : Nat → Bool)
    (graph : Machine.Graph) (generated : compile input suffixFirst suffixNullable = some graph)
    (index state core : Nat) (coordinate : input.nodes[index]? = some (state, core)) :
    ∃ row, build input state core = some row ∧ graph.closure index = row.closure ∧ graph.shift index = row.shift ∧
      graph.first index = suffixFirst (core + 1) ∧ graph.nullable index = suffixNullable (core + 1) := by
  unfold compile at generated
  cases rows : generate input with
  | none => simp [rows] at generated
  | some values =>
    simp only [rows, Option.map_some, Option.some.injEq] at generated
    subst graph
    obtain ⟨row, stored, built⟩ := (generated_reads input values rows).2 index state core coordinate
    exact ⟨row, built, by simp [stored], by simp [stored], by simp [coordinate], by simp [coordinate]⟩
#print axioms compiled_node
end ParserEngine.LookaheadExecution.Graph

namespace ParserEngine.LookaheadExecution.Admission
/-- A restricted source decoder: the complete three owned procedures must
match before graph generation and trace replay are assigned model meaning.
This is not an interpreter for arbitrary Scheme or a compiler correctness theorem. -/
def execute (searchSource graphSource scheduleSource : WalkSource.Datum) (input : Graph.Input)
    (suffixFirst : Nat → Nat) (suffixNullable : Nat → Bool) (seedNode seedMask : Nat) (dequeues : List Nat) :
    Option (Machine.Graph × Machine.State) := do
  if searchSource ≠ Source.search ∨ graphSource ≠ Source.graph ∨ scheduleSource ≠ Source.schedule then none else do
    let graph ← Graph.compile input suffixFirst suffixNullable
    let state ← Machine.replay graph (Machine.initial seedNode seedMask) dequeues
    pure (graph, state)

theorem extracted_quiescent (searchSource graphSource scheduleSource : WalkSource.Datum) (input : Graph.Input)
    (suffixFirst : Nat → Nat) (suffixNullable : Nat → Bool) (seedNode seedMask : Nat) (dequeues : List Nat)
    (graph : Machine.Graph) (state : Machine.State)
    (checked : execute searchSource graphSource scheduleSource input suffixFirst suffixNullable seedNode seedMask dequeues = some (graph, state))
    (finished : state.queue.work = []) :
    Graph.compile input suffixFirst suffixNullable = some graph ∧
      ∀ node bit, (state.queue.cells node).known.testBit bit = true ↔ Machine.Reach graph seedNode seedMask node bit := by
  unfold execute at checked
  split at checked
  · contradiction
  · obtain ⟨generated, built, rest⟩ := Option.bind_eq_some_iff.mp checked
    obtain ⟨after, replayed, result⟩ := Option.bind_eq_some_iff.mp rest
    have same := Option.some.inj result
    cases same
    exact ⟨built, Machine.checked_quiescent graph seedNode seedMask dequeues state replayed finished⟩

/-- Runtime publication reads the solver-owned vector. The physical value is
constructed from the checked execution, not supplied with a final event theorem. -/
def published (state : Machine.State) (store : CompilerExecution.Records.Store) : CompilerExecution.Records.Store :=
  {store with lookaheads := fun node => some (state.queue.cells node).known}

theorem physical_bits (graph : Machine.Graph) (seedNode seedMask : Nat) (witness : List Nat) (state : Machine.State)
    (checked : Machine.replay graph (Machine.initial seedNode seedMask) witness = some state) (finished : state.queue.work = [])
    (store : CompilerExecution.Records.Store) (node width bit : Nat) :
    ∃ mask, (published state store).lookaheads node = some mask ∧
      (bit ∈ CompilerExecution.Records.maskKeys width mask ↔ bit < width ∧ Machine.Reach graph seedNode seedMask node bit) := by
  refine ⟨(state.queue.cells node).known, rfl, ?_⟩
  rw [CompilerExecution.Records.mask_keys, Machine.checked_quiescent graph seedNode seedMask witness state checked finished node bit]
#print axioms extracted_quiescent
#print axioms physical_bits
end ParserEngine.LookaheadExecution.Admission

namespace ParserEngine.LookaheadExecution.Graph
/-- Sequential scratch vector writes, including repeated keys, refine the
logical clear view used by both prediction-list and transition-target caches. -/
def reset : List Nat → (Nat → Option Value) → Nat → Option Value
  | [], values => values
  | key :: rest, values => reset rest (fun other => if other = key then none else values other)

theorem reset_view (keys : List Nat) (values : Nat → Option Value) (key : Nat) :
    reset keys values key = clear keys values key := by
  induction keys generalizing values with
  | nil => simp [reset, clear]
  | cons head rest ih =>
    rw [reset, ih]
    by_cases later : key ∈ rest <;> by_cases same : key = head <;> simp [clear, later, same]

theorem reset_covers (keys : List Nat) (values : Nat → Option Value)
    (covered : ∀ key, values key ≠ none → key ∈ keys) : ∀ key, reset keys values key = none := by
  intro key
  rw [reset_view]
  by_cases member : key ∈ keys
  · simp [clear, member]
  · have absent : values key = none := by
      cases found : values key with
      | none => rfl
      | some value => exact False.elim (member (covered key (by simp [found])))
    simp [clear, member, absent]
#print axioms reset_view
#print axioms reset_covers
end ParserEngine.LookaheadExecution.Graph

namespace ParserEngine.LookaheadExecution.Graph
/-- The next state's prediction computation receives an empty cache for every
key; no equality between the two states' prediction functions is assumed. -/
theorem restart (beforeCompute nextCompute : Nat → Option (List Nat)) (cache : Cache)
    (valid : Valid beforeCompute cache) : Valid nextCompute ⟨reset cache.touched cache.values, []⟩ := by
  have cleared := reset_covers cache.touched cache.values valid.2
  constructor
  · intro key values found
    have impossible : (none : Option (List Nat)) = some values := by simpa only [cleared key] using found
    contradiction
  · intro key present
    exact False.elim (present (cleared key))
#print axioms restart
end ParserEngine.LookaheadExecution.Graph
