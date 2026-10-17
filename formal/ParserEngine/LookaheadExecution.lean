import ParserEngine.ProducerAdmission

/-! Native graph generation and FIFO lookahead execution. Admitted LR state
coordinates, primitive storage/hash/queue operations and host behavior are
explicit boundaries. Static FIRST evidence is activation guarded. -/
namespace ParserEngine.LookaheadExecution.Graph
open Grammar
structure Input where
  nodes : List (Nat × Nat)
  symbol : Nat → Option Symbol
  productions : Nat → List Nat
  initialCore : Nat → Nat
  transition : Nat → Symbol → Option Nat
  nodeIndex : Nat → Nat → Option Nat
structure Edges where
  closure : List Nat
  shift : Option Nat
  deriving DecidableEq, Repr

def predictions (input : Input) (state name : Nat) : Option (List Nat) :=
  (input.productions name).mapM fun production => input.nodeIndex state (input.initialCore production)

def closureRows (input : Input) (state core : Nat) : Option (List Nat) :=
  match input.symbol core with
  | some (.nonterminal name) => predictions input state name
  | _ => some []
def shiftNode (input : Input) (state core : Nat) : Option (Option Nat) :=
  match input.symbol core with
  | none => some none
  | some symbol => (input.transition state symbol).bind fun targetState =>
      (input.nodeIndex targetState (core + 1)).bind fun target => some (some target)
def build (input : Input) (state core : Nat) : Option Edges :=
  (closureRows input state core).bind fun closure =>
    (shiftNode input state core).bind fun shift => some ⟨closure, shift⟩

def generate (input : Input) : Option (List Edges) :=
  input.nodes.mapM fun coordinate => build input coordinate.1 coordinate.2

theorem predictions_members (input : Input) (state name : Nat) (targets : List Nat)
    (execution : predictions input state name = some targets) (target : Nat) :
    target ∈ targets ↔ ∃ production ∈ input.productions name,
      input.nodeIndex state (input.initialCore production) = some target := by
  unfold predictions at execution
  generalize input.productions name = productions at *
  induction productions generalizing targets with
  | nil => simp at execution; subst targets; simp
  | cons head rest ih =>
    cases first : input.nodeIndex state (input.initialCore head) with
    | none => simp [List.mapM_cons, first] at execution
    | some value =>
      cases tail : rest.mapM (fun production => input.nodeIndex state (input.initialCore production)) with
      | none => simp [List.mapM_cons, first, tail] at execution
      | some values =>
        simp [List.mapM_cons, first, tail] at execution
        subst targets
        have exact := ih values tail
        constructor
        · intro member
          rcases List.mem_cons.mp member with equal | member
          · exact ⟨head, by simp, by simpa [← equal] using first⟩
          · obtain ⟨production, member, found⟩ := exact.mp member
            exact ⟨production, by simp [member], found⟩
        · rintro ⟨production, member, found⟩
          rcases List.mem_cons.mp member with equal | member
          · subst production; exact List.mem_cons.mpr (Or.inl (Option.some.inj (found.symm.trans first)))
          · exact List.mem_cons.mpr (Or.inr (exact.mpr ⟨production, member, found⟩))

theorem generated_reads (input : Input) (rows : List Edges) (execution : generate input = some rows) :
    rows.length = input.nodes.length ∧ ∀ (index state core : Nat), input.nodes[index]? = some (state, core) →
      ∃ row, rows[index]? = some row ∧ build input state core = some row := by
  unfold generate at execution
  generalize input.nodes = nodes at *
  induction nodes generalizing rows with
  | nil => simp at execution; subst rows; simp
  | cons head rest ih =>
    cases first : build input head.1 head.2 with
    | none => simp [List.mapM_cons, first] at execution
    | some value =>
      cases tail : rest.mapM (fun coordinate => build input coordinate.1 coordinate.2) with
      | none => simp [List.mapM_cons, first, tail] at execution
      | some values =>
        simp [List.mapM_cons, first, tail] at execution
        subst rows
        obtain ⟨size, checked⟩ := ih values tail
        constructor
        · simp [size]
        · intro index state core present
          cases index with
          | zero =>
            have equal : head = (state, core) := by simpa using present
            subst head
            exact ⟨value, rfl, first⟩
          | succ index =>
            obtain ⟨row, stored, built⟩ := checked index state core (by simpa using present)
            exact ⟨row, by simpa using stored, built⟩

theorem shift_exact (input : Input) (state core : Nat) (row : Edges) (built : build input state core = some row)
    (symbol : Symbol) (afterDot : input.symbol core = some symbol) :
    ∃ targetState target, input.transition state symbol = some targetState ∧
      input.nodeIndex targetState (core + 1) = some target ∧ row.shift = some target := by
  unfold build at built
  obtain ⟨closure, _, rest⟩ := Option.bind_eq_some_iff.mp built
  obtain ⟨shift, emitted, result⟩ := Option.bind_eq_some_iff.mp rest
  have shape : row = ⟨closure, shift⟩ := (Option.some.inj result).symm
  simp only [shiftNode, afterDot] at emitted
  obtain ⟨targetState, transition, rest⟩ := Option.bind_eq_some_iff.mp emitted
  obtain ⟨target, found, same⟩ := Option.bind_eq_some_iff.mp rest
  exact ⟨targetState, target, transition, found, by simpa [shape] using (Option.some.inj same).symm⟩

/-- Cached predictions are valid only in their owning state. Empty prediction
lists are present cache entries, matching Scheme's truthy empty list. -/
structure Cache where
  values : Nat → Option (List Nat)
  touched : List Nat

def Valid (compute : Nat → Option (List Nat)) (cache : Cache) : Prop :=
  (∀ key values, cache.values key = some values → compute key = some values) ∧
    (∀ key, cache.values key ≠ none → key ∈ cache.touched)
def save (cache : Cache) (key : Nat) (value : List Nat) : Cache :=
  ⟨fun other => if other = key then some value else cache.values other, key :: cache.touched⟩
def fetch (compute : Nat → Option (List Nat)) (cache : Cache) (key : Nat) : Option (List Nat × Cache) :=
  match cache.values key with
  | some values => some (values, cache)
  | none => (compute key).map fun values => (values, save cache key values)

theorem fetch_exact (compute : Nat → Option (List Nat)) (cache : Cache) (valid : Valid compute cache)
    (key : Nat) (values : List Nat) (next : Cache) (execution : fetch compute cache key = some (values, next)) :
    compute key = some values ∧ Valid compute next := by
  cases previous : cache.values key with
  | some value =>
    simp [fetch, previous] at execution
    obtain ⟨rfl, rfl⟩ := execution
    exact ⟨valid.1 key value previous, valid⟩
  | none =>
    cases computed : compute key with
    | none => simp [fetch, previous, computed] at execution
    | some value =>
      simp [fetch, previous, computed] at execution
      obtain ⟨rfl, rfl⟩ := execution
      refine ⟨rfl, ?_, ?_⟩
      · intro other result found
        by_cases same : other = key
        · subst other; simp only [save, ↓reduceIte, Option.some.injEq] at found; simpa [found] using computed
        · exact valid.1 other result (by simpa [save, same] using found)
      · intro other present
        by_cases same : other = key
        · simp [save, same]
        · exact List.mem_cons_of_mem _ (valid.2 other (by simpa [save, same] using present))

def clear (keys : List Nat) (values : Nat → Option Value) : Nat → Option Value :=
  fun key => if key ∈ keys then none else values key

theorem clear_exact (compute : Nat → Option (List Nat)) (cache : Cache) (valid : Valid compute cache) :
    ∀ key, clear cache.touched cache.values key = none := by
  intro key
  by_cases present : key ∈ cache.touched
  · simp [clear, present]
  · simp [clear, present]
    cases found : cache.values key with
    | none => rfl
    | some value => exact False.elim (present (valid.2 key (by simp [found])))
#print axioms predictions_members
#print axioms generated_reads
#print axioms shift_exact
#print axioms fetch_exact
#print axioms clear_exact
end ParserEngine.LookaheadExecution.Graph

namespace ParserEngine.LookaheadExecution.Machine
open Encoding
structure Graph where
  closure : Nat → List Nat
  shift : Nat → Option Nat
  first : Nat → Nat
  nullable : Nat → Bool

inductive Emits (graph : Graph) (source bit : Nat) : Nat → Nat → Prop where
  | shift {target} : graph.shift source = some target → Emits graph source bit target bit
  | follow {target} : target ∈ graph.closure source → graph.nullable source = true → Emits graph source bit target bit
  | first {target terminal} : target ∈ graph.closure source → (graph.first source).testBit terminal = true → Emits graph source bit target terminal
inductive Reach (graph : Graph) (seedNode seedMask : Nat) : Nat → Nat → Prop where
  | seed {bit} : seedMask.testBit bit = true → Reach graph seedNode seedMask seedNode bit
  | emit {source bit target terminal} : Reach graph seedNode seedMask source bit → Emits graph source bit target terminal → Reach graph seedNode seedMask target terminal

/-- FIFO differs from Encoding.publish, which is intentionally not reused. -/
def enqueue (queue : Queue) (owner evidence : Nat) : Queue :=
  let before := queue.cells owner
  if difference evidence before.known = 0 then queue else
    ⟨fun index => if index = owner then merge before evidence else queue.cells index,
      if before.pending = 0 then queue.work ++ [owner] else queue.work⟩

theorem enqueue_known (queue : Queue) (owner evidence index : Nat) :
    ((enqueue queue owner evidence).cells index).known =
      if index = owner then (queue.cells index).known ||| evidence else (queue.cells index).known := by
  unfold enqueue
  dsimp only
  split
  next empty =>
    by_cases same : index = owner
    · subst index
      have equal := delta_conservation evidence (queue.cells owner).known
      simpa [empty] using equal
    · simp [same]
  next => by_cases same : index = owner <;> simp [same]

theorem enqueue_monotone (queue : Queue) (owner evidence index bit : Nat)
    (known : (queue.cells index).known.testBit bit = true) :
    ((enqueue queue owner evidence).cells index).known.testBit bit = true := by
  rw [enqueue_known]
  split <;> simp [known]

def sent (queue : Queue) (owner : Nat) : Nat := difference (queue.cells owner).known (queue.cells owner).pending

theorem enqueue_sent (queue : Queue) (owner evidence index : Nat) : sent (enqueue queue owner evidence) index = sent queue index := by
  unfold enqueue
  dsimp only
  split
  · rfl
  next =>
    by_cases same : index = owner
    · subst index; simpa only [sent, ↓reduceIte] using merge_delivered (queue.cells owner) evidence
    · simp [sent, same]

theorem enqueue_queue (queue : Queue) (valid : QueueValid queue) (owner evidence : Nat) : QueueValid (enqueue queue owner evidence) := by
  unfold enqueue
  dsimp only
  split
  · exact valid
  next fresh =>
    constructor
    · split
      next empty =>
        have absent : owner ∉ queue.work := fun member => (valid.support owner).mp member empty
        apply List.nodup_append.mpr
        refine ⟨valid.unique, by simp, ?_⟩
        intro other member value listed equal
        have same : value = owner := by simpa using listed
        subst value
        subst other
        exact absent member
      next => exact valid.unique
    · intro index
      by_cases same : index = owner
      · subst index
        have nonzero : ¬ ((queue.cells owner).pending ||| difference evidence (queue.cells owner).known) = 0 := by
          intro zero; exact fresh (Nat.or_eq_zero_iff.mp zero).2
        split
        · simp [merge, nonzero]
        · simpa [merge, nonzero] using (valid.support owner).mpr (by assumption)
      · split <;> simpa [same, eq_comm] using valid.support index

def PendingKnown (queue : Queue) : Prop := ∀ node bit, (queue.cells node).pending.testBit bit = true → (queue.cells node).known.testBit bit = true

theorem enqueue_pending (queue : Queue) (valid : PendingKnown queue) (owner evidence : Nat) : PendingKnown (enqueue queue owner evidence) := by
  intro index bit pending
  by_cases empty : difference evidence (queue.cells owner).known = 0
  · simpa [enqueue, empty] using valid index bit (by simpa [enqueue, empty] using pending)
  · by_cases same : index = owner
    · subst index
      have given : (merge (queue.cells owner) evidence).pending.testBit bit = true := by simpa [enqueue, empty] using pending
      simpa [enqueue, empty] using pending_conserved (queue.cells owner) evidence bit (valid owner bit) given
    · simpa [enqueue, empty, same] using valid index bit (by simpa [enqueue, empty, same] using pending)

structure State where
  queue : Queue
  activated : Nat → Bool

def publish (state : State) (owner evidence : Nat) : State := {state with queue := enqueue state.queue owner evidence}
def publications : State → List (Nat × Nat) → State
  | state, [] => state
  | state, (owner, evidence) :: rest => publications (publish state owner evidence) rest

theorem publications_monotone (state : State) (events : List (Nat × Nat)) (node bit : Nat)
    (known : (state.queue.cells node).known.testBit bit = true) :
    ((publications state events).queue.cells node).known.testBit bit = true := by
  induction events generalizing state with
  | nil => exact known
  | cons event rest ih => exact ih (publish state event.1 event.2) (enqueue_monotone _ _ _ _ _ known)

theorem publications_sent (state : State) (events : List (Nat × Nat)) (node : Nat) :
    sent (publications state events).queue node = sent state.queue node := by
  induction events generalizing state with
  | nil => rfl
  | cons event rest ih => rw [publications, ih]; exact enqueue_sent _ _ _ _

theorem publications_cover (state : State) (events : List (Nat × Nat)) (event : Nat × Nat) (member : event ∈ events)
    (bit : Nat) (evidence : event.2.testBit bit = true) :
    ((publications state events).queue.cells event.1).known.testBit bit = true := by
  induction events generalizing state with
  | nil => simp at member
  | cons head rest ih =>
    rcases List.mem_cons.mp member with equal | present
    · subst head
      exact publications_monotone (publish state event.1 event.2) rest event.1 bit
        (by simp [publish, enqueue_known, evidence])
    · exact ih (publish state head.1 head.2) present

def Sound (graph : Graph) (seedNode seedMask : Nat) (state : State) : Prop :=
  ∀ node bit, (state.queue.cells node).known.testBit bit = true → Reach graph seedNode seedMask node bit
structure Weak (graph : Graph) (seedNode seedMask : Nat) (state : State) : Prop where
  queue : QueueValid state.queue
  pending : PendingKnown state.queue
  sound : Sound graph seedNode seedMask state

theorem publish_weak (graph : Graph) (seedNode seedMask : Nat) (state : State) (valid : Weak graph seedNode seedMask state)
    (owner evidence : Nat) (origin : ∀ bit, evidence.testBit bit = true → Reach graph seedNode seedMask owner bit) :
    Weak graph seedNode seedMask (publish state owner evidence) := by
  refine ⟨enqueue_queue _ valid.queue _ _, enqueue_pending _ valid.pending _ _, ?_⟩
  intro node bit known
  change ((enqueue state.queue owner evidence).cells node).known.testBit bit = true at known
  rw [enqueue_known] at known
  by_cases same : node = owner
  · subst node
    simp only [↓reduceIte, Nat.testBit_or, Bool.or_eq_true] at known
    exact known.elim (valid.sound owner bit) (origin bit)
  · exact valid.sound node bit (by simpa [same] using known)

theorem publications_weak (graph : Graph) (seedNode seedMask : Nat) (state : State) (valid : Weak graph seedNode seedMask state)
    (events : List (Nat × Nat)) (origins : ∀ event ∈ events, ∀ bit, event.2.testBit bit = true → Reach graph seedNode seedMask event.1 bit) :
    Weak graph seedNode seedMask (publications state events) := by
  induction events generalizing state with
  | nil => exact valid
  | cons event rest ih =>
    apply ih (publish state event.1 event.2) (publish_weak _ _ _ _ valid _ _ (origins event (by simp)))
    intro next present; exact origins next (by simp [present])

def closureEvidence (graph : Graph) (state : State) (node delta : Nat) : Nat :=
  if !state.activated node then
    if graph.nullable node then graph.first node ||| delta else graph.first node
  else if graph.nullable node then delta else 0

def callbacks (graph : Graph) (state : State) (node delta : Nat) : List (Nat × Nat) :=
  (if (graph.closure node).isEmpty || closureEvidence graph state node delta == 0 then []
    else (graph.closure node).map fun target => (target, closureEvidence graph state node delta)) ++
  (match graph.shift node with | none => [] | some target => [(target, delta)])

def drained (graph : Graph) (state : State) (node : Nat) (tail : List Nat) : State :=
  ⟨drain state.queue node tail,
    fun other => if other = node && !(graph.closure node).isEmpty then true else state.activated other⟩

def step (graph : Graph) (state : State) (node : Nat) (tail : List Nat) : State :=
  publications (drained graph state node tail) (callbacks graph state node (state.queue.cells node).pending)
end ParserEngine.LookaheadExecution.Machine

namespace ParserEngine.LookaheadExecution.Machine
open Encoding

theorem nonzero_bit (mask : Nat) (nonzero : mask ≠ 0) : ∃ bit, mask.testBit bit = true := by
  apply Classical.byContradiction
  intro missing
  apply nonzero
  apply (mask_zero_iff mask).mpr
  intro bit
  cases flag : mask.testBit bit with
  | false => rfl
  | true => exact False.elim (missing ⟨bit, flag⟩)

theorem evidence_origin (graph : Graph) (state : State) (node delta bit : Nat)
    (observed : (closureEvidence graph state node delta).testBit bit = true) :
    (state.activated node = false ∧ (graph.first node).testBit bit = true) ∨
      (graph.nullable node = true ∧ delta.testBit bit = true) := by
  cases active : state.activated node <;> cases nullable : graph.nullable node <;>
    simp_all [closureEvidence]

theorem callbacks_origin (graph : Graph) (state : State) (node delta seedNode seedMask : Nat)
    (nonzero : delta ≠ 0) (source : ∀ bit, delta.testBit bit = true → Reach graph seedNode seedMask node bit)
    (event : Nat × Nat) (member : event ∈ callbacks graph state node delta)
    (terminal : Nat) (evidence : event.2.testBit terminal = true) : Reach graph seedNode seedMask event.1 terminal := by
  rcases List.mem_append.mp member with closure | shift
  · split at closure
    · simp at closure
    · obtain ⟨target, present, equal⟩ := List.mem_map.mp closure
      subst event
      rcases evidence_origin graph state node delta terminal evidence with first | follow
      · obtain ⟨bit, pending⟩ := nonzero_bit delta nonzero
        exact Reach.emit (source bit pending) (Emits.first present first.2)
      · exact Reach.emit (source terminal follow.2) (Emits.follow present follow.1)
  · cases found : graph.shift node with
    | none => simp [found] at shift
    | some target =>
      have equal : event = (target, delta) := by simpa [found] using shift
      subst event
      exact Reach.emit (source terminal evidence) (Emits.shift found)

theorem closure_callback (graph : Graph) (state : State) (node delta target bit : Nat)
    (present : target ∈ graph.closure node) (evidence : (closureEvidence graph state node delta).testBit bit = true) :
    (target, closureEvidence graph state node delta) ∈ callbacks graph state node delta := by
  have nonempty : (graph.closure node).isEmpty = false := by
    cases row : graph.closure node with
    | nil => simp [row] at present
    | cons => rfl
  have nonzero : closureEvidence graph state node delta ≠ 0 := by intro zero; simp [zero] at evidence
  apply List.mem_append.mpr
  left
  have mapped : (target, closureEvidence graph state node delta) ∈
      (graph.closure node).map (fun target => (target, closureEvidence graph state node delta)) :=
    List.mem_map.mpr ⟨target, present, rfl⟩
  simpa [nonempty, nonzero] using mapped

theorem shift_callback (graph : Graph) (state : State) (node delta target : Nat)
    (found : graph.shift node = some target) : (target, delta) ∈ callbacks graph state node delta := by
  apply List.mem_append.mpr
  right
  simp [found]

theorem nullable_evidence (graph : Graph) (state : State) (node delta bit : Nat)
    (nullable : graph.nullable node = true) (pending : delta.testBit bit = true) :
    (closureEvidence graph state node delta).testBit bit = true := by
  cases flag : state.activated node <;> simp [closureEvidence, flag, nullable, pending]

theorem first_evidence (graph : Graph) (state : State) (node delta bit : Nat)
    (inactive : state.activated node = false) (first : (graph.first node).testBit bit = true) :
    (closureEvidence graph state node delta).testBit bit = true := by
  cases flag : graph.nullable node <;> simp [closureEvidence, inactive, flag, first]

theorem drained_weak (graph : Graph) (state : State) (seedNode seedMask node : Nat) (tail : List Nat)
    (valid : Weak graph seedNode seedMask state) (front : state.queue.work = node :: tail) :
    Weak graph seedNode seedMask (drained graph state node tail) := by
  refine ⟨drain_queue_valid _ valid.queue node tail front, ?_, ?_⟩
  · intro index bit pending
    by_cases same : index = node
    · simp [drained, drain, same, clear] at pending
    · simpa [drained, drain, same] using valid.pending index bit (by simpa [drained, drain, same] using pending)
  · intro index bit known
    apply valid.sound index bit
    by_cases same : index = node <;> simpa [drained, drain, clear, same] using known

theorem step_weak (graph : Graph) (state : State) (seedNode seedMask node : Nat) (tail : List Nat)
    (valid : Weak graph seedNode seedMask state) (front : state.queue.work = node :: tail) :
    Weak graph seedNode seedMask (step graph state node tail) := by
  apply publications_weak graph seedNode seedMask _ (drained_weak _ _ _ _ _ _ valid front)
  intro event present bit evidence
  exact callbacks_origin graph state node (state.queue.cells node).pending seedNode seedMask
    ((valid.queue.support node).mp (by simp [front]))
    (fun bit pending => valid.sound node bit (valid.pending node bit pending)) event present bit evidence

def Closed (graph : Graph) (state : State) : Prop :=
  ∀ source bit, (sent state.queue source).testBit bit = true →
    ∀ target terminal, Emits graph source bit target terminal → (state.queue.cells target).known.testBit terminal = true

def ActiveClosed (graph : Graph) (state : State) : Prop :=
  ∀ source, state.activated source = true → ∀ target ∈ graph.closure source,
    ∀ bit, (graph.first source).testBit bit = true → (state.queue.cells target).known.testBit bit = true

structure Valid (graph : Graph) (seedNode seedMask : Nat) (state : State) : Prop where
  weak : Weak graph seedNode seedMask state
  closed : Closed graph state
  active : ActiveClosed graph state

theorem step_monotone (graph : Graph) (state : State) (node : Nat) (tail : List Nat) (target bit : Nat)
    (known : (state.queue.cells target).known.testBit bit = true) :
    ((step graph state node tail).queue.cells target).known.testBit bit = true := by
  apply publications_monotone
  by_cases same : target = node <;> simpa [drained, drain, clear, same] using known

theorem step_outputs (graph : Graph) (state : State) (active : ActiveClosed graph state)
    (node : Nat) (tail : List Nat) (bit target terminal : Nat)
    (pending : (state.queue.cells node).pending.testBit bit = true) (edge : Emits graph node bit target terminal) :
    ((step graph state node tail).queue.cells target).known.testBit terminal = true := by
  cases edge with
  | shift found => exact publications_cover _ _ _ (shift_callback _ _ _ _ _ found) _ pending
  | follow member nullable =>
    exact publications_cover _ _ _ (closure_callback _ _ _ _ _ _ member (nullable_evidence _ _ _ _ _ nullable pending)) _
      (nullable_evidence _ _ _ _ _ nullable pending)
  | first member first =>
    cases flag : state.activated node with
    | true => exact step_monotone _ _ _ _ _ _ (active node flag target member terminal first)
    | false =>
      exact publications_cover _ _ _ (closure_callback _ _ _ _ _ _ member (first_evidence _ _ _ _ _ flag first)) _
        (first_evidence _ _ _ _ _ flag first)

theorem sent_known (queue : Queue) (node bit : Nat) (known : (queue.cells node).known.testBit bit = true) :
    (sent queue node).testBit bit = true ∨ (queue.cells node).pending.testBit bit = true := by
  cases flag : (queue.cells node).pending.testBit bit <;> simp [sent, difference_bit, flag, known]

theorem drained_sent (graph : Graph) (state : State) (node : Nat) (tail : List Nat) (index : Nat) :
    sent (drained graph state node tail).queue index =
      if index = node then (state.queue.cells node).known else sent state.queue index := by
  by_cases same : index = node
  · subst index
    apply Nat.eq_of_testBit_eq
    intro bit
    simp [sent, drained, drain, clear, difference_bit]
  · simp [sent, drained, drain, same]

theorem publications_activated (state : State) (events : List (Nat × Nat)) :
    (publications state events).activated = state.activated := by
  induction events generalizing state with
  | nil => rfl
  | cons event rest ih => exact ih (publish state event.1 event.2)

theorem step_valid (graph : Graph) (state : State) (seedNode seedMask node : Nat) (tail : List Nat)
    (valid : Valid graph seedNode seedMask state) (front : state.queue.work = node :: tail) :
    Valid graph seedNode seedMask (step graph state node tail) := by
  refine ⟨step_weak _ _ _ _ _ _ valid.weak front, ?_, ?_⟩
  · intro source bit processed target terminal edge
    have delivered := publications_sent (drained graph state node tail)
      (callbacks graph state node (state.queue.cells node).pending) source
    rw [show sent (step graph state node tail).queue source =
      sent (drained graph state node tail).queue source from delivered, drained_sent] at processed
    by_cases same : source = node
    · subst source
      simp only [↓reduceIte] at processed
      rcases sent_known state.queue node bit processed with previous | pending
      · exact step_monotone _ _ _ _ _ _ (valid.closed node bit previous target terminal edge)
      · exact step_outputs _ _ valid.active _ _ _ _ _ pending edge
    · have previous : (Machine.sent state.queue source).testBit bit = true := by simpa [same] using processed
      exact step_monotone _ _ _ _ _ _ (valid.closed source bit previous target terminal edge)
  · intro source activated target member bit first
    rw [show (step graph state node tail).activated = (drained graph state node tail).activated from publications_activated _ _] at activated
    by_cases same : source = node
    · subst source
      obtain ⟨pendingBit, pending⟩ := nonzero_bit (state.queue.cells node).pending
        ((valid.weak.queue.support node).mp (by simp [front]))
      exact step_outputs _ _ valid.active _ _ pendingBit _ _ pending (Emits.first member first)
    · have previous : state.activated source = true := by simpa [drained, same] using activated
      exact step_monotone _ _ _ _ _ _ (valid.active source previous target member bit first)
end ParserEngine.LookaheadExecution.Machine

namespace ParserEngine.LookaheadExecution.Machine
open Encoding

def empty : State := ⟨⟨fun _ => ⟨0, 0⟩, []⟩, fun _ => false⟩
def initial (seedNode seedMask : Nat) : State := publish empty seedNode seedMask

theorem empty_weak (graph : Graph) (seedNode seedMask : Nat) : Weak graph seedNode seedMask empty := by
  refine ⟨⟨by simp [empty], ?_⟩, ?_, ?_⟩
  · intro owner; simp [empty]
  · intro node bit pending; simp [empty] at pending
  · intro node bit known; simp [empty] at known

theorem initial_valid (graph : Graph) (seedNode seedMask : Nat) : Valid graph seedNode seedMask (initial seedNode seedMask) := by
  refine ⟨publish_weak graph seedNode seedMask empty (empty_weak _ _ _) _ _ (fun _ bit => Reach.seed bit), ?_, ?_⟩
  · intro source bit processed
    have zero : sent (initial seedNode seedMask).queue source = 0 := by
      change sent (enqueue empty.queue seedNode seedMask) source = 0
      rw [enqueue_sent]
      apply Nat.eq_of_testBit_eq
      intro terminal
      simp [empty, sent, difference_bit]
    simp [zero] at processed
  · intro source activated; simp [initial, publish, empty] at activated

inductive Trace (graph : Graph) (seedNode seedMask : Nat) : State → Prop where
  | start : Trace graph seedNode seedMask (initial seedNode seedMask)
  | next {state node tail} : Trace graph seedNode seedMask state → state.queue.work = node :: tail →
      Trace graph seedNode seedMask (step graph state node tail)

theorem trace_valid (graph : Graph) (seedNode seedMask : Nat) (last : State)
    (execution : Trace graph seedNode seedMask last) : Valid graph seedNode seedMask last := by
  induction execution with
  | start => exact initial_valid _ _ _
  | next history front ih => exact step_valid _ _ _ _ _ _ ih front

theorem trace_seed (graph : Graph) (seedNode seedMask : Nat) (last : State)
    (execution : Trace graph seedNode seedMask last) (bit : Nat) (seed : seedMask.testBit bit = true) :
    (last.queue.cells seedNode).known.testBit bit = true := by
  induction execution with
  | start => simp [initial, publish, enqueue_known, empty, seed]
  | next history front ih => exact step_monotone _ _ _ _ _ _ ih

/-- Exact least fixed point of the actual activation-guarded FIFO machine.
The premise is its source-shaped execution, not a generic worklist trace or
an assumption that the final set is closed/sound/complete. -/
theorem quiescent_exact (graph : Graph) (seedNode seedMask : Nat) (last : State)
    (execution : Trace graph seedNode seedMask last) (finished : last.queue.work = []) :
    ∀ node bit, (last.queue.cells node).known.testBit bit = true ↔ Reach graph seedNode seedMask node bit := by
  have valid := trace_valid graph seedNode seedMask last execution
  have pending := (queue_empty_exact last.queue valid.weak.queue).mp finished
  intro node bit
  constructor
  · exact valid.weak.sound node bit
  · intro reached
    induction reached with
    | seed seed => exact trace_seed graph seedNode seedMask last execution _ seed
    | @emit source sourceBit target terminal _ edge ih =>
      have delivered : (sent last.queue source).testBit sourceBit = true := by
        simpa [sent, difference_bit, pending source] using ih
      exact valid.closed source sourceBit delivered target terminal edge

#print axioms enqueue_queue
#print axioms step_valid
#print axioms trace_valid
#print axioms quiescent_exact
end ParserEngine.LookaheadExecution.Machine

namespace ParserEngine.LookaheadExecution.Search
/-- Native half-open binary search, with admitted interval bounds. Physical
row ordering belongs to the LR state producer; lookup correctness is proved. -/
def find (read : Nat → Nat) (target low high : Nat) : Option Nat :=
  if low < high then
    let middle := (low + high) / 2
    if read middle = target then some middle
    else if read middle < target then find read target (middle + 1) high
    else find read target low middle
  else none
termination_by high - low

theorem sound (read : Nat → Nat) (target low high index : Nat) (found : find read target low high = some index) :
    low ≤ index ∧ index < high ∧ read index = target := by
  rw [find] at found
  split at found
  next range =>
    dsimp only at found
    split at found
    next equal => cases found; exact ⟨by omega, by omega, equal⟩
    next unequal =>
      split at found
      next smaller =>
        have result := sound read target ((low + high) / 2 + 1) high index found
        exact ⟨by omega, result.2⟩
      next greater =>
        have result := sound read target low ((low + high) / 2) index found
        exact ⟨result.1, by omega, result.2.2⟩
  next => contradiction
termination_by high - low

theorem complete (read : Nat → Nat) (target low high index : Nat)
    (inside : low ≤ index ∧ index < high) (value : read index = target)
    (sorted : ∀ i j, low ≤ i → j < high → i < j → read i < read j) :
    find read target low high = some index := by
  rw [find]
  split
  next range =>
    dsimp only
    split
    next equal =>
      have same : (low + high) / 2 = index := by
        by_cases left : (low + high) / 2 < index
        · have ordered := sorted ((low + high) / 2) index (by omega) inside.2 left
          omega
        · by_cases right : index < (low + high) / 2
          · have ordered := sorted index ((low + high) / 2) inside.1 (by omega) right
            omega
          · omega
      exact congrArg some same
    next unequal =>
      have different : index ≠ (low + high) / 2 := by
        intro same
        exact unequal (by simpa [same] using value)
      split
      next smaller =>
        have nextLow : (low + high) / 2 < index := by
          by_cases left : index < (low + high) / 2
          · have ordered := sorted index ((low + high) / 2) inside.1 (by omega) left
            omega
          · omega
        apply complete read target ((low + high) / 2 + 1) high index ⟨by omega, inside.2⟩ value
        intro i j first last ordered
        exact sorted i j (by omega) last ordered
      next greater =>
        have nextHigh : index < (low + high) / 2 := by
          by_cases right : (low + high) / 2 < index
          · have ordered := sorted ((low + high) / 2) index (by omega) inside.2 right
            omega
          · omega
        apply complete read target low ((low + high) / 2) index ⟨inside.1, nextHigh⟩ value
        intro i j first last ordered
        exact sorted i j first (by omega) ordered
  next => omega
termination_by high - low
#print axioms sound
#print axioms complete
end ParserEngine.LookaheadExecution.Search

namespace ParserEngine.LookaheadExecution.Source
open WalkSource NativeActions.Source

def search : Datum := defn "lookahead-node-index" ["offsets", "node-items", "state", "item"] [
  app "let" [s "search", l [l [s "low", call "vector-ref" ["offsets", "state"]],
    l [s "high", app "vector-ref" [s "offsets", app "+" [s "state", n 1]]]],
    app "if" [call "=" ["low", "high"], b false,
      app "let*" [l [l [s "middle", app "quotient" [call "+" ["low", "high"], n 2]],
        l [s "found", call "vector-ref" ["node-items", "middle"]]],
        app "cond" [l [call "=" ["found", "item"], s "middle"],
          l [call "<" ["found", "item"], app "search" [app "+" [s "middle", n 1], s "high"]],
          l [s "else", call "search" ["low", "middle"]]]]]]]

def predictionMap : Datum := app "map" [app "lambda" [names ["production"],
  app "let*" [l [l [s "target-item", app "make-core-item" [call "production-id" ["production"], n 0, s "layout"]],
    l [s "target", call "lookahead-node-index" ["offsets", "node-items", "state", "target-item"]]],
    app "unless" [s "target", app "error" [.text "LR(0) closure target is not indexed", s "state", s "item", s "target-item"]], s "target"]],
  app "table-ref" [s "productions-by-lhs", call "nonterminal-name" ["symbol"], app "quote" [l []]]]

def nodeLoop : Datum := app "let" [s "node-loop", l [l [s "node", call "vector-ref" ["offsets", "state"]]],
  app "when" [app "<" [s "node", app "vector-ref" [s "offsets", app "+" [s "state", n 1]]],
    app "let*" [l [l [s "item", call "vector-ref" ["node-items", "node"]], l [s "symbol", call "vector-ref" ["core-symbols", "item"]]],
      app "when" [app "and" [s "symbol", call "nonterminal-symbol?" ["symbol"]],
        app "let*" [l [l [s "id", call "vector-ref" ["core-symbol-ids", "item"]],
          l [s "known", call "vector-ref" ["prediction-targets", "id"]], l [s "predictions", app "or" [s "known", predictionMap]]],
          app "unless" [s "known", app "vector-set!" [s "prediction-targets", s "id", s "predictions"],
            app "set!" [s "prediction-order", call "cons" ["id", "prediction-order"]]],
          app "vector-set!" [s "closure-targets", s "node", s "predictions"]]],
      app "when" [s "symbol", app "let*" [l [l [s "target-state", app "vector-ref" [s "targets", call "vector-ref" ["core-symbol-ids", "item"]]],
        l [s "target", app "and" [s "target-state", app "lookahead-node-index" [s "offsets", s "node-items", s "target-state", app "+" [s "item", n 1]]]]],
        app "unless" [s "target", app "error" [.text "LR(0) shift target is not indexed", s "state", s "item", s "symbol", s "target-state"]],
        app "vector-set!" [s "shift-targets", s "node", s "target"]]]],
    app "node-loop" [app "+" [s "node", n 1]]]]

def indexSymbols : Datum := app "let" [s "index-symbols", l [l [s "item", n 0]],
  app "when" [app "<" [s "item", call "vector-length" ["core-symbols"]],
    app "let" [l [s "symbol", call "vector-ref" ["core-symbols", "item"]],
      app "when" [s "symbol", app "let*" [l [l [s "known", app "table-ref" [s "symbol-index", s "symbol", b false]],
        l [s "id", call "or" ["known", "symbol-count"]]],
        app "unless" [s "known", app "table-set!" [s "symbol-index", s "symbol", s "id"], app "set!" [s "symbol-count", app "+" [s "symbol-count", n 1]]],
        app "vector-set!" [s "core-symbol-ids", s "item", s "id"]]]], app "index-symbols" [app "+" [s "item", n 1]]]]

def graph : Datum := defn "make-lookahead-propagation-graph"
    ["state-transitions", "productions-by-lhs", "core-symbols", "layout", "node-items", "offsets"] [
  app "let*" [l [l [s "node-count", call "vector-length" ["node-items"]],
    l [s "closure-targets", app "make-vector" [s "node-count", app "quote" [l []]]],
    l [s "shift-targets", app "make-vector" [s "node-count", b false]],
    l [s "symbol-index", app "make-table" [.keyword "test", s "equal?"]],
    l [s "symbol-count", n 0], l [s "core-symbol-ids", app "make-vector" [call "vector-length" ["core-symbols"], b false]]],
    indexSymbols,
    app "let" [l [l [s "targets", app "make-vector" [s "symbol-count", b false]], l [s "prediction-targets", app "make-vector" [s "symbol-count", b false]]],
      app "let" [s "state-loop", l [l [s "state", n 0]],
        app "when" [app "<" [s "state", app "-" [call "vector-length" ["offsets"], n 1]],
          app "let" [l [s "prediction-order", app "quote" [l []]],
            app "for-each" [app "lambda" [names ["row"], app "let" [l [s "id", app "table-ref" [s "symbol-index", call "car" ["row"], b false]],
              app "unless" [s "id", app "error" [.text "LR transition symbol is not indexed", s "state", s "row"]],
              app "vector-set!" [s "targets", s "id", call "cdr" ["row"]]]], call "vector-ref" ["state-transitions", "state"]],
            nodeLoop,
            app "for-each" [app "lambda" [names ["id"], app "vector-set!" [s "prediction-targets", s "id", b false]], s "prediction-order"],
            app "for-each" [app "lambda" [names ["row"], app "vector-set!" [s "targets", app "table-ref" [s "symbol-index", call "car" ["row"]], b false]],
              call "vector-ref" ["state-transitions", "state"]]],
          app "state-loop" [app "+" [s "state", n 1]]]]], call "values" ["closure-targets", "shift-targets"]]]

def enqueue : Datum := defn "enqueue-mask!" ["node", "evidence"] [
  app "let*" [l [l [s "known", call "vector-ref" ["lookaheads", "node"]], l [s "novel", call "compiler-index-set-difference" ["evidence", "known"]]],
    app "unless" [call "compiler-index-set-empty?" ["novel"],
      app "vector-set!" [s "lookaheads", s "node", call "compiler-index-set-union" ["known", "novel"]],
      app "vector-set!" [s "pending", s "node", app "compiler-index-set-union" [call "vector-ref" ["pending", "node"], s "novel"]],
      app "unless" [call "vector-ref" ["queued", "node"], app "vector-set!" [s "queued", s "node", b true], call "stdq-enqueue!" ["queue", "node"]]]]]

def workLoop : Datum := app "let" [s "loop", l [], app "unless" [call "stdq-queue-empty?" ["queue"],
  app "let*" [l [l [s "node", call "stdq-dequeue!" ["queue"]], l [s "item", call "vector-ref" ["node-items", "node"]],
    l [s "delta", call "vector-ref" ["pending", "node"]]],
    app "vector-set!" [s "queued", s "node", b false], app "vector-set!" [s "pending", s "node", n 0],
    app "set!" [s "processed-count", app "+" [s "processed-count", n 1]],
    app "when" [app "and" [s "trace?", app "zero?" [app "modulo" [s "processed-count", n 10000]]],
      app "display" [.text "[gerbil-parser-lr] lookahead-items="], call "displayln" ["processed-count"], app "force-output" []],
    app "unless" [app "null?" [call "vector-ref" ["closure-targets", "node"]],
      app "let*" [l [l [s "first?", app "not" [call "vector-ref" ["activated", "node"]]],
        l [s "nullable?", app "vector-ref" [s "nullable-suffixes", app "+" [s "item", n 1]]],
        l [s "evidence", app "cond" [l [s "first?", app "if" [s "nullable?", app "compiler-index-set-union" [app "vector-ref" [s "suffix-first-masks", app "+" [s "item", n 1]], s "delta"],
          app "vector-ref" [s "suffix-first-masks", app "+" [s "item", n 1]]]], l [s "nullable?", s "delta"], l [s "else", n 0]]]],
        app "vector-set!" [s "activated", s "node", b true], app "unless" [call "zero?" ["evidence"],
          app "for-each" [app "lambda" [names ["target"], call "enqueue-mask!" ["target", "evidence"]], call "vector-ref" ["closure-targets", "node"]]]]],
    app "let" [l [s "target", call "vector-ref" ["shift-targets", "node"]], app "when" [s "target", call "enqueue-mask!" ["target", "delta"]]], app "loop" []]]]

def schedule : Datum := defn "propagate-lalr-lookaheads"
    ["states", "state-transitions", "state-count", "productions-by-lhs", "table", "core-symbols", "suffix-first-masks", "nullable-suffixes", "terminal-index", "layout"] [
  app "let*" [l [l [s "queue", app "stdq-make-Queue" []], l [s "processed-count", n 0], l [s "started", app "##current-time-point" []],
    l [s "trace?", app "equal?" [app "getenv" [.text "GERBIL_PARSER_LR_TRACE", b false], .text "1"]]],
    app "let-values" [l [l [names ["offsets", "node-items", "node-count"], call "index-lr0-items" ["states", "state-count"]]],
      app "trace-lookahead-phase" [quoted "lookahead-index", s "node-count", s "started", s "trace?"],
      app "let-values" [l [l [names ["closure-targets", "shift-targets"], call "make-lookahead-propagation-graph"
        ["state-transitions", "productions-by-lhs", "core-symbols", "layout", "node-items", "offsets"]]],
        app "trace-lookahead-phase" [quoted "lookahead-graph", s "node-count", s "started", s "trace?"],
        app "let" [l [l [s "lookaheads", app "make-vector" [s "node-count", n 0]], l [s "pending", app "make-vector" [s "node-count", n 0]],
          l [s "queued", app "make-vector" [s "node-count", b false]], l [s "activated", app "make-vector" [s "node-count", b false]]],
          enqueue,
          app "enqueue-mask!" [app "lookahead-node-index" [s "offsets", s "node-items", n 0, app "make-core-item" [n 0, n 0, s "layout"]],
            app "compiler-index-set-singleton" [call "table-ref" ["terminal-index", "+lr-eof+"]]], workLoop,
          app "trace-lookahead-phase" [quoted "lookahead-propagation", s "processed-count", s "started", s "trace?"],
          call "values" ["lookaheads", "offsets", "processed-count"]]]]]]
end ParserEngine.LookaheadExecution.Source

namespace ParserEngine.LookaheadExecutionWitness
open LookaheadExecution WalkSource
set_option autoImplicit false in
theorem source_checked : searchSource = Source.search ∧ graphSource = Source.graph ∧ scheduleSource = Source.schedule := by
  exact ⟨rfl, rfl, rfl⟩
example : replace (s "activated") (s "queued") scheduleSource ≠ Source.schedule := by decide +kernel
example : replace (s "prediction-order") (s "symbol-index") graphSource ≠ Source.graph := by decide +kernel
#print axioms source_checked
end ParserEngine.LookaheadExecutionWitness

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
