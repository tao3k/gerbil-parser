import ParserEngine.NativeActions

/-! Parser-owned physical storage, resolver replay and validator queue execution.
Primitive Scheme vector/hash/queue operations and compiler/host behavior remain
external. No host VM implementation or timing theorem is claimed. -/
namespace ParserEngine.CompilerExecution.Queue

inductive Reaches (edges : Nat → List Nat) : Nat → Prop where
  | root : Reaches edges 0
  | step {source target} : Reaches edges source → target ∈ edges source → Reaches edges target

/-- remaining is the complement of the native seen vector in the finite state
catalogue. pending contains original state IDs, not discovery ordinals. -/
structure State where
  remaining : List Nat
  pending : List Nat
  deriving DecidableEq, Repr

def rank (state : State) := state.remaining.length + state.pending.length

def enqueue (state : State) (node : Nat) : State :=
  if node ∈ state.remaining then
    ⟨state.remaining.erase node, state.pending ++ [node]⟩ else state

def expand : State → List Nat → State
  | state, [] => state
  | state, node :: rest => expand (enqueue state node) rest

def pop (state : State) : State := {state with pending := state.pending.tail}

theorem enqueue_rank (state : State) (node : Nat) : rank (enqueue state node) = rank state := by
  unfold enqueue
  split
  next present =>
    have positive : 0 < state.remaining.length := List.length_pos_of_mem present
    simp only [rank, List.length_append, List.length_singleton, List.length_erase_of_mem present]
    omega
  next => rfl

theorem expand_rank (state : State) (nodes : List Nat) : rank (expand state nodes) = rank state := by
  induction nodes generalizing state with
  | nil => rfl
  | cons node rest ih => rw [expand, ih, enqueue_rank]

theorem enqueue_remaining (state : State) (node : Nat) : (enqueue state node).remaining ⊆ state.remaining := by
  unfold enqueue
  split
  · exact List.erase_subset
  · exact List.Subset.refl _

theorem expand_remaining (state : State) (nodes : List Nat) : (expand state nodes).remaining ⊆ state.remaining := by
  induction nodes generalizing state with
  | nil => exact List.Subset.refl _
  | cons node rest ih => exact List.Subset.trans (ih _) (enqueue_remaining _ _)

theorem expand_prefix (state : State) (nodes : List Nat) :
    ∃ suffix, (expand state nodes).pending = state.pending ++ suffix := by
  induction nodes generalizing state with
  | nil => exact ⟨[], by simp [expand]⟩
  | cons node rest ih =>
    obtain ⟨suffix, stored⟩ := ih (enqueue state node)
    unfold expand
    unfold enqueue at stored
    split at stored
    next fresh => exact ⟨node :: suffix, by simpa [enqueue, fresh, List.append_assoc] using stored⟩
    next existing => exact ⟨suffix, by simpa [enqueue, existing] using stored⟩

theorem pop_expand (state : State) (nodes : List Nat) (nonempty : state.pending ≠ []) :
    pop (expand state nodes) = expand (pop state) nodes := by
  induction nodes generalizing state with
  | nil => rfl
  | cons node rest ih =>
    have nextNonempty : (enqueue state node).pending ≠ [] := by
      unfold enqueue
      split <;> simp_all
    rw [expand, ih _ nextNonempty, expand]
    congr 1
    unfold enqueue pop
    split
    · cases equation : state.pending with
      | nil => exact False.elim (nonempty equation)
      | cons head tail => rfl
    · rfl

structure Valid (catalogue : List Nat) (edges : Nat → List Nat) (accepts : Nat → Bool) (state : State) : Prop where
  unique : state.remaining.Nodup
  subset : state.remaining ⊆ catalogue
  pending : ∀ node ∈ state.pending, node ∈ catalogue ∧ node ∉ state.remaining
  sound : ∀ node ∈ catalogue, node ∉ state.remaining → Reaches edges node
  closed : ∀ node ∈ catalogue, node ∉ state.remaining →
    node ∈ state.pending ∨ ((∀ target ∈ edges node, target ∉ state.remaining) ∧ accepts node = false)

/-- Mark-before-enqueue is essential: repeated edges and cycles cannot enqueue
an already seen state, and state zero is a successful state identity. -/
theorem enqueue_valid {catalogue edges accepts state} (valid : Valid catalogue edges accepts state)
    (node : Nat) (reachable : Reaches edges node) : Valid catalogue edges accepts (enqueue state node) := by
  unfold enqueue
  split
  next fresh =>
    refine ⟨valid.unique.erase _, List.Subset.trans List.erase_subset valid.subset, ?_, ?_, ?_⟩
    · intro old member
      rcases List.mem_append.mp member with previous | added
      · have known := valid.pending old previous
        exact ⟨known.1, fun present => known.2 (List.mem_of_mem_erase present)⟩
      · have same := List.mem_singleton.mp added
        subst old
        exact ⟨valid.subset fresh, valid.unique.not_mem_erase⟩
    · intro old inside absent
      by_cases equal : old = node
      · simpa [equal] using reachable
      · exact valid.sound old inside (fun present => absent ((List.mem_erase_of_ne equal).mpr present))
    · intro old inside absent
      by_cases equal : old = node
      · exact Or.inl (List.mem_append.mpr (Or.inr (by simp [equal])))
      · have unseen : old ∉ state.remaining := fun present => absent ((List.mem_erase_of_ne equal).mpr present)
        rcases valid.closed old inside unseen with queued | closed
        · exact Or.inl (List.mem_append.mpr (Or.inl queued))
        · exact Or.inr ⟨fun target member present => closed.1 target member (List.mem_of_mem_erase present), closed.2⟩
  next => exact valid

theorem expand_valid {catalogue edges accepts state} (valid : Valid catalogue edges accepts state)
    (nodes : List Nat) (reachable : ∀ node ∈ nodes, Reaches edges node) :
    Valid catalogue edges accepts (expand state nodes) := by
  induction nodes generalizing state with
  | nil => exact valid
  | cons node rest ih =>
    exact ih (enqueue_valid valid node (reachable node (by simp)))
      (fun other member => reachable other (List.mem_cons_of_mem _ member))

theorem expanded_seen (state : State) (unique : state.remaining.Nodup) (nodes : List Nat) :
    ∀ node ∈ nodes, node ∉ (expand state nodes).remaining := by
  induction nodes generalizing state with
  | nil => simp
  | cons head rest ih =>
    have nextUnique : (enqueue state head).remaining.Nodup := by
      unfold enqueue
      split
      · exact unique.erase _
      · exact unique
    intro node member
    rcases List.mem_cons.mp member with same | member
    · subst node
      have absent : head ∉ (enqueue state head).remaining := by
        unfold enqueue
        split
        · exact unique.not_mem_erase
        · assumption
      exact fun present => absent (expand_remaining _ rest present)
    · exact ih (enqueue state head) nextUnique node member

theorem continue_valid {catalogue edges accepts state} (valid : Valid catalogue edges accepts state)
    (owner : Nat) (tail : List Nat) (pending : state.pending = owner :: tail)
    (rejected : accepts owner = false) :
    Valid catalogue edges accepts (pop (expand state (edges owner))) := by
  have ownerKnown := valid.pending owner (by simp [pending])
  have ownerReachable := valid.sound owner ownerKnown.1 ownerKnown.2
  have after := expand_valid valid (edges owner) (fun target member => Reaches.step ownerReachable member)
  obtain ⟨suffix, queue⟩ := expand_prefix state (edges owner)
  have queueExact : (expand state (edges owner)).pending = owner :: (tail ++ suffix) := by simpa [pending] using queue
  refine ⟨after.unique, after.subset, ?_, after.sound, ?_⟩
  · intro node member
    exact after.pending node (by simpa [pop, queueExact] using List.mem_cons_of_mem owner member)
  · intro node inside seen
    rcases after.closed node inside seen with queued | closed
    · by_cases equal : node = owner
      · subst node
        exact Or.inr ⟨expanded_seen state valid.unique (edges owner), rejected⟩
      · apply Or.inl
        change node ∈ (expand state (edges owner)).pending.tail
        rw [queueExact]
        exact (List.mem_cons.mp (queueExact ▸ queued)).resolve_left equal
    · exact Or.inr closed

theorem next_rank {edges : Nat → List Nat} (state : State) (owner : Nat) (tail : List Nat) (pending : state.pending = owner :: tail) :
    rank (pop (expand state (edges owner))) < rank state := by
  obtain ⟨suffix, stored⟩ := expand_prefix state (edges owner)
  have size := expand_rank state (edges owner)
  simp only [rank, pop, stored, pending, List.cons_append, List.tail_cons, List.length_cons, List.length_append] at *
  omega

inductive Result where
  | accepted (node : Nat) (state : State)
  | exhausted (state : State)
  deriving Repr

/-- The native loop scans/enqueues both action and goto targets even on the
accepting iteration, then exits before the next dequeue. No fuel is supplied. -/
theorem pending_shape (state : State) (nonempty : state.pending ≠ []) :
    state.pending = state.pending.headD 0 :: state.pending.tail := by
  cases pending : state.pending <;> simp_all

def run (edges : Nat → List Nat) (accepts : Nat → Bool) (state : State) : Result :=
  if _empty : state.pending = [] then .exhausted state else
    let owner := state.pending.headD 0
    let next := pop (expand state (edges owner))
    if accepts owner then .accepted owner next else run edges accepts next
termination_by rank state
decreasing_by exact next_rank state _ _ (pending_shape state _empty)

theorem run_empty (edges : Nat → List Nat) (accepts : Nat → Bool) (state : State) (pending : state.pending = []) :
    run edges accepts state = .exhausted state := by rw [run]; simp [pending]

theorem run_nonempty (edges : Nat → List Nat) (accepts : Nat → Bool) (state : State)
    (owner : Nat) (tail : List Nat) (pending : state.pending = owner :: tail) :
    run edges accepts state = if accepts owner then .accepted owner (pop (expand state (edges owner)))
      else run edges accepts (pop (expand state (edges owner))) := by rw [run]; simp [pending]

/-- Every possible successful return carries a reachable accepting state;
exhaustion derives closure and rejection, not a supplied final certificate. -/
theorem run_correct {catalogue edges accepts state} (valid : Valid catalogue edges accepts state) :
    match run edges accepts state with
    | .accepted node _ => Reaches edges node ∧ accepts node = true
    | .exhausted last => last.pending = [] ∧ Valid catalogue edges accepts last := by
  cases pending : state.pending with
  | nil =>
    rw [run_empty edges accepts state pending]
    exact ⟨pending, valid⟩
  | cons owner tail =>
    rw [run_nonempty edges accepts state owner tail pending]
    cases flag : accepts owner with
    | true =>
      simp only [↓reduceIte]
      have known := valid.pending owner (by simp [pending])
      exact ⟨valid.sound owner known.1 known.2, flag⟩
    | false =>
      simp only [Bool.false_eq_true, ↓reduceIte]
      exact run_correct (continue_valid valid owner tail pending flag)
termination_by rank state
decreasing_by apply next_rank; assumption

def initial (catalogue : List Nat) : State := enqueue ⟨catalogue, []⟩ 0

theorem initial_valid (catalogue : List Nat) (unique : catalogue.Nodup) :
    Valid catalogue edges accepts (initial catalogue) := by
  apply enqueue_valid (state := ⟨catalogue, []⟩) _ 0 Reaches.root
  refine ⟨unique, List.Subset.refl _, by simp, ?_, ?_⟩
  · intro node inside outside; exact False.elim (outside inside)
  · intro node inside outside; exact False.elim (outside inside)

theorem run_remaining (edges : Nat → List Nat) (accepts : Nat → Bool) (state last : State)
    (execution : run edges accepts state = .exhausted last) : last.remaining ⊆ state.remaining := by
  cases pending : state.pending with
  | nil =>
    rw [run_empty edges accepts state pending] at execution
    cases execution
    exact List.Subset.refl _
  | cons owner tail =>
    rw [run_nonempty edges accepts state owner tail pending] at execution
    cases flag : accepts owner with
    | true => simp [flag] at execution
    | false =>
      have nextExecution : run edges accepts (pop (expand state (edges owner))) = .exhausted last := by
        simpa [flag] using execution
      exact List.Subset.trans (run_remaining edges accepts _ last nextExecution) (expand_remaining state _)
termination_by rank state
decreasing_by apply next_rank; assumption

/-- Exhaustion cannot hide a reachable acceptance when all table edges stay
inside the validated state catalogue. -/
theorem exhaustive_no_accept (catalogue : List Nat) (unique : catalogue.Nodup) (root : 0 ∈ catalogue)
    (edges : Nat → List Nat) (accepts : Nat → Bool)
    (bounded : ∀ node ∈ catalogue, ∀ target ∈ edges node, target ∈ catalogue)
    (last : State) (finished : run edges accepts (initial catalogue) = .exhausted last) :
    ∀ node, Reaches edges node → accepts node = false := by
  have correct := run_correct (initial_valid (edges := edges) (accepts := accepts) catalogue unique)
  rw [finished] at correct
  have rootSeen : 0 ∉ (initial catalogue).remaining := by simp [initial, enqueue, root, unique.not_mem_erase]
  -- The loop only removes unseen entries. The root stays discovered on exit.
  have monotone := run_remaining edges accepts (initial catalogue) last finished
  intro node reached
  have known : node ∈ catalogue ∧ node ∉ last.remaining := by
    induction reached with
    | root => exact ⟨root, fun present => rootSeen (monotone present)⟩
    | @step source target reached edge ih =>
      have closed := (correct.2.closed source ih.1 ih.2).resolve_left (by simp [correct.1])
      exact ⟨bounded source ih.1 target edge, closed.1 target edge⟩
  exact ((correct.2.closed node known.1 known.2).resolve_left (by simp [correct.1])).2

#print axioms enqueue_valid
#print axioms run_correct
#print axioms exhaustive_no_accept
end ParserEngine.CompilerExecution.Queue

namespace ParserEngine.CompilerExecution.Replay
/-- Slot stores and first-touch order are separate from suppressed reductions.
On failure, reductions already recorded by the source remain in private storage;
no later event or row publication executes. -/
structure Store (Action : Type) where
  slots : Nat → Option Action
  reductions : Nat → List Action
  touched : List Nat

def write (slots : Nat → α) (key : Nat) (value : α) : Nat → α :=
  fun index => if index = key then value else slots index

inductive Outcome (Action Error : Type) where
  | ok (store : Store Action)
  | failed (store : Store Action) (error : Error)

def install (resolve : Nat → Action → Action → Except Error Action)
    (isReduction : Action → Bool) (layout : Bool) (store : Store Action) (key : Nat) (incoming : Action) : Outcome Action Error :=
  let recorded := if layout && isReduction incoming then
      {store with reductions := write store.reductions key (incoming :: store.reductions key)} else store
  match store.slots key with
  | none => .ok {recorded with slots := write store.slots key (some incoming), touched := key :: store.touched}
  | some current => match resolve key current incoming with
    | .ok value => .ok {recorded with slots := write store.slots key (some value)}
    | .error error => .failed recorded error

def run (resolve : Nat → Action → Action → Except Error Action)
    (isReduction : Action → Bool) (layout : Bool) : Store Action → List (Nat × Action) → Outcome Action Error
  | store, [] => .ok store
  | store, (key, incoming) :: rest => match install resolve isReduction layout store key incoming with
    | .ok next => run resolve isReduction layout next rest
    | .failed last error => .failed last error

/-- Arbitrary prefix composition preserves the exact failed private store and
never runs the suffix after a resolver exception. -/
theorem run_append (resolve : Nat → Action → Action → Except Error Action)
    (isReduction : Action → Bool) (layout : Bool) (prefixEvents suffix : List (Nat × Action))
    (store : Store Action) :
    run resolve isReduction layout store (prefixEvents ++ suffix) =
      match run resolve isReduction layout store prefixEvents with
      | .ok next => run resolve isReduction layout next suffix
      | .failed last error => .failed last error := by
  induction prefixEvents generalizing store with
  | nil => rfl
  | cons event rest ih =>
    rcases event with ⟨key, incoming⟩
    simp only [List.cons_append, run]
    cases installed : install resolve isReduction layout store key incoming with
    | ok next => exact ih next
    | failed last error => rfl

/-- Logical single-cell replay uses source order and the exact supplied resolver;
it assumes neither associativity nor a final conflict history. -/
def cell (resolve : Nat → Action → Action → Except Error Action) (key : Nat) :
    Option Action → List (Nat × Action) → Except Error (Option Action)
  | before, [] => .ok before
  | before, (target, incoming) :: rest =>
    if target = key then
      match before with
      | none => cell resolve key (some incoming) rest
      | some current => (resolve key current incoming).bind fun value => cell resolve key (some value) rest
    else cell resolve key before rest

theorem slot_projection (resolve : Nat → Action → Action → Except Error Action)
    (isReduction : Action → Bool) (layout : Bool) (events : List (Nat × Action))
    (before after : Store Action) (completed : run resolve isReduction layout before events = .ok after) (key : Nat) :
    cell resolve key (before.slots key) events = .ok (after.slots key) := by
  induction events generalizing before with
  | nil => cases completed; rfl
  | cons event rest ih =>
    rcases event with ⟨target, incoming⟩
    cases previous : before.slots target with
    | none =>
      simp only [run, install, previous] at completed
      have tail := ih _ completed
      by_cases same : target = key
      · subst target
        simpa [cell, previous, write] using tail
      · simpa [cell, same, write, Ne.symm same] using tail
    | some current =>
      cases decision : resolve target current incoming with
      | error error => simp [run, install, previous, decision] at completed
      | ok value =>
        simp only [run, install, previous, decision] at completed
        have tail := ih _ completed
        by_cases same : target = key
        · subst target
          simpa [cell, previous, decision, write, Except.bind] using tail
        · simpa [cell, same, write, Ne.symm same] using tail

/-- Failure preserves old action/touch storage while retaining the earlier raw
reduction insertion. This reflects native mutation order instead of rollback. -/
theorem failure_storage (resolve : Nat → Action → Action → Except Error Action)
    (isReduction : Action → Bool) (store : Store Action) (key : Nat) (incoming current : Action) (error : Error)
    (previous : store.slots key = some current) (decision : resolve key current incoming = .error error)
    (reduction : isReduction incoming = true) :
    install resolve isReduction true store key incoming =
      .failed {store with reductions := write store.reductions key (incoming :: store.reductions key)} error := by
  simp [install, previous, decision, reduction]

#print axioms run_append
#print axioms slot_projection
#print axioms failure_storage
end ParserEngine.CompilerExecution.Replay

namespace ParserEngine.CompilerExecution.Records
open Grammar NativeActions

/-- Option outside a field means a failed physical read; an inner none is the
actual #f/end-of-production payload. These cases must not be conflated. -/
structure Store where
  owners : Nat → Option Nat
  offsets : Nat → Option Nat
  symbols : Nat → Option (Option Grammar.Symbol)
  terminals : Nat → Option (Option Nat)
  targets : Nat → Option (Option Nat)
  precedence : Nat → Option (Option Construction.Priority)
  lookaheads : Nat → Option Nat
  stateItems : Nat → Option (List Nat)
  stateOffsets : Nat → Option Nat
  terminalCount : Nat

structure Extracted where
  item : Events.Observation
  precedence : Option Construction.Priority

def maskKeys (width mask : Nat) : List Nat :=
  (List.range width).filter fun key => mask.testBit key

theorem mask_keys (width mask key : Nat) : key ∈ maskKeys width mask ↔ key < width ∧ mask.testBit key = true := by
  simp [maskKeys]

/-- Source branch order is preserved: shifting reads target/precedence but no
lookahead mask; incomplete nonterminals read neither. -/
def fields (store : Store) (production dot core node : Nat) : Option Extracted := do
  let symbol ← store.symbols core
  let terminal ← store.terminals core
  match terminal with
  | some key => do
      let target ← store.targets key
      let target ← target
      let precedence ← store.precedence production
      pure ⟨⟨production, dot, symbol, some key, some target, []⟩, precedence⟩
  | none =>
      match symbol with
      | some _ => pure ⟨⟨production, dot, symbol, none, none, []⟩, none⟩
      | none => do
          let mask ← store.lookaheads node
          pure ⟨⟨production, dot, none, none, none, maskKeys store.terminalCount mask⟩, none⟩

def read (store : Store) (core node : Nat) : Option Extracted := do
  let production ← store.owners core
  let offset ← store.offsets production
  if offset ≤ core then fields store production (core - offset) core node else none

def scan (store : Store) : List Nat → Nat → Option (List Extracted)
  | [], _ => some []
  | core :: rest, node => do
      let value ← read store core node
      let tail ← scan store rest (node + 1)
      pure (value :: tail)

def state (store : Store) (id : Nat) : Option (List Extracted) := do
  let items ← store.stateItems id
  let offset ← store.stateOffsets id
  scan store items offset

/-- No supplied observation trace: each ordinal read is generated by the scan,
including a shift/nonterminal which does not read its mask but advances node. -/
theorem scan_reads (store : Store) (cores : List Nat) (start : Nat) (values : List Extracted)
    (execution : scan store cores start = some values) :
    values.length = cores.length ∧ ∀ index core, cores[index]? = some core →
      ∃ value, values[index]? = some value ∧ read store core (start + index) = some value := by
  induction cores generalizing start values with
  | nil => simp [scan] at execution; subst values; simp
  | cons head rest ih =>
    cases observed : read store head start with
    | none => simp [scan, observed] at execution
    | some first =>
      cases continued : scan store rest (start + 1) with
      | none => simp [scan, observed, continued] at execution
      | some tail =>
        simp [scan, observed, continued] at execution
        subst values
        have checked := ih (start + 1) tail continued
        constructor
        · simp [checked.1]
        · intro index core member
          cases index with
          | zero =>
            have same : head = core := by simpa using member
            subst core
            exact ⟨first, rfl, by simpa using observed⟩
          | succ index =>
            have found : rest[index]? = some core := by simpa using member
            obtain ⟨value, present, read⟩ := checked.2 index core found
            exact ⟨value, by simpa using present, by simpa [Nat.add_assoc, Nat.add_comm, Nat.add_left_comm] using read⟩

/-- Construct the actual physical owner/offset fields by replaying the admitted
layout writes into arbitrary prior stores; other physical fields are unchanged. -/
def layout (widths : List Nat) (oldOffsets oldOwners : Nat → Option Nat) (fields : Store) : Option Store := do
  let writes ← ItemLayout.Source.fillBlocks 0 0 widths
  pure {fields with
    owners := ItemLayout.Source.writeAll writes oldOwners
    offsets := ItemLayout.Source.writeAll (ItemLayout.Source.indexPass 0 0 widths) oldOffsets}

/-- Full owner/dot normalization follows from real physical writes, for every
valid production interval. It is not an assumption that a final decoder works. -/
theorem dense_read (before after : List Nat) (width dot : Nat) (valid : dot ≤ width)
    (oldOffsets oldOwners : Nat → Option Nat) (data : Store) (node : Nat) :
    ∃ store, layout (before ++ width :: after) oldOffsets oldOwners data = some store ∧
      read store (ItemLayout.cells before + dot) node =
        fields data before.length dot (ItemLayout.cells before + dot) node := by
  obtain ⟨writes, generated, owner⟩ := ItemLayout.Source.physical_owner before after width dot valid oldOwners
  have offset := ItemLayout.Source.physical_offset before (width :: after) oldOffsets
  refine ⟨{data with
    owners := ItemLayout.Source.writeAll writes oldOwners
    offsets := ItemLayout.Source.writeAll (ItemLayout.Source.indexPass 0 0 (before ++ width :: after)) oldOffsets}, ?_, ?_⟩
  · simp [layout, generated]
  · simp [read, owner, offset, fields]

/-- Compose whole-state physical extraction with every source item ordinal. -/
theorem state_reads (store : Store) (id : Nat) (values : List Extracted)
    (execution : state store id = some values) :
    ∃ cores start, store.stateItems id = some cores ∧ store.stateOffsets id = some start ∧
      values.length = cores.length ∧ ∀ index core, cores[index]? = some core →
        ∃ value, values[index]? = some value ∧ read store core (start + index) = some value := by
  obtain ⟨cores, items, rest⟩ := Option.bind_eq_some_iff.mp execution
  obtain ⟨start, offset, scanned⟩ := Option.bind_eq_some_iff.mp rest
  exact ⟨cores, start, items, offset, scan_reads store cores start values scanned⟩

#print axioms mask_keys
#print axioms scan_reads
#print axioms dense_read
#print axioms state_reads
end ParserEngine.CompilerExecution.Records

namespace ParserEngine.CompilerExecution.Queue

def admitted : Result → Bool
  | .accepted _ _ => true
  | .exhausted _ => false

/-- Exact decision equivalence for every finite validated table graph, not just
success soundness. A disconnected acceptance cannot pass the outer validator. -/
theorem acceptance_iff (catalogue : List Nat) (unique : catalogue.Nodup) (root : 0 ∈ catalogue)
    (edges : Nat → List Nat) (accepts : Nat → Bool)
    (bounded : ∀ node ∈ catalogue, ∀ target ∈ edges node, target ∈ catalogue) :
    admitted (run edges accepts (initial catalogue)) = true ↔
      ∃ node, Reaches edges node ∧ accepts node = true := by
  have checked := run_correct (initial_valid (edges := edges) (accepts := accepts) catalogue unique)
  cases execution : run edges accepts (initial catalogue) with
  | accepted node state =>
    rw [execution] at checked
    constructor
    · intro _; exact ⟨node, checked⟩
    · intro _; rfl
  | exhausted state =>
    have impossible := exhaustive_no_accept catalogue unique root edges accepts bounded state execution
    constructor
    · intro contradiction; cases contradiction
    · intro ⟨node, reachable, accepted⟩
      have rejected := impossible node reachable
      simp [accepted] at rejected

#print axioms acceptance_iff
end ParserEngine.CompilerExecution.Queue

namespace ParserEngine.CompilerExecution.Source
open WalkSource NativeActions.Source

def defineValue (name : String) (value : Datum) : Datum := app "def" [s name, value]

def visit : Datum := defn "for-each-shift-target" ["visit", "action"] [
  app "match" [s "action",
    l [pattern "shift" ["target", "_"], call "visit" ["target"]],
    l [pattern "fork" ["branches", "..."], app "for-each" [app "lambda" [names ["branch"],
      call "for-each-shift-target" ["visit", "branch"]], s "branches"]],
    l [pattern "layout-guard" ["primary", "fallback"], call "for-each-shift-target" ["visit", "primary"],
      call "for-each-shift-target" ["visit", "fallback"]], l [s "_", app "void" []]]]
def accept : Datum := defn "accepting-action?" ["action"] [app "match" [s "action",
  l [pattern "accept" [], b true], l [pattern "fork" ["branches", "..."], call "any" ["accepting-action?", "branches"]],
  l [s "_", b false]]]
def queue : Datum := defn "validate-accept-reachability" ["actions", "gotos"] [
  defineValue "states" (call "vector-length" ["actions"]),
  defineValue "seen" (app "make-vector" [s "states", b false]),
  defineValue "pending" (app "stdq-make-Queue" []), defineValue "accepted?" (b false),
  defn "enqueue!" ["state"] [app "unless" [call "vector-ref" ["seen", "state"],
    app "vector-set!" [s "seen", s "state", b true], call "stdq-enqueue!" ["pending", "state"]]],
  app "enqueue!" [n 0], app "let" [s "loop", l [],
    app "unless" [app "or" [s "accepted?", call "stdq-queue-empty?" ["pending"]],
      app "let" [l [s "state", call "stdq-dequeue!" ["pending"]],
        app "set!" [s "accepted?", app "any" [app "lambda" [names ["entry"], app "accepting-action?" [call "cdr" ["entry"]]],
          call "vector-ref" ["actions", "state"]]],
        app "for-each" [app "lambda" [names ["entry"], app "for-each-shift-target" [s "enqueue!", call "cdr" ["entry"]]],
          call "vector-ref" ["actions", "state"]],
        app "for-each" [app "lambda" [names ["entry"], app "enqueue!" [call "cdr" ["entry"]]],
          call "vector-ref" ["gotos", "state"]]], app "loop" []]],
  app "unless" [s "accepted?", app "error" [.text "invalid LR table no reachable acceptance"]]]

def install : Datum := defn "install!" ["terminal-id", "action"] [
  app "when" [app "and" [s "layout?", app "eq?" [call "car" ["action"], quoted "reduce"]],
    app "vector-set!" [s "raw-reductions", s "terminal-id", app "cons" [s "action", call "vector-ref" ["raw-reductions", "terminal-id"]]]],
  app "let" [l [s "current", call "vector-ref" ["state-actions", "terminal-id"]],
    app "if" [s "current", app "vector-set!" [s "state-actions", s "terminal-id",
      app "resolve-action/diagnosed" [s "state-id", call "vector-ref" ["terminal-values", "terminal-id"], s "current", s "action", s "table", s "conflict-policy", s "reject-conflict", s "state-items"]],
      app "begin" [app "vector-set!" [s "state-actions", s "terminal-id", s "action"],
        app "set!" [s "terminal-order", call "cons" ["terminal-id", "terminal-order"]]]]]]
def stateItems : Datum := l [s "state-items", call "vector-ref" ["states", "state-id"]]
def stateGuard : Datum := app "unless" [call "list?" ["state-items"],
  app "error" [.text "LR action state is not an item list", s "state-id", s "state-items"]]
def stateBindings : Datum := l [l [s "terminal-order", app "quote" [l []]],
  l [s "shift-order", app "quote" [l []]],
  l [s "lookahead-node", call "vector-ref" ["lookahead-offsets", "state-id"]]]
def itemWalk : Datum := app "for-each" [NativeActions.Source.event, s "state-items"]

def precedence : Datum := defn "production-precedence" ["value"] [app "car" [call "cddddr" ["value"]]]
end ParserEngine.CompilerExecution.Source

namespace ParserEngine.CompilerExecutionWitness
open WalkSource CompilerExecution
set_option autoImplicit false in
theorem source_checked : queueSource = Source.queue ∧ visitSource = Source.visit ∧ acceptSource = Source.accept ∧
    installSource = Source.install ∧ precedenceSource = Source.precedence ∧
    stateItemsSource = Source.stateItems ∧ stateGuardSource = Source.stateGuard ∧
    stateBindingsSource = Source.stateBindings ∧ itemWalkSource = Source.itemWalk := by
  exact ⟨rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩
example : replace (n 0) (n 1) queueSource ≠ Source.queue := by decide +kernel
example : replace (s "stdq-enqueue!") (s "stdq-dequeue!") queueSource ≠ Source.queue := by decide +kernel
example : replace (s "raw-reductions") (s "state-actions") installSource ≠ Source.install := by decide +kernel
#print axioms source_checked
end ParserEngine.CompilerExecutionWitness

namespace ParserEngine.CompilerExecution.ShiftProjection
open Grammar Construction
/-- A real LR(1) shift witness need not carry EOF lookahead. The native opcode
contains only its target; core projection preserves the actual witness. -/
example :
    let production : Production := ⟨1, [.terminal 7]⟩
    let augmented : Production := ⟨0, [.nonterminal 1]⟩
    let table : Table := ⟨fun _ => [⟨production, 0, some 9⟩], fun _ _ => some 1, fun _ _ => none⟩
    admitEvent [production] 1 augmented table 0 (some 7) (.shift ⟨production, 0, none⟩ 7 1) = true ∧
      (∀ item ∈ table.items 0, item.lookahead ≠ none) := by decide +kernel
end ParserEngine.CompilerExecution.ShiftProjection

namespace ParserEngine.CompilerExecution.Records
open Grammar NativeActions

/-- Coherence at state-item indices and physical reads belongs to the state,
lookahead and catalogue producer, not to a final-table certificate. -/
def PhysicalFacts (store : Store) (g : CFG) (start : Nat) (augmented : Production)
    (table : Table) (id : Nat) (keyOf : Nat → Option Nat) : Prop :=
  ∀ cores offset, store.stateItems id = some cores → store.stateOffsets id = some offset →
    ∀ index core value, cores[index]? = some core → read store core (offset + index) = some value →
      Events.Facts g start augmented table id keyOf value.item

def plan (store : Store) (id : Nat) (choices : Extracted → Construction.Choice)
    (rows : CFG) (keyOf : Nat → Option Nat) : Option TablePublication.Events := do
  let values ← state store id
  Events.plan rows keyOf (values.map fun value => (choices value, value.item))

theorem plan_admitted (store : Store) (id : Nat) (choices : Extracted → Construction.Choice)
    (g : CFG) (start : Nat) (augmented : Production) (table : Table) (keyOf : Nat → Option Nat)
    (physical : PhysicalFacts store g start augmented table id keyOf)
    (events : TablePublication.Events) (execution : plan store id choices (augmented :: g) keyOf = some events) :
    ∀ entry ∈ events, Construction.admitEvent g start augmented table id (keyOf entry.1) entry.2.2 = true := by
  obtain ⟨values, extracted, replay⟩ := Option.bind_eq_some_iff.mp execution
  obtain ⟨cores, offset, items, offsets, size, reads⟩ := state_reads store id values extracted
  apply Events.plan_admitted g start augmented table id keyOf _ _ events replay
  intro entry member
  obtain ⟨value, present, same⟩ := List.mem_map.mp member
  subst entry
  obtain ⟨index, found⟩ := List.mem_iff_getElem?.mp present
  cases coreRead : cores[index]? with
  | none =>
    have outside : cores.length ≤ index := List.getElem?_eq_none_iff.mp coreRead
    have empty : values[index]? = none := List.getElem?_eq_none (by omega)
    simp [found] at empty
  | some core =>
    obtain ⟨observed, valueRead, executed⟩ := reads index core coreRead
    have equal : observed = value := Option.some.inj (valueRead.symm.trans found)
    subst observed
    exact physical cores offset items offsets index core value coreRead executed
end ParserEngine.CompilerExecution.Records

namespace ParserEngine.CompilerExecution
open Grammar
/-- Whole-record generation feeds frozen-row publication. The producer supplies
physical catalogue coherence; support of final actions is derived here. -/
theorem published_from_records (input : TablePublication.Input) (store : Records.Store)
    (g : CFG) (start : Nat) (augmented : Production)
    (choices : Nat → Records.Extracted → Construction.Choice)
    (physical : ∀ state, Records.PhysicalFacts store g start augmented (TablePublication.skeleton input) state input.keyOf)
    (generated : ∀ state events, input.plans[state]? = some events →
      Records.plan store state (choices state) (augmented :: g) input.keyOf = some events)
    (bounds : ∀ (state : Nat) (events : TablePublication.Events), input.plans[state]? = some events →
      ∀ entry ∈ events, entry.1 < input.width) :
    TableSupported g start augmented (TablePublication.physicalTable input) := by
  rw [TablePublication.physical_table_exact]
  apply TablePublication.complete_table input g start augmented bounds
  intro state events stored
  exact Records.plan_admitted store state (choices state) g start augmented (TablePublication.skeleton input)
    input.keyOf (physical state) events (generated state events stored)
#print axioms Records.plan_admitted
#print axioms published_from_records
end ParserEngine.CompilerExecution

namespace ParserEngine.CompilerExecution.ValidatedGraph
open NativeActions
abbrev ActionRows := List (List (Option Nat × Validator.Action))

def targets (action : Validator.Action) : List Nat :=
  (Validator.atoms action).filterMap fun atom => match atom with
    | .shift target _ => some target.toNat
    | _ => none

def accepting : Validator.Action → Bool
  | .atomic .accept => true
  | .fork first second rest => (first :: second :: rest).any fun atom => match atom with | .accept => true | _ => false
  | _ => false

def edges (actions : ActionRows) (gotos : List (List Int)) (state : Nat) : List Nat :=
  ((actions[state]?).getD []).flatMap (fun entry => targets entry.2) ++ ((gotos[state]?).getD []).map Int.toNat

def accepts (actions : ActionRows) (state : Nat) : Bool :=
  ((actions[state]?).getD []).any fun entry => accepting entry.2

theorem target_bounded {ceiling states productions layout key action target}
    (admitted : Validator.action ceiling states productions layout key action = true)
    (present : target ∈ targets action) : target < states := by
  obtain ⟨atom, member, equation⟩ := List.mem_filterMap.mp present
  have checked := Validator.action_members admitted atom member
  cases atom <;> simp only at equation
  case shift value priority =>
    simp only [Option.some.injEq] at equation
    subst target
    simp only [Validator.atomic, Validator.bounded, decide_eq_true_eq] at checked
    omega
  all_goals contradiction

/-- The outer queue's finite-domain premise follows from the actual action and
goto admission layer, rather than assuming a well-behaved graph separately. -/
theorem graph_bounded (actions : ActionRows) (gotos : List (List Int)) (ceiling productions : Nat) (layout : Bool)
    (actionRows : ∀ (state : Nat) (entry : Option Nat × Validator.Action), entry ∈ (actions[state]?).getD [] →
      Validator.action ceiling actions.length productions layout entry.1 entry.2 = true)
    (gotoRows : ∀ (state : Nat) (target : Int), target ∈ (gotos[state]?).getD [] → Validator.bounded ceiling actions.length target = true) :
    ∀ state ∈ List.range actions.length, ∀ target ∈ edges actions gotos state, target ∈ List.range actions.length := by
  intro state _ target member
  apply List.mem_range.mpr
  rcases List.mem_append.mp member with shift | goto
  · obtain ⟨entry, present, origin⟩ := List.mem_flatMap.mp shift
    exact target_bounded (actionRows state entry present) origin
  · obtain ⟨encoded, present, equal⟩ := List.mem_map.mp goto
    subst target
    have checked := gotoRows state encoded present
    simp only [Validator.bounded, decide_eq_true_eq] at checked
    omega

theorem outer_queue_exact (actions : ActionRows) (gotos : List (List Int)) (ceiling productions : Nat) (layout : Bool)
    (positive : 0 < actions.length)
    (actionRows : ∀ (state : Nat) (entry : Option Nat × Validator.Action), entry ∈ (actions[state]?).getD [] →
      Validator.action ceiling actions.length productions layout entry.1 entry.2 = true)
    (gotoRows : ∀ (state : Nat) (target : Int), target ∈ (gotos[state]?).getD [] → Validator.bounded ceiling actions.length target = true) :
    Queue.admitted (Queue.run (edges actions gotos) (accepts actions) (Queue.initial (List.range actions.length))) = true ↔
      ∃ node, Queue.Reaches (edges actions gotos) node ∧ accepts actions node = true :=
  Queue.acceptance_iff _ List.nodup_range (List.mem_range.mpr positive) _ _
    (graph_bounded actions gotos ceiling productions layout actionRows gotoRows)
#print axioms target_bounded
#print axioms outer_queue_exact
end ParserEngine.CompilerExecution.ValidatedGraph
