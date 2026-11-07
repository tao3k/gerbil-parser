import ParserEngine.KernelConstruction

/-! Source-shaped dequeue, append-only discovery and row publication. The
queue contains the exact unprocessed ID interval, not merely bounded IDs. -/
namespace ParserEngine.ConstructionQueue
open StateInterning TransitionPublication KernelConstruction

def FIFO (registry : Registry Nat (List Nat)) : Prop :=
  registry.processed ≤ registry.keys.length ∧
  registry.pending = List.range' registry.processed (registry.keys.length - registry.processed)

theorem intern_fifo (g : Grammar.CFG) (before : Registry Nat (List Nat)) (fifo : FIFO before) (mask : Nat) :
    FIFO (intern (compute g) before mask).registry := by
  have bound := fifo.1
  unfold intern
  split
  · exact fifo
  · refine ⟨by simp; omega, ?_⟩
    simp only [List.length_append, List.length_singleton]
    have difference : before.keys.length + 1 - before.processed = (before.keys.length - before.processed) + 1 := by omega
    rw [difference, List.range'_1_concat, fifo.2]
    congr 1
    congr 1
    omega

theorem publish_fifo (g : Grammar.CFG) (before : Registry Nat (List Nat)) (fifo : FIFO before) (groups : List (Nat × Nat)) :
    FIFO (publish (compute g) before groups).registry := by
  induction groups generalizing before with
  | nil => exact fifo
  | cons group rest ih => exact ih _ (intern_fifo g before fifo group.2)

theorem dequeue_fifo (before : Registry Nat (List Nat)) (fifo : FIFO before)
    (node : Nat) (tail : List Nat) (front : before.pending = node :: tail) :
    node = before.processed ∧ FIFO (drain before tail) := by
  have positive : 0 < before.keys.length - before.processed := by
    by_cases zero : before.keys.length - before.processed = 0
    · have empty : before.pending = [] := by rw [fifo.2, zero]; rfl
      rw [empty] at front
      cases front
    · omega
  have shape : List.range' before.processed (before.keys.length - before.processed) =
      before.processed :: List.range' (before.processed + 1) (before.keys.length - (before.processed + 1)) := by
    have size : before.keys.length - before.processed = (before.keys.length - (before.processed + 1)) + 1 := by omega
    rw [size, List.range'_succ]
  have same := List.cons.inj (front.symm.trans (fifo.2.trans shape))
  exact ⟨same.1, by simpa only [FIFO, drain] using And.intro (by omega : before.processed + 1 ≤ before.keys.length) same.2⟩

structure Machine where
  registry : Registry Nat (List Nat)
  transitions : Nat → Option (List (Nat × Nat))

def tick (g : Grammar.CFG) (before : Machine) : Machine :=
  match before.registry.pending with
  | [] => before
  | node :: tail =>
    let drained := drain before.registry tail
    match before.registry.keys[node]? with
    | none => before
    | some mask =>
      let published := publish (compute g) drained (inputs g mask)
      ⟨published.registry, ProducerAdmission.Storage.update before.transitions node published.rows⟩

/-- Every captured plan comes from the initial kernel or a generated group. -/
def Generated (g : Grammar.CFG) (registry : Registry Nat (List Nat)) : Prop :=
  ∀ mask ∈ registry.keys, History g mask

theorem intern_generated (g : Grammar.CFG) (before : Registry Nat (List Nat))
    (generated : Generated g before) (mask : Nat) (history : History g mask) :
    Generated g (intern (compute g) before mask).registry := by
  intro key member
  unfold intern at member
  split at member
  · exact generated key member
  · simp only [List.mem_append, List.mem_singleton] at member
    rcases member with old | rfl
    · exact generated key old
    · exact history

theorem publish_generated (g : Grammar.CFG) (before : Registry Nat (List Nat)) (generated : Generated g before)
    (groups : List (Nat × Nat)) (histories : ∀ key mask, (key, mask) ∈ groups → History g mask) :
    Generated g (publish (compute g) before groups).registry := by
  induction groups generalizing before with
  | nil => exact generated
  | cons group rest ih =>
    apply ih _ (intern_generated g before generated group.2 (histories group.1 group.2 (by simp)))
    intro key mask member
    exact histories key mask (List.mem_cons_of_mem group member)

theorem group_history (g : Grammar.CFG) (mask : Nat) (history : History g mask)
    (key target : Nat) (member : (key, target) ∈ inputs g mask) : History g target := by
  obtain ⟨row, belongs, same⟩ := List.mem_map.mp member
  have equal : row.mask = target := congrArg Prod.snd same
  rw [← equal]
  exact .next history row belongs

/-- Finite physical cores bound the entire discovery universe. This is a
termination bound, not a practical state-count or runtime estimate. -/
def capacity (g : Grammar.CFG) : Nat := 2 ^ (Viable.denseCoordinates (g.map fun rule => rule.rhs.length)).length

theorem mask_lt_capacity (g : Grammar.CFG) (mask : Nat) (history : History g mask) : mask < capacity g := by
  have bounded := history_bound history
  by_cases small : mask < capacity g
  · exact small
  · obtain ⟨bit, high, present⟩ := Nat.exists_ge_and_testBit_of_ge_two_pow (Nat.le_of_not_gt small)
    have low := bounded bit present
    omega

theorem count_bound (g : Grammar.CFG) (before : Registry Nat (List Nat))
    (valid : Valid (compute g) before) (generated : Generated g before) : before.keys.length ≤ capacity g := by
  have bounded : before.keys ⊆ List.range (capacity g) := by
    intro mask member
    exact List.mem_range.mpr (mask_lt_capacity g mask (generated mask member))
  simpa using valid.unique.length_le_of_subset bounded

structure Invariant (g : Grammar.CFG) (machine : Machine) : Prop where
  valid : Valid (compute g) machine.registry
  fifo : FIFO machine.registry
  generated : Generated g machine.registry

theorem tick_invariant (g : Grammar.CFG) (before : Machine) (admitted : Invariant g before) :
    Invariant g (tick g before) := by
  cases front : before.registry.pending with
  | nil => simpa [tick, front] using admitted
  | cons node tail =>
    have bound := admitted.valid.bound node (by rw [front]; simp)
    let mask := before.registry.keys[node]
    have read : before.registry.keys[node]? = some mask := List.getElem?_eq_getElem bound
    have history := admitted.generated mask (List.mem_of_getElem? read)
    have drained := drain_valid (compute g) before.registry admitted.valid node tail front
    have fifo := (dequeue_fifo before.registry admitted.fifo node tail front).2
    refine ⟨?_, ?_, ?_⟩
    · simpa [tick, front, read] using TransitionPublication.valid (compute g) _ drained (inputs g mask)
    · simpa [tick, front, read] using publish_fifo g _ fifo (inputs g mask)
    · simpa [tick, front, read] using publish_generated g (drain before.registry tail) admitted.generated
        (inputs g mask) (group_history g mask history)

theorem publish_processed (g : Grammar.CFG) (before : Registry Nat (List Nat)) (groups : List (Nat × Nat)) :
    (publish (compute g) before groups).registry.processed = before.processed := by
  induction groups generalizing before with
  | nil => rfl
  | cons group rest ih =>
    rw [publish, ih]
    unfold intern
    split <;> rfl

theorem tick_progress (g : Grammar.CFG) (before : Machine) (admitted : Invariant g before)
    (pending : before.registry.pending ≠ []) :
    (tick g before).registry.processed = before.registry.processed + 1 := by
  cases front : before.registry.pending with
  | nil => exact False.elim (pending front)
  | cons node tail =>
    have bound := admitted.valid.bound node (by rw [front]; simp)
    have read := List.getElem?_eq_getElem bound
    simp only [tick, front, read]
    exact publish_processed g _ _

theorem tick_rank (g : Grammar.CFG) (before : Machine) (admitted : Invariant g before)
    (pending : before.registry.pending ≠ []) :
    capacity g - (tick g before).registry.processed < capacity g - before.registry.processed := by
  have accounting := admitted.valid.accounting
  have bounded := count_bound g before.registry admitted.valid admitted.generated
  have positive : 0 < before.registry.pending.length := List.length_pos_iff.mpr pending
  rw [tick_progress g before admitted pending]
  omega

/-- Unbounded recursion terminates from the finite kernel universe. No test
fuel or arbitrary iteration limit serves as a termination assumption. -/
def finish (g : Grammar.CFG) (before : Machine) (admitted : Invariant g before) : Machine :=
  if _pending : before.registry.pending = [] then before
  else finish g (tick g before) (tick_invariant g before admitted)
termination_by capacity g - before.registry.processed
decreasing_by exact tick_rank g before admitted _pending

theorem finish_invariant (g : Grammar.CFG) (before : Machine) (admitted : Invariant g before) :
    Invariant g (finish g before admitted) := by
  rw [finish]
  split
  · exact admitted
  · exact finish_invariant g _ (tick_invariant g before admitted)
termination_by capacity g - before.registry.processed
decreasing_by exact tick_rank g before admitted (by assumption)

theorem finish_empty (g : Grammar.CFG) (before : Machine) (admitted : Invariant g before) :
    (finish g before admitted).registry.pending = [] := by
  rw [finish]
  split
  · assumption
  · exact finish_empty g _ (tick_invariant g before admitted)
termination_by capacity g - before.registry.processed
decreasing_by exact tick_rank g before admitted (by assumption)

def empty : Registry Nat (List Nat) := ⟨[], fun _ => none, fun _ => none, fun _ => none, [], 0⟩
def initial (g : Grammar.CFG) : Machine := ⟨(intern (compute g) empty 1).registry, fun _ => none⟩

theorem empty_valid (g : Grammar.CFG) : Valid (compute g) empty := by
  refine ⟨by simp [empty], ?_, ?_, ?_, rfl, by simp [empty], ?_⟩
  · intro key id; simp [empty]
  · intro id key read; simp [empty] at read
  · intro id key read; simp [empty] at read
  · intro id member; simp [empty] at member

theorem initial_invariant (g : Grammar.CFG) (nonempty : g ≠ []) : Invariant g (initial g) := by
  refine ⟨intern_valid (compute g) empty (empty_valid g) 1, ?_, ?_⟩
  · simp [FIFO, initial, intern, empty]
  · apply intern_generated g empty
    · intro mask member; cases member
    · exact .initial nonempty

theorem finish_processed (g : Grammar.CFG) (before : Machine) (admitted : Invariant g before) :
    (finish g before admitted).registry.processed = (finish g before admitted).registry.keys.length := by
  have accounting := (finish_invariant g before admitted).valid.accounting
  rw [finish_empty, List.length_nil, Nat.zero_add] at accounting
  exact accounting

end ParserEngine.ConstructionQueue
