import ParserEngine.ConstructionQueue

/-! Every processed state owns exactly one complete row. Previously published
source IDs and target IDs survive the remaining queue drain. -/
namespace ParserEngine.ConstructionRows
open StateInterning TransitionPublication KernelConstruction ConstructionQueue

def RowValid (g : Grammar.CFG) (registry : Registry Nat (List Nat)) (node : Nat) (edges : List (Nat × Nat)) : Prop :=
  ∃ mask, registry.keys[node]? = some mask ∧
    (∀ key target, (key, target) ∈ edges → ∃ kernel, (key, kernel) ∈ inputs g mask ∧ registry.index kernel = some target) ∧
    (∀ key kernel, (key, kernel) ∈ inputs g mask → ∃ target, (key, target) ∈ edges ∧ registry.index kernel = some target)

def Published (g : Grammar.CFG) (machine : Machine) : Prop :=
  ∀ node, node < machine.registry.processed → ∃ edges, machine.transitions node = some edges ∧ RowValid g machine.registry node edges

theorem keys_retained (g : Grammar.CFG) (before : Registry Nat (List Nat)) (groups : List (Nat × Nat))
    (node mask : Nat) (read : before.keys[node]? = some mask) :
    (publish (compute g) before groups).registry.keys[node]? = some mask := by
  induction groups generalizing before with
  | nil => exact read
  | cons group rest ih =>
    apply ih
    rw [(StateInterning.retained (compute g) before group.2 node (PhysicalCoreExtraction.lookup_bound read)).1]
    exact read

theorem group_indexed (g : Grammar.CFG) (before : Registry Nat (List Nat)) (groups : List (Nat × Nat))
    (key kernel : Nat) (member : (key, kernel) ∈ groups) :
    ∃ target, (publish (compute g) before groups).registry.index kernel = some target := by
  induction groups generalizing before with
  | nil => cases member
  | cons group rest ih =>
    rcases List.mem_cons.mp member with same | later
    · cases same
      exact ⟨_, retains (compute g) _ rest kernel _ (intern_lookup (compute g) before kernel)⟩
    · exact ih _ later

theorem row_retained (g : Grammar.CFG) (before : Registry Nat (List Nat)) (groups : List (Nat × Nat))
    (node : Nat) (edges : List (Nat × Nat)) (valid : RowValid g before node edges) :
    RowValid g (publish (compute g) before groups).registry node edges := by
  obtain ⟨mask, read, sound, complete⟩ := valid
  refine ⟨mask, keys_retained g before groups node mask read, ?_, ?_⟩
  · intro key target member
    obtain ⟨kernel, group, indexed⟩ := sound key target member
    exact ⟨kernel, group, retains (compute g) before groups kernel target indexed⟩
  · intro key kernel group
    obtain ⟨target, member, indexed⟩ := complete key kernel group
    exact ⟨target, member, retains (compute g) before groups kernel target indexed⟩

theorem newly_published (g : Grammar.CFG) (before : Registry Nat (List Nat)) (node mask : Nat)
    (read : before.keys[node]? = some mask) :
    RowValid g (publish (compute g) before (inputs g mask)).registry node
      (publish (compute g) before (inputs g mask)).rows := by
  refine ⟨mask, keys_retained g before _ node mask read, ?_, ?_⟩
  · intro key target member
    exact (row_exact (compute g) before (inputs g mask) key target).mp member
  · intro key kernel member
    obtain ⟨target, indexed⟩ := group_indexed g before (inputs g mask) key kernel member
    exact ⟨target, (row_exact (compute g) before (inputs g mask) key target).mpr ⟨kernel, member, indexed⟩, indexed⟩

theorem tick_published (g : Grammar.CFG) (before : Machine) (admitted : Invariant g before) (published : Published g before) :
    Published g (tick g before) := by
  cases front : before.registry.pending with
  | nil => simpa [tick, front] using published
  | cons node tail =>
    have bound := admitted.valid.bound node (by rw [front]; simp)
    let mask := before.registry.keys[node]
    have read : before.registry.keys[node]? = some mask := List.getElem?_eq_getElem bound
    have nodeID := (dequeue_fifo before.registry admitted.fifo node tail front).1
    intro id processed
    have limit : id < before.registry.processed + 1 := by
      simpa only [tick, front, read, publish_processed, drain] using processed
    by_cases current : id = node
    · subst id
      refine ⟨(publish (compute g) (drain before.registry tail) (inputs g mask)).rows, ?_, ?_⟩
      · simp [tick, front, read, ProducerAdmission.Storage.update]
      · simpa only [tick, front, read] using newly_published g (drain before.registry tail) node mask read
    · have old : id < before.registry.processed := by omega
      obtain ⟨edges, stored, semantics⟩ := published id old
      refine ⟨edges, ?_, ?_⟩
      · simpa [tick, front, read, ProducerAdmission.Storage.update, current] using stored
      · simpa only [tick, front, read] using row_retained g (drain before.registry tail) (inputs g mask) id edges semantics

theorem finish_published (g : Grammar.CFG) (before : Machine) (admitted : Invariant g before) (published : Published g before) :
    Published g (finish g before admitted) := by
  rw [finish]
  split
  · exact published
  · exact finish_published g _ (tick_invariant g before admitted) (tick_published g before admitted published)
termination_by capacity g - before.registry.processed
decreasing_by exact tick_rank g before admitted (by assumption)

theorem initial_published (g : Grammar.CFG) : Published g (initial g) := by
  intro node bound
  simp [initial, intern, empty] at bound

/-- Final publication is complete for every discovered state, including
states discovered during the last nonempty queue iteration. -/
theorem complete (g : Grammar.CFG) (nonempty : g ≠ []) :
    let final := finish g (initial g) (initial_invariant g nonempty)
    ∀ node, node < final.registry.keys.length → ∃ edges, final.transitions node = some edges ∧ RowValid g final.registry node edges := by
  intro final node inside
  have processed := finish_processed g (initial g) (initial_invariant g nonempty)
  apply finish_published g (initial g) (initial_invariant g nonempty) (initial_published g) node
  simpa only [processed] using inside

end ParserEngine.ConstructionRows
