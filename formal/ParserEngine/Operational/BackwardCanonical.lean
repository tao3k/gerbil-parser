import ParserEngine.Operational.HeapScheduling
import ParserEngine.Construction

/-! Operational adapters for Scheme object storage, finite native scheduling
and dynamically scoped layout. These semantics describe the used primitives,
not an interpreter or a correctness theorem for Gambit/GCC. -/
namespace ParserEngine.Operational

namespace Backward

abbrev Buckets := List (Nat × List Nat)

def insert (key vertex : Nat) : Buckets → Buckets
  | [] => [(key, [vertex])]
  | (label, members) :: rest =>
      if key = label then (label, vertex :: members) :: rest
      else (label, members) :: insert key vertex rest

def buckets (signature : Nat → Nat) : List Nat → Buckets
  | [] => []
  | vertex :: rest => insert (signature vertex) vertex (buckets signature rest)

def mass (groups : Buckets) : Nat := (groups.map (fun group => group.2.length)).sum

theorem insert_mass (key vertex : Nat) (groups : Buckets) : mass (insert key vertex groups) = mass groups + 1 := by
  induction groups with
  | nil => simp [insert, mass]
  | cons group rest ih =>
    rcases group with ⟨label, members⟩
    by_cases same : key = label
    · simp [insert, same, mass]; omega
    · simp only [insert, same, ite_false, mass, List.map_cons, List.sum_cons] at *
      omega

theorem buckets_mass (signature : Nat → Nat) (vertices : List Nat) :
    mass (buckets signature vertices) = vertices.length := by
  induction vertices with
  | nil => rfl
  | cons vertex rest ih => simp only [buckets, insert_mass, List.length_cons, ih]

theorem insert_nonempty {groups : Buckets}
    (valid : ∀ group ∈ groups, group.2 ≠ []) (key vertex : Nat) :
    ∀ group ∈ insert key vertex groups, group.2 ≠ [] := by
  induction groups with
  | nil => simp [insert]
  | cons group rest ih =>
    rcases group with ⟨label, members⟩
    by_cases same : key = label
    · simp only [insert, same, ite_true, List.mem_cons]
      intro candidate member
      rcases member with rfl | previous
      · simp
      · exact valid candidate (List.mem_cons_of_mem _ previous)
    · simp only [insert, same, ite_false, List.mem_cons]
      intro candidate member
      rcases member with rfl | previous
      · exact valid _ (by simp)
      · exact ih (fun other member => valid other (List.mem_cons_of_mem _ member)) candidate previous

theorem buckets_nonempty (signature : Nat → Nat) (vertices : List Nat) :
    ∀ group ∈ buckets signature vertices, group.2 ≠ [] := by
  induction vertices with
  | nil => simp [buckets]
  | cons vertex rest ih => exact insert_nonempty ih _ _

theorem nonempty_bucket_count {signature : Nat → Nat} {vertices : List Nat} (nonempty : vertices ≠ []) :
    1 ≤ (buckets signature vertices).length := by
  have same := buckets_mass signature vertices
  cases equation : buckets signature vertices with
  | nil => simp [equation, mass] at same; exact False.elim (nonempty (List.eq_nil_of_length_eq_zero same.symm))
  | cons head tail => simp

structure Partition where
  blocks : List (List Nat)
  nonempty : ∀ block ∈ blocks, block ≠ []

def vertices (partition : Partition) : Nat := (partition.blocks.map List.length).sum

def refine (signature : List Nat → Nat → Nat) (partition : Partition) : Partition :=
  ⟨partition.blocks.flatMap (fun block => (buckets (signature block) block).map Prod.snd), by
    intro block member
    obtain ⟨old, present, nested⟩ := List.mem_flatMap.mp member
    obtain ⟨bucket, inBucket, same⟩ := List.mem_map.mp nested
    subst block
    exact buckets_nonempty _ _ bucket inBucket⟩

theorem refined_vertices (signature : List Nat → Nat → Nat) (partition : Partition) :
    vertices (refine signature partition) = vertices partition := by
  have helper : ∀ blocks : List (List Nat),
      ((blocks.flatMap (fun block => (buckets (signature block) block).map Prod.snd)).map List.length).sum =
        (blocks.map List.length).sum := by
    intro blocks
    induction blocks with
    | nil => rfl
    | cons head rest ih =>
      simp only [List.flatMap_cons, List.map_append, List.sum_append, List.map_cons, List.sum_cons, ih]
      have total := buckets_mass (signature head) head
      simpa only [mass, List.map_map, Function.comp_def] using congrArg (fun n => n + (rest.map List.length).sum) total
  exact helper partition.blocks

theorem partition_density (partition : Partition) : partition.blocks.length ≤ vertices partition := by
  have helper : ∀ blocks : List (List Nat), (∀ block ∈ blocks, block ≠ []) →
      blocks.length ≤ (blocks.map List.length).sum := by
    intro blocks valid
    induction blocks with
    | nil => simp
    | cons head rest ih =>
      have positive : 0 < head.length := List.length_pos_iff.mpr (valid head (by simp))
      have tail := ih (fun block member => valid block (List.mem_cons_of_mem _ member))
      simp only [List.length_cons, List.map_cons, List.sum_cons]
      omega
  exact helper _ partition.nonempty

def changed (signature : List Nat → Nat → Nat) (partition : Partition) : Bool :=
  partition.blocks.any (fun block => decide (1 < (buckets (signature block) block).length))

theorem refine_count_ge (signature : List Nat → Nat → Nat) (blocks : List (List Nat))
    (valid : ∀ block ∈ blocks, block ≠ []) :
    blocks.length ≤ (blocks.flatMap (fun block => (buckets (signature block) block).map Prod.snd)).length := by
  induction blocks with
  | nil => simp
  | cons head rest ih =>
    have positive := nonempty_bucket_count (signature := signature head) (valid head (by simp))
    have later := ih (fun b m => valid b (List.mem_cons_of_mem _ m))
    simp only [List.flatMap_cons, List.length_append, List.length_map, List.length_cons]
    omega

theorem changed_grows (signature : List Nat → Nat → Nat) (partition : Partition)
    (split : changed signature partition = true) :
    partition.blocks.length < (refine signature partition).blocks.length := by
  have witness : ∃ block ∈ partition.blocks, 1 < (buckets (signature block) block).length := by
    obtain ⟨block, member, split⟩ := List.any_eq_true.mp split
    exact ⟨block, member, of_decide_eq_true split⟩
  have helper : ∀ blocks : List (List Nat), (∀ block ∈ blocks, block ≠ []) →
      (∃ block ∈ blocks, 1 < (buckets (signature block) block).length) →
      blocks.length < (blocks.flatMap (fun block => (buckets (signature block) block).map Prod.snd)).length := by
    intro blocks valid hasSplit
    induction blocks with
    | nil => simp at hasSplit
    | cons head rest ih =>
      have headBound := nonempty_bucket_count (signature := signature head) (valid head (by simp))
      have restBound := refine_count_ge signature rest (fun b m => valid b (List.mem_cons_of_mem _ m))
      obtain ⟨block, member, growth⟩ := hasSplit
      simp only [List.mem_cons] at member
      simp only [List.flatMap_cons, List.length_append, List.length_map, List.length_cons]
      rcases member with rfl | member
      · omega
      · have later := ih (fun b m => valid b (List.mem_cons_of_mem _ m)) ⟨block, member, growth⟩
        omega
  exact helper partition.blocks partition.nonempty witness

/-- Backward continuation is the actual multi-bucket predicate. Growth is
proved from bucket construction, rather than supplied as a changed-flag axiom. -/
def rounds (signature : Partition → List Nat → Nat → Nat) (partition : Partition) : Nat :=
  if changed (signature partition) partition then
    1 + rounds signature (refine (signature partition) partition)
  else 1
termination_by vertices partition - partition.blocks.length
decreasing_by
  have growth := changed_grows (signature partition) partition (by assumption)
  have density := partition_density (refine (signature partition) partition)
  have conserved := refined_vertices (signature partition) partition
  omega

theorem round_bound (signature : Partition → List Nat → Nat → Nat) (partition : Partition) :
    rounds signature partition ≤ vertices partition - partition.blocks.length + 1 := by
  rw [rounds]
  split
  · have later := round_bound signature (refine (signature partition) partition)
    have growth := changed_grows (signature partition) partition (by assumption)
    have conserved := refined_vertices (signature partition) partition
    have density := partition_density (refine (signature partition) partition)
    omega
  · omega
termination_by vertices partition - partition.blocks.length
decreasing_by
  have growth := changed_grows (signature partition) partition (by assumption)
  have conserved := refined_vertices (signature partition) partition
  have density := partition_density (refine (signature partition) partition)
  omega

end Backward

namespace Canonical

/-- Dense masks have one row per physical core, including zero rows. Sparse
native keys emit nonzero rows in physical-core order. -/
def sparse (core : Nat) : List Nat → List (Nat × Nat)
  | [] => []
  | mask :: rest => if mask = 0 then sparse (core + 1) rest
      else (core, mask) :: sparse (core + 1) rest

def matrices : Nat → Nat → List (List Nat)
  | 0, _ => [[]]
  | cores + 1, terminals =>
      (matrices cores terminals).flatMap (fun tail =>
        (List.range (2 ^ terminals)).map (fun mask => mask :: tail))

theorem matrix_is_enumerated {cores terminals : Nat} {masks : List Nat}
    (width : masks.length = cores) (bounded : ∀ mask ∈ masks, mask < 2 ^ terminals) :
    masks ∈ matrices cores terminals := by
  induction cores generalizing masks with
  | zero => have empty := List.eq_nil_of_length_eq_zero width; simp [empty, matrices]
  | succ cores ih =>
    cases masks with
    | nil => simp at width
    | cons mask tail =>
      have restWidth : tail.length = cores := by simpa using width
      have restBound := fun value member => bounded value (List.mem_cons_of_mem mask member)
      apply List.mem_flatMap.mpr
      refine ⟨tail, ih restWidth restBound, ?_⟩
      exact List.mem_map.mpr ⟨mask, List.mem_range.mpr (bounded mask (by simp)), rfl⟩

def catalogue (cores terminals : Nat) : List (List (Nat × Nat)) :=
  (matrices cores terminals).map (sparse 0)

theorem matrix_count (cores terminals : Nat) :
    (matrices cores terminals).length = (2 ^ terminals) ^ cores := by
  induction cores with
  | zero => simp [matrices]
  | succ cores ih =>
    simp only [matrices, List.length_flatMap, List.length_map, List.length_range]
    rw [List.map_const', List.sum_replicate_nat, ih, Nat.pow_succ]

theorem catalogue_count (cores terminals : Nat) :
    (catalogue cores terminals).length = (2 ^ terminals) ^ cores := by
  simp [catalogue, matrix_count]

theorem native_key_admitted {cores terminals : Nat} {masks : List Nat}
    (width : masks.length = cores) (bounded : ∀ mask ∈ masks, mask < 2 ^ terminals) :
    sparse 0 masks ∈ catalogue cores terminals :=
  List.mem_map.mpr ⟨masks, matrix_is_enumerated width bounded, rfl⟩

theorem intern_catalogue_bound [BEq Key] [LawfulBEq Key]
    (domain keys : List Key) (key : Key) (unique : keys.Nodup)
    (included : ∀ old ∈ keys, old ∈ domain) (admitted : key ∈ domain) :
    (Construction.intern keys key).1.length ≤ domain.length := by
  apply List.Nodup.length_le_of_subset (Construction.intern_unique keys key unique)
  intro old member
  unfold Construction.intern at member
  split at member
  · exact included old member
  · rcases List.mem_append.mp member with earlier | new
    · exact included old earlier
    · have same := List.mem_singleton.mp new
      simpa only [same] using admitted

/-- Native canonical closure may enqueue duplicate owners: every enqueue is
still charged to a strict mask growth. This differs from LALR coalescing. -/
def closurePublish [BEq Fact] [LawfulBEq Fact]
    (state : Scheduling.Budget Fact Nat) (owner : Nat) (incoming : List Fact) : Scheduling.Budget Fact Nat :=
  if incoming.any (fun fact => state.remaining.contains fact) then
    ⟨Scheduling.eraseMany state.remaining incoming, owner :: state.queued⟩ else state

theorem closure_enqueue_decreases [BEq Fact] [LawfulBEq Fact]
    (state : Scheduling.Budget Fact Nat) (owner : Nat) (incoming : List Fact)
    (fresh : ∃ fact ∈ incoming, fact ∈ state.remaining) :
    Scheduling.rank (closurePublish state owner incoming) < Scheduling.rank state := by
  have shorter := Scheduling.erase_many_fresh_decreases _ _ fresh
  have live : incoming.any (fun fact => state.remaining.contains fact) = true := by
    obtain ⟨fact, member, known⟩ := fresh
    exact List.any_eq_true.mpr ⟨fact, member, List.contains_iff_mem.mpr known⟩
  simp only [closurePublish, live, ite_true, Scheduling.rank, List.length_cons]
  omega

structure Queue (Key : Type) where
  keys : List Key
  pending : List Nat
  processed : Nat

def intern [BEq Key] [LawfulBEq Key] (state : Queue Key) (key : Key) : Queue Key :=
  if key ∈ state.keys then state else
    ⟨state.keys ++ [key], state.pending ++ [state.keys.length], state.processed⟩

def rank (domain : List Key) (state : Queue Key) : Nat :=
  2 * (domain.length - state.keys.length) + state.pending.length

theorem existing_never_requeues [BEq Key] [LawfulBEq Key] (state : Queue Key) (key : Key)
    (existing : key ∈ state.keys) : intern state key = state := by simp [intern, existing]

theorem novel_intern_decreases [BEq Key] [LawfulBEq Key]
    (domain : List Key) (state : Queue Key) (key : Key)
    (unique : state.keys.Nodup) (included : ∀ old ∈ state.keys, old ∈ domain)
    (admitted : key ∈ domain) (novel : key ∉ state.keys) : rank domain (intern state key) < rank domain state := by
  have bound := intern_catalogue_bound domain state.keys key unique included admitted
  simp only [Construction.intern, novel, ite_false, List.length_append, List.length_singleton] at bound
  simp only [rank, intern, novel, ite_false, List.length_append, List.length_singleton]
  omega

theorem intern_accounting [BEq Key] [LawfulBEq Key] (state : Queue Key) (key : Key)
    (balanced : state.pending.length + state.processed = state.keys.length) :
    (intern state key).pending.length + (intern state key).processed = (intern state key).keys.length := by
  unfold intern
  split
  · exact balanced
  · simp only [List.length_append, List.length_singleton]
    omega

def drain (state : Queue Key) : Queue Key :=
  match state.pending with
  | [] => state
  | _ :: rest => {state with pending := rest, processed := state.processed + 1}

theorem drain_decreases (domain : List Key) (state : Queue Key)
    (work : state.pending ≠ []) : rank domain (drain state) < rank domain state := by
  cases pending : state.pending with
  | nil => exact False.elim (work pending)
  | cons head rest => simp [drain, pending, rank]

theorem completed_processes_catalogue (state : Queue Key)
    (balanced : state.pending.length + state.processed = state.keys.length)
    (finished : state.pending = []) : state.processed = state.keys.length := by
  simpa [finished] using balanced

theorem intern_keys [BEq Key] [LawfulBEq Key] (state : Queue Key) (key : Key) :
    (intern state key).keys = (Construction.intern state.keys key).1 := by
  unfold intern Construction.intern
  split <;> rfl

theorem drain_keys (state : Queue Key) : (drain state).keys = state.keys := by
  unfold drain
  split <;> rfl

theorem drain_accounting (state : Queue Key)
    (balanced : state.pending.length + state.processed = state.keys.length) :
    (drain state).pending.length + (drain state).processed = (drain state).keys.length := by
  unfold drain
  split
  · exact balanced
  · rename_i head rest equation
    simp only [equation, List.length_cons] at balanced
    dsimp only
    omega

structure Admitted (domain : List Key) where
  queue : Queue Key
  unique : queue.keys.Nodup
  included : ∀ key ∈ queue.keys, key ∈ domain
  balanced : queue.pending.length + queue.processed = queue.keys.length

def admit [BEq Key] [LawfulBEq Key] {domain : List Key}
    (state : Admitted domain) (key : Key) (member : key ∈ domain) : Admitted domain :=
  ⟨intern state.queue key, by rw [intern_keys]; exact Construction.intern_unique _ _ state.unique,
    by
      intro old present
      unfold intern at present
      split at present
      · exact state.included old present
      · rcases List.mem_append.mp present with previous | added
        · exact state.included old previous
        · have same := List.mem_singleton.mp added
          simpa only [same] using member,
    intern_accounting _ _ state.balanced⟩

def removeHead {domain : List Key} (state : Admitted domain) : Admitted domain :=
  ⟨drain state.queue, by rw [drain_keys]; exact state.unique,
    by rw [drain_keys]; exact state.included, drain_accounting _ state.balanced⟩

def expand [BEq Key] [LawfulBEq Key] {domain : List Key} (state : Admitted domain)
    (targets : List Key) (valid : ∀ key ∈ targets, key ∈ domain) : Admitted domain :=
  match targets with
  | [] => state
  | key :: rest => expand (admit state key (valid key (by simp))) rest
      (fun other member => valid other (List.mem_cons_of_mem key member))

theorem admit_rank_le [BEq Key] [LawfulBEq Key] {domain : List Key}
    (state : Admitted domain) (key : Key) (member : key ∈ domain) :
    rank domain (admit state key member).queue ≤ rank domain state.queue := by
  by_cases existing : key ∈ state.queue.keys
  · simp [admit, existing_never_requeues _ _ existing]
  · exact Nat.le_of_lt (novel_intern_decreases domain state.queue key state.unique state.included member existing)

theorem expand_rank_le [BEq Key] [LawfulBEq Key] {domain : List Key}
    (state : Admitted domain) (targets : List Key) (valid : ∀ key ∈ targets, key ∈ domain) :
    rank domain (expand state targets valid).queue ≤ rank domain state.queue := by
  induction targets generalizing state with
  | nil => exact Nat.le_refl _
  | cons key rest ih =>
    exact Nat.le_trans (ih _ _) (admit_rank_le state key (valid key (by simp)))

/-- One real drain is followed by a finite symbol traversal. Old states do not
requeue; every fresh target is admitted into the grammar-derived catalogue. -/
def finish [BEq Key] [LawfulBEq Key] {domain : List Key}
    (targets : Admitted domain → Nat → List Key)
    (accepted : ∀ state owner key, key ∈ targets state owner → key ∈ domain)
    (state : Admitted domain) : Admitted domain :=
  match equation : state.queue.pending with
  | [] => state
  | owner :: _ =>
      let next := expand (removeHead state) (targets state owner) (accepted state owner)
      have progress : rank domain next.queue < rank domain state.queue := by
        have drained := drain_decreases domain state.queue (by rw [equation]; simp)
        have expanded := expand_rank_le (removeHead state) (targets state owner) (accepted state owner)
        exact Nat.lt_of_le_of_lt expanded drained
      finish targets accepted next
termination_by rank domain state.queue
decreasing_by exact progress

theorem finish_quiescent [BEq Key] [LawfulBEq Key] {domain : List Key}
    (targets : Admitted domain → Nat → List Key)
    (accepted : ∀ state owner key, key ∈ targets state owner → key ∈ domain)
    (state : Admitted domain) : (finish targets accepted state).queue.pending = [] := by
  rw [finish]
  split
  next stopped => exact stopped
  next owner rest equation => exact finish_quiescent targets accepted _
termination_by rank domain state.queue
decreasing_by
  rename_i owner _ equation _ _
  have drained := drain_decreases domain state.queue (by rw [equation]; simp)
  have expanded := expand_rank_le (removeHead state) (targets state owner) (accepted state owner)
  exact Nat.lt_of_le_of_lt expanded drained

theorem finish_processes_all_keys [BEq Key] [LawfulBEq Key] {domain : List Key}
    (targets : Admitted domain → Nat → List Key)
    (accepted : ∀ state owner key, key ∈ targets state owner → key ∈ domain)
    (state : Admitted domain) :
    (finish targets accepted state).queue.processed = (finish targets accepted state).queue.keys.length :=
  completed_processes_catalogue _ (finish targets accepted state).balanced (finish_quiescent targets accepted state)

end Canonical


end ParserEngine.Operational
