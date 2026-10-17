import ParserEngine.ConstructionProtocol
import ParserEngine.TemplateCapabilities

/-! Stateful template consumption across initial installation, novel-state
interning, ordered row publication and the complete terminating queue. -/
namespace ParserEngine.CachedConstruction
open Grammar StateInterning KernelConstruction ConstructionQueue
abbrev Cache := TemplateCapabilities.Cache

def intern (g : CFG) (cache : Cache g) (before : Registry Nat (List Nat)) (kernel : Nat) :
    Result Nat (List Nat) × Cache g :=
  match before.index kernel with
  | some id => (⟨before, id, false⟩, cache)
  | none =>
    let built := TemplateCapabilities.build g ⟨cache, kernel⟩
    (StateInterning.intern (fun _ => built.product) before kernel, built.cache)

theorem intern_exact (g : CFG) (cache : Cache g) (before : Registry Nat (List Nat)) (kernel : Nat) :
    (intern g cache before kernel).1 = StateInterning.intern (compute g) before kernel := by
  cases read : before.index kernel with
  | some id => simp [intern, StateInterning.intern, read]
  | none => simp [intern, StateInterning.intern, read, TemplateCapabilities.build_exact g ⟨cache, kernel⟩]

def publish (g : CFG) (cache : Cache g) (before : Registry Nat (List Nat)) :
    List (Nat × Nat) → TransitionPublication.Published (List Nat) × Cache g
  | [] => (⟨before, []⟩, cache)
  | (symbol, kernel) :: rest =>
    let installed := intern g cache before kernel
    let later := publish g installed.2 installed.1.registry rest
    (⟨later.1.registry, (symbol, installed.1.id) :: later.1.rows⟩, later.2)

theorem publish_exact (g : CFG) (cache : Cache g) (before : Registry Nat (List Nat)) (groups : List (Nat × Nat)) :
    (publish g cache before groups).1 = TransitionPublication.publish (compute g) before groups := by
  induction groups generalizing cache before with
  | nil => rfl
  | cons group rest ih =>
    simp only [publish, ih, intern_exact, TransitionPublication.publish]

/-- Every installed kernel plan retains a template in this builder's cache. -/
def Covered (g : CFG) (cache : Cache g) (registry : Registry Nat (List Nat)) : Prop :=
  ∀ kernel ∈ registry.keys, cache.store (TemplateCapabilities.predictionMask g kernel) =
    some (TemplateCapabilities.materialize g (TemplateCapabilities.predictionMask g kernel))

theorem intern_covered (g : CFG) (cache : Cache g) (before : Registry Nat (List Nat))
    (covered : Covered g cache before) (kernel : Nat) :
    Covered g (intern g cache before kernel).2 (intern g cache before kernel).1.registry := by
  cases read : before.index kernel with
  | some id => simpa [intern, read] using covered
  | none =>
    intro key member
    change (intern g cache before kernel).2.store _ = _
    simp only [intern, read]
    rw [intern_exact] at member
    simp only [StateInterning.intern, read, List.mem_append, List.mem_singleton] at member
    rcases member with old | same
    · exact TemplateCapabilities.build_retained g ⟨cache, kernel⟩ _ _ (covered key old)
    · subst key
      exact TemplateCapabilities.build_stored g ⟨cache, kernel⟩

theorem publish_covered (g : CFG) (cache : Cache g) (before : Registry Nat (List Nat))
    (covered : Covered g cache before) (groups : List (Nat × Nat)) :
    Covered g (publish g cache before groups).2 (publish g cache before groups).1.registry := by
  induction groups generalizing cache before with
  | nil => exact covered
  | cons group rest ih => exact ih _ _ (intern_covered g cache before covered group.2)

/-- The cache entry agrees with the immutable template in each published
physical plan, not just with an unrelated set of discovered kernel keys. -/
theorem plan_template (g : CFG) (cache : Cache g) (before : Registry Nat (List Nat))
    (valid : Valid (compute g) before) (covered : Covered g cache before)
    (node kernel : Nat) (read : before.keys[node]? = some kernel) :
    ∃ template, cache.store (TemplateCapabilities.predictionMask g kernel) = some template ∧
      before.plans node = some (kernel, template.items) ∧
      template.groups = SparseGroupMasks.sharedRows (SymbolDiscovery.physicalIDs g) template.items := by
  refine ⟨TemplateCapabilities.materialize g (TemplateCapabilities.predictionMask g kernel),
    covered kernel (List.mem_of_getElem? read), ?_, rfl⟩
  exact valid.plans node kernel read

structure Traversal where
  sort : List SparseGroupMasks.Row → List SparseGroupMasks.Row
  ordered : ConstructionProtocol.SortContract sort
  width : Nat
  positive : 0 < width
  population : Nat → Nat
  fixnum : Nat → Bool
  firsts : Nat → SparseGroupMasks.Store

/-- Read the immutable template retained by a kernel plan. Absence is excluded
by Covered; it is not silently repaired by calling the factory again. -/
def groups (g : CFG) (traversal : Traversal) (cache : Cache g) (kernel : Nat) (firsts : SparseGroupMasks.Store) : List SparseGroupMasks.Row :=
  match cache.store (TemplateCapabilities.predictionMask g kernel) with
  | none => []
  | some template =>
    let context : TraversalCapabilities.Context := ⟨kernel, traversal.width, traversal.positive,
      traversal.population kernel, traversal.fixnum kernel⟩
    let scratch := GroupScratch.scan (SymbolDiscovery.physicalIDs g) (TraversalCapabilities.callbacks context).val
      (GroupScratch.borrow firsts)
    SparseGroupMasks.merge (fun key => scratch.masks key != 0)
      (traversal.sort (ConstructionProtocol.physicalOwn (SymbolDiscovery.physicalIDs g) template.items scratch)) template.groups []

theorem groups_exact (g : CFG) (traversal : Traversal) (cache : Cache g) (kernel : Nat)
    (found : cache.store (TemplateCapabilities.predictionMask g kernel) =
      some (TemplateCapabilities.materialize g (TemplateCapabilities.predictionMask g kernel)))
    (bounded : PublishedClosure.KernelBound g kernel) (firsts : SparseGroupMasks.Store) :
    groups g traversal cache kernel firsts = rows g kernel := by
  simp only [groups, found]
  change ConstructionProtocol.traversedGroups traversal.sort g
    ⟨kernel, traversal.width, traversal.positive, traversal.population kernel, traversal.fixnum kernel⟩ firsts = _
  exact ConstructionProtocol.traversedGroups_exact _ traversal.ordered _ _ bounded _

def tick (g : CFG) (traversal : Traversal) (cache : Cache g) (before : Machine) : Machine × Cache g :=
  match before.registry.pending with
  | [] => (before, cache)
  | node :: tail =>
    match before.registry.plans node with
    | none => (before, cache)
    | some (kernel, _) =>
      let inputs := (groups g traversal cache kernel (traversal.firsts before.registry.processed)).map fun row => (row.key, row.mask)
      let published := publish g cache (TransitionPublication.drain before.registry tail) inputs
      (⟨published.1.registry, ProducerAdmission.Storage.update before.transitions node published.1.rows⟩, published.2)

theorem tick_exact (g : CFG) (traversal : Traversal) (cache : Cache g) (before : Machine)
    (admitted : Invariant g before) (covered : Covered g cache before.registry) : (tick g traversal cache before).1 = ConstructionQueue.tick g before := by
  cases front : before.registry.pending with
  | nil => simp [tick, ConstructionQueue.tick, front]
  | cons node tail =>
    have bound := admitted.valid.bound node (by rw [front]; simp)
    let kernel := before.registry.keys[node]
    have read : before.registry.keys[node]? = some kernel := List.getElem?_eq_getElem bound
    have history := admitted.generated kernel (List.mem_of_getElem? read)
    have plan := admitted.valid.plans node kernel read
    have exactGroups := groups_exact g traversal cache kernel (covered kernel (List.mem_of_getElem? read)) (history_bound history) (traversal.firsts before.registry.processed)
    simp only [tick, ConstructionQueue.tick, front, read, plan, exactGroups, publish_exact, inputs]

theorem tick_covered (g : CFG) (traversal : Traversal) (cache : Cache g) (before : Machine)
    (covered : Covered g cache before.registry) :
    Covered g (tick g traversal cache before).2 (tick g traversal cache before).1.registry := by
  cases front : before.registry.pending with
  | nil => simpa [tick, front] using covered
  | cons node tail =>
    cases read : before.registry.plans node with
    | none => simpa [tick, front, read] using covered
    | some plan =>
      rcases plan with ⟨kernel, items⟩
      simp only [tick, front, read]
      exact publish_covered g cache (TransitionPublication.drain before.registry tail) covered _

def finish (g : CFG) (traversal : Traversal) (cache : Cache g) (before : Machine)
    (admitted : Invariant g before) (covered : Covered g cache before.registry) : Machine × Cache g :=
  if _empty : before.registry.pending = [] then (before, cache)
  else
    let later := tick g traversal cache before
    have valid : Invariant g later.1 := by
      dsimp only [later]
      rw [tick_exact g traversal cache before admitted covered]
      exact tick_invariant g before admitted
    finish g traversal later.2 later.1 valid (tick_covered g traversal cache before covered)
termination_by capacity g - before.registry.processed
decreasing_by
  rw [tick_exact g traversal cache before admitted covered]
  exact tick_rank g before admitted _empty

theorem finish_exact (g : CFG) (traversal : Traversal) (cache : Cache g) (before : Machine) (admitted : Invariant g before)
    (covered : Covered g cache before.registry) :
    (finish g traversal cache before admitted covered).1 = ConstructionQueue.finish g before admitted := by
  rw [finish, ConstructionQueue.finish]
  split
  · rfl
  · rw [finish_exact]
    simp only [tick_exact g traversal cache before admitted covered]
termination_by capacity g - before.registry.processed
decreasing_by
  rw [tick_exact g traversal cache before admitted covered]
  exact tick_rank g before admitted (by assumption)

theorem finish_covered (g : CFG) (traversal : Traversal) (cache : Cache g) (before : Machine)
    (admitted : Invariant g before) (covered : Covered g cache before.registry) :
    Covered g (finish g traversal cache before admitted covered).2
      (finish g traversal cache before admitted covered).1.registry := by
  rw [finish]
  split
  · exact covered
  · exact finish_covered g traversal _ _ _ (tick_covered g traversal cache before covered)
termination_by capacity g - before.registry.processed
decreasing_by
  rw [tick_exact g traversal cache before admitted covered]
  exact tick_rank g before admitted (by assumption)

def initial (g : CFG) : Machine × Cache g :=
  let installed := intern g (TemplateCapabilities.empty g) ConstructionQueue.empty 1
  (⟨installed.1.registry, fun _ => none⟩, installed.2)

theorem initial_exact (g : CFG) : (initial g).1 = ConstructionQueue.initial g := by
  simp only [initial, intern_exact, ConstructionQueue.initial]

theorem initial_covered (g : CFG) : Covered g (initial g).2 (initial g).1.registry :=
  intern_covered g _ ConstructionQueue.empty (by intro _ member; cases member) 1

/-- Cache ownership begins before initial installation and passes through
all discovered targets. No final-state validity certificate is supplied. -/
def construct (g : CFG) (nonempty : g ≠ []) (traversal : Traversal) : ConstructionProtocol.Product g × Cache g :=
  let start := initial g
  have valid : Invariant g start.1 := by rw [initial_exact]; exact initial_invariant g nonempty
  let result := finish g traversal start.2 start.1 valid (initial_covered g)
  have same : result.1 = (ConstructionProtocol.construct g nonempty).machine := by
    dsimp only [result]
    rw [finish_exact]
    simp only [start, initial_exact, ConstructionProtocol.construct]
  (⟨result.1, same.symm ▸ (ConstructionProtocol.construct g nonempty).invariant,
      same.symm ▸ (ConstructionProtocol.construct g nonempty).published,
      same.symm ▸ (ConstructionProtocol.construct g nonempty).empty⟩, result.2)

theorem construct_exact (g : CFG) (nonempty : g ≠ []) (traversal : Traversal) :
    (construct g nonempty traversal).1.machine = (ConstructionProtocol.construct g nonempty).machine := by
  simp only [construct, finish_exact, initial_exact, ConstructionProtocol.construct]

theorem construct_covered (g : CFG) (nonempty : g ≠ []) (traversal : Traversal) :
    Covered g (construct g nonempty traversal).2 (construct g nonempty traversal).1.machine.registry :=
  finish_covered g traversal _ _ _ (initial_covered g)

/-- Source-shape admission for the factory, its cache key and the complete
builder is independent of the algorithm-level simulation above. -/
theorem source_current : LRTransitionWitness.builderSource = LRTransitionSource.builder ∧
    LRTemplateWitness.templateSource = LRTemplateSource.template ∧
    LRTemplateWitness.cacheSource = LRTemplateSource.templateFor :=
  ⟨LRTransitionWitness.source_checked, LRTemplateWitness.source_checked⟩

end ParserEngine.CachedConstruction
