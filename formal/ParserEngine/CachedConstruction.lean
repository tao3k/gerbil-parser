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
    let built := TemplateCapabilities.build g ⟨cache, kernel, before.keys.length⟩
    (StateInterning.intern (fun _ => built.product) before kernel, built.cache)

theorem intern_exact (g : CFG) (cache : Cache g) (before : Registry Nat (List Nat)) (kernel : Nat) :
    (intern g cache before kernel).1 = StateInterning.intern (compute g) before kernel := by
  cases read : before.index kernel with
  | some id => simp [intern, StateInterning.intern, read]
  | none => simp [intern, StateInterning.intern, read, TemplateCapabilities.build_exact g ⟨cache, kernel, before.keys.length⟩]

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

theorem intern_scratch (g : CFG) (cache : Cache g) (before : Registry Nat (List Nat)) (kernel : Nat) :
    (intern g cache before kernel).2.scratch = cache.scratch := by
  cases found : before.index kernel <;> simp [intern, found, TemplateCapabilities.build_scratch]

theorem publish_scratch (g : CFG) (cache : Cache g) (before : Registry Nat (List Nat)) (groups : List (Nat × Nat)) :
    (publish g cache before groups).2.scratch = cache.scratch := by
  induction groups generalizing cache before with
  | nil => rfl
  | cons group rest ih => simp only [publish, ih, intern_scratch]

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
    · exact TemplateCapabilities.build_retained g ⟨cache, kernel, before.keys.length⟩ _ _ (covered key old)
    · subst key
      exact TemplateCapabilities.build_stored g ⟨cache, kernel, before.keys.length⟩

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

/-- Physical pair/reference reads agree with each installed abstract plan. -/
def Aligned (g : CFG) (cache : Cache g) (registry : Registry Nat (List Nat)) : Prop :=
  ∀ node kernel, registry.keys[node]? = some kernel →
    cache.readTemplate node = some (kernel, TemplateCapabilities.materialize g (TemplateCapabilities.predictionMask g kernel))

theorem intern_aligned (g : CFG) (cache : Cache g) (before : Registry Nat (List Nat))
    (aligned : Aligned g cache before) (kernel : Nat) :
    Aligned g (intern g cache before kernel).2 (intern g cache before kernel).1.registry := by
  cases lookup : before.index kernel with
  | some id => simpa [intern, lookup] using aligned
  | none =>
    intro node key read
    rw [intern_exact] at read
    simp only [StateInterning.intern, lookup] at read
    simp only [intern, lookup]
    by_cases old : node < before.keys.length
    · rw [TemplateCapabilities.build_other_plan g ⟨cache, kernel, before.keys.length⟩ node (Nat.ne_of_lt old)]
      exact aligned node key (by simpa [List.getElem?_append_left old] using read)
    · rw [List.getElem?_append_right (by omega)] at read
      have same : key = kernel := by simpa using List.mem_of_getElem? read
      have zero : node - before.keys.length = 0 := by
        by_cases zero : node - before.keys.length = 0
        · exact zero
        · have outside : ([kernel] : List Nat).length ≤ node - before.keys.length := by simp; omega
          rw [List.getElem?_eq_none outside] at read
          cases read
      have equal : node = before.keys.length := by omega
      subst key; subst node
      exact TemplateCapabilities.build_plan g ⟨cache, kernel, before.keys.length⟩

theorem publish_aligned (g : CFG) (cache : Cache g) (before : Registry Nat (List Nat))
    (aligned : Aligned g cache before) (groups : List (Nat × Nat)) :
    Aligned g (publish g cache before groups).2 (publish g cache before groups).1.registry := by
  induction groups generalizing cache before with
  | nil => exact aligned
  | cons group rest ih => exact ih _ _ (intern_aligned g cache before aligned group.2)

/-- A vector witness is generated by the source's counted copy and captured-ID
writes, alongside this exact memory and discovered-state count. -/
def Physical (g : CFG) (cache : Cache g) (registry : Registry Nat (List Nat)) : Prop :=
  ∃ slots, PlanStorage.Trace (TemplateCapabilities.materialize g) (TemplateCapabilities.predictionMask g)
    cache.memory registry.keys.length slots

theorem intern_physical (g : CFG) (cache : Cache g) (before : Registry Nat (List Nat))
    (physical : Physical g cache before) (kernel : Nat) :
    Physical g (intern g cache before kernel).2 (intern g cache before kernel).1.registry := by
  cases found : before.index kernel with
  | some id => simpa [intern, found] using physical
  | none =>
    obtain ⟨slots, trace⟩ := physical
    simp only [intern, found, Physical, StateInterning.intern, List.length_append, List.length_singleton,
      TemplateCapabilities.build_memory]
    exact ⟨_, PlanStorage.Trace.next trace kernel⟩

theorem publish_physical (g : CFG) (cache : Cache g) (before : Registry Nat (List Nat))
    (physical : Physical g cache before) (groups : List (Nat × Nat)) :
    Physical g (publish g cache before groups).2 (publish g cache before groups).1.registry := by
  induction groups generalizing cache before with
  | nil => exact physical
  | cons group rest ih => exact ih _ _ (intern_physical g cache before physical group.2)

structure Traversal where
  sort : List SparseGroupMasks.Row → List SparseGroupMasks.Row
  ordered : ConstructionProtocol.SortContract sort
  width : Nat
  positive : 0 < width
  population : Nat → Nat
  fixnum : Nat → Bool


theorem clean_borrow (scratch : GroupScratch.State)
    (clean : scratch.masks = (fun _ => 0) ∧ scratch.order = []) : scratch = GroupScratch.borrow scratch.firsts := by
  cases scratch with
  | mk masks firsts order =>
    obtain ⟨rfl, rfl⟩ := clean
    rfl

theorem reset_clean (symbol : Nat → Option Nat) (items : List Nat) (before : GroupScratch.State)
    (clean : before.masks = (fun _ => 0) ∧ before.order = []) :
    (GroupScratch.reset (GroupScratch.scan symbol items before)).masks = (fun _ => 0) ∧
      (GroupScratch.reset (GroupScratch.scan symbol items before)).order = [] := by
  rw [clean_borrow before clean]
  exact ⟨GroupScratch.borrowed_reset symbol items before.firsts, rfl⟩

/-- The same scratch cells survive each call. Only touched masks are reset;
first-item slots come from the preceding scan rather than an external oracle. -/
def runGroup (g : CFG) (traversal : Traversal) (cache : Cache g) (kernel : Nat)
    (template : TemplateCapabilities.Template) : List SparseGroupMasks.Row × Cache g :=
  let context : TraversalCapabilities.Context := ⟨kernel, traversal.width, traversal.positive,
    traversal.population kernel, traversal.fixnum kernel⟩
  let scanned := GroupScratch.scan (SymbolDiscovery.physicalIDs g) (TraversalCapabilities.callbacks context).val cache.scratch
  let rows := SparseGroupMasks.merge (fun key => scanned.masks key != 0)
    (traversal.sort (TemplateIndex.own template.index scanned)) template.groups []
  (rows, ⟨cache.memory, cache.admitted, GroupScratch.reset scanned, reset_clean _ _ _ cache.clean⟩)

theorem groups_exact (g : CFG) (traversal : Traversal) (cache : Cache g) (kernel : Nat)
    (bounded : PublishedClosure.KernelBound g kernel) :
    (runGroup g traversal cache kernel (TemplateCapabilities.materialize g (TemplateCapabilities.predictionMask g kernel))).1 = rows g kernel := by
  dsimp only [runGroup, TemplateCapabilities.materialize]
  rw [TemplateIndex.own_exact, clean_borrow cache.scratch cache.clean]
  change ConstructionProtocol.traversedGroups traversal.sort g
    ⟨kernel, traversal.width, traversal.positive, traversal.population kernel, traversal.fixnum kernel⟩ cache.scratch.firsts = _
  exact ConstructionProtocol.traversedGroups_exact _ traversal.ordered _ _ bounded _

/-- Scratch writes cannot mutate any template object or physical plan slot. -/
theorem runGroup_memory (g : CFG) (traversal : Traversal) (cache : Cache g) (kernel : Nat)
    (template : TemplateCapabilities.Template) : (runGroup g traversal cache kernel template).2.memory = cache.memory := rfl

theorem runGroup_scratch (g : CFG) (traversal : Traversal) (cache : Cache g) (kernel : Nat)
    (template : TemplateCapabilities.Template) :
    (runGroup g traversal cache kernel template).2.scratch =
      GroupScratch.reset (GroupScratch.scan (SymbolDiscovery.physicalIDs g)
        (TraversalCapabilities.callbacks ⟨kernel, traversal.width, traversal.positive,
          traversal.population kernel, traversal.fixnum kernel⟩).val cache.scratch) := rfl

def tick (g : CFG) (traversal : Traversal) (cache : Cache g) (before : Machine) : Machine × Cache g :=
  match before.registry.pending with
  | [] => (before, cache)
  | node :: tail =>
    match cache.readTemplate node with
    | none => (before, cache)
    | some (kernel, template) =>
      let grouped := runGroup g traversal cache kernel template
      let inputs := grouped.1.map fun row => (row.key, row.mask)
      let published := publish g grouped.2 (TransitionPublication.drain before.registry tail) inputs
      (⟨published.1.registry, ProducerAdmission.Storage.update before.transitions node published.1.rows⟩, published.2)

theorem tick_exact (g : CFG) (traversal : Traversal) (cache : Cache g) (before : Machine)
    (admitted : Invariant g before) (_covered : Covered g cache before.registry) (aligned : Aligned g cache before.registry) :
    (tick g traversal cache before).1 = ConstructionQueue.tick g before := by
  cases front : before.registry.pending with
  | nil => simp [tick, ConstructionQueue.tick, front]
  | cons node tail =>
    have bound := admitted.valid.bound node (by rw [front]; simp)
    let kernel := before.registry.keys[node]
    have read : before.registry.keys[node]? = some kernel := List.getElem?_eq_getElem bound
    have history := admitted.generated kernel (List.mem_of_getElem? read)
    have plan := aligned node kernel read
    have exactGroups := groups_exact g traversal cache kernel (history_bound history)
    simp only [tick, ConstructionQueue.tick, front, read, plan, exactGroups, publish_exact, inputs]

/-- Target installation and publication preserve exactly the scratch returned
by this source plan's group call, including its intentionally stale firsts. -/
theorem tick_scratch (g : CFG) (traversal : Traversal) (cache : Cache g) (before : Machine)
    (node : Nat) (tail : List Nat) (kernel : Nat) (template : TemplateCapabilities.Template)
    (front : before.registry.pending = node :: tail)
    (plan : cache.readTemplate node = some (kernel, template)) :
    (tick g traversal cache before).2.scratch = (runGroup g traversal cache kernel template).2.scratch := by
  simp only [tick, front, plan, publish_scratch]

theorem tick_covered (g : CFG) (traversal : Traversal) (cache : Cache g) (before : Machine)
    (covered : Covered g cache before.registry) :
    Covered g (tick g traversal cache before).2 (tick g traversal cache before).1.registry := by
  cases front : before.registry.pending with
  | nil => simpa [tick, front] using covered
  | cons node tail =>
    cases read : cache.readTemplate node with
    | none => simpa [tick, front, read] using covered
    | some plan =>
      rcases plan with ⟨kernel, items⟩
      simp only [tick, front, read]
      exact publish_covered g (runGroup g traversal cache kernel items).2 (TransitionPublication.drain before.registry tail) covered _

theorem tick_aligned (g : CFG) (traversal : Traversal) (cache : Cache g) (before : Machine)
    (aligned : Aligned g cache before.registry) :
    Aligned g (tick g traversal cache before).2 (tick g traversal cache before).1.registry := by
  cases front : before.registry.pending with
  | nil => simpa [tick, front] using aligned
  | cons node tail =>
    cases read : cache.readTemplate node with
    | none => simpa [tick, front, read] using aligned
    | some plan =>
      rcases plan with ⟨kernel, items⟩
      simp only [tick, front, read]
      exact publish_aligned g (runGroup g traversal cache kernel items).2 (TransitionPublication.drain before.registry tail) aligned _

theorem tick_physical (g : CFG) (traversal : Traversal) (cache : Cache g) (before : Machine)
    (physical : Physical g cache before.registry) :
    Physical g (tick g traversal cache before).2 (tick g traversal cache before).1.registry := by
  cases front : before.registry.pending with
  | nil => simpa [tick, front] using physical
  | cons node tail =>
    cases read : cache.readTemplate node with
    | none => simpa [tick, front, read] using physical
    | some plan =>
      rcases plan with ⟨kernel, template⟩
      simp only [tick, front, read]
      exact publish_physical g (runGroup g traversal cache kernel template).2
        (TransitionPublication.drain before.registry tail) physical _

def finish (g : CFG) (traversal : Traversal) (cache : Cache g) (before : Machine)
    (admitted : Invariant g before) (covered : Covered g cache before.registry)
    (aligned : Aligned g cache before.registry) : Machine × Cache g :=
  if _empty : before.registry.pending = [] then (before, cache)
  else
    let later := tick g traversal cache before
    have valid : Invariant g later.1 := by
      dsimp only [later]
      rw [tick_exact g traversal cache before admitted covered aligned]
      exact tick_invariant g before admitted
    finish g traversal later.2 later.1 valid (tick_covered g traversal cache before covered) (tick_aligned g traversal cache before aligned)
termination_by capacity g - before.registry.processed
decreasing_by
  rw [tick_exact g traversal cache before admitted covered aligned]
  exact tick_rank g before admitted _empty

theorem finish_exact (g : CFG) (traversal : Traversal) (cache : Cache g) (before : Machine) (admitted : Invariant g before)
    (covered : Covered g cache before.registry) (aligned : Aligned g cache before.registry) :
    (finish g traversal cache before admitted covered aligned).1 = ConstructionQueue.finish g before admitted := by
  rw [finish, ConstructionQueue.finish]
  split
  · rfl
  · rw [finish_exact]
    simp only [tick_exact g traversal cache before admitted covered aligned]
termination_by capacity g - before.registry.processed
decreasing_by
  rw [tick_exact g traversal cache before admitted covered aligned]
  exact tick_rank g before admitted (by assumption)

theorem finish_covered (g : CFG) (traversal : Traversal) (cache : Cache g) (before : Machine)
    (admitted : Invariant g before) (covered : Covered g cache before.registry) (aligned : Aligned g cache before.registry) :
    Covered g (finish g traversal cache before admitted covered aligned).2
      (finish g traversal cache before admitted covered aligned).1.registry := by
  rw [finish]
  split
  · exact covered
  · exact finish_covered g traversal _ _ _ (tick_covered g traversal cache before covered) (tick_aligned g traversal cache before aligned)
termination_by capacity g - before.registry.processed
decreasing_by
  rw [tick_exact g traversal cache before admitted covered aligned]
  exact tick_rank g before admitted (by assumption)

theorem finish_aligned (g : CFG) (traversal : Traversal) (cache : Cache g) (before : Machine)
    (admitted : Invariant g before) (covered : Covered g cache before.registry) (aligned : Aligned g cache before.registry) :
    Aligned g (finish g traversal cache before admitted covered aligned).2
      (finish g traversal cache before admitted covered aligned).1.registry := by
  rw [finish]
  split
  · exact aligned
  · exact finish_aligned g traversal _ _ _ (tick_covered g traversal cache before covered) (tick_aligned g traversal cache before aligned)
termination_by capacity g - before.registry.processed
decreasing_by
  rw [tick_exact g traversal cache before admitted covered aligned]
  exact tick_rank g before admitted (by assumption)

theorem finish_physical (g : CFG) (traversal : Traversal) (cache : Cache g) (before : Machine)
    (admitted : Invariant g before) (covered : Covered g cache before.registry) (aligned : Aligned g cache before.registry)
    (physical : Physical g cache before.registry) :
    Physical g (finish g traversal cache before admitted covered aligned).2
      (finish g traversal cache before admitted covered aligned).1.registry := by
  rw [finish]
  split
  · exact physical
  · exact finish_physical g traversal _ _ _ (tick_covered g traversal cache before covered)
      (tick_aligned g traversal cache before aligned) (tick_physical g traversal cache before physical)
termination_by capacity g - before.registry.processed
decreasing_by
  rw [tick_exact g traversal cache before admitted covered aligned]
  exact tick_rank g before admitted (by assumption)

def initial (g : CFG) : Machine × Cache g :=
  let installed := intern g (TemplateCapabilities.empty g) ConstructionQueue.empty 1
  (⟨installed.1.registry, fun _ => none⟩, installed.2)

theorem initial_exact (g : CFG) : (initial g).1 = ConstructionQueue.initial g := by
  simp only [initial, intern_exact, ConstructionQueue.initial]

theorem initial_covered (g : CFG) : Covered g (initial g).2 (initial g).1.registry :=
  intern_covered g _ ConstructionQueue.empty (by intro _ member; cases member) 1

theorem initial_aligned (g : CFG) : Aligned g (initial g).2 (initial g).1.registry :=
  intern_aligned g _ ConstructionQueue.empty (by intro _ _ read; cases read) 1

theorem initial_physical (g : CFG) : Physical g (initial g).2 (initial g).1.registry :=
  intern_physical g _ ConstructionQueue.empty ⟨_, PlanStorage.Trace.start⟩ 1

/-- Cache ownership begins before initial installation and passes through
all discovered targets. No final-state validity certificate is supplied. -/
def construct (g : CFG) (nonempty : g ≠ []) (traversal : Traversal) : ConstructionProtocol.Product g × Cache g :=
  let start := initial g
  have valid : Invariant g start.1 := by rw [initial_exact]; exact initial_invariant g nonempty
  let result := finish g traversal start.2 start.1 valid (initial_covered g) (initial_aligned g)
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
  finish_covered g traversal _ _ _ (initial_covered g) (initial_aligned g)

theorem construct_aligned (g : CFG) (nonempty : g ≠ []) (traversal : Traversal) :
    Aligned g (construct g nonempty traversal).2 (construct g nonempty traversal).1.machine.registry :=
  finish_aligned g traversal _ _ _ (initial_covered g) (initial_aligned g)

theorem construct_physical (g : CFG) (nonempty : g ≠ []) (traversal : Traversal) :
    Physical g (construct g nonempty traversal).2 (construct g nonempty traversal).1.machine.registry :=
  finish_physical g traversal _ _ _ (initial_covered g) (initial_aligned g) (initial_physical g)

/-- Every final plan read has a concrete vector generated by the entire append
history. Capacity and copy coverage are derived, not supplied by the caller. -/
theorem constructed_vector (g : CFG) (nonempty : g ≠ []) (traversal : Traversal) :
    let result := construct g nonempty traversal
    ∃ slots, slots.length = result.2.memory.capacity ∧ result.1.machine.registry.keys.length ≤ slots.length ∧
      ∀ node, node < result.1.machine.registry.keys.length →
        PlanStorage.decodeVector result.2.memory.objects slots node = result.2.readTemplate node := by
  obtain ⟨slots, trace⟩ := construct_physical g nonempty traversal
  exact ⟨slots, (PlanStorage.trace_shape trace).1.symm, (PlanStorage.trace_shape trace).2.1,
    fun node inside => PlanStorage.trace_decode trace node inside⟩

/-- Source-shape admission for the factory, its cache key and the complete
builder is independent of the algorithm-level simulation above. -/
theorem source_current : LRTransitionWitness.builderSource = LRTransitionSource.builder ∧
    LRTemplateWitness.templateSource = LRTemplateSource.template ∧
    LRTemplateWitness.cacheSource = LRTemplateSource.templateFor :=
  ⟨LRTransitionWitness.source_checked, LRTemplateWitness.source_checked⟩

end ParserEngine.CachedConstruction
