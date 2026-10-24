import ParserEngine.CachedConstruction

/-! A single source-shaped heap is threaded by the retained template consumer
and queue. Its joint history and all three logical views are derived together,
for every positive initial layout and arbitrary nonempty grammar. -/
namespace ParserEngine.ConstructionHeap
open Grammar StateInterning ConstructionQueue CachedConstruction ConstructionStorage

structure Owned (layout : PlanStorage.Layout) (g : CFG) (cache : Cache g)
    (registry : Registry Nat (List Nat)) (rows : Nat → Option Row) : Prop where
  trace : ConstructionStorage.Trace layout registry.keys.length cache.storage
  capacity : cache.storage.states.length = cache.memory.capacity
  views : Refines cache.storage registry.states cache.memory.plans rows

theorem intern_owned (layout : PlanStorage.Layout) (g : CFG) (cache : Cache g)
    (before : Registry Nat (List Nat)) (rows : Nat → Option Row)
    (owned : Owned layout g cache before rows) (kernel : Nat) :
    Owned layout g (CachedConstruction.intern g cache before kernel).2
      (CachedConstruction.intern g cache before kernel).1.registry rows := by
  cases found : before.index kernel with
  | some id => simpa [CachedConstruction.intern, found] using owned
  | none =>
    have shape := trace_shape owned.trace
    let plan : PlanStorage.Plan := ⟨kernel, (PlanStorage.fetch (TemplateCapabilities.materialize g)
      cache.memory (TemplateCapabilities.predictionMask g kernel)).1⟩
    have history := ConstructionStorage.Trace.append owned.trace (KernelConstruction.compute g kernel).1 plan
    have views := append_refines cache.storage before.keys.length (KernelConstruction.compute g kernel).1 plan shape owned.views
    refine ⟨?_, ?_, ?_⟩
    · simpa only [CachedConstruction.intern, found, StateInterning.intern, List.length_append,
        List.length_singleton, TemplateCapabilities.build_storage] using history
    · simp only [CachedConstruction.intern, found, TemplateCapabilities.build_storage,
        TemplateCapabilities.build_memory, append_exact cache.storage before.keys.length _ _ shape, VectorStorage.append_length,
        PlanStorage.install, owned.capacity]
    · have states : (CachedConstruction.intern g cache before kernel).1.registry.states =
          ProducerAdmission.Storage.update before.states before.keys.length (KernelConstruction.compute g kernel).1 := by
        rw [intern_exact]
        simp only [StateInterning.intern, found]
      rw [states]
      simpa only [CachedConstruction.intern, found, TemplateCapabilities.build_storage,
        TemplateCapabilities.build_memory, PlanStorage.install_plans] using views

theorem publish_owned (layout : PlanStorage.Layout) (g : CFG) (cache : Cache g)
    (before : Registry Nat (List Nat)) (rows : Nat → Option Row)
    (owned : Owned layout g cache before rows) (groups : List (Nat × Nat)) :
    Owned layout g (CachedConstruction.publish g cache before groups).2
      (CachedConstruction.publish g cache before groups).1.registry rows := by
  induction groups generalizing cache before with
  | nil => exact owned
  | cons group rest ih => exact ih _ _ (intern_owned layout g cache before rows owned group.2)

theorem publish_bound (g : CFG) (cache : Cache g) (before : Registry Nat (List Nat)) (groups : List (Nat × Nat)) :
    before.keys.length ≤ (CachedConstruction.publish g cache before groups).1.registry.keys.length := by
  induction groups generalizing cache before with
  | nil => exact Nat.le_refl _
  | cons group rest ih =>
    have step : before.keys.length ≤ (CachedConstruction.intern g cache before group.2).1.registry.keys.length := by
      rw [intern_exact]
      unfold StateInterning.intern
      split
      · exact Nat.le_refl _
      · simp
    exact Nat.le_trans step (ih _ _)

theorem tick_owned (layout : PlanStorage.Layout) (g : CFG) (traversal : Traversal) (cache : Cache g)
    (before : Machine) (admitted : Invariant g before)
    (owned : Owned layout g cache before.registry before.transitions) :
    Owned layout g (CachedConstruction.tick g traversal cache before).2
      (CachedConstruction.tick g traversal cache before).1.registry
      (CachedConstruction.tick g traversal cache before).1.transitions := by
  cases front : before.registry.pending with
  | nil => simpa only [CachedConstruction.tick, front] using owned
  | cons node tail =>
    cases read : cache.readTemplate node with
    | none => simpa only [CachedConstruction.tick, front, read] using owned
    | some pair =>
      obtain ⟨kernel, template⟩ := pair
      let grouped := CachedConstruction.runGroup g traversal cache kernel template
      let drained := TransitionPublication.drain before.registry tail
      let inputs := grouped.1.map fun row => (row.key, row.mask)
      have groupedOwned : Owned layout g grouped.2 drained before.transitions :=
        ⟨owned.trace, owned.capacity, owned.views⟩
      have result := publish_owned layout g grouped.2 drained before.transitions groupedOwned inputs
      have bound : node < (CachedConstruction.publish g grouped.2 drained inputs).1.registry.keys.length :=
        Nat.lt_of_lt_of_le (admitted.valid.bound node (by rw [front]; simp))
          (publish_bound g grouped.2 drained inputs)
      have history := ConstructionStorage.Trace.publish result.trace node
        (CachedConstruction.publish g grouped.2 drained inputs).1.rows bound
      have views := publish_refines _ _ node (CachedConstruction.publish g grouped.2 drained inputs).1.rows
        (trace_shape result.trace) bound result.views
      refine ⟨?_, ?_, ?_⟩
      · simpa only [CachedConstruction.tick, front, read] using history
      · simpa only [CachedConstruction.tick, front, read, ConstructionStorage.publish] using result.capacity
      · simpa only [CachedConstruction.tick, front, read] using views

theorem finish_owned (layout : PlanStorage.Layout) (g : CFG) (traversal : Traversal) (cache : Cache g)
    (before : Machine) (admitted : Invariant g before) (covered : Covered g cache before.registry)
    (aligned : Aligned g cache before.registry) (owned : Owned layout g cache before.registry before.transitions) :
    Owned layout g (CachedConstruction.finish g traversal cache before admitted covered aligned).2
      (CachedConstruction.finish g traversal cache before admitted covered aligned).1.registry
      (CachedConstruction.finish g traversal cache before admitted covered aligned).1.transitions := by
  rw [CachedConstruction.finish]
  split
  · exact owned
  · exact finish_owned layout g traversal _ _ _ (tick_covered g traversal cache before covered)
      (tick_aligned g traversal cache before aligned) (tick_owned layout g traversal cache before admitted owned)
termination_by capacity g - before.registry.processed
decreasing_by
  rw [tick_exact g traversal cache before admitted covered aligned]
  exact tick_rank g before admitted (by assumption)

theorem initial_owned (layout : PlanStorage.Layout) (g : CFG) :
    Owned layout g (CachedConstruction.initial g layout).2
      (CachedConstruction.initial g layout).1.registry (CachedConstruction.initial g layout).1.transitions := by
  have emptyOwned : Owned layout g (TemplateCapabilities.empty g layout) ConstructionQueue.empty (fun _ => none) :=
    ⟨ConstructionStorage.Trace.start, by simp [TemplateCapabilities.empty, ConstructionStorage.empty, PlanStorage.empty],
      empty_refines layout⟩
  exact intern_owned layout g _ _ _ emptyOwned 1

theorem construct_owned (layout : PlanStorage.Layout) (g : CFG) (nonempty : g ≠ []) (traversal : Traversal) :
    let result := CachedConstruction.construct g nonempty traversal layout
    Owned layout g result.2 result.1.machine.registry result.1.machine.transitions :=
  finish_owned layout g traversal _ _ _ (initial_covered g layout) (initial_aligned g layout) (initial_owned layout g)

end ParserEngine.ConstructionHeap
