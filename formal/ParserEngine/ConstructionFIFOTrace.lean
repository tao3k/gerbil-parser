import ParserEngine.ConstructionFIFO

/-! Safety of the actions emitted by actual FIFO execution. A separate safe
history with the same final heap is insufficient for this obligation. -/
namespace ParserEngine.ConstructionFIFO
open ConstructionQueue CachedConstruction ConstructionStorage

theorem intern_safe (layout : PlanStorage.Layout) (g : Grammar.CFG) (cache : Cache g)
    (before : StateInterning.Registry Nat (List Nat)) (rows : Nat → Option Row)
    (owned : ConstructionHeap.Owned layout g cache before rows) (kernel : Nat) :
    let installed := CachedConstruction.intern g cache before kernel
    ConstructionControl.Safe ConstructionControl.canonical
      (if installed.1.fresh then [appendAction g cache kernel] else []) before.keys.length cache.storage := by
  cases found : before.index kernel with
  | some id => simp only [CachedConstruction.intern, found, Bool.false_eq_true, ↓reduceIte, ConstructionControl.Safe]
  | none =>
    simp only [CachedConstruction.intern, found, StateInterning.intern, ↓reduceIte, appendAction, ConstructionControl.Safe]
    exact ⟨trace_shape owned.trace, ConstructionControl.growth_safe _ _ (trace_shape owned.trace), trivial⟩

theorem groups_safe (layout : PlanStorage.Layout) (g : Grammar.CFG) (cache : Cache g)
    (before : StateInterning.Registry Nat (List Nat)) (rows : Nat → Option Row)
    (owned : ConstructionHeap.Owned layout g cache before rows) (inputs : List (Nat × Nat)) (accumulator : Row) :
    ConstructionControl.Safe ConstructionControl.canonical (groups canonical g cache before inputs accumulator).actions
      before.keys.length cache.storage := by
  induction inputs generalizing cache before accumulator with
  | nil => trivial
  | cons group rest ih =>
    simp only [groups, target_exact]
    apply ConstructionControl.safe_append _ _ _ _ _ (intern_safe layout g cache before rows owned group.2)
    rw [← ConstructionAppend.execute_exact, intern_replay]
    exact ih _ _ (ConstructionHeap.intern_owned layout g cache before rows owned group.2) _

theorem tick_safe (layout : PlanStorage.Layout) (g : Grammar.CFG) (traversal : Traversal) (cache : Cache g)
    (before : Machine) (admitted : Invariant g before) (aligned : Aligned g cache before.registry)
    (owned : ConstructionHeap.Owned layout g cache before.registry before.transitions) :
    ConstructionControl.Safe ConstructionControl.canonical (tick canonical g traversal cache before).actions
      before.registry.keys.length cache.storage := by
  cases front : before.registry.pending with
  | nil => simp [tick, front, ConstructionControl.Safe]
  | cons node tail =>
    have bound := admitted.valid.bound node (by rw [front]; simp)
    let kernel := before.registry.keys[node]
    have read : before.registry.keys[node]? = some kernel := List.getElem?_eq_getElem bound
    have plan := aligned node kernel read
    let grouped := runGroup g traversal cache kernel (TemplateCapabilities.materialize g (TemplateCapabilities.predictionMask g kernel))
    let drained := TransitionPublication.drain before.registry tail
    let inputs := grouped.1.map fun row => (row.key, row.mask)
    have groupedOwned : ConstructionHeap.Owned layout g grouped.2 drained before.transitions :=
      ⟨owned.trace, owned.capacity, owned.views⟩
    have prior := groups_safe layout g grouped.2 drained before.transitions groupedOwned inputs []
    have inside : node < (groups canonical g grouped.2 drained inputs []).registry.keys.length := by
      rw [groups_registry]
      exact Nat.lt_of_lt_of_le bound (ConstructionHeap.publish_bound g grouped.2 drained inputs)
    simp only [tick, front, List.cons_ne_nil, ↓reduceIte,
      show canonical.queue = [.dequeue, .process, .groups, .publish] from rfl,
      List.foldl_cons, List.foldl_nil, queueStep, plan, List.nil_append]
    apply ConstructionControl.safe_append _ _ _ _ _ prior
    rw [← ConstructionAppend.execute_exact]
    change ConstructionControl.Safe ConstructionControl.canonical
      [.publish node (groups canonical g grouped.2 drained inputs []).rows.reverse]
      (ConstructionAppend.execute ConstructionControl.canonical ConstructionAppend.canonical
        (groups canonical g grouped.2 drained inputs []).actions drained.keys.length grouped.2.storage).1
      (ConstructionAppend.execute ConstructionControl.canonical ConstructionAppend.canonical
        (groups canonical g grouped.2 drained inputs []).actions drained.keys.length grouped.2.storage).2
    rw [groups_replay]
    exact ⟨inside, trivial⟩

theorem finish_safe (layout : PlanStorage.Layout) (g : Grammar.CFG) (traversal : Traversal) (cache : Cache g)
    (before : Machine) (admitted : Invariant g before) (covered : Covered g cache before.registry)
    (aligned : Aligned g cache before.registry) (owned : ConstructionHeap.Owned layout g cache before.registry before.transitions) :
    ConstructionControl.Safe ConstructionControl.canonical (finish g traversal cache before admitted covered aligned).actions
      before.registry.keys.length cache.storage := by
  rw [finish]
  split
  · trivial
  · dsimp only
    apply ConstructionControl.safe_append _ _ _ _ _ (tick_safe layout g traversal cache before admitted aligned owned)
    rw [← ConstructionAppend.execute_exact, tick_replay g traversal cache before admitted aligned]
    apply finish_safe layout g traversal _ _ _ _ _
    have same := tick_exact g traversal cache before admitted aligned
    rw [show (tick canonical g traversal cache before).machine = (CachedConstruction.tick g traversal cache before).1 from congrArg Prod.fst same,
      show (tick canonical g traversal cache before).cache = (CachedConstruction.tick g traversal cache before).2 from congrArg Prod.snd same]
    exact ConstructionHeap.tick_owned layout g traversal cache before admitted owned
termination_by capacity g - before.registry.processed
decreasing_by
  rw [show (tick canonical g traversal cache before).machine = (CachedConstruction.tick g traversal cache before).1 from
    congrArg Prod.fst (tick_exact g traversal cache before admitted aligned),
    CachedConstruction.tick_exact g traversal cache before admitted covered aligned]
  exact tick_rank g before admitted (by assumption)

theorem construct_safe (g : Grammar.CFG) (nonempty : g ≠ []) (traversal : Traversal) (layout : PlanStorage.Layout) :
    ConstructionControl.Safe ConstructionControl.canonical (construct g nonempty traversal layout).actions 0 (ConstructionStorage.empty layout) := by
  let initial := start canonical ConstructionInitialization.canonical g layout
  have valid : Invariant g initial.1 := by dsimp only [initial]; rw [start_exact, CachedConstruction.initial_exact]; exact initial_invariant g nonempty
  have covered : Covered g initial.2 initial.1.registry := by dsimp only [initial]; rw [start_exact]; exact initial_covered g layout
  have aligned : Aligned g initial.2 initial.1.registry := by dsimp only [initial]; rw [start_exact]; exact initial_aligned g layout
  have owned : ConstructionHeap.Owned layout g initial.2 initial.1.registry initial.1.transitions := by
    dsimp only [initial]; rw [start_exact]; exact ConstructionHeap.initial_owned layout g
  have later := finish_safe layout g traversal initial.2 initial.1 valid covered aligned owned
  have count : initial.1.registry.keys.length = 1 := by dsimp only [initial]; rw [start_exact]; rfl
  have heap : initial.2.storage = ConstructionStorage.append (ConstructionStorage.empty layout) 0
      (KernelConstruction.compute g 1).1 ⟨1, (PlanStorage.fetch (TemplateCapabilities.materialize g)
        (TemplateCapabilities.empty g layout).memory (TemplateCapabilities.predictionMask g 1)).1⟩ := by
    dsimp only [initial]; rw [start_exact]
    simp only [CachedConstruction.initial, CachedConstruction.intern, ConstructionQueue.empty,
      TemplateCapabilities.build_storage, TemplateCapabilities.empty]
    rfl
  rw [count, heap] at later
  simp only [construct, appendAction, ConstructionControl.Safe, ConstructionControl.append_exact]
  exact ⟨empty_shape layout, ConstructionControl.growth_safe _ _ (empty_shape layout), later⟩

theorem build_safe (g : Grammar.CFG) (nonempty : g ≠ []) (traversal : Traversal) (layout : PlanStorage.Layout) :
    ConstructionControl.Safe ConstructionControl.canonical (build g nonempty traversal layout).actions 0 (ConstructionStorage.empty layout) :=
  construct_safe g nonempty traversal layout

end ParserEngine.ConstructionFIFO
