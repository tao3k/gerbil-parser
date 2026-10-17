import ParserEngine.ConstructionFIFOTrace

/-! Every prefix of the actual source-order queue retains its physical plan
readiness. This is a trajectory invariant, not a final-cache certificate or a
caller-supplied template. Host vector and reference representation stay external. -/
namespace ParserEngine.ConstructionReadiness
open ConstructionQueue CachedConstruction ConstructionStorage

inductive Visit (g : Grammar.CFG) (traversal : Traversal) (layout : PlanStorage.Layout) :
    Machine → Cache g → Prop where
  | initial : Visit g traversal layout
      (ConstructionFIFO.start ConstructionFIFO.canonical ConstructionInitialization.canonical g layout).1
      (ConstructionFIFO.start ConstructionFIFO.canonical ConstructionInitialization.canonical g layout).2
  | next {machine : Machine} {cache : Cache g} : Visit g traversal layout machine cache →
      Visit g traversal layout (ConstructionFIFO.tick ConstructionFIFO.canonical g traversal cache machine).machine
        (ConstructionFIFO.tick ConstructionFIFO.canonical g traversal cache machine).cache

structure Facts (g : Grammar.CFG) (layout : PlanStorage.Layout) (machine : Machine) (cache : Cache g) : Prop where
  valid : Invariant g machine
  covered : Covered g cache machine.registry
  aligned : Aligned g cache machine.registry
  owned : ConstructionHeap.Owned layout g cache machine.registry machine.transitions

theorem visited (g : Grammar.CFG) (nonempty : g ≠ []) (traversal : Traversal) (layout : PlanStorage.Layout)
    (machine : Machine) (cache : Cache g) (visit : Visit g traversal layout machine cache) :
    Facts g layout machine cache := by
  induction visit with
  | initial =>
    rw [ConstructionFIFO.start_exact]
    exact ⟨by rw [CachedConstruction.initial_exact]; exact initial_invariant g nonempty,
      initial_covered g layout, initial_aligned g layout, ConstructionHeap.initial_owned layout g⟩
  | @next before cache prior ih =>
    have same := ConstructionFIFO.tick_exact g traversal cache before ih.valid ih.aligned
    have machineEq := congrArg Prod.fst same
    have cacheEq := congrArg Prod.snd same
    dsimp only at machineEq cacheEq
    constructor
    · rw [machineEq, CachedConstruction.tick_exact g traversal cache before ih.valid ih.covered ih.aligned]
      exact tick_invariant g before ih.valid
    · rw [machineEq, cacheEq]
      exact tick_covered g traversal cache before ih.covered
    · rw [machineEq, cacheEq]
      exact tick_aligned g traversal cache before ih.aligned
    · rw [machineEq, cacheEq]
      exact ConstructionHeap.tick_owned layout g traversal cache before ih.valid ih.owned

def Ready (g : Grammar.CFG) (machine : Machine) (cache : Cache g) : Prop :=
  ∀ node tail, machine.registry.pending = node :: tail →
    ∃ kernel, machine.registry.keys[node]? = some kernel ∧ node < cache.storage.plans.length ∧
      cache.readTemplate node = some (kernel, TemplateCapabilities.materialize g (TemplateCapabilities.predictionMask g kernel)) ∧
      PlanStorage.decodeVector cache.memory.objects cache.storage.plans node =
        some (kernel, TemplateCapabilities.materialize g (TemplateCapabilities.predictionMask g kernel))

theorem ready (g : Grammar.CFG) (layout : PlanStorage.Layout) (machine : Machine) (cache : Cache g)
    (facts : Facts g layout machine cache) : Ready g machine cache := by
  intro node tail front
  have bound := facts.valid.valid.bound node (by rw [front]; simp)
  let kernel := machine.registry.keys[node]
  have key : machine.registry.keys[node]? = some kernel := List.getElem?_eq_getElem bound
  have shape := trace_shape facts.owned.trace
  refine ⟨kernel, key, ?_, facts.aligned node kernel key, ?_⟩
  · rw [shape.plans]
    exact Nat.lt_of_lt_of_le bound shape.bounded
  · have physical : PlanStorage.decodeVector cache.memory.objects cache.storage.plans node = cache.readTemplate node := by
      simp only [PlanStorage.decodeVector, facts.owned.views.plans, TemplateCapabilities.Cache.readTemplate, PlanStorage.decode]
    rw [physical]
    exact facts.aligned node kernel key

def Complete (g : Grammar.CFG) (traversal : Traversal) (layout : PlanStorage.Layout) : Prop :=
  ∀ machine cache, Visit g traversal layout machine cache → Ready g machine cache

theorem complete (g : Grammar.CFG) (nonempty : g ≠ []) (traversal : Traversal) (layout : PlanStorage.Layout) :
    Complete g traversal layout := fun machine cache visit => ready g layout machine cache
      (visited g nonempty traversal layout machine cache visit)

theorem finish_visit (g : Grammar.CFG) (traversal : Traversal) (layout : PlanStorage.Layout)
    (cache : Cache g) (machine : Machine) (valid : Invariant g machine)
    (covered : Covered g cache machine.registry) (aligned : Aligned g cache machine.registry)
    (visit : Visit g traversal layout machine cache) :
    Visit g traversal layout (ConstructionFIFO.finish g traversal cache machine valid covered aligned).machine
      (ConstructionFIFO.finish g traversal cache machine valid covered aligned).cache := by
  rw [ConstructionFIFO.finish]
  split
  · exact visit
  · dsimp only
    exact finish_visit g traversal layout _ _ _ _ _ (.next visit)
termination_by capacity g - machine.registry.processed
decreasing_by
  rw [show (ConstructionFIFO.tick ConstructionFIFO.canonical g traversal cache machine).machine =
      (CachedConstruction.tick g traversal cache machine).1 from
    congrArg Prod.fst (ConstructionFIFO.tick_exact g traversal cache machine valid aligned),
    CachedConstruction.tick_exact g traversal cache machine valid covered aligned]
  exact tick_rank g machine valid (by assumption)

theorem construct_visit (g : Grammar.CFG) (nonempty : g ≠ []) (traversal : Traversal) (layout : PlanStorage.Layout) :
    Visit g traversal layout (ConstructionFIFO.construct g nonempty traversal layout).machine
      (ConstructionFIFO.construct g nonempty traversal layout).cache := by
  unfold ConstructionFIFO.construct
  dsimp only
  exact finish_visit g traversal layout _ _ _ _ _ .initial

end ParserEngine.ConstructionReadiness
