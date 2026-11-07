import ParserEngine.ConstructionAppend

/-! Install effects are computed from the certified template producer and the
decoded append frame. No preconstructed registry is used as an output oracle.
Registry states/plans are logical views; heap and registration are frame effects. -/
namespace ParserEngine.ConstructionInstallation
open ConstructionStorage ConstructionAppend ProducerAdmission

def registry (before : StateInterning.Registry Nat (List Nat)) (kernel : Nat)
    (product : List Nat × List Nat) (frame : Frame) : StateInterning.Registry Nat (List Nat) where
  keys := before.keys ++ [kernel]
  index := replayIndex before.index frame.registered
  states := Storage.update before.states frame.index product.1
  plans := Storage.update before.plans frame.index (kernel, product.2)
  pending := before.pending
  processed := before.processed

structure Installed (g : Grammar.CFG) where
  registry : StateInterning.Registry Nat (List Nat)
  cache : CachedConstruction.Cache g
  id : Nat
  count : Nat
  returned : Option Nat

def afterGrowth (heap : Heap) (count : Nat) (state : List Nat) (plan : PlanStorage.Plan) : Frame :=
  canonical.tail.foldl (step ConstructionControl.canonical state plan) ⟨heap, count, 0, [], none⟩

theorem growth_prefix (heap : Heap) (count : Nat) (state : List Nat) (plan : PlanStorage.Plan) :
    afterGrowth (ConstructionControl.grow ConstructionControl.canonical heap count) count state plan =
      run ConstructionControl.canonical canonical heap count state plan := rfl

def append (g : Grammar.CFG) (cache : CachedConstruction.Cache g)
    (before : StateInterning.Registry Nat (List Nat)) (kernel : Nat) : Installed g :=
  let prepared := ConstructionControl.grow ConstructionControl.canonical cache.storage before.keys.length
  let built := TemplateCapabilities.produce g ⟨{cache with storage := prepared}, kernel, before.keys.length⟩
  let plan : PlanStorage.Plan := ⟨kernel, (PlanStorage.fetch (TemplateCapabilities.materialize g)
    cache.memory (TemplateCapabilities.predictionMask g kernel)).1⟩
  let frame := afterGrowth prepared before.keys.length built.product.1 plan
  ⟨registry before kernel built.product frame, {built.cache with storage := frame.heap},
    frame.returned.getD 0, frame.count, frame.returned⟩

/-- The native installer itself does not enqueue. The source callback owns
that later effect, after adding its row to the accumulator. -/
theorem append_exact (g : Grammar.CFG) (cache : CachedConstruction.Cache g)
    (before : StateInterning.Registry Nat (List Nat)) (kernel : Nat) (fresh : before.index kernel = none) :
    let installed := CachedConstruction.intern g cache before kernel
    append g cache before kernel =
      ⟨{installed.1.registry with pending := before.pending}, installed.2, before.keys.length,
        before.keys.length + 1, some before.keys.length⟩ := by
  dsimp only [append]
  rw [TemplateCapabilities.produce_reframe g ⟨cache, kernel, before.keys.length⟩]
  dsimp only
  rw [growth_prefix]
  simp only [run_exact, registry, replayIndex, List.foldl_cons, List.foldl_nil,
    CachedConstruction.intern, fresh, StateInterning.intern, TemplateCapabilities.build, Option.getD_some]
  rfl

theorem append_accounting (g : Grammar.CFG) (cache : CachedConstruction.Cache g)
    (before : StateInterning.Registry Nat (List Nat)) (kernel : Nat) :
    (append g cache before kernel).count = (append g cache before kernel).registry.keys.length ∧
      (append g cache before kernel).returned = some before.keys.length := by
  simp [append, registry, growth_prefix, run_exact]

theorem append_id (g : Grammar.CFG) (cache : CachedConstruction.Cache g)
    (before : StateInterning.Registry Nat (List Nat)) (kernel : Nat) :
    (append g cache before kernel).id = before.keys.length := by
  simp [append, growth_prefix, run_exact]

theorem append_queue (g : Grammar.CFG) (cache : CachedConstruction.Cache g)
    (before : StateInterning.Registry Nat (List Nat)) (kernel : Nat) :
    (append g cache before kernel).registry.pending = before.pending ∧
      (append g cache before kernel).registry.processed = before.processed := ⟨rfl, rfl⟩

end ParserEngine.ConstructionInstallation
