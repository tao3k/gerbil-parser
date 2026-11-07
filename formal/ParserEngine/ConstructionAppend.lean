import ParserEngine.ConstructionControl

/-! Decode and execute the parser-owned append protocol. Template lookup and
closure values are supplied by the certified producer; their normal-return
host implementation remains external. The index write is a logical table
effect, not a proof of the host hash representation. -/
namespace ParserEngine.ConstructionAppend
open WalkSource ConstructionStorage

inductive Instruction where
  | grow | bind | state | plan | register | advance | result
  deriving DecidableEq

def canonical : List Instruction := [.grow, .bind, .state, .plan, .register, .advance, .result]

def instruction : Datum → Option Instruction
  | .list [.symbol "vector-set!", .symbol "states", .symbol "index",
      .list [.symbol "lr0-closure", .symbol "kernel", .symbol "template"]] => some .state
  | .list [.symbol "vector-set!", .symbol "kernel-plans", .symbol "index",
      .list [.symbol "cons", .symbol "kernel", .symbol "template"]] => some .plan
  | .list [.symbol "table-set!", .symbol "state-index", .symbol "kernel", .symbol "index"] => some .register
  | .list [.symbol "set!", .symbol "state-count", .list [.symbol "+", .symbol "state-count", .integer 1]] => some .advance
  | .symbol "index" => some .result
  | _ => none

def decode : Datum → Option (List Instruction)
  | .list [.symbol "def", .list [.symbol "append-state!", .symbol "kernel"],
      .list [.symbol "grow!"], .list [.symbol "let", .list [
        .list [.symbol "index", .symbol "state-count"],
        .list [.symbol "template", .list [.symbol "template-for", .symbol "kernel"]]],
        one, two, three, four, five]] => do
    let body ← [one, two, three, four, five].mapM instruction
    pure (.grow :: .bind :: body)
  | _ => none

structure Frame where
  heap : Heap
  count : Nat
  index : Nat
  registered : List (Nat × Nat)
  returned : Option Nat

def step (growth : ConstructionControl.Program) (state : List Nat) (plan : PlanStorage.Plan)
    (frame : Frame) : Instruction → Frame
  | .grow => {frame with heap := ConstructionControl.grow growth frame.heap frame.count}
  | .bind => {frame with index := frame.count}
  | .state => {frame with heap := {frame.heap with states := frame.heap.states.set frame.index (some state)}}
  | .plan => {frame with heap := {frame.heap with plans := frame.heap.plans.set frame.index (some plan)}}
  | .register => {frame with registered := frame.registered ++ [(plan.kernel, frame.index)]}
  | .advance => {frame with count := frame.count + 1}
  | .result => {frame with returned := some frame.index}

def run (growth : ConstructionControl.Program) (program : List Instruction)
    (heap : Heap) (count : Nat) (state : List Nat) (plan : PlanStorage.Plan) : Frame :=
  program.foldl (step growth state plan) ⟨heap, count, 0, [], none⟩

/-- The captured old index survives counter mutation and is both registered
and returned. No caller supplies the resulting heap, ID or counter. -/
theorem run_exact (heap : Heap) (count : Nat) (state : List Nat) (plan : PlanStorage.Plan) :
    run ConstructionControl.canonical canonical heap count state plan =
      ⟨ConstructionStorage.append heap count state plan, count + 1, count, [(plan.kernel, count)], some count⟩ := by
  simp only [run, canonical, List.foldl_cons, List.foldl_nil, step, List.nil_append]
  rw [← ConstructionControl.append_exact]
  rfl

/-- Registration sees both writes and the old counter; only the following
instruction advances it. This excludes a final-state-only correspondence. -/
theorem registration_stage (heap : Heap) (count : Nat) (state : List Nat) (plan : PlanStorage.Plan) :
    run ConstructionControl.canonical [.grow, .bind, .state, .plan, .register] heap count state plan =
      ⟨ConstructionStorage.append heap count state plan, count, count, [(plan.kernel, count)], none⟩ := by
  simp only [run, List.foldl_cons, List.foldl_nil, step, List.nil_append]
  rw [← ConstructionControl.append_exact]
  rfl

/-- Both physical writes use the captured index inside their corresponding
prepared vectors. The first write cannot invalidate the second bound. -/
theorem write_bounds (heap : Heap) (count : Nat) (shape : Shape heap count) :
    count < (ConstructionControl.grow ConstructionControl.canonical heap count).states.length ∧
      count < (ConstructionControl.grow ConstructionControl.canonical heap count).plans.length := by
  have room := VectorStorage.room count heap.states.length shape.bounded shape.positive
  have prepared := ConstructionStorage.append_exact heap count [] ⟨0, 0⟩ shape
  have states := congrArg (fun (h : Heap) => h.states.length) prepared
  have plans := congrArg (fun (h : Heap) => h.plans.length) prepared
  simp only [ConstructionStorage.append, ← ConstructionControl.grow_exact, List.length_set,
    VectorStorage.append_length, shape.plans] at states plans
  exact ⟨states.symm ▸ room, plans.symm ▸ room⟩

theorem schema_decoded : decode LRConstructionSource.appendState = some canonical := rfl

theorem source_decoded (builder growth installation template : Datum) (recipe : ConstructionExtraction.Recipe)
    (admitted : ConstructionExtraction.extract builder growth installation template = some recipe) :
    decode installation = some canonical := by
  rw [(ConstructionExtraction.extracted_sources _ _ _ _ recipe admitted).2.2.1]
  exact schema_decoded

def execute (growth : ConstructionControl.Program) (program : List Instruction) :
    List ConstructionControl.Action → Nat → Heap → Nat × Heap
  | [], count, heap => (count, heap)
  | .append state plan :: rest, count, heap =>
    let frame := run growth program heap count state plan
    execute growth program rest frame.count frame.heap
  | .publish node row :: rest, count, heap => execute growth program rest count (ConstructionStorage.publish heap node row)

theorem execute_exact (actions : List ConstructionControl.Action) (count : Nat) (heap : Heap) :
    execute ConstructionControl.canonical canonical actions count heap =
      ConstructionControl.execute ConstructionControl.canonical actions count heap := by
  induction actions generalizing count heap with
  | nil => rfl
  | cons action rest ih =>
    cases action with
    | append state plan => simp only [execute, run_exact, ih, ConstructionControl.execute, ConstructionControl.append_exact]
    | publish node row => exact ih _ _

def Realizes (layout : PlanStorage.Layout) (count : Nat) (heap : Heap) : Prop :=
  ∃ actions, ConstructionControl.Safe ConstructionControl.canonical actions 0 (ConstructionStorage.empty layout) ∧
    execute ConstructionControl.canonical canonical actions 0 (ConstructionStorage.empty layout) = (count, heap)

theorem trace_realizes (trace : ConstructionStorage.Trace layout count heap) : Realizes layout count heap := by
  obtain ⟨actions, safe, result⟩ := ConstructionControl.trace_realizes trace
  exact ⟨actions, safe, (execute_exact actions _ _).trans result⟩

/-- Specialize the executable append to the actual retained producer. The
closure and shared reference are computed by that producer, not an unrelated
caller-supplied pair that merely happens to reach the same final heap. -/
theorem intern_step (g : Grammar.CFG) (cache : CachedConstruction.Cache g)
    (before : StateInterning.Registry Nat (List Nat)) (kernel : Nat) (fresh : before.index kernel = none) :
    let plan : PlanStorage.Plan := ⟨kernel, (PlanStorage.fetch (TemplateCapabilities.materialize g)
      cache.memory (TemplateCapabilities.predictionMask g kernel)).1⟩
    let result := CachedConstruction.intern g cache before kernel
    run ConstructionControl.canonical canonical cache.storage before.keys.length
      (KernelConstruction.compute g kernel).1 plan =
      ⟨result.2.storage, result.1.registry.keys.length, before.keys.length,
        [(kernel, before.keys.length)], some before.keys.length⟩ := by
  dsimp only
  rw [run_exact]
  simp only [CachedConstruction.intern, fresh, TemplateCapabilities.build_storage,
    StateInterning.intern, List.length_append, List.length_singleton]

def replayIndex (before : Nat → Option Nat) (writes : List (Nat × Nat)) : Nat → Option Nat :=
  writes.foldl (fun index write => ProducerAdmission.Storage.update index write.1 write.2) before

/-- Registration is not only an event label: replaying its logical table
effect yields the actual producer's complete index function, at every key. -/
theorem intern_index (g : Grammar.CFG) (cache : CachedConstruction.Cache g)
    (before : StateInterning.Registry Nat (List Nat)) (kernel : Nat) (fresh : before.index kernel = none)
    (state : List Nat) (reference : Nat) :
    replayIndex before.index
      (run ConstructionControl.canonical canonical cache.storage before.keys.length state ⟨kernel, reference⟩).registered =
        (CachedConstruction.intern g cache before kernel).1.registry.index := by
  simp only [run_exact, replayIndex, List.foldl_cons, List.foldl_nil,
    CachedConstruction.intern, fresh, StateInterning.intern]
  rfl

def executeSource (growth installation : Datum) (actions : List ConstructionControl.Action)
    (layout : PlanStorage.Layout) : Option (Nat × Heap) :=
  (ConstructionControl.decode growth).bind fun growth =>
  (decode installation).bind fun installation =>
    if growth = ConstructionControl.canonical ∧ installation = canonical then
      some (execute growth installation actions 0 (ConstructionStorage.empty layout)) else none

theorem source_realizes (builder growth installation template : Datum) (recipe : ConstructionExtraction.Recipe)
    (admitted : ConstructionExtraction.extract builder growth installation template = some recipe)
    (trace : ConstructionStorage.Trace recipe.layout count heap) :
    ∃ actions, ConstructionControl.Safe ConstructionControl.canonical actions 0 (ConstructionStorage.empty recipe.layout) ∧
      executeSource growth installation actions recipe.layout = some (count, heap) := by
  obtain ⟨actions, safe, result⟩ := trace_realizes trace
  refine ⟨actions, safe, ?_⟩
  simp only [executeSource, ConstructionControl.source_decoded _ _ _ _ recipe admitted,
    source_decoded _ _ _ _ recipe admitted, Option.bind_some, and_self, ↓reduceIte, result]

theorem unsupported_rejected (growth installation : Datum) (actions : List ConstructionControl.Action)
    (layout : PlanStorage.Layout) (program : List Instruction) (decoded : decode installation = some program)
    (different : program ≠ canonical) : executeSource growth installation actions layout = none := by
  simp only [executeSource]
  cases read : ConstructionControl.decode growth with
  | none => rfl
  | some value => simp only [Option.bind_some, decoded, different, and_false, ↓reduceIte]

end ParserEngine.ConstructionAppend
