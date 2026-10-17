import ParserEngine.ConstructionInstallation

/-! The builder initializes slot zero with an already-one counter. This is a
distinct source path, not an invocation of append-state!. Host vector and
template/closure evaluation keep their normal-return contracts. -/
namespace ParserEngine.ConstructionInitialization
open WalkSource ConstructionStorage ConstructionAppend

structure Envelope where
  installation : Datum
  register : Datum
  queue : Datum

def envelope : Datum → Option Envelope
  | .list [.symbol "def", _, .list [.symbol "let*", _,
      .list [.symbol "let-values", _, .list [.symbol "let", _, _, installation, registration, _, _, queue]]]] =>
    some ⟨installation, registration, queue⟩
  | _ => none

def write : Datum → Option Instruction
  | .list [.symbol "vector-set!", .symbol "states", .integer 0,
      .list [.symbol "lr0-closure", .symbol "initial-kernel", .symbol "template"]] => some .state
  | .list [.symbol "vector-set!", .symbol "kernel-plans", .integer 0,
      .list [.symbol "cons", .symbol "initial-kernel", .symbol "template"]] => some .plan
  | .list [.symbol "table-set!", .symbol "state-index", .symbol "initial-kernel", .integer 0] => some .register
  | _ => none

def block : Datum → Option (List Instruction)
  | .list [.symbol "let", .list [.symbol "template", .list [.symbol "template-for", .symbol "initial-kernel"]], one, two] =>
    [one, two].mapM write
  | _ => none

def counter (name : String) : Datum → Option Datum
  | .list [.symbol "def", _, .list [.symbol "let*", .list entries, _]] =>
    ConstructionExtraction.binding name (.list [.symbol "let", .list entries])
  | _ => none

def decode (source : Datum) : Option (List Instruction) :=
  (envelope source).bind fun parts =>
  (block parts.installation).bind fun body =>
  (write parts.register).bind fun registration =>
  if counter "state-count" source = some (.integer 1) ∧
      counter "processed-count" source = some (.integer 0) then
    some (body ++ [registration]) else none

def canonical : List Instruction := [.state, .plan, .register]

def run (program : List Instruction) (layout : PlanStorage.Layout) (state : List Nat) (plan : PlanStorage.Plan) : Frame :=
  program.foldl (step ConstructionControl.canonical state plan) ⟨ConstructionStorage.empty layout, 1, 0, [], none⟩

theorem run_exact (layout : PlanStorage.Layout) (state : List Nat) (plan : PlanStorage.Plan) :
    run canonical layout state plan =
      ⟨ConstructionStorage.append (ConstructionStorage.empty layout) 0 state plan, 1, 0, [(plan.kernel, 0)], none⟩ := by
  have nonzero : ¬0 = layout.initial := by have := layout.positive; omega
  simp [run, canonical, step, ConstructionStorage.append, ConstructionStorage.empty, nonzero]

theorem write_bounds (layout : PlanStorage.Layout) :
    0 < (ConstructionStorage.empty layout).states.length ∧ 0 < (ConstructionStorage.empty layout).plans.length := by
  simpa [ConstructionStorage.empty] using And.intro layout.positive layout.positive

/-- Both values exist before the initial key becomes visible. The special
path neither advances the already-one counter nor returns an append ID. -/
theorem before_registration (layout : PlanStorage.Layout) (state : List Nat) (plan : PlanStorage.Plan) :
    let frame := run [.state, .plan] layout state plan
    frame.registered = [] ∧ frame.count = 1 ∧ frame.index = 0 ∧ frame.returned = none ∧
      frame.heap = (run canonical layout state plan).heap := by
  exact ⟨rfl, rfl, rfl, rfl, rfl⟩

set_option maxRecDepth 8192 in
theorem schema_decoded (initial : Nat) : decode (LRTransitionSource.builderWith initial) = some canonical := by
  have counters (name : String) : counter name (LRTransitionSource.builderWith initial) =
      ConstructionExtraction.binding name (.list [.symbol "let", .list [
        CompilerSemanticsSource.binding "productions-by-lhs" (NativeActions.Source.call "production-index-by-lhs" ["productions"]),
        CompilerSemanticsSource.binding "prediction-catalog" (NativeActions.Source.call "make-core-prediction-catalog" ["core-symbols", "productions-by-lhs", "layout"]),
        CompilerSemanticsSource.binding "initial-kernel" (app "compiler-index-set-add" [.integer 0, app "make-core-item" [.integer 0, .integer 0, .symbol "layout"]]),
        CompilerSemanticsSource.binding "templates" (app "make-table" [.keyword "test", .symbol "equal?"]),
        CompilerSemanticsSource.binding "states" (app "make-vector" [.integer initial, .boolean false]),
        CompilerSemanticsSource.binding "state-transitions" (app "make-vector" [.integer initial, CompilerSemanticsSource.emptyList]),
        CompilerSemanticsSource.binding "kernel-plans" (app "make-vector" [.integer initial, .boolean false]),
        CompilerSemanticsSource.binding "state-index" (app "make-table" [.keyword "test", .symbol "equal?"]),
        CompilerSemanticsSource.binding "state-count" (.integer 1), CompilerSemanticsSource.binding "processed-count" (.integer 0),
        CompilerSemanticsSource.binding "trace?" (app "equal?" [app "getenv" [.text "GERBIL_PARSER_LR_TRACE", .boolean false], .text "1"])]]) := rfl
  have count : counter "state-count" (LRTransitionSource.builderWith initial) = some (.integer 1) := by
    rw [counters]
    simp [ConstructionExtraction.binding, ConstructionExtraction.bindings, ConstructionExtraction.bindingList,
      CompilerSemanticsSource.binding, app]
  have processed : counter "processed-count" (LRTransitionSource.builderWith initial) = some (.integer 0) := by
    rw [counters]
    simp [ConstructionExtraction.binding, ConstructionExtraction.bindings, ConstructionExtraction.bindingList,
      CompilerSemanticsSource.binding, app]
  unfold decode
  rw [count, processed]
  simp only [and_self, ↓reduceIte]
  rfl

theorem source_decoded (builder growth installation template : Datum) (recipe : ConstructionExtraction.Recipe)
    (admitted : ConstructionExtraction.extract builder growth installation template = some recipe) :
    decode builder = some canonical := by
  rw [(ConstructionExtraction.extracted_sources _ _ _ _ recipe admitted).1]
  exact schema_decoded recipe.initial

/-- Certified template values feed the actual initialization frame. The
registry and heap are generated from its effects; no cached initial result is
read to populate the output. -/
def initial (program : List Instruction) (g : Grammar.CFG) (layout : PlanStorage.Layout) :
    ConstructionQueue.Machine × CachedConstruction.Cache g :=
  let cache := TemplateCapabilities.empty g layout
  let built := TemplateCapabilities.produce g ⟨cache, 1, 0⟩
  let plan : PlanStorage.Plan := ⟨1, (PlanStorage.fetch (TemplateCapabilities.materialize g)
    cache.memory (TemplateCapabilities.predictionMask g 1)).1⟩
  let frame := run program layout built.product.1 plan
  (⟨ConstructionInstallation.registry ConstructionQueue.empty 1 built.product frame, fun _ => none⟩,
    {built.cache with storage := frame.heap})

/-- Initialization has no enqueue. The outer source queue owns the single
enqueue of ID zero, after initialization and registration complete. -/
theorem initial_exact (g : Grammar.CFG) (layout : PlanStorage.Layout) :
    initial canonical g layout =
      (⟨{(CachedConstruction.initial g layout).1.registry with pending := []},
        (CachedConstruction.initial g layout).1.transitions⟩, (CachedConstruction.initial g layout).2) := by
  simp only [initial, run_exact, ConstructionInstallation.registry, replayIndex, List.foldl_cons, List.foldl_nil,
    CachedConstruction.initial, CachedConstruction.intern, ConstructionQueue.empty, StateInterning.intern,
    TemplateCapabilities.build]
  rfl

theorem initial_accounting (g : Grammar.CFG) (layout : PlanStorage.Layout) :
    (initial canonical g layout).1.registry.keys = [1] ∧
      (initial canonical g layout).1.registry.pending = [] ∧
      (initial canonical g layout).1.registry.processed = 0 := ⟨rfl, rfl, rfl⟩

end ParserEngine.ConstructionInitialization
