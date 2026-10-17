import ParserEngine.ConstructionExtraction

/-! Source admission, the complete queue and physical/index observations share
one retained construction. Host primitives remain external premises. -/
namespace ParserEngine.ConstructionConsumer
open Grammar Viable LeanPoo.Functional

structure Sources where
  builder : WalkSource.Datum
  growth : WalkSource.Datum
  append : WalkSource.Datum
  template : WalkSource.Datum

structure Admitted (sources : Sources) where
  recipe : ConstructionExtraction.Recipe
  extracted : ConstructionExtraction.extract sources.builder sources.growth sources.append sources.template = some recipe
  layout : recipe.initial = 128

/-- The modeled queue starts at 128: another source layout must fail closed. -/
def prepare (sources : Sources) : Option (Admitted sources) :=
  match read : ConstructionExtraction.extract sources.builder sources.growth sources.append sources.template with
  | none => none
  | some recipe => if layout : recipe.initial = 128 then some ⟨recipe, read, layout⟩ else none

theorem wrong_layout_rejected (sources : Sources) (recipe : ConstructionExtraction.Recipe)
    (read : ConstructionExtraction.extract sources.builder sources.growth sources.append sources.template = some recipe)
    (different : recipe.initial ≠ 128) : prepare sources = none := by
  unfold prepare
  split
  · rfl
  · next chosen found =>
      have same : chosen = recipe := Option.some.inj (found.symm.trans read)
      subst chosen
      simp only [dite_eq_right different]

theorem prepared (sources : Sources) (recipe : ConstructionExtraction.Recipe)
    (read : ConstructionExtraction.extract sources.builder sources.growth sources.append sources.template = some recipe)
    (layout : recipe.initial = 128) : (prepare sources).isSome = true := by
  unfold prepare
  split
  · next absent => rw [absent] at read; cases read
  · next chosen found =>
      have same : chosen = recipe := Option.some.inj (found.symm.trans read)
      subst chosen
      simp only [dite_eq_left layout, Option.isSome_some]

def current : Sources := ⟨LRTransitionWitness.builderSource, LRConstructionWitness.growSource,
  LRConstructionWitness.appendSource, LRTemplateWitness.templateSource⟩

theorem current_admitted : (prepare current).isSome = true := by
  have source := ConstructionExtraction.source_current
  change (ConstructionExtraction.extract current.builder current.growth current.append current.template).map
    (fun recipe => (recipe.initial, recipe.factor)) = some (128, 2) at source
  cases read : ConstructionExtraction.extract current.builder current.growth current.append current.template with
  | none => rw [read] at source; cases source
  | some recipe =>
    rw [read] at source
    have scalar : (recipe.initial, recipe.factor) = (128, 2) := Option.some.inj source
    exact prepared current recipe read (congrArg Prod.fst scalar)

structure Context where
  grammar : CFG
  nonempty : grammar ≠ []
  traversal : CachedConstruction.Traversal

inductive Capability where | construction deriving DecidableEq
abbrev Value (context : Context) (_ : Capability) :=
  ConstructionProtocol.Product context.grammar × CachedConstruction.Cache context.grammar

def Joint (context : Context)
    (data : Requirements.Results (fun c => Value c) context [Capability.construction]) : Prop :=
  data.1.1.machine = (ConstructionProtocol.construct context.grammar context.nonempty).machine ∧
    CachedConstruction.Aligned context.grammar data.1.2 data.1.1.machine.registry ∧
    CachedConstruction.Physical context.grammar data.1.2 data.1.1.machine.registry

def retained : Requirements.Factories Context (fun c => Value c) [Capability.construction] :=
  (fun context => CachedConstruction.construct context.grammar context.nonempty context.traversal, PUnit.unit)

def certified : Requirements.Certified [Capability.construction] Joint :=
  Requirements.certify retained Joint (by
      intro context
      exact ⟨CachedConstruction.construct_exact context.grammar context.nonempty context.traversal,
        CachedConstruction.construct_aligned context.grammar context.nonempty context.traversal,
        CachedConstruction.construct_physical context.grammar context.nonempty context.traversal⟩)

structure Product (context : Context) where
  construction : Value context .construction
  exactMachine : construction.1.machine = (ConstructionProtocol.construct context.grammar context.nonempty).machine
  aligned : CachedConstruction.Aligned context.grammar construction.2 construction.1.machine.registry
  physical : CachedConstruction.Physical context.grammar construction.2 construction.1.machine.registry

/-- Admission precedes factory execution. The consumer retains the queue's
actual cache; an index or storage certificate is never supplied by a caller. -/
def construct (sources : Sources) (context : Context) : Option (Product context) :=
  (prepare sources).map fun _ => certified.consume (fun _ data proof => ⟨data.1, proof.1, proof.2.1, proof.2.2⟩) context

theorem rejected (sources : Sources) (context : Context) (failure : prepare sources = none) :
    construct sources context = none := by simp [construct, failure]

theorem admitted (sources : Sources) (context : Context) (product : Product context)
    (success : construct sources context = some product) : ∃ receipt : Admitted sources, prepare sources = some receipt := by
  unfold construct at success
  cases read : prepare sources with
  | none => simp [read] at success
  | some receipt => exact ⟨receipt, rfl⟩

/-- Every successful observation is bound to all admitted source declarations
and the queue's actual initial layout, rather than just a source checksum. -/
theorem source_correspondence (sources : Sources) (context : Context) (product : Product context)
    (success : construct sources context = some product) :
    sources.builder = LRTransitionSource.builderWith 128 ∧
      sources.growth = LRConstructionSource.growWith 2 ∧
      sources.append = LRConstructionSource.appendState ∧ sources.template = LRTemplateSource.template := by
  obtain ⟨receipt, _⟩ := admitted sources context product success
  have matching := ConstructionExtraction.extracted_sources _ _ _ _ receipt.recipe receipt.extracted
  rw [receipt.layout, receipt.recipe.doubled] at matching
  exact matching

def Product.query (product : Product context) (node key : Nat) : Option SparseGroupMasks.Row :=
  (product.construction.2.readTemplate node).bind fun pair => pair.2.index key

def queryVector (objects : Nat → Option TemplateCapabilities.Template)
    (slots : List (Option PlanStorage.Plan)) (node key : Nat) : Option SparseGroupMasks.Row :=
  (PlanStorage.decodeVector objects slots node).bind fun pair => pair.2.index key

/-- Arbitrary allocated nodes in the generated copy/write history yield both
exact kernel identity and the generated shared-index lookup law. -/
theorem physical_index (context : Context) (product : Product context) :
    ∃ slots, PlanStorage.Trace (TemplateCapabilities.materialize context.grammar)
        (TemplateCapabilities.predictionMask context.grammar) product.construction.2.memory
        product.construction.1.machine.registry.keys.length slots ∧
      ∀ node kernel, product.construction.1.machine.registry.keys[node]? = some kernel →
        PlanStorage.decodeVector product.construction.2.memory.objects slots node =
          some (kernel, TemplateCapabilities.materialize context.grammar
            (TemplateCapabilities.predictionMask context.grammar kernel)) ∧
        ∀ key, queryVector product.construction.2.memory.objects slots node key =
          let symbol := SymbolDiscovery.physicalIDs context.grammar
          let items := PredictionMaterialization.materialize context.grammar
            (TemplateCapabilities.predictionMask context.grammar kernel)
          if key ∈ SparseGroupMasks.keys symbol items then
            some ⟨SparseGroupMasks.first symbol items key, key, SparseGroupMasks.mask symbol items key⟩ else none := by
  obtain ⟨slots, trace⟩ := product.physical
  refine ⟨slots, trace, ?_⟩
  intro node kernel read
  have inside : node < product.construction.1.machine.registry.keys.length := by
    by_cases inside : node < product.construction.1.machine.registry.keys.length
    · exact inside
    · have absent := List.getElem?_eq_none (Nat.le_of_not_gt inside)
      rw [absent] at read
      cases read
  have plan := product.aligned node kernel read
  refine ⟨?_, ?_⟩
  · rw [PlanStorage.trace_decode trace node inside]
    exact plan
  · intro key
    simp only [queryVector, PlanStorage.trace_decode trace node inside]
    change product.query node key = _
    simp only [Product.query, plan, Option.bind_some, TemplateCapabilities.materialize]
    exact TemplateIndex.lookup _ _ key

/-- Every allocated node has a complete published row in the retained result. -/
theorem complete (context : Context) (product : Product context) (node : Nat)
    (inside : node < product.construction.1.machine.registry.keys.length) :
    ∃ edges, product.construction.1.machine.transitions node = some edges ∧
      ConstructionRows.RowValid context.grammar product.construction.1.machine.registry node edges := by
  rw [product.exactMachine] at inside ⊢
  exact ConstructionProtocol.complete context.grammar context.nonempty node inside

/-- The plan read and paper target belong to the same completed machine. -/
theorem paper_target (context : Context) (product : Product context) (node key target : Nat)
    (edges : List (Nat × Nat)) (inside : node < product.construction.1.machine.registry.keys.length)
    (stored : product.construction.1.machine.transitions node = some edges) (edge : (key, target) ∈ edges)
    (symbol : Symbol) (selected : SymbolDiscovery.index context.grammar symbol = some key) :
    ∃ kernel values, product.construction.2.readTemplate node =
        some (kernel, TemplateCapabilities.materialize context.grammar
          (TemplateCapabilities.predictionMask context.grammar kernel)) ∧
      product.construction.1.machine.registry.states target = some values ∧
      ∀ child, (∃ code ∈ values, decodeCore context.grammar code = some child) ↔
        Closure context.grammar (fun item => ∃ root,
          Closure context.grammar (PublishedClosure.EncodedKernel context.grammar kernel) root ∧ advance root symbol item) child := by
  have result := ConstructionProtocol.paper_target context.grammar context.nonempty node key target edges
    (by simpa only [product.exactMachine] using inside)
    (by simpa only [product.exactMachine] using stored) edge symbol selected
  obtain ⟨kernel, values, read, state, meaning⟩ := result
  rw [← product.exactMachine] at read state
  exact ⟨kernel, values, product.aligned node kernel read, state, meaning⟩

end ParserEngine.ConstructionConsumer
