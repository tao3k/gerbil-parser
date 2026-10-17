import ParserEngine.ConstructionExtraction
import ParserEngine.ConstructionHeap

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

/-- Every extracted positive initial capacity drives the complete queue. -/
def prepare (sources : Sources) : Option (Admitted sources) :=
  match read : ConstructionExtraction.extract sources.builder sources.growth sources.append sources.template with
  | none => none
  | some recipe => some ⟨recipe, read⟩

theorem prepared (sources : Sources) (recipe : ConstructionExtraction.Recipe)
    (read : ConstructionExtraction.extract sources.builder sources.growth sources.append sources.template = some recipe) :
    (prepare sources).isSome = true := by
  unfold prepare
  split
  · next absent => rw [absent] at read; cases read
  · rfl

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
    exact prepared current recipe read

structure Context where
  grammar : CFG
  nonempty : grammar ≠ []
  traversal : CachedConstruction.Traversal

inductive Capability where | construction deriving DecidableEq
structure Request where
  context : Context
  layout : PlanStorage.Layout

abbrev Value (request : Request) (_ : Capability) :=
  ConstructionProtocol.Product request.context.grammar × CachedConstruction.Cache request.context.grammar

def Joint (request : Request)
    (data : Requirements.Results (fun c => Value c) request [Capability.construction]) : Prop :=
  data.1.1.machine = (ConstructionProtocol.construct request.context.grammar request.context.nonempty).machine ∧
    CachedConstruction.Aligned request.context.grammar data.1.2 data.1.1.machine.registry ∧
    CachedConstruction.Physical request.context.grammar data.1.2 data.1.1.machine.registry ∧
    ConstructionHeap.Owned request.layout request.context.grammar data.1.2 data.1.1.machine.registry data.1.1.machine.transitions

def retained : Requirements.Factories Request (fun c => Value c) [Capability.construction] :=
  (fun request => CachedConstruction.construct request.context.grammar request.context.nonempty
    request.context.traversal request.layout, PUnit.unit)

def certified : Requirements.Certified [Capability.construction] Joint :=
  Requirements.certify retained Joint (by
      intro request
      exact ⟨CachedConstruction.construct_exact _ request.context.nonempty _ request.layout,
        CachedConstruction.construct_aligned _ request.context.nonempty _ request.layout,
        CachedConstruction.construct_physical _ request.context.nonempty _ request.layout,
        ConstructionHeap.construct_owned request.layout _ request.context.nonempty _⟩)

structure Product (context : Context) where
  layout : PlanStorage.Layout
  construction : ConstructionProtocol.Product context.grammar × CachedConstruction.Cache context.grammar
  exactMachine : construction.1.machine = (ConstructionProtocol.construct context.grammar context.nonempty).machine
  aligned : CachedConstruction.Aligned context.grammar construction.2 construction.1.machine.registry
  physical : CachedConstruction.Physical context.grammar construction.2 construction.1.machine.registry
  owned : ConstructionHeap.Owned layout context.grammar construction.2 construction.1.machine.registry construction.1.machine.transitions

def build (request : Request) : Product request.context :=
  certified.consume (Output := fun request => Product request.context) (fun request data proof =>
    ⟨request.layout, data.1, proof.1, proof.2.1, proof.2.2.1, proof.2.2.2⟩) request

/-- Admission precedes factory execution. The consumer retains the queue's
actual cache; an index or storage certificate is never supplied by a caller. -/
def construct (sources : Sources) (context : Context) : Option (Product context) :=
  (prepare sources).map fun receipt => build ⟨context, receipt.recipe.layout⟩

theorem rejected (sources : Sources) (context : Context) (failure : prepare sources = none) :
    construct sources context = none := by simp [construct, failure]

theorem admitted (sources : Sources) (context : Context) (product : Product context)
    (success : construct sources context = some product) :
    ∃ receipt : Admitted sources, prepare sources = some receipt ∧ product.layout = receipt.recipe.layout := by
  unfold construct at success
  cases read : prepare sources with
  | none => simp [read] at success
  | some receipt =>
    simp only [read, Option.map_some] at success
    cases Option.some.inj success
    exact ⟨receipt, rfl, rfl⟩

def schema (initial : Nat) : Sources := ⟨LRTransitionSource.builderWith initial,
  LRConstructionSource.growWith 2, LRConstructionSource.appendState, LRTemplateSource.template⟩

/-- Universal source-to-complete-construction admission. Positive capacities
are extracted and executed, not assumed to equal some independent layout. -/
theorem schema_constructs (initial : Nat) (positive : 0 < initial) (context : Context) :
    ∃ product, construct (schema initial) context = some product ∧ product.layout.initial = initial := by
  have parameters := ConstructionExtraction.schema_recipe initial positive
  change (ConstructionExtraction.extract (schema initial).builder (schema initial).growth
    (schema initial).append (schema initial).template).map (fun recipe => (recipe.initial, recipe.factor)) = some (initial, 2) at parameters
  cases read : ConstructionExtraction.extract (schema initial).builder (schema initial).growth
      (schema initial).append (schema initial).template with
  | none => rw [read] at parameters; cases parameters
  | some recipe =>
    rw [read] at parameters
    have values : (recipe.initial, recipe.factor) = (initial, 2) := Option.some.inj parameters
    have available := prepared (schema initial) recipe read
    cases gate : prepare (schema initial) with
    | none => rw [gate] at available; cases available
    | some receipt =>
      have same : receipt.recipe = recipe := Option.some.inj (receipt.extracted.symm.trans read)
      refine ⟨build ⟨context, receipt.recipe.layout⟩, by simp only [construct, gate, Option.map_some], ?_⟩
      change receipt.recipe.initial = initial
      rw [same]
      exact congrArg Prod.fst values

/-- Every successful observation is bound to all admitted source declarations
and the queue's actual initial layout, rather than just a source checksum. -/
theorem source_correspondence (sources : Sources) (context : Context) (product : Product context)
    (success : construct sources context = some product) :
    sources.builder = LRTransitionSource.builderWith product.layout.initial ∧
      sources.growth = LRConstructionSource.growWith 2 ∧
      sources.append = LRConstructionSource.appendState ∧ sources.template = LRTemplateSource.template := by
  obtain ⟨receipt, _, layout⟩ := admitted sources context product success
  have matching := ConstructionExtraction.extracted_sources _ _ _ _ receipt.recipe receipt.extracted
  rw [receipt.recipe.doubled] at matching
  rw [layout]
  exact matching

def Product.query (product : Product context) (node key : Nat) : Option SparseGroupMasks.Row :=
  (product.construction.2.readTemplate node).bind fun pair => pair.2.index key

def queryVector (objects : Nat → Option TemplateCapabilities.Template)
    (slots : List (Option PlanStorage.Plan)) (node key : Nat) : Option SparseGroupMasks.Row :=
  (PlanStorage.decodeVector objects slots node).bind fun pair => pair.2.index key

/-- This is the heap actually threaded through the queue. All three vectors
share the admitted origin and capacity; every read refines the same result. -/
theorem joint_observations (context : Context) (product : Product context) :
    ConstructionStorage.Trace product.layout product.construction.1.machine.registry.keys.length
        product.construction.2.storage ∧
      product.construction.2.storage.states.length = product.construction.2.memory.capacity ∧
      product.construction.2.storage.plans.length = product.construction.2.memory.capacity ∧
      product.construction.2.storage.rows.length = product.construction.2.memory.capacity ∧
      ∀ node, product.construction.2.storage.states[node]?.join = product.construction.1.machine.registry.states node ∧
        PlanStorage.decodeVector product.construction.2.memory.objects product.construction.2.storage.plans node =
          product.construction.2.readTemplate node ∧
        product.construction.2.storage.rows[node]?.join = product.construction.1.machine.transitions node := by
  have shape := ConstructionStorage.trace_shape product.owned.trace
  refine ⟨product.owned.trace, product.owned.capacity, shape.plans.trans product.owned.capacity,
    shape.rows.trans product.owned.capacity, ?_⟩
  intro node
  refine ⟨product.owned.views.states node, ?_, product.owned.views.rows node⟩
  simp only [PlanStorage.decodeVector, product.owned.views.plans, TemplateCapabilities.Cache.readTemplate, PlanStorage.decode]

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

/-- Every completed row is readable from the jointly generated physical heap. -/
theorem physical_complete (context : Context) (product : Product context) (node : Nat)
    (inside : node < product.construction.1.machine.registry.keys.length) :
    ∃ edges, ConstructionStorage.rowAt product.construction.2.storage node = edges ∧
      ConstructionRows.RowValid context.grammar product.construction.1.machine.registry node edges := by
  obtain ⟨edges, stored, meaning⟩ := complete context product node inside
  refine ⟨edges, ?_, meaning⟩
  simp only [ConstructionStorage.rowAt, product.owned.views.rows, stored, Option.getD_some]

/-- A physically read row leads to physically read target states with paper
goto/closure meaning; source plans, targets and rows use the same joint heap. -/
theorem physical_paper_target (context : Context) (product : Product context) (node key target : Nat)
    (edges : List (Nat × Nat)) (inside : node < product.construction.1.machine.registry.keys.length)
    (stored : ConstructionStorage.rowAt product.construction.2.storage node = edges) (edge : (key, target) ∈ edges)
    (symbol : Symbol) (selected : SymbolDiscovery.index context.grammar symbol = some key) :
    ∃ kernel values, PlanStorage.decodeVector product.construction.2.memory.objects product.construction.2.storage.plans node =
        some (kernel, TemplateCapabilities.materialize context.grammar (TemplateCapabilities.predictionMask context.grammar kernel)) ∧
      product.construction.2.storage.states[target]?.join = some values ∧
      ∀ child, (∃ code ∈ values, decodeCore context.grammar code = some child) ↔
        Closure context.grammar (fun item => ∃ root,
          Closure context.grammar (PublishedClosure.EncodedKernel context.grammar kernel) root ∧ advance root symbol item) child := by
  obtain ⟨actual, published, _⟩ := complete context product node inside
  have decoded : ConstructionStorage.rowAt product.construction.2.storage node = actual := by
    simp only [ConstructionStorage.rowAt, product.owned.views.rows, published, Option.getD_some]
  have same : actual = edges := decoded.symm.trans stored
  have logical : product.construction.1.machine.transitions node = some edges := by simpa only [same] using published
  obtain ⟨kernel, values, plan, states, meaning⟩ := paper_target context product node key target edges inside logical edge symbol selected
  have decode := ((joint_observations context product).2.2.2.2 node).2.1
  exact ⟨kernel, values, decode.trans plan, (product.owned.views.states target).trans states, meaning⟩

/-- Shared-table queries through the joint plan vector retain both present and
absent-key meaning. The physical vector is not reconstructed after the queue. -/
theorem joint_index (context : Context) (product : Product context) (node kernel key : Nat)
    (read : product.construction.1.machine.registry.keys[node]? = some kernel) :
    queryVector product.construction.2.memory.objects product.construction.2.storage.plans node key =
      let symbol := SymbolDiscovery.physicalIDs context.grammar
      let items := PredictionMaterialization.materialize context.grammar
        (TemplateCapabilities.predictionMask context.grammar kernel)
      if key ∈ SparseGroupMasks.keys symbol items then
        some ⟨SparseGroupMasks.first symbol items key, key, SparseGroupMasks.mask symbol items key⟩ else none := by
  have decode := ((joint_observations context product).2.2.2.2 node).2.1
  simp only [queryVector, decode, product.aligned node kernel read, Option.bind_some, TemplateCapabilities.materialize]
  exact TemplateIndex.lookup _ _ key

end ParserEngine.ConstructionConsumer
