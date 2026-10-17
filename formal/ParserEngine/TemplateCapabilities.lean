import ParserEngine.TemplateCache
import ParserEngine.KernelConstruction
import LeanPoo.Functional.CertifiedConsumer

/-! Builder-local templates are cached by aggregate prediction mask, not kernel
identity. The retained LeanPoo factory carries cache preservation; its consumer
builds each kernel's own closure from the shared immutable data. -/
namespace ParserEngine.TemplateCapabilities
open Grammar LeanPoo.Functional

structure Template where
  items : List Nat
  groups : List SparseGroupMasks.Row

def materialize (g : CFG) (mask : Nat) : Template :=
  let items := PredictionMaterialization.materialize g mask
  ⟨items, SparseGroupMasks.sharedRows (SymbolDiscovery.physicalIDs g) items⟩

def predictionMask (g : CFG) (kernel : Nat) : Nat :=
  PredictionMaterialization.aggregate g (NameDiscovery.catalogue g).index
    (PredictionMaterialization.completed (NameDiscovery.catalogue g).keys (NameDiscovery.catalogue g).index
      (NameDiscovery.name_index g) g) (PublishedClosure.coreItems g kernel)

structure Cache (g : CFG) where
  store : TemplateCache.Store Template
  valid : TemplateCache.Valid (materialize g) store

def empty (g : CFG) : Cache g := ⟨TemplateCache.empty, by intro _ _ found; cases found⟩

structure Context (g : CFG) where
  cache : Cache g
  kernel : Nat

inductive Capability where | template deriving DecidableEq
abbrev Value (g : CFG) (_ : Context g) (_ : Capability) := Template × TemplateCache.Store Template

def retained (g : CFG) : Requirements.Factories (Context g) (Value g) [Capability.template] :=
  (fun context => TemplateCache.fetch (materialize g) context.cache.store (predictionMask g context.kernel), PUnit.unit)

def Joint (g : CFG) (context : Context g)
    (data : Requirements.Results (Value g) context [Capability.template]) : Prop :=
  data.1.1 = materialize g (predictionMask g context.kernel) ∧
    TemplateCache.Valid (materialize g) data.1.2

def certified (g : CFG) : Requirements.Certified [Capability.template] (Joint g) :=
  Requirements.certify (retained g) (Joint g) (by
    intro context
    exact ⟨TemplateCache.value_exact _ _ context.cache.valid _,
      TemplateCache.preserves _ _ context.cache.valid _⟩)

structure Built (g : CFG) where
  product : List Nat × List Nat
  cache : Cache g

/-- Output construction runs even on a cache hit: sharing predictions must
never share the closure of two different kernels. -/
def build (g : CFG) : Context g → Built g :=
  (certified g).consume fun context data proof =>
    ⟨(LRStateSemantics.merge (PublishedClosure.coreItems g context.kernel) data.1.1.items [], data.1.1.items),
      ⟨data.1.2, proof.2⟩⟩

theorem build_exact (g : CFG) (context : Context g) :
    (build g context).product = KernelConstruction.compute g context.kernel := by
  have exactData := ((certified g).build context).property.1
  change (TemplateCache.fetch (materialize g) context.cache.store (predictionMask g context.kernel)).1 = _ at exactData
  change (LRStateSemantics.merge (PublishedClosure.coreItems g context.kernel)
    (TemplateCache.fetch (materialize g) context.cache.store (predictionMask g context.kernel)).1.items [],
    (TemplateCache.fetch (materialize g) context.cache.store (predictionMask g context.kernel)).1.items) = _
  rw [exactData]
  rfl

/-- The consumer's next cache really contains the requested aggregate, not
merely a validity proof for whichever entries happened to be retained. -/
theorem build_stored (g : CFG) (context : Context g) :
    (build g context).cache.store (predictionMask g context.kernel) =
      some (materialize g (predictionMask g context.kernel)) :=
  TemplateCache.fetch_stored _ _ context.cache.valid _

theorem build_retained (g : CFG) (context : Context g) (mask : Nat) (value : Template)
    (found : context.cache.store mask = some value) :
    (build g context).cache.store mask = some value :=
  TemplateCache.fetch_retained _ _ _ _ _ found

/-- Exact aggregate-key hits return the same cache and template, including
when a different kernel requests them. The materializer is not evaluated. -/
theorem cache_hit (g : CFG) (cache : Cache g) (left right : Nat) (value : Template)
    (shared : predictionMask g left = predictionMask g right)
    (found : cache.store (predictionMask g left) = some value) :
    TemplateCache.fetch (materialize g) cache.store (predictionMask g right) = (value, cache.store) := by
  rw [← shared]
  simp [TemplateCache.fetch, found]

end ParserEngine.TemplateCapabilities
