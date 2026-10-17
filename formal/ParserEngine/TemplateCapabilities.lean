import ParserEngine.TemplateCache
import ParserEngine.PlanStorage
import ParserEngine.GroupScratch
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
  memory : PlanStorage.Memory Template
  admitted : PlanStorage.Valid (materialize g) (predictionMask g) memory
  scratch : GroupScratch.State
  clean : scratch.masks = (fun _ => 0) ∧ scratch.order = []

def Cache.store (cache : Cache g) := PlanStorage.view cache.memory
theorem Cache.valid (cache : Cache g) : TemplateCache.Valid (materialize g) cache.store :=
  PlanStorage.view_valid _ _ _ cache.admitted

def empty (g : CFG) : Cache g where
  memory := PlanStorage.empty
  admitted := by
    constructor
    · intro _ _ found; cases found
    · intro _ _ found; cases found
  scratch := GroupScratch.borrow (fun _ => 0)
  clean := ⟨rfl, rfl⟩

def Cache.readTemplate (cache : Cache g) (node : Nat) := PlanStorage.decode cache.memory node

structure Context (g : CFG) where
  cache : Cache g
  kernel : Nat
  node : Nat

def load (g : CFG) (context : Context g) : Template × Cache g :=
  let memory := PlanStorage.install (materialize g) (predictionMask g) context.cache.memory context.node context.kernel
  ((PlanStorage.view memory (predictionMask g context.kernel)).getD ⟨[], []⟩,
    ⟨memory, PlanStorage.install_valid _ _ _ context.cache.admitted _ _, context.cache.scratch, context.cache.clean⟩)

theorem load_exact (g : CFG) (context : Context g) : (load g context).1 = materialize g (predictionMask g context.kernel) := by
  have installed := PlanStorage.install_stored (materialize g) (predictionMask g) context.cache.memory
    context.cache.admitted context.node context.kernel
  simp only [load, installed, Option.getD_some]

inductive Capability where | template deriving DecidableEq
abbrev Value (g : CFG) (_ : Context g) (_ : Capability) := Template × PlanStorage.Memory Template

def retained (g : CFG) : Requirements.Factories (Context g) (Value g) [Capability.template] :=
  (fun context => ((load g context).1, (load g context).2.memory), PUnit.unit)

def Joint (g : CFG) (context : Context g)
    (data : Requirements.Results (Value g) context [Capability.template]) : Prop :=
  data.1.1 = materialize g (predictionMask g context.kernel) ∧
    PlanStorage.Valid (materialize g) (predictionMask g) data.1.2 ∧
    data.1.2 = PlanStorage.install (materialize g) (predictionMask g) context.cache.memory context.node context.kernel

def certified (g : CFG) : Requirements.Certified [Capability.template] (Joint g) :=
  Requirements.certify (retained g) (Joint g) (by
    intro context
    exact ⟨load_exact g context, (load g context).2.admitted, rfl⟩)

structure Built (g : CFG) where
  product : List Nat × List Nat
  cache : Cache g

/-- Output construction runs even on a cache hit: sharing predictions must
never share the closure of two different kernels. -/
def build (g : CFG) : Context g → Built g :=
  (certified g).consume fun context data proof =>
    ⟨(LRStateSemantics.merge (PublishedClosure.coreItems g context.kernel) data.1.1.items [], data.1.1.items),
      ⟨data.1.2, proof.2.1, context.cache.scratch, context.cache.clean⟩⟩

theorem build_exact (g : CFG) (context : Context g) :
    (build g context).product = KernelConstruction.compute g context.kernel := by
  change (LRStateSemantics.merge (PublishedClosure.coreItems g context.kernel) (load g context).1.items [],
    (load g context).1.items) = _
  rw [load_exact]
  rfl

theorem build_memory (g : CFG) (context : Context g) :
    (build g context).cache.memory = PlanStorage.install (materialize g) (predictionMask g)
      context.cache.memory context.node context.kernel := rfl

/-- The consumer's next cache really contains the requested aggregate, not
merely a validity proof for whichever entries happened to be retained. -/
theorem build_stored (g : CFG) (context : Context g) :
    (build g context).cache.store (predictionMask g context.kernel) =
      some (materialize g (predictionMask g context.kernel)) :=
  PlanStorage.install_stored _ _ _ context.cache.admitted _ _

theorem build_retained (g : CFG) (context : Context g) (mask : Nat) (value : Template)
    (found : context.cache.store mask = some value) :
    (build g context).cache.store mask = some value :=
  PlanStorage.install_retained _ _ _ context.cache.admitted _ _ _ _ found

theorem build_plan (g : CFG) (context : Context g) :
    (build g context).cache.readTemplate context.node = some (context.kernel, materialize g (predictionMask g context.kernel)) := by
  unfold Cache.readTemplate
  rw [build_memory, PlanStorage.decode_install _ _ _ context.cache.admitted]

theorem build_other_plan (g : CFG) (context : Context g) (old : Nat) (different : old ≠ context.node) :
    (build g context).cache.readTemplate old = context.cache.readTemplate old := by
  unfold Cache.readTemplate
  rw [build_memory, PlanStorage.decode_other _ _ _ context.cache.admitted _ _ _ different]

theorem build_scratch (g : CFG) (context : Context g) :
    (build g context).cache.scratch = context.cache.scratch := rfl

/-- Exact aggregate-key hits return the same cache and template, including
when a different kernel requests them. The materializer is not evaluated. -/
theorem cache_hit (g : CFG) (cache : Cache g) (left right : Nat) (value : Template)
    (shared : predictionMask g left = predictionMask g right)
    (found : cache.store (predictionMask g left) = some value) :
    TemplateCache.fetch (materialize g) cache.store (predictionMask g right) = (value, cache.store) := by
  rw [← shared]
  simp [TemplateCache.fetch, found]

end ParserEngine.TemplateCapabilities
