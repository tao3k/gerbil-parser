import ParserEngine.LRTransitionSource
import ParserEngine.NameDiscovery

/-! Instantiate transition publication's pointwise factory with the completed
prediction/materialization/closure chain. Published target rows have grammar
meaning without a caller-supplied final-state certificate. -/
namespace ParserEngine.PublishedClosure
open Grammar Viable PredictionMaterialization

def coreItems (g : CFG) (mask : Nat) : List Nat :=
  (List.range (denseCoordinates (g.map fun rule => rule.rhs.length)).length).filter fun code => mask.testBit code

def KernelBound (g : CFG) (mask : Nat) : Prop :=
  ∀ code, mask.testBit code = true → code < (denseCoordinates (g.map fun rule => rule.rhs.length)).length

theorem core_membership (g : CFG) (mask : Nat) (bounded : KernelBound g mask) (code : Nat) :
    code ∈ coreItems g mask ↔ mask.testBit code = true := by
  simp only [coreItems, List.mem_filter, List.mem_range]
  exact ⟨And.right, fun present => ⟨bounded code present, present⟩⟩

def EncodedKernel (g : CFG) (mask : Nat) (root : Core) : Prop :=
  ∃ code, mask.testBit code = true ∧ decodeCore g code = some root

theorem kernel_exact (g : CFG) (mask : Nat) (bounded : KernelBound g mask) :
    Kernel g (coreItems g mask) = EncodedKernel g mask := by
  funext root
  apply propext
  constructor
  · rintro ⟨code, member, decoded⟩
    exact ⟨code, (core_membership g mask bounded code).mp member, decoded⟩
  · rintro ⟨code, bit, decoded⟩
    exact ⟨code, (core_membership g mask bounded code).mpr bit, decoded⟩

def factory (keys : List Nat) (index : Nat → Option Nat) (valid : PredictionExtraction.NameIndex keys index)
    (g : CFG) (mask : Nat) : List Nat × List Nat :=
  let seeds := coreItems g mask
  let predicted := materialize g (aggregate g index (completed keys index valid g) seeds)
  (LRStateSemantics.merge seeds predicted [], predicted)

theorem factory_exact (keys : List Nat) (index : Nat → Option Nat) (valid : PredictionExtraction.NameIndex keys index)
    (g : CFG) (covered : Covers keys g) (mask : Nat) (item : Core) :
    (∃ code ∈ (factory keys index valid g mask).1, decodeCore g code = some item) ↔
      Closure g (Kernel g (coreItems g mask)) item := closure_exact keys index valid g covered _ item

/-- Later state appends retain the target's published closure row and its
kernel/template plan. This composes semantic materialization with row IDs. -/
theorem target_exact (keys : List Nat) (index : Nat → Option Nat) (valid : PredictionExtraction.NameIndex keys index)
    (g : CFG) (covered : Covers keys g) (before : StateInterning.Registry Nat (List Nat))
    (admitted : StateInterning.Valid (factory keys index valid g) before)
    (groups : List (Nat × Nat)) (bounded : ∀ symbol mask, (symbol, mask) ∈ groups → KernelBound g mask)
    (symbol target : Nat)
    (member : (symbol, target) ∈ (TransitionPublication.publish (factory keys index valid g) before groups).rows) :
    ∃ mask row, (symbol, mask) ∈ groups ∧
      (TransitionPublication.publish (factory keys index valid g) before groups).registry.states target = some row ∧
      ∀ item, (∃ code ∈ row, decodeCore g code = some item) ↔ Closure g (EncodedKernel g mask) item := by
  obtain ⟨mask, group, lookup⟩ := (TransitionPublication.row_exact _ before groups symbol target).mp member
  have preserved := TransitionPublication.valid _ before admitted groups
  have source := (preserved.index mask target).mp lookup
  refine ⟨mask, (factory keys index valid g mask).1, group, preserved.states target mask source, ?_⟩
  intro item
  rw [← kernel_exact g mask (bounded symbol mask group)]
  exact factory_exact keys index valid g covered mask item

/-- Grammar-owned discovery discharges both the hash/list law and name
coverage; these are no longer final semantic certificates supplied by a caller. -/
theorem discovered_factory_exact (g : CFG) (mask : Nat) (item : Core) :
    (∃ code ∈ (factory (NameDiscovery.catalogue g).keys (NameDiscovery.catalogue g).index
      (NameDiscovery.name_index g) g mask).1, decodeCore g code = some item) ↔
        Closure g (Kernel g (coreItems g mask)) item :=
  factory_exact _ _ (NameDiscovery.name_index g) g (NameDiscovery.coverage g) mask item

end ParserEngine.PublishedClosure
