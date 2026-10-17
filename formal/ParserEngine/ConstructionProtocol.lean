import ParserEngine.ConstructionRows
import ParserEngine.GroupOrder
import ParserEngine.GroupScratch
import ParserEngine.SparseGroupSource

/-! One construction contract connects paper closure/goto, sorted sparse
products, borrowed storage, complete FIFO execution and published targets.
Source schemas and native controls qualify its restricted execution boundary. -/
namespace ParserEngine.ConstructionProtocol
open Grammar Viable StateInterning KernelConstruction ConstructionQueue ConstructionRows

open SparseGroupMasks

def physicalOwn (symbol : Nat → Option Nat) (predictions : List Nat) (scratch : GroupScratch.State) : List Row :=
  scratch.order.map fun key =>
    ⟨if key ∈ keys symbol predictions then min (scratch.firsts key) (first symbol predictions key)
      else scratch.firsts key, key, scratch.masks key ||| SparseGroupMasks.mask symbol predictions key⟩

def SortContract (sort : List Row → List Row) : Prop :=
  ∀ input, (sort input).Perm input ∧ (sort input).Pairwise (fun a b => a.first ≤ b.first)

theorem physical_own (symbol : Nat → Option Nat) (kernel predictions : List Nat) (firsts : Store) :
    (physicalOwn symbol predictions (GroupScratch.scan symbol kernel (GroupScratch.borrow firsts))).Perm
      (ownUnsorted symbol kernel predictions) := by
  let scratch := GroupScratch.scan symbol kernel (GroupScratch.borrow firsts)
  have permutation := (GroupScratch.order_permutation symbol kernel firsts).map
    (fun key => (⟨if key ∈ keys symbol predictions then min (scratch.firsts key) (first symbol predictions key)
      else scratch.firsts key, key, scratch.masks key ||| SparseGroupMasks.mask symbol predictions key⟩ : Row))
  have same : (keys symbol kernel).map (fun key => (⟨if key ∈ keys symbol predictions then min (scratch.firsts key) (first symbol predictions key)
      else scratch.firsts key, key, scratch.masks key ||| SparseGroupMasks.mask symbol predictions key⟩ : Row)) =
      ownUnsorted symbol kernel predictions := by
    apply List.map_congr_left
    intro key member
    have initialized := GroupScratch.first_ready symbol kernel (GroupScratch.borrow firsts) key rfl member
    have masks := GroupScratch.scan_masks symbol kernel (GroupScratch.borrow firsts)
    dsimp only [scratch]
    rw [initialized, masks]
    rfl
  rw [same] at permutation
  exact permutation

/-- The mutable touched order and stale first slots refine the pure group
producer. Host sorting needs only its usual sorted-permutation contract. -/
theorem native_group_call (sort : List Row → List Row) (contract : SortContract sort)
    (symbol : Nat → Option Nat) (kernel predictions : List Nat) (firsts : Store) :
    let scratch := GroupScratch.scan symbol kernel (GroupScratch.borrow firsts)
    merge (fun key => scratch.masks key != 0) (sort (physicalOwn symbol predictions scratch)) (sharedRows symbol predictions) [] =
      groups symbol kernel predictions ∧ (GroupScratch.reset scratch).masks = (fun _ => 0) := by
  intro scratch
  have permutation := (contract (physicalOwn symbol predictions scratch)).1.trans (physical_own symbol kernel predictions firsts)
  have orderedRows := (contract (physicalOwn symbol predictions scratch)).2
  have own := GroupOrder.native_own_sort symbol kernel predictions _ permutation orderedRows
  have masks := GroupScratch.scan_masks symbol kernel (GroupScratch.borrow firsts)
  refine ⟨?_, GroupScratch.borrowed_reset symbol kernel firsts⟩
  rw [own]
  change merge _ _ _ _ = merge _ _ _ _
  congr 1
  funext key
  change ((GroupScratch.scan symbol kernel (GroupScratch.borrow firsts)).masks key != 0) = _
  rw [masks]
  rfl

structure Product (g : CFG) where
  machine : Machine
  invariant : Invariant g machine
  published : ConstructionRows.Published g machine
  empty : machine.registry.pending = []

def construct (g : CFG) (nonempty : g ≠ []) : Product g :=
  let admitted := initial_invariant g nonempty
  ⟨finish g (initial g) admitted, finish_invariant g _ admitted,
    finish_published g _ admitted (initial_published g), finish_empty g _ admitted⟩

theorem complete (g : CFG) (nonempty : g ≠ []) (node : Nat)
    (inside : node < (construct g nonempty).machine.registry.keys.length) :
    ∃ edges, (construct g nonempty).machine.transitions node = some edges ∧
      RowValid g (construct g nonempty).machine.registry node edges := ConstructionRows.complete g nonempty node inside

/-- The final table's edges carry paper goto/closure meaning. Publication
and every later append have already been composed; no final-table certificate
or finite fixture catalogue is required as an input. -/
theorem paper_target (g : CFG) (nonempty : g ≠ []) (node key target : Nat) (edges : List (Nat × Nat))
    (inside : node < (construct g nonempty).machine.registry.keys.length)
    (stored : (construct g nonempty).machine.transitions node = some edges)
    (edge : (key, target) ∈ edges) (symbol : Symbol) (selected : SymbolDiscovery.index g symbol = some key) :
    ∃ mask values, (construct g nonempty).machine.registry.keys[node]? = some mask ∧
      (construct g nonempty).machine.registry.states target = some values ∧
      ∀ child, (∃ code ∈ values, decodeCore g code = some child) ↔
        Closure g (fun item => ∃ root, Closure g (PublishedClosure.EncodedKernel g mask) root ∧ advance root symbol item) child := by
  let product := construct g nonempty
  obtain ⟨actual, actualStored, mask, read, sound, _⟩ := complete g nonempty node inside
  have same : actual = edges := Option.some.inj (actualStored.symm.trans stored)
  subst actual
  obtain ⟨targetMask, group, indexed⟩ := sound key target edge
  have sourceHistory := product.invariant.generated mask (List.mem_of_getElem? read)
  obtain ⟨row, member, pair⟩ := List.mem_map.mp group
  have sameKey : row.key = key := congrArg Prod.fst pair
  have sameMask : row.mask = targetMask := congrArg Prod.snd pair
  have meaning := SparseGoto.generated_goto g mask row member symbol (by simpa [sameKey] using selected)
  have equality : PublishedClosure.EncodedKernel g targetMask =
      (fun item => ∃ root, Closure g (PublishedClosure.EncodedKernel g mask) root ∧ advance root symbol item) := by
    funext child
    apply propext
    rw [← sameMask]
    have derived := meaning child
    rw [PublishedClosure.kernel_exact g mask (history_bound sourceHistory)] at derived
    exact derived
  have targetRead := (product.invariant.valid.index targetMask target).mp indexed
  refine ⟨mask, (compute g targetMask).1, read, product.invariant.valid.states target targetMask targetRead, ?_⟩
  intro child
  have semantic := PublishedClosure.discovered_factory_exact g targetMask child
  rw [PublishedClosure.kernel_exact g targetMask (input_bound g mask key targetMask group), equality] at semantic
  exact semantic

/-- Source-shape admission is checked separately from mathematical execution.
The three complete declarations are generated from the current Scheme source. -/
theorem source_current : LRTransitionWitness.builderSource = LRTransitionSource.builder ∧
    SparseGroupWitness.symbolSource = SparseGroupSource.symbolCatalogue ∧
    SparseGroupWitness.groupSource = SparseGroupSource.groupConstructor :=
  ⟨LRTransitionWitness.source_checked, SparseGroupWitness.source_checked⟩

/-- A group call can reuse any first-item storage left by an earlier call.
Sorted output, exact first writes, exact mask accumulation and global cleanup
are returned together as a single lifecycle obligation. -/
theorem grouping (g : CFG) (mask : Nat) (firsts : SparseGroupMasks.Store) :
    (rows g mask).Pairwise (fun a b => a.first ≤ b.first) ∧
    (GroupScratch.scan (SymbolDiscovery.physicalIDs g) (PublishedClosure.coreItems g mask) (GroupScratch.borrow firsts)).masks =
      SparseGroupMasks.mask (SymbolDiscovery.physicalIDs g) (PublishedClosure.coreItems g mask) ∧
    (GroupScratch.reset (GroupScratch.scan (SymbolDiscovery.physicalIDs g) (PublishedClosure.coreItems g mask)
      (GroupScratch.borrow firsts))).masks = (fun _ => 0) ∧
    (∀ key ∈ SparseGroupMasks.keys (SymbolDiscovery.physicalIDs g) (PublishedClosure.coreItems g mask),
      (GroupScratch.scan (SymbolDiscovery.physicalIDs g) (PublishedClosure.coreItems g mask) (GroupScratch.borrow firsts)).firsts key =
        SparseGroupMasks.first (SymbolDiscovery.physicalIDs g) (PublishedClosure.coreItems g mask) key) := by
  refine ⟨GroupOrder.generated_sorted g mask, ?_, GroupScratch.borrowed_reset _ _ firsts, ?_⟩
  · exact GroupScratch.scan_masks _ _ _
  · intro key member
    exact GroupScratch.first_ready _ _ _ key rfl member

end ParserEngine.ConstructionProtocol
