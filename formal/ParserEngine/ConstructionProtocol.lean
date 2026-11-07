import ParserEngine.TraversalCapabilities
import ParserEngine.BitTraversalSource
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

/-- The native enumeration boundary is discharged by the selected LeanPoo
capability, then projected into the grammar's existing physical layout. -/
theorem callback_cores (g : CFG) (context : TraversalCapabilities.Context)
    (bounded : PublishedClosure.KernelBound g context.mask) :
    (TraversalCapabilities.callbacks context).val = PublishedClosure.coreItems g context.mask := by
  rw [TraversalCapabilities.callbacks_exact]
  apply BitTraversal.ordered_unique _ _ (BitTraversal.direct_sorted _) ((List.pairwise_lt_range).filter _)
  intro bit
  rw [BitTraversal.direct_member]
  exact (PublishedClosure.core_membership g context.mask bounded bit).symm

/-- Execute grouping through the projected capability, rather than merely
attaching an unused traversal certificate to the abstract construction. -/
def traversedGroups (sort : List Row → List Row) (g : CFG) (context : TraversalCapabilities.Context) (firsts : Store) : List Row :=
  let kernel := (TraversalCapabilities.callbacks context).val
  let scratch := GroupScratch.scan (SymbolDiscovery.physicalIDs g) kernel (GroupScratch.borrow firsts)
  merge (fun key => scratch.masks key != 0) (sort (physicalOwn (SymbolDiscovery.physicalIDs g) (predictions g context.mask) scratch))
    (sharedRows (SymbolDiscovery.physicalIDs g) (predictions g context.mask)) []

theorem traversedGroups_exact (sort : List Row → List Row) (contract : SortContract sort)
    (g : CFG) (context : TraversalCapabilities.Context) (bounded : PublishedClosure.KernelBound g context.mask) (firsts : Store) :
    traversedGroups sort g context firsts = rows g context.mask := by
  unfold traversedGroups
  rw [callback_cores g context bounded]
  exact (native_group_call sort contract (SymbolDiscovery.physicalIDs g) (PublishedClosure.coreItems g context.mask)
    (predictions g context.mask) firsts).1

def traversalTick (sort : List Row → List Row) (g : CFG) (width : Nat) (population : Nat → Nat) (positive : 0 < width)
    (fixnum : Nat → Bool) (before : Machine) : Machine :=
  match before.registry.pending with
  | [] => before
  | node :: tail =>
    match before.registry.keys[node]? with
    | none => before
    | some mask =>
      let context : TraversalCapabilities.Context := ⟨mask, width, positive, population mask, fixnum mask⟩
      let groups := (traversedGroups sort g context (fun _ => 0)).map fun row => (row.key, row.mask)
      let published := TransitionPublication.publish (compute g) (TransitionPublication.drain before.registry tail) groups
      ⟨published.registry, ProducerAdmission.Storage.update before.transitions node published.rows⟩

/-- Substitute the selected traversal into the actual whole queue iteration,
including append/enqueue and final publication, with no final-state premise. -/
theorem traversal_tick_exact (sort : List Row → List Row) (contract : SortContract sort)
    (g : CFG) (width : Nat) (population : Nat → Nat) (positive : 0 < width) (fixnum : Nat → Bool)
    (before : Machine) (admitted : Invariant g before) :
    traversalTick sort g width population positive fixnum before = tick g before := by
  cases front : before.registry.pending with
  | nil => simp [traversalTick, tick, front]
  | cons node tail =>
    have bound := admitted.valid.bound node (by rw [front]; simp)
    let mask := before.registry.keys[node]
    have read : before.registry.keys[node]? = some mask := List.getElem?_eq_getElem bound
    have history := admitted.generated mask (List.mem_of_getElem? read)
    have exactGroups := traversedGroups_exact sort contract g ⟨mask, width, positive, population mask, fixnum mask⟩
      (KernelConstruction.history_bound history) (fun _ => 0)
    simp only [traversalTick, tick, front, read, exactGroups, inputs]

def traversalFinish (sort : List Row → List Row) (contract : SortContract sort)
    (g : CFG) (width : Nat) (population : Nat → Nat) (positive : 0 < width) (fixnum : Nat → Bool)
    (before : Machine) (admitted : Invariant g before) : Machine :=
  if _empty : before.registry.pending = [] then before
  else
    let later := traversalTick sort g width population positive fixnum before
    have valid : Invariant g later := by
      dsimp only [later]
      rw [traversal_tick_exact sort contract g width population positive fixnum before admitted]
      exact tick_invariant g before admitted
    traversalFinish sort contract g width population positive fixnum later valid
termination_by capacity g - before.registry.processed
decreasing_by
  rw [traversal_tick_exact sort contract g width population positive fixnum before admitted]
  exact tick_rank g before admitted _empty

theorem traversal_finish_exact (sort : List Row → List Row) (contract : SortContract sort)
    (g : CFG) (width : Nat) (population : Nat → Nat) (positive : 0 < width) (fixnum : Nat → Bool)
    (before : Machine) (admitted : Invariant g before) :
    traversalFinish sort contract g width population positive fixnum before admitted = finish g before admitted := by
  rw [traversalFinish, finish]
  split
  · rfl
  next pending =>
    rw [traversal_finish_exact]
    simp only [traversal_tick_exact sort contract g width population positive fixnum before admitted]
termination_by capacity g - before.registry.processed
decreasing_by
  rw [traversal_tick_exact sort contract g width population positive fixnum before admitted]
  exact tick_rank g before admitted (by assumption)

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
The complete declaration schemas are generated from the current Scheme source. -/
theorem source_current : LRTransitionWitness.builderSource = LRTransitionSource.builder ∧
    SparseGroupWitness.symbolSource = SparseGroupSource.symbolCatalogue ∧
    SparseGroupWitness.groupSource = SparseGroupSource.groupConstructor ∧
    BitTraversalWitness.source = BitTraversalSource.declaration ∧ BitTraversalWitness.widthSource = BitTraversalSource.fieldWidth :=
  ⟨LRTransitionWitness.source_checked, SparseGroupWitness.source_checked.1,
    SparseGroupWitness.source_checked.2, BitTraversalWitness.source_checked⟩

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
