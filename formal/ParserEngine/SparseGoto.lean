import ParserEngine.SparseGroupMasks
import ParserEngine.SymbolDiscovery

/-! Compose sparse group bits with physical symbol decoding, paper goto and
layout bounds. These results do not assume a final per-group meaning witness. -/
namespace ParserEngine.SparseGoto
open Grammar Viable PhysicalCoreExtraction SparseGroupMasks

def physicalRows (g : CFG) (kernel predictions : List Nat) : List Row :=
  groups (SymbolDiscovery.physicalIDs g) kernel predictions

def Parent (g : CFG) (kernel predictions : List Nat) (root : Core) : Prop :=
  ∃ code ∈ LRStateSemantics.merge kernel predictions [], decodeCore g code = some root

theorem row_origins (g : CFG) (kernel predictions : List Nat) (row : Row)
    (member : row ∈ physicalRows g kernel predictions) (bit : Nat) :
    row.mask.testBit bit = true ↔
      ∃ code ∈ LRStateSemantics.merge kernel predictions [],
        SymbolDiscovery.physicalIDs g code = some row.key ∧ bit = code + 1 := by
  have exactBits := groups_bit (SymbolDiscovery.physicalIDs g) kernel predictions row.key bit
  rw [row_bit, ← row_mask _ _ _ row member] at exactBits
  constructor
  · intro present
    rcases exactBits.mp present with ⟨code, seed, symbol, address⟩ | ⟨code, predicted, symbol, address⟩
    · exact ⟨code, (LRStateSemantics.merge_member kernel predictions [] code).mpr (Or.inr (Or.inl seed)), symbol, address⟩
    · exact ⟨code, (LRStateSemantics.merge_member kernel predictions [] code).mpr (Or.inr (Or.inr predicted)), symbol, address⟩
  · rintro ⟨code, source, symbol, address⟩
    rcases (LRStateSemantics.merge_member kernel predictions [] code).mp source with impossible | seed | predicted
    · cases impossible
    · exact exactBits.mpr (Or.inl ⟨code, seed, symbol, address⟩)
    · exact exactBits.mpr (Or.inr ⟨code, predicted, symbol, address⟩)

/-- Successful physical after-dot extraction prevents crossing the completed
slot of any production. This discharges KernelBound for every emitted group. -/
theorem kernel_bound (g : CFG) (kernel predictions : List Nat) (row : Row)
    (member : row ∈ physicalRows g kernel predictions) : PublishedClosure.KernelBound g row.mask := by
  intro bit present
  obtain ⟨code, _, indexed, address⟩ := (row_origins g kernel predictions row member bit).mp present
  obtain ⟨symbol, found, _⟩ := SymbolDiscovery.id_origin indexed
  obtain ⟨root, decoded, next⟩ := symbol_defined found
  obtain ⟨_, _, _, bound⟩ := advance_extracted decoded next
  simpa [address] using bound

/-- Sparse own/shared grouping has exactly the paper's goto kernel meaning,
including zero symbol IDs and duplicate semantic production values. -/
theorem goto_exact (g : CFG) (kernel predictions : List Nat) (row : Row)
    (member : row ∈ physicalRows g kernel predictions) (symbol : Symbol)
    (selected : SymbolDiscovery.index g symbol = some row.key) (child : Core) :
    PublishedClosure.EncodedKernel g row.mask child ↔
      ∃ root, Parent g kernel predictions root ∧ advance root symbol child := by
  constructor
  · rintro ⟨bit, present, decodedChild⟩
    obtain ⟨code, source, indexed, address⟩ := (row_origins g kernel predictions row member bit).mp present
    obtain ⟨actual, found, actualIndex⟩ := SymbolDiscovery.id_origin indexed
    have same := SymbolDiscovery.index_injective actualIndex selected
    subst actual
    obtain ⟨root, decoded, next⟩ := symbol_defined found
    obtain ⟨after, emitted, advanced, _⟩ := advance_extracted decoded next
    have sameChild : after = child := Option.some.inj (emitted.symm.trans (by simpa [address] using decodedChild))
    subst after
    exact ⟨root, ⟨code, source, decoded⟩, advanced⟩
  · rintro ⟨root, ⟨code, source, decoded⟩, advanced⟩
    have found : symbolAt g code = some symbol := (decoded_symbol decoded).trans advanced.2
    have indexed : SymbolDiscovery.physicalIDs g code = some row.key := by simp [SymbolDiscovery.physicalIDs, found, selected]
    obtain ⟨after, emitted, actualAdvance, _⟩ := advance_extracted decoded advanced.2
    have same : after = child := actualAdvance.1.trans advanced.1.symm
    refine ⟨code + 1, (row_origins g kernel predictions row member _).mpr ⟨code, source, indexed, rfl⟩, ?_⟩
    simpa [same] using emitted

/-- Instantiate with the completed prediction/template factory. No final
closure certificate, templateExact or caller-supplied group bound is needed. -/
theorem generated_goto (g : CFG) (mask : Nat) (row : Row)
    (member : row ∈ physicalRows g (PublishedClosure.coreItems g mask)
      ((PublishedClosure.factory (NameDiscovery.catalogue g).keys (NameDiscovery.catalogue g).index (NameDiscovery.name_index g) g mask).2))
    (symbol : Symbol) (selected : SymbolDiscovery.index g symbol = some row.key) (child : Core) :
    PublishedClosure.EncodedKernel g row.mask child ↔
      ∃ root, Closure g (PredictionMaterialization.Kernel g (PublishedClosure.coreItems g mask)) root ∧ advance root symbol child := by
  rw [goto_exact g _ _ row member symbol selected child]
  constructor
  · rintro ⟨root, parent, advanced⟩
    exact ⟨root, (PublishedClosure.discovered_factory_exact g mask root).mp parent, advanced⟩
  · rintro ⟨root, parent, advanced⟩
    exact ⟨root, (PublishedClosure.discovered_factory_exact g mask root).mpr parent, advanced⟩

end ParserEngine.SparseGoto
