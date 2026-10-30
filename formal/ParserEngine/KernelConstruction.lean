import ParserEngine.SparseGoto

/-! Kernel bounds and target goto meaning are derived along construction,
starting at the actual augmented dot-zero bit, not supplied per final row. -/
namespace ParserEngine.KernelConstruction
open Grammar Viable PublishedClosure

def compute (g : CFG) := factory (NameDiscovery.catalogue g).keys (NameDiscovery.catalogue g).index (NameDiscovery.name_index g) g

def predictions (g : CFG) (mask : Nat) : List Nat := (compute g mask).2
def rows (g : CFG) (mask : Nat) : List SparseGroupMasks.Row :=
  SparseGoto.physicalRows g (coreItems g mask) (predictions g mask)
def inputs (g : CFG) (mask : Nat) : List (Nat × Nat) := (rows g mask).map fun row => (row.key, row.mask)

inductive History (g : CFG) : Nat → Prop where
  | initial (nonempty : g ≠ []) : History g 1
  | next {mask} (earlier : History g mask) (row : SparseGroupMasks.Row) (member : row ∈ rows g mask) : History g row.mask

theorem initial_bound (g : CFG) (nonempty : g ≠ []) : KernelBound g 1 := by
  intro code bit
  have one : (1 : Nat) = 2 ^ 0 := rfl
  rw [one, Nat.testBit_two_pow] at bit
  have same : 0 = code := of_decide_eq_true bit
  subst code
  cases g with
  | nil => exact False.elim (nonempty rfl)
  | cons head rest => simp only [List.map_cons, denseCoordinates, List.length_append, List.length_map, List.length_range]; omega

theorem history_bound {g : CFG} {mask : Nat} (history : History g mask) : KernelBound g mask := by
  cases history with
  | initial nonempty => exact initial_bound g nonempty
  | next earlier row member => exact SparseGoto.kernel_bound g _ _ row member

theorem input_bound (g : CFG) (mask symbol target : Nat) (member : (symbol, target) ∈ inputs g mask) : KernelBound g target := by
  obtain ⟨row, belongs, same⟩ := List.mem_map.mp member
  have equal : row.mask = target := congrArg Prod.snd same
  rw [← equal]
  exact SparseGoto.kernel_bound g _ _ row belongs

/-- Complete publication-to-paper composition for each returned target row.
The pointwise factory, source mask bounds, group meaning and target lookup are
all instantiated; no per-target semantic certificate is a premise. -/
theorem target_goto_exact (g : CFG) (mask : Nat) (history : History g mask)
    (before : StateInterning.Registry Nat (List Nat)) (valid : StateInterning.Valid (compute g) before)
    (key target : Nat) (member : (key, target) ∈ (TransitionPublication.publish (compute g) before (inputs g mask)).rows)
    (symbol : Symbol) (selected : SymbolDiscovery.index g symbol = some key) :
    ∃ row, (TransitionPublication.publish (compute g) before (inputs g mask)).registry.states target = some row ∧
      ∀ child, (∃ code ∈ row, decodeCore g code = some child) ↔
        Closure g (fun item => ∃ root, Closure g (EncodedKernel g mask) root ∧ advance root symbol item) child := by
  obtain ⟨targetMask, values, belongs, stored, semantics⟩ :=
    PublishedClosure.target_exact _ _ (NameDiscovery.name_index g) g (NameDiscovery.coverage g) before valid
      (inputs g mask) (input_bound g mask) key target member
  obtain ⟨group, groupMember, same⟩ := List.mem_map.mp belongs
  have sameKey : group.key = key := congrArg Prod.fst same
  have sameMask : group.mask = targetMask := congrArg Prod.snd same
  have kernelMeaning : EncodedKernel g targetMask =
      (fun item => ∃ root, Closure g (EncodedKernel g mask) root ∧ advance root symbol item) := by
    funext item
    apply propext
    rw [← sameMask]
    have derived := SparseGoto.generated_goto g mask group groupMember symbol (by simpa [sameKey] using selected) item
    rw [kernel_exact g mask (history_bound history)] at derived
    exact derived
  refine ⟨values, stored, ?_⟩
  intro child
  simpa only [kernelMeaning] using semantics child

end ParserEngine.KernelConstruction
