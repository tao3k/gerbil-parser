import ParserEngine.ConstructionGrammar

/-! Declared nonterminals are source owners and allocated helpers. Reservation
contains references as well, including undefined ones; it is not declaration.
No production row is needed to declare an empty-language helper. -/
namespace ParserEngine.LoweringDeclarationVocabulary
open Lowering

def directory (augmented : Nat) (rules : Rules) (bindings : List (Nat × Owner)) : List Nat :=
  augmented :: (rules.map Prod.fst ++ bindings.map Prod.fst)

theorem authored (present : row ∈ rules) : row.1 ∈ directory augmented rules bindings := by
  simp only [directory, List.mem_cons, List.mem_append]
  exact Or.inr (Or.inl (List.mem_map.mpr ⟨row, present, rfl⟩))

theorem allocated (present : binding ∈ bindings) : binding.1 ∈ directory augmented rules bindings := by
  simp only [directory, List.mem_cons, List.mem_append]
  exact Or.inr (Or.inr (List.mem_map.mpr ⟨binding, present, rfl⟩))

/-- Reserving an undefined reference prevents its capture by a future helper;
it does not silently promote that reference to a declaration. -/
theorem reserved_undeclared (history : Names.Trace (Names.initial reserved) after)
    (reservedName : name ∈ reserved) (unknown : name ∉ rules.map Prod.fst)
    (different : name ≠ augmented) : name ∉ directory augmented rules after.bindings := by
  intro present
  simp only [directory, List.mem_cons, List.mem_append] at present
  rcases present with same | source | helper
  · exact different same
  · exact unknown source
  · obtain ⟨binding, member, equal⟩ := List.mem_map.mp helper
    exact Names.reserved_not_helper history binding member (equal.symm ▸ reservedName)

/-- Declaration admission is independent of an allocated helper's row count. -/
theorem allocated_operand (input : ConstructionGrammar.Input)
    (directoryRead : input.declarations = directory augmented rules bindings)
    (present : binding ∈ bindings) (operand : ProducerAdmission.Operand)
    (base : operand.base = .nonterminal binding.1) (ordinary : binding.1 ≠ input.augmented) :
    ConstructionGrammar.operandAllowed input operand = true := by
  apply ConstructionGrammar.declared_operand input operand base _ ordinary
  rw [directoryRead]
  exact allocated present

end ParserEngine.LoweringDeclarationVocabulary
