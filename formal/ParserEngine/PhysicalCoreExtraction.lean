import ParserEngine.PublishedClosure

/-! Decode arbitrary physical addresses, including completed slots. The inverse
retains production IDs even when two semantic production values are equal. -/
namespace ParserEngine.PhysicalCoreExtraction
open Grammar Viable

theorem lookup_bound {Value : Type} {values : List Value} {index : Nat} {value : Value}
    (read : values[index]? = some value) : index < values.length := by
  by_cases valid : index < values.length
  · exact valid
  · simp [List.getElem?_eq_none (by omega : values.length ≤ index)] at read

theorem coordinate_inverse (widths : List Nat) (code production dot : Nat)
    (read : (denseCoordinates widths)[code]? = some (production, dot)) :
    ∃ width, widths[production]? = some width ∧ dot ≤ width ∧ code = denseOffset widths production + dot := by
  induction widths generalizing code production dot with
  | nil => simp [denseCoordinates] at read
  | cons width rest ih =>
    by_cases inside : code < width + 1
    · rw [denseCoordinates, List.getElem?_append_left (by simpa using inside),
        List.getElem?_map, List.getElem?_range inside] at read
      have same : (0, code) = (production, dot) := Option.some.inj read
      obtain ⟨first, second⟩ := Prod.mk.inj same
      subst production; subst dot
      exact ⟨width, rfl, by omega, by simp [denseOffset]⟩
    · rw [denseCoordinates, List.getElem?_append_right (by simpa using Nat.le_of_not_gt inside), List.getElem?_map] at read
      obtain ⟨coordinate, source, same⟩ := Option.map_eq_some_iff.mp read
      simp only [List.length_map, List.length_range] at source
      rcases coordinate with ⟨parent, position⟩
      obtain ⟨first, second⟩ := Prod.mk.inj same
      subst production; subst dot
      obtain ⟨actual, row, bound, address⟩ := ih (code - (width + 1)) parent position source
      refine ⟨actual, row, bound, ?_⟩
      simp only [denseOffset]
      omega

theorem core_inverse {g : CFG} {code : Nat} {root : Core} (read : decodeCore g code = some root) :
    ∃ production rule dot, g[production]? = some rule ∧ root = ⟨rule, dot⟩ ∧
      dot ≤ rule.rhs.length ∧ code = denseOffset (g.map fun p => p.rhs.length) production + dot := by
  unfold decodeCore at read
  obtain ⟨coordinate, source, rest⟩ := Option.bind_eq_some_iff.mp read
  obtain ⟨rule, row, result⟩ := Option.bind_eq_some_iff.mp rest
  have same : root = ⟨rule, coordinate.2⟩ := Option.some.inj result.symm
  obtain ⟨width, widthRow, bound, address⟩ := coordinate_inverse _ code coordinate.1 coordinate.2 source
  have widthExact : width = rule.rhs.length := by simpa [row] using widthRow.symm
  subst width
  exact ⟨coordinate.1, rule, coordinate.2, row, same, bound, address⟩

def physicalSymbols (g : CFG) : List (Option Symbol) :=
  g.flatMap fun rule => rule.rhs.map some ++ [none]
def symbolAt (g : CFG) (code : Nat) : Option Symbol := (physicalSymbols g)[code]?.join

theorem symbol_roundtrip {g : CFG} {production dot : Nat} {rule : Production}
    (row : g[production]? = some rule) (valid : dot ≤ rule.rhs.length) :
    symbolAt g (denseOffset (g.map fun p => p.rhs.length) production + dot) = rule.rhs[dot]? := by
  induction g generalizing production with
  | nil => simp at row
  | cons head rest ih =>
    cases production with
    | zero =>
      have same : head = rule := Option.some.inj row
      subst head
      simp only [physicalSymbols, List.flatMap_cons, symbolAt, List.map_cons, denseOffset, Nat.zero_add]
      rw [List.getElem?_append_left (by simp; omega)]
      by_cases before : dot < rule.rhs.length
      · rw [List.getElem?_append_left (by simpa using before : dot < (rule.rhs.map some).length), List.getElem?_map]
        cases rule.rhs[dot]? <;> rfl
      · have endDot : dot = rule.rhs.length := by omega
        subst dot
        simp
    | succ production =>
      have source : rest[production]? = some rule := row
      simp only [symbolAt, physicalSymbols, List.flatMap_cons, List.map_cons, denseOffset]
      rw [List.getElem?_append_right (by simp; omega)]
      have address : head.rhs.length + 1 + denseOffset (rest.map fun (p : Grammar.Production) => p.rhs.length) production + dot -
          (head.rhs.map some ++ [none]).length = denseOffset (rest.map fun (p : Grammar.Production) => p.rhs.length) production + dot := by simp; omega
      rw [address]
      exact ih source

theorem decoded_symbol {g : CFG} {code : Nat} {root : Core} (read : decodeCore g code = some root) :
    symbolAt g code = root.production.rhs[root.dot]? := by
  obtain ⟨production, rule, dot, row, same, bound, address⟩ := core_inverse read
  subst root; rw [address]; exact symbol_roundtrip row bound

theorem symbols_length (g : CFG) : (physicalSymbols g).length =
    (denseCoordinates (g.map fun p => p.rhs.length)).length := by
  induction g with
  | nil => rfl
  | cons rule rest ih =>
    change ((rule.rhs.map some ++ [none]) ++ physicalSymbols rest).length =
      ((List.range (rule.rhs.length + 1)).map (fun dot => (0, dot)) ++
        (denseCoordinates (rest.map fun (p : Grammar.Production) => p.rhs.length)).map (fun (coordinate : Nat × Nat) => (coordinate.1 + 1, coordinate.2))).length
    simp only [List.length_append, List.length_map, List.length_singleton, List.length_range]
    omega

theorem symbol_defined {g : CFG} {code : Nat} {symbol : Symbol} (found : symbolAt g code = some symbol) :
    ∃ root, decodeCore g code = some root ∧ root.production.rhs[root.dot]? = some symbol := by
  cases raw : (physicalSymbols g)[code]? with
  | none => simp [symbolAt, raw] at found
  | some value =>
    have bound := lookup_bound raw
    rw [symbols_length] at bound
    cases coordinate : (denseCoordinates (g.map fun p => p.rhs.length))[code]? with
    | none => simp [List.getElem?_eq_getElem bound] at coordinate
    | some position =>
      obtain ⟨width, widthRow, _, _⟩ := coordinate_inverse _ code position.1 position.2 coordinate
      simp only [List.getElem?_map, Option.map_eq_some_iff] at widthRow
      obtain ⟨rule, row, _⟩ := widthRow
      have decoded : decodeCore g code = some ⟨rule, position.2⟩ := by simp [decodeCore, coordinate, row]
      exact ⟨⟨rule, position.2⟩, decoded, (decoded_symbol decoded).symm.trans found⟩

/-- The next after-dot symbol simultaneously justifies physical +1, its
semantic advance and the bound required by the target kernel representation. -/
theorem advance_extracted {g : CFG} {code : Nat} {root : Core} {symbol : Symbol}
    (read : decodeCore g code = some root) (next : root.production.rhs[root.dot]? = some symbol) :
    ∃ child, decodeCore g (code + 1) = some child ∧ advance root symbol child ∧
      code + 1 < (denseCoordinates (g.map fun p => p.rhs.length)).length := by
  obtain ⟨production, rule, dot, row, same, _, address⟩ := core_inverse read
  subst root
  have within : dot < rule.rhs.length := lookup_bound next
  have widthRow : (g.map fun p => p.rhs.length)[production]? = some rule.rhs.length := by simp [row]
  have coordinate := dense_coordinate_roundtrip widthRow (by omega : dot + 1 ≤ rule.rhs.length)
  refine ⟨⟨rule, dot + 1⟩, ?_, ⟨rfl, next⟩, ?_⟩
  · rw [address]
    simpa [Nat.add_assoc] using decode_core_roundtrip row (by omega : dot + 1 ≤ rule.rhs.length)
  · rw [address]
    simpa [Nat.add_assoc] using lookup_bound coordinate

end ParserEngine.PhysicalCoreExtraction
