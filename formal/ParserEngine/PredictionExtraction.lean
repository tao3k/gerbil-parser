import ParserEngine.PredictionTermination
import ParserEngine.StateInterning

/-! Derive prediction-row metadata from the grammar and the interner's local
name-index law. A final per-row or per-mask semantic certificate is not an input. -/
namespace ParserEngine.PredictionExtraction
open Grammar PredictionCatalog

-- Native discovery conses entries, so initialization traverses descending IDs.
def names (keys : List Nat) : List Nat := (List.range keys.length).reverse
def name (keys : List Nat) (node : Nat) : Nat := (keys[node]?).getD 0

def extract (index : Nat → Option Nat) (rule : Grammar.Production) : Row :=
  ⟨index rule.lhs, match rule.rhs[0]? with | some (Grammar.Symbol.nonterminal key) => index key | _ => none⟩
def rows (index : Nat → Option Nat) (g : CFG) : List Row := g.map (extract index)

structure NameIndex (keys : List Nat) (index : Nat → Option Nat) : Prop where
  lookup : ∀ key node, index key = some node ↔ keys[node]? = some key

/-- State interning's hash/list correspondence is reused for local name IDs. -/
theorem from_registry {Template : Type} (compute : Nat → List Nat × Template)
    (registry : StateInterning.Registry Nat Template) (valid : StateInterning.Valid compute registry) :
    NameIndex registry.keys registry.index := ⟨valid.index⟩

theorem indexed_bound {keys index} (valid : NameIndex keys index) {key node : Nat}
    (found : index key = some node) : node ∈ names keys := by
  have read := (valid.lookup key node).mp found
  apply List.mem_reverse.mpr
  apply List.mem_range.mpr
  by_cases bound : node < keys.length
  · exact bound
  · simp [List.getElem?_eq_none (by omega : keys.length ≤ node)] at read

theorem indexed_name {keys index} (valid : NameIndex keys index) {key node : Nat}
    (found : index key = some node) : name keys node = key := by
  simp only [name, (valid.lookup key node).mp found, Option.getD_some]

theorem key_index {keys index} (valid : NameIndex keys index) {key : Nat} (member : key ∈ keys) :
    ∃ node, index key = some node := by
  obtain ⟨before, after, same⟩ := List.mem_iff_append.mp member
  exact ⟨before.length, (valid.lookup key before.length).mpr (by rw [same]; simp)⟩

theorem selected_name {keys index} (valid : NameIndex keys index) (node : Nat) (selected : node ∈ names keys) :
    index (name keys node) = some node := by
  apply (valid.lookup _ _).mpr
  simp only [names, List.mem_reverse, List.mem_range] at selected
  simp [name, List.getElem?_eq_getElem selected]

theorem bounded (keys : List Nat) (index : Nat → Option Nat) (valid : NameIndex keys index) (g : CFG) :
    Bounded (rows index g) (names keys) := by
  constructor
  · intro id row read node owner
    simp only [rows, List.getElem?_map, Option.map_eq_some_iff] at read
    obtain ⟨rule, source, rfl⟩ := read
    exact indexed_bound valid owner
  · intro id row read node first
    simp only [rows, List.getElem?_map, Option.map_eq_some_iff] at read
    obtain ⟨rule, source, rfl⟩ := read
    unfold extract at first
    split at first
    next key => exact indexed_bound valid first
    next other => cases first

/-- Every first-symbol nonterminal was seen by make-core-symbol-catalog's name
indexing pass. The full RHS coverage proof can supply this smaller premise. -/
theorem decoded (keys : List Nat) (index : Nat → Option Nat) (valid : NameIndex keys index) (g : CFG)
    (covered : ∀ rule ∈ g, ∀ key, rule.rhs[0]? = some (Grammar.Symbol.nonterminal key) → key ∈ keys) :
    Decoded g (rows index g) (names keys) (name keys) := by
  constructor
  · intro id row read
    simp only [rows, List.getElem?_map, Option.map_eq_some_iff] at read
    obtain ⟨rule, source, _⟩ := read
    exact ⟨rule, source⟩
  · intro id rule source
    exact ⟨extract index rule, by simp [rows, source]⟩
  · intro id row rule read source node selected
    have equal : extract index rule = row := by simpa [rows, source] using read
    subst row
    change index rule.lhs = some node ↔ rule.lhs = name keys node
    constructor
    · intro found; exact (indexed_name valid found).symm
    · intro same; rw [same]; exact selected_name valid node selected
  · intro id row rule read source target
    have equal : extract index rule = row := by simpa [rows, source] using read
    subst row
    constructor
    · intro first
      obtain ⟨child, indexed⟩ := key_index valid (covered rule (List.mem_of_getElem? source) target first)
      exact ⟨child, by simp [extract, first, indexed], indexed_name valid indexed⟩
    · rintro ⟨child, first, same⟩
      unfold extract at first
      split at first
      next key equation =>
        have actual := indexed_name valid first
        have equal : key = target := actual.symm.trans same
        simpa only [equal] using equation
      next other => cases first

end ParserEngine.PredictionExtraction
