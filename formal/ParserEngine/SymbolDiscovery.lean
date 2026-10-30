import ParserEngine.PhysicalSymbolDiscovery

/-! Symbol IDs are derived from physical symbols by first-occurrence interning.
The index is injective across both terminal and nonterminal symbol classes. -/
namespace ParserEngine.SymbolDiscovery
open Grammar PhysicalCoreExtraction

def encode : Symbol → Nat | .terminal key => 2 * key | .nonterminal key => 2 * key + 1

theorem encode_injective {left right : Symbol} (same : encode left = encode right) : left = right := by
  cases left <;> cases right <;> simp only [encode, Symbol.terminal.injEq, Symbol.nonterminal.injEq] at * <;> omega

def stream (g : CFG) : List Nat := (physicalSymbols g).filterMap fun value => value.map encode

def registry (g : CFG) : StateInterning.Registry Nat Unit := NameDiscovery.discover NameDiscovery.empty (stream g)
def index (g : CFG) (symbol : Symbol) : Option Nat := (registry g).index (encode symbol)

theorem valid (g : CFG) : StateInterning.Valid NameDiscovery.compute (registry g) :=
  NameDiscovery.discover_valid _ NameDiscovery.empty_valid _

theorem index_injective {g : CFG} {left right : Symbol} {id : Nat}
    (a : index g left = some id) (b : index g right = some id) : left = right := by
  have leftRead := ((valid g).index (encode left) id).mp a
  have rightRead := ((valid g).index (encode right) id).mp b
  exact encode_injective (Option.some.inj (leftRead.symm.trans rightRead))

theorem indexed_member {g : CFG} {symbol : Symbol} (member : some symbol ∈ physicalSymbols g) :
    ∃ id, index g symbol = some id := by
  exact NameDiscovery.discovered _ _ (encode symbol)
    (List.mem_filterMap.mpr ⟨some symbol, member, rfl⟩)

theorem indexed_at {g : CFG} {code : Nat} {symbol : Symbol} (found : symbolAt g code = some symbol) :
    ∃ id, index g symbol = some id := by
  cases read : (physicalSymbols g)[code]? with
  | none => simp [symbolAt, read] at found
  | some value =>
    cases value with
    | none => simp [symbolAt, read] at found
    | some actual =>
      have same : actual = symbol := by simpa [symbolAt, read] using found
      subst actual
      exact indexed_member (List.mem_of_getElem? read)

def physicalIDs (g : CFG) (code : Nat) : Option Nat := (symbolAt g code).bind (index g)

theorem id_origin {g : CFG} {code id : Nat} (found : physicalIDs g code = some id) :
    ∃ symbol, symbolAt g code = some symbol ∧ index g symbol = some id := Option.bind_eq_some_iff.mp found

end ParserEngine.SymbolDiscovery
