import ParserEngine.SymbolDiscovery

/-! Project the actual raw production records and core-symbol vector writes.
ProducerAdmission.Terminal ordinal decoding is explicit; annotations/actions do not enter cores.
Vector primitives and the host/compiler remain external assumptions. -/
namespace ParserEngine.PhysicalOperandExtraction
open ProducerAdmission

def rawCells (rows : List (List ProducerAdmission.Operand)) : List (Option ProducerAdmission.Symbol) :=
  rows.flatMap fun row => row.map (fun (operand : ProducerAdmission.Operand) => some operand.base) ++ [none]

def Fresh (store : Nat → Option (Option ProducerAdmission.Symbol)) (start : Nat) : Prop :=
  ∀ index, start ≤ index → store index = some none

theorem fill_read (rows : List (List ProducerAdmission.Operand)) (start : Nat) (store : Nat → Option (Option ProducerAdmission.Symbol))
    (fresh : Fresh store start) (code : Nat) (inside : code < CoreSymbols.cells rows) :
    CoreSymbols.fill start rows store (start + code) = (rawCells rows)[code]? := by
  induction rows generalizing start store code with
  | nil => simp [CoreSymbols.cells] at inside
  | cons row rest ih =>
    simp only [CoreSymbols.fill]
    by_cases before : code < row.length
    · rw [CoreSymbols.before rest (start + row.length + 1) (start + code) _ (by omega)]
      have written := NodeCoordinates.written_at (row.map fun (operand : ProducerAdmission.Operand) => some operand.base) start code store (by simpa using before)
      simpa [rawCells, List.getElem?_append_left (by simp; omega : code < (row.map (fun (operand : ProducerAdmission.Operand) => some operand.base) ++ [none]).length),
        List.getElem?_append_left (by simpa using before : code < (row.map fun (operand : ProducerAdmission.Operand) => some operand.base).length)] using written
    · by_cases completed : code = row.length
      · subst code
        rw [CoreSymbols.before rest (start + row.length + 1) (start + row.length) _ (by omega),
          Storage.after _ start (start + row.length) store (by simp), fresh _ (by omega)]
        simp [rawCells]
      · have later : row.length + 1 ≤ code := by omega
        have freshTail : Fresh (Storage.write start (row.map fun (operand : ProducerAdmission.Operand) => some operand.base) store) (start + row.length + 1) := by
          intro index bound
          rw [Storage.after _ start index store (by simp; omega)]
          exact fresh index (by omega)
        have tailBound : code - (row.length + 1) < CoreSymbols.cells rest := by
          change code < row.length + 1 + CoreSymbols.cells rest at inside
          omega
        have address : start + code = (start + row.length + 1) + (code - (row.length + 1)) := by omega
        rw [address, ih _ _ freshTail _ tailBound]
        simp only [rawCells, List.flatMap_cons]
        symm
        rw [List.getElem?_append_right (by simp; omega)]
        simp only [List.length_append, List.length_map, List.length_singleton]

def project (terminal : ProducerAdmission.Terminal → Nat) : ProducerAdmission.Symbol → Grammar.Symbol
  | .terminal key => .terminal (terminal key)
  | .nonterminal key => .nonterminal key

def grammar (terminal : ProducerAdmission.Terminal → Nat) (productions : List ProducerAdmission.Production) : Grammar.CFG :=
  productions.map fun production => ⟨production.lhs, production.rhs.map fun (operand : ProducerAdmission.Operand) => project terminal operand.base⟩

theorem cells_projection (terminal : ProducerAdmission.Terminal → Nat) (productions : List ProducerAdmission.Production) :
    PhysicalCoreExtraction.physicalSymbols (grammar terminal productions) =
      (rawCells (productions.map ProducerAdmission.Production.rhs)).map (fun value => value.map (project terminal)) := by
  induction productions with
  | nil => rfl
  | cons head rest ih =>
    simp [PhysicalCoreExtraction.physicalSymbols, grammar, rawCells, List.map_map,
      Function.comp_def] at ih ⊢
    exact ih

theorem raw_length (rows : List (List ProducerAdmission.Operand)) : (rawCells rows).length = CoreSymbols.cells rows := by
  induction rows with
  | nil => rfl
  | cons row rest ih =>
    change ((row.map (fun (operand : ProducerAdmission.Operand) => some operand.base) ++ [none]) ++ rawCells rest).length =
      row.length + 1 + CoreSymbols.cells rest
    simp only [List.length_append, List.length_map, List.length_singleton]
    omega

/-- Every bounded physical read from the filled native vector equals the
abstract grammar's after-dot view. No per-core correspondence certificate is
supplied; the projection is constructed from raw fields and actual writes. -/
theorem extracted_symbol (terminal : ProducerAdmission.Terminal → Nat) (productions : List ProducerAdmission.Production) (code : Nat)
    (inside : code < CoreSymbols.cells (productions.map ProducerAdmission.Production.rhs)) :
    ((CoreSymbols.fill 0 (productions.map ProducerAdmission.Production.rhs) (fun _ => some none) code).join.map (project terminal)) =
      PhysicalCoreExtraction.symbolAt (grammar terminal productions) code := by
  have written := fill_read (productions.map ProducerAdmission.Production.rhs) 0 (fun _ => some none) (by intro index bound; rfl) code inside
  simp only [Nat.zero_add] at written
  rw [written]
  simp only [PhysicalCoreExtraction.symbolAt, cells_projection, List.getElem?_map]
  cases (rawCells (productions.map ProducerAdmission.Production.rhs))[code]? with
  | none => rfl
  | some value => cases value <;> rfl

end ParserEngine.PhysicalOperandExtraction
