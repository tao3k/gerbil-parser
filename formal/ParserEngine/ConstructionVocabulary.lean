import ParserEngine.ConstructionEncoding

/-! Vocabulary transport across reader encoding and the retained constructor.
Both catalogues must preserve first occurrence, tags and EOF position; an
injective key encoding alone does not establish ordinal preservation. -/
namespace ParserEngine.ConstructionVocabulary
open ConstructionEncoding

/-- Total only for expressing map laws; observed uses are proved successful. -/
def key (input : Input) (raw : Terminal) : ProducerAdmission.Terminal :=
  tagged raw ((lookup (catalogue input) raw).getD 0)

theorem encoded_key (input : Input) (raw : Terminal) (encoded : ProducerAdmission.Terminal)
    (success : encodeTerminal input raw = some encoded) : encoded = key input raw := by
  obtain ⟨code, read, equal⟩ := Option.map_eq_some_iff.mp success
  subst encoded
  simp [key, read]

theorem key_encoded (input : Input) (raw : Terminal) (member : raw ∈ catalogue input) :
    encodeTerminal input raw = some (key input raw) := by
  cases read : lookup (catalogue input) raw with
  | none => exact False.elim ((List.idxOf?_eq_none_iff.mp read) member)
  | some code => simp [encodeTerminal, key, read]

theorem key_roundtrip (input : Input) (raw : Terminal) (member : raw ∈ catalogue input) :
    decodeTerminal input (key input raw) = some raw :=
  terminal_roundtrip input raw _ (key_encoded input raw member)

/-- Any left inverse on the scanned domain preserves the actual cons/reverse
collector, including repeated keys and seed keys. No global encoding premise. -/
theorem collect_transport [DecidableEq α] [DecidableEq β] (encode : α → β) (decode : β → Option α)
    (values found : List α)
    (inverse : ∀ value ∈ values ++ found, decode (encode value) = some value) :
    ProducerAdmission.collect (values.map encode) (found.map encode) =
      (ProducerAdmission.collect values found).map encode := by
  induction values generalizing found with
  | nil => rfl
  | cons head rest ih =>
    have same : encode head ∈ found.map encode ↔ head ∈ found := by
      constructor
      · intro member
        obtain ⟨other, present, equal⟩ := List.mem_map.mp member
        have a := inverse head (by simp)
        have b := inverse other (by simp [present])
        rw [equal] at b
        have identity := Option.some.inj (a.symm.trans b)
        exact identity ▸ present
      · exact fun member => List.mem_map.mpr ⟨head, member, rfl⟩
    by_cases present : head ∈ found
    · simp only [List.map_cons, ProducerAdmission.collect, ite_eq_left present, ite_eq_left (same.mpr present)]
      exact ih found (fun value member => inverse value (by simp only [List.mem_append, List.mem_cons] at *; grind))
    · simp only [List.map_cons, ProducerAdmission.collect, ite_eq_right present, ite_eq_right (not_congr same |>.mpr present)]
      exact ih (head :: found) (fun value member => inverse value (by simp only [List.mem_append, List.mem_cons] at *; grind))

/-- Reuse the existing partial traversal theorem for arbitrary observations. -/
theorem mapM_observe (convert : α → Option β) (before : α → γ) (after : β → γ)
    (raw : List α) (encoded : List β) (success : raw.mapM convert = some encoded)
    (law : ∀ value result, convert value = some result → after result = before value) :
    encoded.map after = raw.map before := by
  obtain ⟨length, positions⟩ := mapM_trace convert raw encoded success
  apply List.ext_getElem?
  intro index
  cases read : raw[index]? with
  | none =>
    have bound : encoded.length ≤ index := by
      rw [← length]
      exact List.getElem?_eq_none_iff.mp read
    simp [List.getElem?_map, read, List.getElem?_eq_none_iff.mpr bound]
  | some value =>
    obtain ⟨result, observed, mapped⟩ := positions index value read
    simp [List.getElem?_map, read, observed, law value result mapped]

def rawTerminal (value : WalkSource.Datum) : Option Terminal :=
  (operand value).bind fun symbol => match symbol with | .terminal key => some key | _ => none

theorem operand_terminal (input : Input) (raw : WalkSource.Datum) (encoded : ProducerAdmission.Operand)
    (success : normalizeOperand input raw = some encoded) :
    ProducerAdmission.terminalOf encoded = (rawTerminal raw).map (key input) := by
  obtain ⟨symbol, read, rest⟩ := Option.bind_eq_some_iff.mp success
  obtain ⟨base, encodedRead, equal⟩ := Option.map_eq_some_iff.mp rest
  subst encoded
  cases symbol with
  | nonterminal name =>
    obtain ⟨code, _, equal⟩ := Option.map_eq_some_iff.mp encodedRead
    subst base
    simp [ProducerAdmission.terminalOf, rawTerminal, read]
  | terminal terminal =>
    obtain ⟨value, mapped, equal⟩ := Option.map_eq_some_iff.mp encodedRead
    subst base
    simp [ProducerAdmission.terminalOf, rawTerminal, read, encoded_key input terminal value mapped]

theorem row_terminals (input : Input) (raw : Record) (encoded : ConstructionGrammar.Record)
    (success : normalizeRow input raw = some encoded) :
    encoded.value.rhs.filterMap ProducerAdmission.terminalOf = (raw.rhs.filterMap rawTerminal).map (key input) := by
  obtain ⟨lhs, _, rest⟩ := Option.bind_eq_some_iff.mp success
  obtain ⟨rhs, mapped, equal⟩ := Option.map_eq_some_iff.mp rest
  subst encoded
  have observed := mapM_observe (normalizeOperand input)
    (fun value => (rawTerminal value).map (key input)) ProducerAdmission.terminalOf raw.rhs rhs mapped
    (operand_terminal input)
  have filtered := congrArg (List.filterMap id) observed
  simpa [List.filterMap_map, List.map_filterMap, Function.comp_def] using filtered

theorem terminal_stream (input : Input) (normalized : ConstructionGrammar.Input)
    (success : normalize input = some normalized) :
    ProducerAdmission.terminals (ConstructionGrammar.productions normalized) = (terminals input).map (key input) := by
  have observed := mapM_observe (normalizeRow input)
    (fun row => (row.rhs.filterMap rawTerminal).map (key input))
    (fun row => row.value.rhs.filterMap ProducerAdmission.terminalOf)
    input.records normalized.records (normalization_fields input normalized success).2.2 (row_terminals input)
  have flattened := congrArg List.flatten observed
  have rawView : terminals input = input.records.flatMap (fun row => row.rhs.filterMap rawTerminal) := by
    unfold terminals
    rw [List.filterMap_flatMap]
    rfl
  unfold ProducerAdmission.terminals ConstructionGrammar.productions
  rw [List.flatMap_map, List.filterMap_flatMap, rawView, List.map_flatMap]
  change (normalized.records.map (fun row => row.value.rhs.filterMap ProducerAdmission.terminalOf)).flatten =
    (input.records.map (fun row => (row.rhs.filterMap rawTerminal).map (key input))).flatten
  exact flattened

theorem catalogue_transport (input : Input) (normalized : ConstructionGrammar.Input)
    (success : normalize input = some normalized) :
    ProducerAdmission.catalogue (ConstructionGrammar.productions normalized) = (catalogue input).map (key input) := by
  have transport := collect_transport (key input) (decodeTerminal input) (terminals input) [.eof]
    (fun value member => key_roundtrip input value (by
      simp only [catalogue, List.mem_reverse, ProducerAdmission.collect_members]
      simpa [List.mem_append, List.mem_singleton, or_comm] using member))
  have eof : key input .eof = .eof := rfl
  simp only [ProducerAdmission.catalogue, terminal_stream input normalized success]
  simpa only [List.map_singleton, eof, List.map_reverse, catalogue] using congrArg List.reverse transport

/-- Exact source ordinal is retained by the second, producer-owned catalogue. -/
theorem key_ordinal (input : Input) (normalized : ConstructionGrammar.Input)
    (success : normalize input = some normalized) (raw : Terminal) (code : Nat)
    (read : lookup (catalogue input) raw = some code) (nonEOF : raw ≠ .eof) :
    ProducerAdmission.keyCode (ConstructionGrammar.productions normalized) (key input raw) = some code := by
  have located := lookup_sound (catalogue input) raw code read
  have bound := (List.getElem?_eq_some_iff.mp located).1
  let prefixKeys := (catalogue input).take code
  let suffixKeys := (catalogue input).drop (code + 1)
  have decomposition : catalogue input = prefixKeys ++ raw :: suffixKeys := by
    have split := List.take_append_drop (code + 1) (catalogue input)
    rw [List.take_add_one, located] at split
    simpa only [prefixKeys, suffixKeys, Option.toList_some, List.append_assoc, List.singleton_append] using split.symm
  have generated : ProducerAdmission.catalogue (ConstructionGrammar.productions normalized) =
      prefixKeys.map (key input) ++ key input raw :: suffixKeys.map (key input) := by
    rw [catalogue_transport input normalized success, decomposition, List.map_append, List.map_cons]
  have notEOF : key input raw ≠ .eof := by cases raw <;> simp_all [key, tagged]
  have ordinal := ProducerAdmission.key_code_exact _ _ _ _ generated notEOF
  simpa [prefixKeys, List.length_take, Nat.min_eq_left (Nat.le_of_lt bound)] using ordinal

theorem terminal_symbol (input : Input) (normalized : ConstructionGrammar.Input)
    (success : normalize input = some normalized) (value : WalkSource.Datum) (encoded : ProducerAdmission.Operand)
    (converted : normalizeOperand input value = some encoded) (raw : Terminal) (code : Nat)
    (terminal : rawTerminal value = some raw) (read : lookup (catalogue input) raw = some code)
    (nonEOF : raw ≠ .eof) : ConstructionGrammar.symbol normalized encoded = .terminal code := by
  have observed := operand_terminal input value encoded converted
  rw [terminal, Option.map_some] at observed
  have base : encoded.base = .terminal (key input raw) := by
    cases encoded with
    | mk base annotations => cases base <;> simp_all [ProducerAdmission.terminalOf]
  simp only [ConstructionGrammar.symbol, base, key_ordinal input normalized success raw code read nonEOF, Option.getD_some]

/-- The complete retained result keeps each raw terminal at its physical
production/operand position with the original catalogue ordinal. -/
theorem factory_terminal (sources : ConstructionConsumer.Sources) (input : Input)
    (traversal : CachedConstruction.Traversal) (output : Output sources input traversal)
    (rowIndex operandIndex : Nat) (record : Record) (value : WalkSource.Datum)
    (rowRead : input.records[rowIndex]? = some record) (operandRead : record.rhs[operandIndex]? = some value)
    (raw : Terminal) (code : Nat) (terminal : rawTerminal value = some raw)
    (read : lookup (catalogue input) raw = some code) (nonEOF : raw ≠ .eof) :
    ∃ production, (ConstructionGrammar.grammar output.normalized)[rowIndex]? = some production ∧
      production.rhs[operandIndex]? = some (.terminal code) := by
  obtain ⟨normalized, position, converted⟩ := (table_positions input output.normalized output.encoded).2 rowIndex record rowRead
  obtain ⟨lhs, _, rest⟩ := Option.bind_eq_some_iff.mp converted
  obtain ⟨rhs, mapped, equal⟩ := Option.map_eq_some_iff.mp rest
  subst normalized
  obtain ⟨encoded, operandAt, convertedOperand⟩ := (mapM_trace (normalizeOperand input) record.rhs rhs mapped).2 operandIndex value operandRead
  refine ⟨ConstructionGrammar.row output.normalized ⟨record.id, ⟨lhs, rhs, .ordinary 0⟩⟩, ?_, ?_⟩
  · rw [ConstructionGrammar.project_index, position]
    rfl
  · simp only [ConstructionGrammar.row, List.getElem?_map, operandAt, Option.map_some]
    rw [terminal_symbol input output.normalized output.encoded value encoded convertedOperand raw code terminal read nonEOF]

/-- Symbol discovery is derived from the retained physical occurrence, rather
than supplied as a separate raw-token-to-table-key certificate. -/
theorem factory_symbol_index (sources : ConstructionConsumer.Sources) (input : Input)
    (traversal : CachedConstruction.Traversal) (output : Output sources input traversal)
    (rowIndex operandIndex : Nat) (record : Record) (value : WalkSource.Datum)
    (rowRead : input.records[rowIndex]? = some record) (operandRead : record.rhs[operandIndex]? = some value)
    (raw : Terminal) (code : Nat) (terminal : rawTerminal value = some raw)
    (read : lookup (catalogue input) raw = some code) (nonEOF : raw ≠ .eof) :
    ∃ symbolId, SymbolDiscovery.index (ConstructionGrammar.grammar output.normalized) (.terminal code) = some symbolId := by
  obtain ⟨production, published, atOperand⟩ := factory_terminal sources input traversal output
    rowIndex operandIndex record value rowRead operandRead raw code terminal read nonEOF
  have bounded := Nat.le_of_lt (PhysicalCoreExtraction.lookup_bound atOperand)
  have atCore := PhysicalCoreExtraction.symbol_roundtrip published bounded
  rw [atOperand] at atCore
  exact SymbolDiscovery.indexed_at atCore

/-- A raw terminal occurrence reaches every physical edge for its discovered
symbol in the same LeanPoo-produced machine, with the paper goto/closure law. -/
theorem factory_paper_target (sources : ConstructionConsumer.Sources) (input : Input)
    (traversal : CachedConstruction.Traversal) (output : Output sources input traversal)
    (rowIndex operandIndex : Nat) (record : Record) (value : WalkSource.Datum)
    (rowRead : input.records[rowIndex]? = some record) (operandRead : record.rhs[operandIndex]? = some value)
    (raw : Terminal) (code : Nat) (terminal : rawTerminal value = some raw)
    (read : lookup (catalogue input) raw = some code) (nonEOF : raw ≠ .eof) :
    let g := ConstructionGrammar.grammar output.normalized
    let product := output.result.product
    ∃ symbolId, SymbolDiscovery.index g (.terminal code) = some symbolId ∧
      ∀ node target edges, node < product.construction.1.machine.registry.keys.length →
        ConstructionStorage.rowAt product.construction.2.storage node = edges → (symbolId, target) ∈ edges →
        ∃ kernel values, PlanStorage.decodeVector product.construction.2.memory.objects product.construction.2.storage.plans node =
          some (kernel, TemplateCapabilities.materialize g (TemplateCapabilities.predictionMask g kernel)) ∧
          product.construction.2.storage.states[target]?.join = some values ∧
          ∀ child, (∃ address ∈ values, Viable.decodeCore g address = some child) ↔
            Viable.Closure g (fun item => ∃ root,
              Viable.Closure g (PublishedClosure.EncodedKernel g kernel) root ∧ Viable.advance root (.terminal code) item) child := by
  obtain ⟨symbolId, selected⟩ := factory_symbol_index sources input traversal output
    rowIndex operandIndex record value rowRead operandRead raw code terminal read nonEOF
  refine ⟨symbolId, selected, ?_⟩
  intro node target edges inside stored edge
  exact ConstructionConsumer.physical_paper_target
    (ConstructionGrammar.context output.normalized output.result.receipt traversal) output.result.product
    node symbolId target edges inside stored edge (.terminal code) selected

end ParserEngine.ConstructionVocabulary
