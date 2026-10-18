import ParserEngine.ConstructionGrammar
import Init.Data.List.Monadic

/-! Raw reader names/text to normalized construction input. String equality
models Scheme symbol/text contents; host reader and symbol interning stay
external. Finite directories provide checked partial codes, never hash values. -/
namespace ParserEngine.ConstructionEncoding
open WalkSource

def lookup [DecidableEq α] (keys : List α) (key : α) : Option Nat := keys.idxOf? key

theorem lookup_sound [DecidableEq α] (keys : List α) (key : α) (code : Nat)
    (found : lookup keys key = some code) : keys[code]? = some key := by
  obtain ⟨bound, value, _⟩ := List.idxOf?_eq_some_iff.mp found
  simpa [value] using List.getElem?_eq_getElem bound

theorem lookup_injective [DecidableEq α] (keys : List α) (a b : α) (code : Nat)
    (left : lookup keys a = some code) (right : lookup keys b = some code) : a = b :=
  Option.some.inj ((lookup_sound keys a code left).symm.trans (lookup_sound keys b code right))

inductive Terminal where
  | eof | token (name : String) | literal (text : String)
  | layoutStart (text : String) | layoutNext (text : String)
  deriving DecidableEq, Repr

inductive Symbol where
  | nonterminal (name : String) | terminal (key : Terminal)
  deriving DecidableEq, Repr

/-- Match the real canonical base shapes. A symbol payload is never accepted
as literal/layout text, nor is an empty text silently assigned a code. -/
def base : Datum → Option Symbol
  | .list [.symbol "nonterminal", .symbol name] => some (.nonterminal name)
  | .list [.symbol "terminal", .symbol "token", .symbol name] => some (.terminal (.token name))
  | .list [.symbol "terminal", .symbol "literal", .text text] =>
      if text.isEmpty then none else some (.terminal (.literal text))
  | .list [.symbol "terminal", .symbol "layout-start", .text text] =>
      if text.isEmpty then none else some (.terminal (.layoutStart text))
  | .list [.symbol "terminal", .symbol "layout-next", .text text] =>
      if text.isEmpty then none else some (.terminal (.layoutNext text))
  | _ => none

def operand : Datum → Option Symbol
  | .list [.symbol "marked", value, .list _] => base value
  | value => base value

structure Record where
  id : Nat
  lhs : String
  rhs : List Datum

structure Input where
  names : List String
  records : List Record
  augmented : String
  root : String
  rules : Lowering.Rules
  decode : Nat → Lowering.Owner
  recipes : List Names.Recipe
  declarations : List Nat

def readName : Datum → Option String
  | .text name => some name | _ => none

def readRecord : Datum → Option Record
  | .list [.integer id, .symbol lhs, .list rhs] => some ⟨id, lhs, rhs⟩
  | _ => none

/-- The producer supplies reader data, not encoded IDs. The existing origin
metadata is retained; all structural names and records come from raw data. -/
def read (value : Datum) (origins : ConstructionGrammar.Input) : Option Input :=
  match value with
  | .list [.list names, .list records, .symbol augmented, .symbol root, .boolean _] =>
    (names.mapM readName).bind fun names => (records.mapM readRecord).map fun records =>
      ⟨names, records, augmented, root, origins.rules, origins.decode, origins.recipes, origins.declarations⟩
  | _ => none

def terminals (input : Input) : List Terminal :=
  (input.records.flatMap Record.rhs).filterMap fun value =>
    (operand value).bind fun symbol => match symbol with | .terminal key => some key | _ => none

def catalogue (input : Input) : List Terminal :=
  (ProducerAdmission.collect (terminals input) [.eof]).reverse

def tagged (key : Terminal) (code : Nat) : ProducerAdmission.Terminal :=
  match key with
  | .eof => .eof | .token _ => .token code | .literal _ => .literal code
  | .layoutStart _ => .layoutStart code | .layoutNext _ => .layoutNext code

def encodeTerminal (input : Input) (key : Terminal) : Option ProducerAdmission.Terminal :=
  (lookup (catalogue input) key).map (tagged key)

def decodeTerminal (input : Input) : ProducerAdmission.Terminal → Option Terminal
  | .eof => some .eof
  | .token code => ((catalogue input)[code]?).bind fun key => match key with | .token name => some (.token name) | _ => none
  | .literal code => ((catalogue input)[code]?).bind fun key => match key with | .literal text => some (.literal text) | _ => none
  | .layoutStart code => ((catalogue input)[code]?).bind fun key => match key with | .layoutStart text => some (.layoutStart text) | _ => none
  | .layoutNext code => ((catalogue input)[code]?).bind fun key => match key with | .layoutNext text => some (.layoutNext text) | _ => none

theorem terminal_roundtrip (input : Input) (key : Terminal) (encoded : ProducerAdmission.Terminal)
    (success : encodeTerminal input key = some encoded) : decodeTerminal input encoded = some key := by
  unfold encodeTerminal at success
  cases found : lookup (catalogue input) key with
  | none => simp [found] at success
  | some code =>
    simp only [found, Option.map_some, Option.some.injEq] at success
    subst encoded
    have read := lookup_sound (catalogue input) key code found
    cases key <;> simp [tagged, decodeTerminal, read]

theorem terminal_injective (input : Input) (a b : Terminal) (encoded : ProducerAdmission.Terminal)
    (left : encodeTerminal input a = some encoded) (right : encodeTerminal input b = some encoded) : a = b :=
  Option.some.inj ((terminal_roundtrip input a encoded left).symm.trans (terminal_roundtrip input b encoded right))

def encodeSymbol (input : Input) : Symbol → Option ProducerAdmission.Symbol
  | .nonterminal name => (lookup input.names name).map .nonterminal
  | .terminal key => (encodeTerminal input key).map .terminal

def decodeSymbol (input : Input) : ProducerAdmission.Symbol → Option Symbol
  | .nonterminal code => (input.names[code]?).map .nonterminal
  | .terminal key => (decodeTerminal input key).map .terminal

theorem symbol_roundtrip (input : Input) (raw : Symbol) (encoded : ProducerAdmission.Symbol)
    (success : encodeSymbol input raw = some encoded) : decodeSymbol input encoded = some raw := by
  cases raw with
  | nonterminal name =>
    simp only [encodeSymbol] at success
    cases found : lookup input.names name with
    | none => simp [found] at success
    | some code =>
      simp only [found, Option.map_some, Option.some.injEq] at success
      subst encoded
      simp [decodeSymbol, lookup_sound input.names name code found]
  | terminal key =>
    simp only [encodeSymbol] at success
    cases found : encodeTerminal input key with
    | none => simp [found] at success
    | some value =>
      simp only [found, Option.map_some, Option.some.injEq] at success
      subst encoded
      simp [decodeSymbol, terminal_roundtrip input key value found]

theorem symbol_injective (input : Input) (a b : Symbol) (encoded : ProducerAdmission.Symbol)
    (left : encodeSymbol input a = some encoded) (right : encodeSymbol input b = some encoded) : a = b :=
  Option.some.inj ((symbol_roundtrip input a encoded left).symm.trans (symbol_roundtrip input b encoded right))

def normalizeOperand (input : Input) (value : Datum) : Option ProducerAdmission.Operand :=
  (operand value).bind fun raw => (encodeSymbol input raw).map fun symbol => ⟨symbol, []⟩

theorem operand_roundtrip (input : Input) (value : Datum) (encoded : ProducerAdmission.Operand)
    (success : normalizeOperand input value = some encoded) :
    ∃ raw, operand value = some raw ∧ decodeSymbol input encoded.base = some raw := by
  change (operand value).bind (fun raw => (encodeSymbol input raw).map
    (fun symbol => (⟨symbol, []⟩ : ProducerAdmission.Operand))) = some encoded at success
  obtain ⟨raw, read, encodedRead⟩ := Option.bind_eq_some_iff.mp success
  obtain ⟨symbol, code, equal⟩ := Option.map_eq_some_iff.mp encodedRead
  subst encoded
  exact ⟨raw, read, symbol_roundtrip input raw symbol code⟩

/-- One successful partial traversal accounts for every original position;
neither failed decodes nor duplicate values can be dropped by filterMap/zip. -/
theorem mapM_trace (f : α → Option β) (raw : List α) (encoded : List β)
    (success : raw.mapM f = some encoded) :
    raw.length = encoded.length ∧ ∀ (index : Nat) (value : α), raw[index]? = some value →
      ∃ result, encoded[index]? = some result ∧ f value = some result := by
  induction raw generalizing encoded with
  | nil =>
    have equal : encoded = [] := Option.some.inj success.symm
    subst encoded
    exact ⟨rfl, by intro index value read; simp at read⟩
  | cons first rest ih =>
    rw [List.mapM_cons] at success
    cases headRead : f first with
    | none => simp [headRead] at success
    | some head =>
      cases mapped : rest.mapM f with
      | none => simp [headRead, mapped] at success
      | some tail =>
        simp [headRead, mapped] at success
        subst encoded
        obtain ⟨length, positions⟩ := ih tail mapped
        refine ⟨congrArg Nat.succ length, ?_⟩
        intro index value read
        cases index with
        | zero =>
          have equal : first = value := Option.some.inj read
          subst value
          exact ⟨head, rfl, headRead⟩
        | succ index => exact positions index value read

def normalizeRow (input : Input) (record : Record) : Option ConstructionGrammar.Record :=
  (lookup input.names record.lhs).bind fun lhs =>
    (record.rhs.mapM (normalizeOperand input)).map fun rhs => ⟨record.id, ⟨lhs, rhs, .ordinary 0⟩⟩

def normalize (input : Input) : Option ConstructionGrammar.Input :=
  (lookup input.names input.augmented).bind fun augmented =>
    (lookup input.names input.root).bind fun root =>
      (input.records.mapM (normalizeRow input)).map fun records =>
        ⟨records, augmented, root, root, input.rules, input.decode, input.recipes, input.declarations⟩

theorem normalization_fields (input : Input) (normalized : ConstructionGrammar.Input)
    (success : normalize input = some normalized) :
    lookup input.names input.augmented = some normalized.augmented ∧
    lookup input.names input.root = some normalized.root ∧
    input.records.mapM (normalizeRow input) = some normalized.records := by
  change (lookup input.names input.augmented).bind (fun augmented =>
    (lookup input.names input.root).bind (fun root =>
      (input.records.mapM (normalizeRow input)).map (fun records =>
        (⟨records, augmented, root, root, input.rules, input.decode, input.recipes, input.declarations⟩ : ConstructionGrammar.Input)))) = some normalized at success
  obtain ⟨augmented, augRead, rest⟩ := Option.bind_eq_some_iff.mp success
  obtain ⟨root, rootRead, rest⟩ := Option.bind_eq_some_iff.mp rest
  obtain ⟨records, rowsRead, equal⟩ := Option.map_eq_some_iff.mp rest
  subst normalized
  exact ⟨augRead, rootRead, rowsRead⟩

theorem row_roundtrip (input : Input) (raw : Record) (normalized : ConstructionGrammar.Record)
    (success : normalizeRow input raw = some normalized) :
    normalized.id = raw.id ∧ input.names[normalized.value.lhs]? = some raw.lhs ∧
    raw.rhs.length = normalized.value.rhs.length ∧
    ∀ (index : Nat) (value : Datum), raw.rhs[index]? = some value →
      ∃ (result : ProducerAdmission.Operand) (symbol : Symbol), normalized.value.rhs[index]? = some result ∧
        operand value = some symbol ∧ decodeSymbol input result.base = some symbol := by
  change (lookup input.names raw.lhs).bind (fun lhs =>
    (raw.rhs.mapM (normalizeOperand input)).map (fun rhs =>
      (⟨raw.id, ⟨lhs, rhs, .ordinary 0⟩⟩ : ConstructionGrammar.Record))) = some normalized at success
  obtain ⟨lhs, lhsRead, rest⟩ := Option.bind_eq_some_iff.mp success
  obtain ⟨rhs, rhsRead, equal⟩ := Option.map_eq_some_iff.mp rest
  subst normalized
  obtain ⟨length, positions⟩ := mapM_trace (normalizeOperand input) raw.rhs rhs rhsRead
  refine ⟨rfl, lookup_sound input.names raw.lhs lhs lhsRead, length, ?_⟩
  intro index value read
  obtain ⟨result, position, mapped⟩ := positions index value read
  obtain ⟨symbol, rawRead, decoded⟩ := operand_roundtrip input value result mapped
  exact ⟨result, symbol, position, rawRead, decoded⟩

theorem table_positions (input : Input) (normalized : ConstructionGrammar.Input)
    (success : normalize input = some normalized) :
    input.records.length = normalized.records.length ∧
      ∀ (index : Nat) (value : Record), input.records[index]? = some value →
        ∃ result, normalized.records[index]? = some result ∧ normalizeRow input value = some result :=
  mapM_trace (normalizeRow input) input.records normalized.records (normalization_fields input normalized success).2.2

/-- Raw name/text failure rejects before either grammar admission or the
retained LeanPoo factory. No normalized CFG is supplied alongside the input. -/
structure Output (sources : ConstructionConsumer.Sources) (input : Input) (traversal : CachedConstruction.Traversal) where
  normalized : ConstructionGrammar.Input
  encoded : normalize input = some normalized
  result : ConstructionGrammar.Output normalized traversal
  built : ConstructionGrammar.construct sources normalized traversal = some result

def construct (sources : ConstructionConsumer.Sources) (input : Input) (traversal : CachedConstruction.Traversal) :
    Option (Output sources input traversal) :=
  match encoded : normalize input with
  | none => none
  | some normalized =>
    match built : ConstructionGrammar.construct sources normalized traversal with
    | none => none
    | some result => some ⟨normalized, encoded, result, built⟩

theorem rejected (sources : ConstructionConsumer.Sources) (input : Input) (traversal : CachedConstruction.Traversal)
    (failure : normalize input = none) : construct sources input traversal = none := by
  unfold construct
  split
  · rfl
  · next normalized read => rw [failure] at read; cases read

theorem complete (sources : ConstructionConsumer.Sources) (input : Input) (traversal : CachedConstruction.Traversal)
    (output : Output sources input traversal) :
    ConstructionFIFO.executeSource sources.builder sources.growth sources.append sources.template
      (ConstructionGrammar.grammar output.normalized) (ConstructionGrammar.nonempty output.normalized output.result.receipt) traversal =
      some ((output.result.product.construction.1.machine, output.result.product.construction.2), output.result.product.actions) ∧
    ConstructionReadiness.Complete (ConstructionGrammar.grammar output.normalized) traversal output.result.product.layout ∧
    ConstructionReadiness.Visit (ConstructionGrammar.grammar output.normalized) traversal output.result.product.layout
      output.result.product.construction.1.machine output.result.product.construction.2 :=
  ConstructionGrammar.complete sources output.normalized traversal output.result output.built

/-- Exact physical positions in the returned factory's CFG correspond to the
original raw records, with original LHS names recoverable from their codes. -/
theorem factory_positions (sources : ConstructionConsumer.Sources) (input : Input)
    (traversal : CachedConstruction.Traversal) (output : Output sources input traversal)
    (index : Nat) (raw : Record) (read : input.records[index]? = some raw) :
    ∃ record, output.normalized.records[index]? = some record ∧
      (ConstructionGrammar.grammar output.normalized)[index]? = some (ConstructionGrammar.row output.normalized record) ∧
      raw.id = index ∧ input.names[record.value.lhs]? = some raw.lhs := by
  obtain ⟨record, position, normalized⟩ := (table_positions input output.normalized output.encoded).2 index raw read
  have row := row_roundtrip input raw record normalized
  refine ⟨record, position, ?_, ?_, row.2.1⟩
  · rw [ConstructionGrammar.project_index, position]
    rfl
  · rw [← row.1]
    exact ConstructionGrammar.id_index output.normalized output.result.receipt index record position

end ParserEngine.ConstructionEncoding
