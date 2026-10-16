import ParserEngine.RawRequest
import ParserEngine.Construction

/-! Deterministic runtime table and selector decoding. Terminal-name encoding,
source vector extraction and cell-origin admission are explicit boundaries. -/
namespace ParserEngine.TableBridge
open Grammar

inductive RuntimeAction where
  | shift (state : Nat)
  | reduce (productionId : Nat)
  | accept
  deriving DecidableEq, Repr

def decodeAction (productions : CFG) : RuntimeAction → Option Grammar.Action
  | .shift state => some (.shift state)
  | .reduce id => (productions[id]?).map Grammar.Action.reduce
  | .accept => some .accept

/-- Lookup is performed before action decoding. An invalid first duplicate
must not fall through to a later action. Native validation rejects bad IDs. -/
def lookup (row : List (Option Nat × RuntimeAction)) (key : Option Nat) : Option RuntimeAction :=
  match row with
  | [] => none
  | entry :: rest => if entry.1 = key then some entry.2 else lookup rest key

theorem lookup_origin (row : List (Option Nat × RuntimeAction)) (key : Option Nat)
    (action : RuntimeAction) (found : lookup row key = some action) :
    (key, action) ∈ row := by
  induction row with
  | nil => simp [lookup] at found
  | cons entry rest ih =>
    rcases entry with ⟨entryKey, entryAction⟩
    simp only [lookup] at found
    split at found
    · simp_all
    · exact List.mem_cons_of_mem _ (ih found)

theorem reduction_id_exact (productions : CFG) (id : Nat) (production : Production)
    (decoded : decodeAction productions (.reduce id) = some (.reduce production)) :
    productions[id]? = some production := by
  simp only [decodeAction] at decoded
  cases row : productions[id]? with
  | none => simp [row] at decoded
  | some value => simp_all

theorem invalid_first_cell_blocks_fallback (productions : CFG) (key : Option Nat)
    (encoded : RuntimeAction) (rest : List (Option Nat × RuntimeAction))
    (invalid : decodeAction productions encoded = none) :
    (lookup ((key, encoded) :: rest) key).bind (decodeAction productions) = none := by
  simp [lookup, invalid]

structure RuntimeTable where
  productions : CFG
  rows : Nat → List (Option Nat × RuntimeAction)
  /-- Compiler-origin evidence, not a new field in the native runtime record. -/
  items : Nat → List Item
  goto : Nat → Symbol → Option Nat

def decodeTable (runtime : RuntimeTable) : Table :=
  ⟨runtime.items, runtime.goto,
    fun state key => (lookup (runtime.rows state) key).bind (decodeAction runtime.productions)⟩

/-- Each native cell needs its LR-item origin; schema/range validation alone
is insufficient. Existing construction/publication proofs supply this layer. -/
def CellOrigins (runtime : RuntimeTable) (grammar : CFG) (start : Nat)
    (augmented : Production) : Prop :=
  ∀ state key encoded, (key, encoded) ∈ runtime.rows state → ∀ action,
    decodeAction runtime.productions encoded = some action →
    CellSupported grammar start augmented (decodeTable runtime) state key action

theorem decoded_table_supported (runtime : RuntimeTable) (grammar : CFG) (start : Nat)
    (augmented : Production) (origins : CellOrigins runtime grammar start augmented) :
    TableSupported grammar start augmented (decodeTable runtime) := by
  intro state key action published
  obtain ⟨encoded, found, decoded⟩ := Option.bind_eq_some_iff.mp published
  exact origins state key encoded (lookup_origin _ _ _ found) action decoded

/-- Published opcodes retain the physical item/production identity before
semantic rule projection, including the reserved augmentation identity. -/
def runtimeAction : Construction.PhysicalEvent → RuntimeAction
  | .shift _ _ target => .shift target
  | .reduce item => .reduce item.production
  | .accept _ => .accept

theorem physical_action_decodes (productions : CFG) (event : Construction.PhysicalEvent)
    (decoded : Construction.RawEvent)
    (physical : Construction.decodePhysicalEvent productions event = some decoded) :
    decodeAction productions (runtimeAction event) = some (Construction.eventAction decoded) := by
  cases event with
  | shift item terminal target =>
    cases row : productions[item.production]? <;>
      simp_all [Construction.decodePhysicalEvent, Construction.decodePhysical,
        Construction.eventAction, decodeAction, runtimeAction] <;> subst decoded <;> rfl
  | reduce item =>
    by_cases zero : item.production = 0
    · simp [Construction.decodePhysicalEvent, zero] at physical
    · cases row : productions[item.production]? <;>
        simp_all [Construction.decodePhysicalEvent, Construction.decodePhysical,
          Construction.eventAction, decodeAction, runtimeAction] <;> subst decoded <;> rfl
  | accept item =>
    by_cases zero : item.production = 0
    · cases row : productions[item.production]? <;>
        simp_all [Construction.decodePhysicalEvent, Construction.decodePhysical,
          Construction.eventAction, decodeAction, runtimeAction] <;> subst decoded <;> rfl
    · simp [Construction.decodePhysicalEvent, zero] at physical

/-- Derive cell support from the existing physical publication admission,
rather than asking the runtime table adapter to assume TableSupported. -/
theorem origins_from_publication (runtime : RuntimeTable) (grammar : CFG) (start : Nat)
    (augmented : Production) (productionView : runtime.productions = augmented :: grammar)
    (published : ∀ state key encoded, (key, encoded) ∈ runtime.rows state →
      ∃ event, runtimeAction event = encoded ∧
        Construction.admitPhysicalEvent grammar start augmented (decodeTable runtime)
          state key event = true) :
    CellOrigins runtime grammar start augmented := by
  intro state key encoded member action decoded
  obtain ⟨event, opcode, admitted⟩ := published state key encoded member
  obtain ⟨physical, view, supported⟩ := Construction.admitted_physical_event_supported admitted
  have same := physical_action_decodes (augmented :: grammar) event physical view
  rw [opcode, ← productionView] at same
  have equal : Construction.eventAction physical = action := Option.some.inj (same.symm.trans decoded)
  simpa [equal] using supported

abbrev Entry := Option Nat × RuntimeAction

def decodeEntry (productions : CFG) (entry : Entry) : Option (Option Nat × Grammar.Action) :=
  (decodeAction productions entry.2).map fun action => (entry.1, action)

/-- Original indexed entries, not merely equal-looking copied action values.
Case folding and kind matching must preserve the same published cell key. -/
def IndexedMeaning (runtime : RuntimeTable) (realizes : Nat → Lookahead.RawToken → Prop)
    (state : Nat) (input : Option Lookahead.RawToken) (entry : Entry) : Prop :=
  lookup (runtime.rows state) entry.1 = some entry.2 ∧
    ∀ raw terminal, input = some raw → entry.1 = some terminal → realizes terminal raw

def select (runtime : RuntimeTable) (rows : Nat → Lookahead.SelectorRow Entry)
    (state : Nat) (input : Option Lookahead.RawToken) : Option (Option Nat × Grammar.Action) :=
  (Lookahead.selectRaw (rows state) input).bind (decodeEntry runtime.productions)

theorem selected_meaning (runtime : RuntimeTable)
    (realizes : Nat → Lookahead.RawToken → Prop) (rows : Nat → Lookahead.SelectorRow Entry)
    (indexed : ∀ state, Lookahead.RowSupported (rows state) (IndexedMeaning runtime realizes state))
    (state : Nat) (input : Option Lookahead.RawToken) (key : Option Nat) (action : Grammar.Action)
    (chosen : select runtime rows state input = some (key, action)) :
    RawRequest.RowMeaning (decodeTable runtime) realizes state input (key, action) := by
  obtain ⟨entry, found, decoded⟩ := Option.bind_eq_some_iff.mp chosen
  have meaning := Lookahead.selected_raw_supported (rows state)
    (IndexedMeaning runtime realizes state) (indexed state) input entry found
  unfold decodeEntry at decoded
  obtain ⟨value, decodedAction, pair⟩ := Option.map_eq_some_iff.mp decoded
  have keys : entry.1 = key := congrArg Prod.fst pair
  have actions : value = action := congrArg Prod.snd pair
  subst value
  constructor
  · simp [decodeTable, ← keys, meaning.1, decodedAction]
  · exact fun raw terminal rawEq keyEq => meaning.2 raw terminal rawEq (keys.trans keyEq)

/-- Construct the certified engine from decoded runtime tables and indexed
rows. Graph construction/coverage and every producer certificate remain named;
none are inferred from passing a finite grammar fixture. -/
def engine (runtime : RuntimeTable) (grammar : CFG) (start : Nat) (augmented : Production)
    (graph : Viable.Graph) (realizes : Nat → Lookahead.RawToken → Prop)
    (rows : Nat → Lookahead.SelectorRow Entry)
    (constructed : Viable.Constructed grammar augmented graph)
    (foreign : augmented ∉ grammar) (shape : augmented.rhs = [.nonterminal start])
    (origins : CellOrigins runtime grammar start augmented)
    (covered : ∀ state item, item ∈ runtime.items state →
      (⟨item.production, item.dot⟩ : Viable.Core) ∈ graph.items state)
    (sameGoto : runtime.goto = graph.goto)
    (indexed : ∀ state, Lookahead.RowSupported (rows state) (IndexedMeaning runtime realizes state)) :
    RawRequest.Engine Lookahead.RawToken where
  grammar := grammar
  start := start
  augmented := augmented
  graph := graph
  table := decodeTable runtime
  realizesToken := realizes
  select := select runtime rows
  constructed := constructed
  foreign := foreign
  shape := shape
  supported := decoded_table_supported runtime grammar start augmented origins
  covered := covered
  sameGoto := sameGoto
  selected := fun state input key action chosen =>
    (selected_meaning runtime realizes rows indexed state input key action chosen).1
  matched := fun state input terminal action chosen =>
    (selected_meaning runtime realizes rows indexed state (some input) (some terminal) action chosen).2
      input terminal rfl rfl

end ParserEngine.TableBridge
