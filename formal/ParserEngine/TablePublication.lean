import ParserEngine.TableBridge
import ParserEngine.ItemLayout

/-! Final LR table publication. Rows are computed from bounded producer events,
not supplied with a final CellOrigins certificate. Native row/terminal extraction,
conflict-policy equivalence, primitive storage and producer admission are named
boundaries. This does not interpret arbitrary Scheme or prove host liveness. -/
namespace ParserEngine.TablePublication
open Grammar Construction
abbrev Events := List (Nat × Choice × RawEvent)

/-- Decoded view of every alternative, including suppressed layout reductions.
The native spec keeps forks/guards; selection chooses one retained alternative. -/
def row (width : Nat) (keyOf : Nat → Option Nat) (enabled allowed : Bool)
    (events : Events) : List (Option Nat × Grammar.Action) :=
  (List.range width).flatMap fun key =>
    match (vectorEventRow width events).toList[key]? with
    | some (some result) =>
      (selectLayout allowed (publishLayout enabled result (rawReductions (cellEvents key events)))).map
        fun action => (keyOf key, action)
    | _ => []

abbrev StoredRow := List (Option Nat × LayoutCell)

/-- Freeze resolved cells and layout fallbacks before scratch storage is reused. -/
def cellsRow (width : Nat) (keyOf : Nat → Option Nat) (enabled : Bool) (events : Events) : StoredRow :=
  (List.range width).flatMap fun key =>
    match (vectorEventRow width events).toList[key]? with
    | some (some result) => [(keyOf key, publishLayout enabled result (rawReductions (cellEvents key events)))]
    | _ => []

def project (entries : StoredRow) (allowed : Bool) : List (Option Nat × Grammar.Action) :=
  entries.flatMap fun entry => (selectLayout allowed entry.2).map fun action => (entry.1, action)

theorem project_cells (width : Nat) (keyOf : Nat → Option Nat) (enabled allowed : Bool) (events : Events) :
    project (cellsRow width keyOf enabled events) allowed = row width keyOf enabled allowed events := by
  simp only [project, cellsRow, row, List.flatMap_assoc]
  congr 1
  funext key
  cases (vectorEventRow width events).toList[key]? with
  | none => rfl
  | some slot => cases slot <;> simp

def select (entries : List (Option Nat × Grammar.Action)) (key : Option Nat) (alternative : Nat) :
    Option Grammar.Action :=
  (entries.filterMap fun entry => if entry.1 = key then some entry.2 else none)[alternative]?

theorem selected_entry {entries key alternative action}
    (chosen : select entries key alternative = some action) : (key, action) ∈ entries := by
  have member := List.mem_of_getElem? chosen
  obtain ⟨entry, present, equation⟩ := List.mem_filterMap.mp member
  split at equation
  next same => simpa [← Option.some.inj equation, ← same] using present
  next => contradiction

/-- Arbitrary state counts, terminal counts, conflict choices and layout policy.
No history or support premise is required of the completed row. -/
theorem row_origin {width keyOf enabled allowed events}
    {origin : Option Nat → Grammar.Action → Prop}
    (bounds : ∀ entry ∈ events, entry.1 < width)
    (sources : ∀ entry ∈ events, origin (keyOf entry.1) (eventAction entry.2.2)) :
    ∀ entry ∈ row width keyOf enabled allowed events, origin entry.1 entry.2 := by
  intro entry member
  obtain ⟨key, _, present⟩ := List.mem_flatMap.mp member
  cases stored : (vectorEventRow width events).toList[key]? with
  | none => simp [stored] at present
  | some slot =>
    cases slot with
    | none => simp [stored] at present
    | some result =>
      simp only [stored] at present
      have selected : ∃ action,
          action ∈ selectLayout allowed (publishLayout enabled result (rawReductions (cellEvents key events))) ∧
          (keyOf key, action) = entry := by simpa [stored] using List.mem_map.mp present
      obtain ⟨action, selected, same⟩ := selected
      subst entry
      exact (vector_layout_publication (origin := fun key action => origin (keyOf key) action) bounds sources enabled allowed).1 key result stored action selected

structure Input where
  width : Nat
  keyOf : Nat → Option Nat
  enabled : Bool
  allowed : Nat → Option Nat → Bool
  alternative : Nat → Option Nat → Nat
  plans : List Events
  items : Nat → List Item
  goto : Nat → Symbol → Option Nat

def skeleton (input : Input) : Table := ⟨input.items, input.goto, fun _ _ => none⟩

def table (input : Input) : Table :=
  ⟨input.items, input.goto, fun state key =>
    (input.plans[state]?).bind fun events =>
      select (row input.width input.keyOf input.enabled (input.allowed state key) events)
        key (input.alternative state key)⟩

theorem outside_empty (input : Input) (state : Nat) (outside : input.plans.length ≤ state) (key : Option Nat) :
    (table input).action state key = none := by
  simp [table, List.getElem?_eq_none outside]

theorem cell_transport (input : Input) (g : CFG) (start : Nat) (augmented : Production)
    (state : Nat) (key : Option Nat) (action : Grammar.Action) :
    CellSupported g start augmented (skeleton input) state key action ↔
      CellSupported g start augmented (table input) state key action := by
  cases action <;> rfl

/-- The final table support proof consumes producer evidence, not a desired
property of the final table, its actions, or its publication trace. -/
theorem complete_table (input : Input) (g : CFG) (start : Nat) (augmented : Production)
    (bounds : ∀ (state : Nat) (events : Events), input.plans[state]? = some events →
      ∀ entry : Nat × Choice × RawEvent, entry ∈ events → entry.1 < input.width)
    (admission : ∀ (state : Nat) (events : Events), input.plans[state]? = some events → ∀ entry ∈ events,
      admitEvent g start augmented (skeleton input) state (input.keyOf entry.1) entry.2.2 = true) :
    TableSupported g start augmented (table input) := by
  intro state key action published
  obtain ⟨events, read, chosen⟩ := Option.bind_eq_some_iff.mp published
  have member := selected_entry chosen
  have supported := row_origin (bounds state events read)
    (fun entry present => admitted_event_supported (admission state events read entry present))
    (key, action) member
  exact (cell_transport input g start augmented state key action).mp supported

/-- Ascending row publication writes each state once and derives the counter.
The old store is arbitrary; no previously published cell is read or reused. -/
def writeRows {α : Type} : Nat → List α → (Nat → Option α) → (Nat → Option α) × Nat
  | _, [], store => (store, 0)
  | state, entry :: rest, store =>
    let next := writeRows (state + 1) rest (fun key => if key = state then some entry else store key)
    (next.1, next.2 + 1)

theorem publication_count (entries : List α) (start : Nat) (store : Nat → Option α) :
    (writeRows start entries store).2 = entries.length := by
  induction entries generalizing start store with
  | nil => rfl
  | cons entry rest ih => simp [writeRows, ih]

theorem untouched_before (entries : List α) (start key : Nat) (store : Nat → Option α) (before : key < start) :
    (writeRows start entries store).1 key = store key := by
  induction entries generalizing start store with
  | nil => rfl
  | cons entry rest ih =>
    simp only [writeRows]
    rw [ih (start + 1) _ (by omega)]
    simp [show key ≠ start by omega]

theorem published_slot (entries : List α) (start index : Nat) (store : Nat → Option α)
    (inside : index < entries.length) :
    (writeRows start entries store).1 (start + index) = entries[index]? := by
  induction entries generalizing start index store with
  | nil => simp at inside
  | cons entry rest ih =>
    cases index with
    | zero =>
      simp only [writeRows, Nat.add_zero]
      rw [untouched_before rest (start + 1) start _ (by omega)]
      simp
    | succ index =>
      have valid : index < rest.length := by simpa using inside
      simpa [writeRows, Nat.add_assoc, Nat.add_comm, Nat.add_left_comm] using
        ih (start + 1) index (fun key => if key = start then some entry else store key) valid

theorem untouched_after (entries : List α) (start key : Nat) (store : Nat → Option α)
    (after : start + entries.length ≤ key) :
    (writeRows start entries store).1 key = store key := by
  induction entries generalizing start store with
  | nil => rfl
  | cons entry rest ih =>
    simp only [writeRows]
    rw [ih (start + 1) _ (by simpa [Nat.add_assoc, Nat.add_comm, Nat.add_left_comm] using after)]
    simp [show key ≠ start by simp only [List.length_cons] at after; omega]

theorem stored_values (entries : List α) (state : Nat) :
    (writeRows 0 entries (fun _ => none)).1 state = entries[state]? := by
  by_cases inside : state < entries.length
  · simpa using published_slot entries 0 state (fun _ => none) inside
  · have outside : entries.length ≤ state := by omega
    rw [untouched_after entries 0 state _ (by simpa using outside)]
    simp [List.getElem?_eq_none outside]

def frozenRows (input : Input) : List StoredRow :=
  input.plans.map (cellsRow input.width input.keyOf input.enabled)

def storedRows (input : Input) : Nat → Option StoredRow :=
  (writeRows 0 (frozenRows input) (fun _ => none)).1

theorem stored_rows (input : Input) (state : Nat) :
    storedRows input state = (input.plans[state]?).map (cellsRow input.width input.keyOf input.enabled) := by
  simp [storedRows, stored_values, frozenRows, List.getElem?_map]

def physicalTable (input : Input) : Table :=
  ⟨input.items, input.goto, fun state key =>
    (storedRows input state).bind fun entries =>
      select (project entries (input.allowed state key)) key (input.alternative state key)⟩

theorem physical_table_exact (input : Input) : physicalTable input = table input := by
  simp only [physicalTable, stored_rows, table, Option.bind_map, Function.comp_def, project_cells]

/-- Certification composes through the stored state rows to the existing raw
request engine. State construction, coverage and lexical selector certificates
remain explicit; final action support is derived, not another input. -/
def engine (input : Input) (g : CFG) (start : Nat) (augmented : Production)
    (graph : Viable.Graph) (realizes : Nat → Lookahead.RawToken → Prop)
    (rows : Nat → Lookahead.SelectorRow (Option Nat × Grammar.Action))
    (bounds : ∀ (state : Nat) (events : Events), input.plans[state]? = some events →
      ∀ entry ∈ events, entry.1 < input.width)
    (admission : ∀ (state : Nat) (events : Events), input.plans[state]? = some events → ∀ entry ∈ events,
      admitEvent g start augmented (skeleton input) state (input.keyOf entry.1) entry.2.2 = true)
    (constructed : Viable.Constructed g augmented graph)
    (foreign : augmented ∉ g) (shape : augmented.rhs = [.nonterminal start])
    (covered : ∀ state item, item ∈ input.items state →
      (⟨item.production, item.dot⟩ : Viable.Core) ∈ graph.items state)
    (sameGoto : input.goto = graph.goto)
    (indexed : ∀ state, Lookahead.RowSupported (rows state)
      (RawRequest.RowMeaning (physicalTable input) realizes state)) : RawRequest.Engine Lookahead.RawToken where
  grammar := g
  start := start
  augmented := augmented
  graph := graph
  table := physicalTable input
  realizesToken := realizes
  select := fun state token => Lookahead.selectRaw (rows state) token
  constructed := constructed
  foreign := foreign
  shape := shape
  supported := by rw [physical_table_exact]; exact complete_table input g start augmented bounds admission
  covered := covered
  sameGoto := sameGoto
  selected := fun state token key action chosen =>
    (RawRequest.builtin_action_and_terminal (physicalTable input) realizes rows indexed state token key action chosen).1
  matched := fun state token terminal action chosen =>
    (RawRequest.builtin_action_and_terminal (physicalTable input) realizes rows indexed state (some token) (some terminal) action chosen).2
      token terminal rfl rfl

#print axioms engine
#print axioms stored_rows
#print axioms physical_table_exact
#print axioms row_origin
#print axioms complete_table
#print axioms published_slot
end ParserEngine.TablePublication

namespace ParserEngine.TablePublication.Source
open WalkSource
/-- Recognize the complete publication suffix, including the counter guard,
validation before construction, every spec field and each algorithm branch.
Earlier lowering/FIRST/state builders remain independently owned producers. -/
def field (key : String) (value : Datum) : Datum := app "cons" [quoted key, value]
def trace (phase : String) (count : Datum) : Datum := app "trace-lr-phase" [quoted phase, count, s "started"]
def algorithm : Datum := app "case" [s "construction",
  l [l [s "canonical-lr1"], quoted "canonical-lr1-reference-v1"],
  l [l [s "partitioned-lr1"], quoted "conflict-partitioned-lr1-v1"],
  l [l [s "follow-partition-lr1"], quoted "follow-partition-lr1-v1"],
  l [s "else", quoted "lalr1-lr0-fixed-point-v1"]]
def spec : Datum := app "list" [
  field "schema" (.text "gerbil-parser.lr-spec.v1"),
  field "case-insensitive?" (s "case-insensitive?"),
  field "productions" (s "productions"), field "nullable" (s "nullable"), field "first" (s "first"),
  field "algorithm" algorithm, field "state-count" (s "state-count"),
  field "lr0-state-visit-count" (app "and" [app "eq?" [s "construction", quoted "lalr"], s "lr0-state-visit-count"]),
  field "lookahead-item-visit-count" (app "and" [app "eq?" [s "construction", quoted "lalr"], s "lookahead-item-visit-count"]),
  field "actions" (s "actions"), field "gotos" (s "gotos")]
def branch (first : String) : Datum :=
  app "cons" [field first (s "lr0-state-visit-count"),
    app "cons" [field "output-item-count" (s "lookahead-item-visit-count"), s "spec"]]
def publish : Datum := app "let" [l [s "spec", spec],
  app "case" [s "construction", l [l [s "lalr"], s "spec"],
    l [l [s "follow-partition-lr1"], branch "follow-block-count"],
    l [s "else", branch "canonical-state-count"]]]
def suffix : Datum := app "let-values" [
  l [l [l [s "actions", s "action-state-publication-count"],
    app "build-actions" [s "states", s "state-count", s "lookaheads", s "lookahead-offsets", s "transitions",
      s "table", s "terminal-values", s "layout", s "core-symbols", s "conflict-policy",
      app "lambda" [l [s "condition", s "state", s "items", s "terminal", s "left", s "right"],
        app "reject-lr-conflict" [s "condition", s "rules", s "root", s "conflict-policy", s "table", s "layout", s "core-symbols",
          s "state", s "items", s "terminal", s "left", s "right"]]]]],
  app "unless" [app "=" [s "action-state-publication-count", s "state-count"],
    app "error" [.text "LR action rows were not published exactly once per state", s "state-count", s "action-state-publication-count"]],
  trace "actions" (app "vector-length" [s "actions"]),
  app "let" [l [s "gotos", app "build-gotos" [s "transitions", s "state-count"]],
    app "validate-lr-tables" [s "productions", s "actions", s "gotos"],
    trace "gotos" (app "vector-length" [s "gotos"]), publish]]

def admitted (source : Datum) : Bool :=
  decide (ItemLayout.Source.atPath [3, 3, 6, 3, 3] source = some suffix)

/-- Admission assigns this recognized tail its restricted publication meaning.
It is not a general interpreter or a proof of the opaque producer calls. -/
def execute (source : Datum) (input : Input) (reported : Nat) (validated : Bool) : Option Grammar.Table :=
  if admitted source && validated && reported == input.plans.length then some (table input) else none

theorem checked_table (source : Datum) (input : Input) (checked : admitted source = true) :
    execute source input (writeRows 0 (frozenRows input) (fun _ => none)).2 true = some (table input) := by
  simp [execute, checked, publication_count, frozenRows]
theorem validation_blocks (source : Datum) (input : Input) (reported : Nat) : execute source input reported false = none := by
  simp [execute]
theorem count_mismatch_blocks (source : Datum) (input : Input) (reported : Nat) (validated : Bool)
    (mismatch : reported ≠ input.plans.length) : execute source input reported validated = none := by
  simp [execute, mismatch]
end ParserEngine.TablePublication.Source

namespace ParserEngine.TablePublicationWitness
open WalkSource TablePublication

theorem publication_source_checked : Source.admitted publicationSource = true := by decide +kernel
example : Source.admitted (replace (s "validate-lr-tables") (s "void") publicationSource) = false := by decide +kernel
example : Source.admitted (replace (s "action-state-publication-count") (s "state-count") publicationSource) = false := by decide +kernel
example : Source.admitted (replace (Source.field "actions" (s "actions")) (Source.field "actions" (s "gotos")) publicationSource) = false := by decide +kernel
example : Source.admitted (replace (s "nullable") (s "first") publicationSource) = false := by decide +kernel

/-- Whole-table action safety from checked publication plus arbitrary producer
plans. No finite grammar enumeration or final-table support premise occurs. -/
theorem complete_publication (input : Input) (g : Grammar.CFG) (start : Nat) (augmented : Grammar.Production)
    (bounds : ∀ (state : Nat) (events : Events), input.plans[state]? = some events →
      ∀ entry ∈ events, entry.1 < input.width)
    (admission : ∀ (state : Nat) (events : Events), input.plans[state]? = some events → ∀ entry ∈ events,
      Construction.admitEvent g start augmented (skeleton input) state (input.keyOf entry.1) entry.2.2 = true) :
    Source.execute publicationSource input (writeRows 0 (frozenRows input) (fun _ => none)).2 true = some (table input) ∧
      (writeRows 0 (frozenRows input) (fun _ => none)).2 = input.plans.length ∧
      (∀ state, storedRows input state =
        (input.plans[state]?).map (cellsRow input.width input.keyOf input.enabled)) ∧
      physicalTable input = table input ∧
      Grammar.TableSupported g start augmented (physicalTable input) := by
  refine ⟨Source.checked_table _ _ publication_source_checked,
    (by simp [publication_count, frozenRows]), stored_rows input, physical_table_exact input, ?_⟩
  rw [physical_table_exact]
  exact complete_table input g start augmented bounds admission
#print axioms publication_source_checked
#print axioms complete_publication
end ParserEngine.TablePublicationWitness
