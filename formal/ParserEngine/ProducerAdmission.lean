import ParserEngine.CompilerExecution

/-! Parser producer storage and declaration-derived validator inputs.
Source admission is restricted; Scheme/compiler/host primitives remain external. -/
namespace ParserEngine.ProducerAdmission

inductive Terminal where
  | eof | token (name : Nat) | literal (text : Nat)
  | layoutStart (text : Nat) | layoutNext (text : Nat)
  deriving DecidableEq, Repr
inductive Symbol where
  | terminal (key : Terminal) | nonterminal (name : Nat)
  deriving DecidableEq, Repr
structure Operand where
  base : Symbol
  annotations : List Nat
  deriving DecidableEq, Repr
inductive Action where
  | ordinary (identity : Nat) | layoutEnd (arguments : List Nat)
  deriving DecidableEq, Repr
structure Production where
  lhs : Nat
  rhs : List Operand
  action : Action
  deriving DecidableEq, Repr

def terminalOf : Operand → Option Terminal
  | ⟨.terminal key, _⟩ => some key
  | _ => none

def terminals (productions : List Production) : List Terminal :=
  (productions.flatMap fun production => production.rhs).filterMap terminalOf

/-- Source scans left-to-right, conses only unseen bases, then reverses once.
The list is the logical view of the native equal?-keyed seen table. -/
def collect [DecidableEq Key] : List Key → List Key → List Key
  | [], found => found
  | key :: rest, found => collect rest (if key ∈ found then found else key :: found)

theorem collect_members [DecidableEq Key] (values found : List Key) (key : Key) :
    key ∈ collect values found ↔ key ∈ found ∨ key ∈ values := by
  induction values generalizing found with
  | nil => simp [collect]
  | cons head rest ih =>
    rw [collect, ih]
    by_cases present : head ∈ found <;> simp_all [or_assoc, or_left_comm, or_comm]

theorem collect_unique [DecidableEq Key] (values found : List Key) (unique : found.Nodup) :
    (collect values found).Nodup := by
  induction values generalizing found with
  | nil => exact unique
  | cons head rest ih =>
    apply ih
    by_cases present : head ∈ found <;> simp [present, unique]

def catalogue (productions : List Production) : List Terminal :=
  (collect (terminals productions) [.eof]).reverse

theorem catalogue_members (productions : List Production) (key : Terminal) :
    key ∈ catalogue productions ↔ key = .eof ∨ key ∈ terminals productions := by
  simp [catalogue, collect_members]

theorem catalogue_unique (productions : List Production) : (catalogue productions).Nodup := by
  change (collect (terminals productions) [Terminal.eof]).reverse.Pairwise (fun a b => a ≠ b)
  rw [List.pairwise_reverse]
  exact (collect_unique (terminals productions) [Terminal.eof] (by simp)).imp (fun h => Ne.symm h)

/-- Exact declaration-derived capability; action tags and RHS base tags both
contribute, while operand annotations cannot mint layout capability. -/
def layoutKey : Terminal → Bool
  | .layoutStart _ | .layoutNext _ => true
  | _ => false

def layoutOperand (operand : Operand) : Bool := match operand.base with
  | .terminal key => layoutKey key
  | _ => false

def layoutAction : Action → Bool
  | .layoutEnd _ => true
  | _ => false

def layout (productions : List Production) : Bool :=
  productions.any fun production => layoutAction production.action || production.rhs.any layoutOperand

theorem layout_exact (productions : List Production) : layout productions = true ↔
    (∃ production ∈ productions, layoutAction production.action = true) ∨
      ∃ production ∈ productions, ∃ operand ∈ production.rhs, layoutOperand operand = true := by
  simp only [layout, List.any_eq_true, Bool.or_eq_true]
  constructor
  · rintro ⟨production, member, action | operand⟩
    · exact Or.inl ⟨production, member, action⟩
    · obtain ⟨value, present, flag⟩ := operand
      exact Or.inr ⟨production, member, value, present, flag⟩
  · rintro (⟨production, member, action⟩ | ⟨production, member, operand, present, flag⟩)
    · exact ⟨production, member, Or.inl action⟩
    · exact ⟨production, member, Or.inr ⟨operand, present, flag⟩⟩

def actionKey (productions : List Production) (key : Terminal) : Bool :=
  decide (key ∈ catalogue productions) && (layout productions || !layoutKey key)
def gotoKey (productions : List Production) (name : Nat) : Bool :=
  productions.any fun production => decide (production.lhs = name) ||
    production.rhs.any (fun operand => decide (operand.base = .nonterminal name))

theorem goto_key_exact (productions : List Production) (name : Nat) : gotoKey productions name = true ↔
    ∃ production ∈ productions, production.lhs = name ∨
      ∃ operand ∈ production.rhs, operand.base = .nonterminal name := by simp [gotoKey]

theorem action_key_exact (productions : List Production) (key : Terminal) : actionKey productions key = true ↔
    (key = .eof ∨ key ∈ terminals productions) ∧ (layout productions = true ∨ layoutKey key = false) := by
  simp [actionKey, catalogue_members]

theorem layout_key_declared (productions : List Production) (key : Terminal)
    (member : key ∈ terminals productions) (flag : layoutKey key = true) : layout productions = true := by
  obtain ⟨operand, present, equal⟩ := List.mem_filterMap.mp member
  obtain ⟨production, owner, occurrence⟩ := List.mem_flatMap.mp present
  apply (layout_exact productions).mpr
  refine Or.inr ⟨production, owner, operand, occurrence, ?_⟩
  cases operand with | mk base annotations => cases base <;> simp_all [terminalOf, layoutOperand]

theorem catalogue_admitted (productions : List Production) (key : Terminal)
    (member : key ∈ catalogue productions) : actionKey productions key = true := by
  apply (action_key_exact productions key).mpr
  refine ⟨(catalogue_members _ _).mp member, ?_⟩
  cases flag : layoutKey key with
  | false => exact Or.inr rfl
  | true =>
    rcases (catalogue_members _ _).mp member with eof | declared
    · subst key; simp [layoutKey] at flag
    · exact Or.inl (layout_key_declared productions key declared flag)

namespace Storage
/-- Primitive vector writes are an explicit host contract; the parser owns the
ordered write replay and bounds. Both payload false and unavailable read remain distinct. -/
def update (store : Nat → Option Value) (index : Nat) (value : Value) : Nat → Option Value :=
  fun key => if key = index then some value else store key

def write : Nat → List Value → (Nat → Option Value) → Nat → Option Value
  | _, [], store => store
  | index, value :: rest, store => write (index + 1) rest (update store index value)

theorem before (values : List Value) (start index : Nat) (store : Nat → Option Value) (earlier : index < start) :
    write start values store index = store index := by
  induction values generalizing start store with
  | nil => rfl
  | cons head rest ih =>
    rw [write, ih (start + 1) _ (by omega)]
    simp [update, Nat.ne_of_lt earlier]

theorem write_at (prefixValues suffix : List Value) (value : Value) (start : Nat) (store : Nat → Option Value) :
    write start (prefixValues ++ value :: suffix) store (start + prefixValues.length) = some value := by
  induction prefixValues generalizing start store with
  | nil =>
    simp only [List.nil_append, List.length_nil, Nat.add_zero, write]
    rw [before suffix (start + 1) start _ (by omega)]
    simp [update]
  | cons head rest ih =>
    simpa [write, Nat.add_assoc, Nat.add_comm, Nat.add_left_comm] using ih (start + 1) (update store start head)

/-- Actual terminal-index installation, with the reverse catalogue as input. -/
def index [DecidableEq Key] : Nat → List Key → (Key → Option Nat) → Key → Option Nat
  | _, [], store => store
  | offset, key :: rest, store => index (offset + 1) rest (fun target => if target = key then some offset else store target)

theorem index_absent [DecidableEq Key] (values : List Key) (offset : Nat) (store : Key → Option Nat) (key : Key)
    (absent : key ∉ values) : index offset values store key = store key := by
  induction values generalizing offset store with
  | nil => rfl
  | cons head rest ih =>
    simp only [List.mem_cons, not_or] at absent
    rw [index, ih (offset + 1) _ absent.2]
    simp [absent.1]

theorem index_at [DecidableEq Key] (prefixValues suffix : List Key) (key : Key) (offset : Nat)
    (store : Key → Option Nat) (absent : key ∉ suffix) :
    index offset (prefixValues ++ key :: suffix) store key = some (offset + prefixValues.length) := by
  induction prefixValues generalizing offset store with
  | nil => simp [index, index_absent suffix (offset + 1) _ key absent]
  | cons head rest ih =>
    simpa [index, Nat.add_assoc, Nat.add_comm, Nat.add_left_comm] using ih (offset + 1) (fun target => if target = head then some offset else store target)
end Storage

namespace Lookahead
abbrev Row := List (Nat × Nat)

def starts : Nat → Nat → List Row → (Nat → Option Nat) → Nat → Option Nat
  | _, _, [], store => store
  | state, offset, row :: rest, store =>
    starts (state + 1) (offset + row.length) rest (Storage.update store state offset)

theorem starts_before (rows : List Row) (state offset index : Nat) (store : Nat → Option Nat) (earlier : index < state) :
    starts state offset rows store index = store index := by
  induction rows generalizing state offset store with
  | nil => rfl
  | cons row rest ih =>
    rw [starts, ih (state + 1) (offset + row.length) _ (by omega)]
    simp [Storage.update, Nat.ne_of_lt earlier]

theorem starts_at (prefixRows suffix : List Row) (row : Row) (state offset : Nat) (store : Nat → Option Nat) :
    starts state offset (prefixRows ++ row :: suffix) store (state + prefixRows.length) =
      some (offset + (prefixRows.map List.length).sum) := by
  induction prefixRows generalizing state offset store with
  | nil =>
    simp only [List.nil_append, List.length_nil, Nat.add_zero, starts, List.map_nil, List.sum_nil]
    rw [starts_before suffix (state + 1) (offset + row.length) state _ (by omega)]
    simp [Storage.update]
  | cons head rest ih =>
    simpa [starts, Nat.add_assoc, Nat.add_comm, Nat.add_left_comm] using
      ih (state + 1) (offset + head.length) (Storage.update store state offset)

/-- Canonical publication projects both fields from the same ordered sparse
rows, preserving empty rows and zero masks. No semantic follow relation is assumed. -/
def cores (rows : List Row) (old : Nat → Option Nat) : Nat → Option Nat :=
  Storage.write 0 (rows.flatten.map Prod.fst) old
def masks (rows : List Row) (old : Nat → Option Nat) : Nat → Option Nat :=
  Storage.write 0 (rows.flatten.map Prod.snd) old

theorem packed_pair (prefixRows suffixRows : List Row) (prefixItems suffixItems : Row) (core mask : Nat)
    (oldCores oldMasks : Nat → Option Nat) :
    let rows := prefixRows ++ (prefixItems ++ (core, mask) :: suffixItems) :: suffixRows
    let node := (prefixRows.map List.length).sum + prefixItems.length
    cores rows oldCores node = some core ∧ masks rows oldMasks node = some mask := by
  dsimp
  have size : prefixRows.flatten.length = (prefixRows.map List.length).sum := by simp
  constructor
  · simpa only [cores, List.flatten_append, List.flatten_cons, List.map_append, List.map_cons, List.length_map, List.length_append, Nat.zero_add, size, List.append_assoc, List.cons_append] using
      Storage.write_at ((prefixRows.flatten ++ prefixItems).map Prod.fst) ((suffixItems ++ suffixRows.flatten).map Prod.fst) core 0 oldCores
  · simpa only [masks, List.flatten_append, List.flatten_cons, List.map_append, List.map_cons, List.length_map, List.length_append, Nat.zero_add, size, List.append_assoc, List.cons_append] using
      Storage.write_at ((prefixRows.flatten ++ prefixItems).map Prod.snd) ((suffixItems ++ suffixRows.flatten).map Prod.snd) mask 0 oldMasks
end Lookahead

#print axioms catalogue_members
#print axioms catalogue_unique
#print axioms layout_exact
#print axioms catalogue_admitted
#print axioms Storage.index_at
#print axioms Lookahead.starts_at
#print axioms Lookahead.packed_pair
end ParserEngine.ProducerAdmission

namespace ParserEngine.ProducerAdmission
/-- Native action publication uses the narrower layout-end predicate. The
validator accepts the full declaration-derived capability; these are not equal. -/
def fallbackEnabled (productions : List Production) : Bool :=
  productions.any fun production => layoutAction production.action

theorem fallback_capable (productions : List Production) (enabled : fallbackEnabled productions = true) :
    layout productions = true := by
  obtain ⟨production, member, flag⟩ := List.any_eq_true.mp enabled
  exact (layout_exact productions).mpr (Or.inl ⟨production, member, flag⟩)

example :
    let declarations : List Production := [⟨0, [⟨.terminal (.layoutStart 1), []⟩], .ordinary 0⟩]
    layout declarations = true ∧ fallbackEnabled declarations = false := by decide +kernel
end ParserEngine.ProducerAdmission

namespace ParserEngine.ProducerAdmission.Source
open WalkSource NativeActions.Source

def layout : Datum := defn "layout-productions?" ["productions"] [app "any" [
  app "lambda" [names ["production"], app "or" [app "layout-end-action?" [call "production-action" ["production"]],
    app "any" [app "lambda" [names ["operand"], app "match" [call "base-symbol" ["operand"],
      l [l [s "@list", quoted "terminal", app "or" [quoted "layout-start", quoted "layout-next"], s "_"], b true],
      l [s "_", b false]]], call "production-rhs" ["production"]]]], s "productions"]]

def vocabulary : Datum := defn "production-vocabulary" ["productions"] [
  app "def" [s "nonterminals", app "make-table" [.keyword "test", s "eq?"]],
  app "for-each" [app "lambda" [names ["production"],
    app "table-set!" [s "nonterminals", call "production-lhs" ["production"], b true],
    app "for-each" [app "lambda" [names ["operand"],
      app "let" [l [s "symbol", call "base-symbol" ["operand"]],
        app "when" [call "nonterminal-symbol?" ["symbol"],
          app "table-set!" [s "nonterminals", call "nonterminal-name" ["symbol"], b true]]]],
      call "production-rhs" ["production"]]], s "productions"],
  app "let-values" [l [l [names ["_", "terminals"], call "production-terminal-catalog" ["productions"]]],
    call "values" ["terminals", "nonterminals"]]]

def catalog : Datum := defn "production-terminal-catalog" ["productions"] [
  app "let" [l [l [s "seen", app "make-table" [.keyword "test", s "equal?"]]],
    app "table-set!" [s "seen", s "+lr-eof+", b true],
    app "let" [s "loop-productions", l [l [s "rest", s "productions"], l [s "found", call "list" ["+lr-eof+"]]],
      app "if" [call "null?" ["rest"],
        app "let" [l [l [s "terminal-values", app "list->vector" [call "reverse" ["found"]]],
          l [s "terminal-index", app "make-table" [.keyword "test", s "equal?"]]],
          app "let" [s "install", l [l [s "offset", n 0]],
            app "when" [app "<" [s "offset", call "vector-length" ["terminal-values"]],
              app "table-set!" [s "terminal-index", call "vector-ref" ["terminal-values", "offset"], s "offset"],
              app "install" [app "+" [s "offset", n 1]]]], call "values" ["terminal-values", "terminal-index"]],
        app "let" [s "loop-symbols", l [l [s "symbols", app "production-rhs" [call "car" ["rest"]]], l [s "next", s "found"]],
          app "if" [call "null?" ["symbols"], app "loop-productions" [call "cdr" ["rest"], s "next"],
            app "let" [l [s "symbol", app "base-symbol" [call "car" ["symbols"]]],
              app "loop-symbols" [call "cdr" ["symbols"],
                app "if" [app "and" [call "terminal-symbol?" ["symbol"], app "not" [app "table-ref" [s "seen", s "symbol", b false]]],
                  app "begin" [app "table-set!" [s "seen", s "symbol", b true], call "cons" ["symbol", "next"]], s "next"]]]]]]]]]

def coreSymbols : Datum := defn "make-core-symbol-catalog" ["table", "layout"] [
  app "let" [l [l [s "catalog", app "make-vector" [call "core-item-count" ["layout"], b false]]],
    app "let" [s "production-loop", l [l [s "production-id", n 0]],
      app "when" [app "<" [s "production-id", call "vector-length" ["table"]],
        app "let" [s "symbol-loop", l [l [s "rest", app "production-rhs" [call "vector-ref" ["table", "production-id"]]], l [s "dot", n 0]],
          app "unless" [call "null?" ["rest"],
            app "vector-set!" [s "catalog", call "make-core-item" ["production-id", "dot", "layout"], app "base-symbol" [call "car" ["rest"]]],
            app "symbol-loop" [call "cdr" ["rest"], app "+" [s "dot", n 1]]]],
        app "production-loop" [app "+" [s "production-id", n 1]]]], s "catalog"]]

def itemIndex : Datum := defn "index-lr0-items" ["states", "state-count"] [
  app "let" [l [l [s "offsets", app "make-vector" [app "+" [s "state-count", n 1], n 0]], l [s "node-count", n 0]],
    app "let" [s "count-states", l [l [s "state", n 0]],
      app "when" [call "<" ["state", "state-count"],
        app "vector-set!" [s "offsets", s "state", s "node-count"],
        app "set!" [s "node-count", app "+" [s "node-count", app "length" [call "vector-ref" ["states", "state"]]]],
        app "count-states" [app "+" [s "state", n 1]]]],
    app "vector-set!" [s "offsets", s "state-count", s "node-count"],
    app "let" [l [s "node-items", app "make-vector" [s "node-count", n 0]],
      app "let" [s "index-states", l [l [s "state", n 0]], app "when" [call "<" ["state", "state-count"],
        app "let" [l [l [s "node", call "vector-ref" ["offsets", "state"]]],
          app "for-each" [app "lambda" [names ["item"], app "vector-set!" [s "node-items", s "node", s "item"],
            app "set!" [s "node", app "+" [s "node", n 1]]], call "vector-ref" ["states", "state"]]],
        app "index-states" [app "+" [s "state", n 1]]]], call "values" ["offsets", "node-items", "node-count"]]]]

def canonicalPublication : Datum := app "let" [l [
    l [s "core-states", app "make-vector" [s "state-count", b false]],
    l [s "offsets", app "make-vector" [app "+" [s "state-count", n 1], n 0]], l [s "node-count", n 0]],
  app "let" [s "state-loop", l [l [s "index", n 0]], app "when" [call "<" ["index", "state-count"],
    app "let" [l [s "state", call "ExtensibleVector-ref" ["states", "index"]],
      app "vector-set!" [s "offsets", s "index", s "node-count"],
      app "vector-set!" [s "core-states", s "index", app "map" [s "car", s "state"]],
      app "set!" [s "node-count", app "+" [s "node-count", call "length" ["state"]]]],
    app "state-loop" [app "+" [s "index", n 1]]]],
  app "vector-set!" [s "offsets", s "state-count", s "node-count"],
  app "let" [l [l [s "lookaheads", app "make-vector" [s "node-count", n 0]], l [s "node", n 0]],
    app "let" [s "fill", l [l [s "index", n 0]], app "when" [call "<" ["index", "state-count"],
      app "for-each" [app "lambda" [names ["entry"], app "vector-set!" [s "lookaheads", s "node", call "cdr" ["entry"]],
        app "set!" [s "node", app "+" [s "node", n 1]]], call "ExtensibleVector-ref" ["states", "index"]],
      app "fill" [app "+" [s "index", n 1]]]],
    app "values" [s "core-states", s "state-count", s "lookaheads", s "offsets",
      app "materialize-transitions" [call "ExtensibleVector->vector" ["state-transitions"], s "state-count"],
      s "terminal-values", s "layout", s "core-symbols", s "state-count", s "node-count"]]]
end ParserEngine.ProducerAdmission.Source

namespace ParserEngine.ProducerAdmission.Source
open WalkSource NativeActions.Source

def fallback : Datum := l [s "layout?", app "vector-any" [
  app "lambda" [names ["production"], app "layout-end-action?" [call "production-action" ["production"]]], s "table"]]

def terminal : Datum := defn "terminal?" ["value"] [app "and" [call "lr-terminal?" ["value"],
  app "or" [call "equal?" ["value", "+lr-eof+"], call "canonical-base-symbol?" ["value"]]]]

def outer : Datum := defn "validate-lr-tables" ["productions", "actions", "gotos"] [
  app "def" [s "production-count", call "length" ["productions"]],
  app "def" [s "layout?", call "layout-productions?" ["productions"]],
  app "unless" [app "and" [call "fixnum?" ["production-count"], app "fx>=" [s "production-count", n 0],
    call "vector?" ["actions"], call "vector?" ["gotos"], app "positive?" [call "vector-length" ["actions"]],
    app "=" [call "vector-length" ["actions"], call "vector-length" ["gotos"]]],
    app "error" [.text "invalid LR table dimensions", s "production-count", s "actions", s "gotos"]],
  app "let-values" [l [l [names ["terminals", "nonterminals"], call "production-vocabulary" ["productions"]]],
    app "def" [s "states", call "vector-length" ["actions"]],
    app "validate-rows" [s "actions", app "lambda" [names ["key"], app "and" [call "terminal?" ["key"],
      app "table-ref" [s "terminals", s "key", b false], app "or" [s "layout?", app "not" [app "memq" [call "cadr" ["key"], app "quote" [names ["layout-start", "layout-next"]]]]]]],
      call "make-action-validator" ["states", "production-count", "layout?"], quoted "actions"],
    app "validate-rows" [s "gotos", app "lambda" [names ["key"], app "and" [call "symbol?" ["key"], app "table-ref" [s "nonterminals", s "key", b false]]],
      app "lambda" [names ["target", "_"], call "bounded-index?" ["target", "states"]], quoted "gotos"]],
  call "validate-accept-reachability" ["actions", "gotos"], call "values" ["actions", "gotos"]]
end ParserEngine.ProducerAdmission.Source

namespace ParserEngine.ProducerAdmissionWitness
open ProducerAdmission WalkSource
set_option autoImplicit false in
theorem source_checked : layoutSource = Source.layout ∧ vocabularySource = Source.vocabulary ∧
    catalogSource = Source.catalog ∧ coreSymbolsSource = Source.coreSymbols ∧
    itemIndexSource = Source.itemIndex ∧ canonicalSource = Source.canonicalPublication ∧
    fallbackSource = Source.fallback ∧ terminalSource = Source.terminal ∧ outerSource = Source.outer := by
  exact ⟨rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩
example : replace (s "base-symbol") (s "values") catalogSource ≠ Source.catalog := by decide +kernel
example : replace (s "cdr") (s "car") canonicalSource ≠ Source.canonicalPublication := by decide +kernel
#print axioms source_checked
end ParserEngine.ProducerAdmissionWitness

namespace ParserEngine.ProducerAdmission.Storage
theorem after (values : List Value) (start index : Nat) (store : Nat → Option Value)
    (later : start + values.length ≤ index) : write start values store index = store index := by
  induction values generalizing start store with
  | nil => rfl
  | cons head rest ih =>
    rw [write, ih (start + 1) _ (by simp only [List.length_cons] at later; omega)]
    simp [update, show index ≠ start by simp only [List.length_cons] at later; omega]
end ParserEngine.ProducerAdmission.Storage

namespace ParserEngine.ProducerAdmission.CoreSymbols
/-- Complete slots retain the initial #f. The source writes only RHS operands,
then skips the completed slot when advancing to the next production. -/
def cells (rows : List (List Operand)) : Nat := (rows.map fun row => row.length + 1).sum

def fill : Nat → List (List Operand) → (Nat → Option (Option Symbol)) → Nat → Option (Option Symbol)
  | _, [], store => store
  | offset, row :: rest, store =>
    fill (offset + row.length + 1) rest (Storage.write offset (row.map fun operand => some operand.base) store)

theorem before (rows : List (List Operand)) (start index : Nat) (store : Nat → Option (Option Symbol))
    (earlier : index < start) : fill start rows store index = store index := by
  induction rows generalizing start store with
  | nil => rfl
  | cons row rest ih =>
    rw [fill, ih (start + row.length + 1) _ (by omega)]
    exact Storage.before _ start index store earlier

theorem symbol_at (prefixRows suffixRows : List (List Operand)) (prefixItems suffixItems : List Operand)
    (operand : Operand) (start : Nat) (store : Nat → Option (Option Symbol)) :
    fill start (prefixRows ++ (prefixItems ++ operand :: suffixItems) :: suffixRows) store
      (start + cells prefixRows + prefixItems.length) = some (some operand.base) := by
  induction prefixRows generalizing start store with
  | nil =>
    simp only [List.nil_append, cells, List.map_nil, List.sum_nil, Nat.add_zero, fill]
    rw [before suffixRows _ _ _ (by simp only [List.length_append, List.length_cons]; omega)]
    simpa only [List.map_append, List.map_cons, List.length_map] using
      Storage.write_at (prefixItems.map fun operand => some operand.base)
        (suffixItems.map fun operand => some operand.base) (some operand.base) start store
  | cons row rest ih =>
    simpa [fill, cells, Nat.add_assoc, Nat.add_comm, Nat.add_left_comm] using
      ih (start + row.length + 1) (Storage.write start (row.map fun operand => some operand.base) store)

theorem complete_at (prefixRows suffixRows : List (List Operand)) (row : List Operand)
    (start : Nat) (store : Nat → Option (Option Symbol)) :
    fill start (prefixRows ++ row :: suffixRows) store (start + cells prefixRows + row.length) =
      store (start + cells prefixRows + row.length) := by
  induction prefixRows generalizing start store with
  | nil =>
    simp only [List.nil_append, cells, List.map_nil, List.sum_nil, Nat.add_zero, fill]
    rw [before suffixRows _ _ _ (by omega)]
    exact Storage.after _ start _ store (by simp)
  | cons head rest ih =>
    simp only [List.cons_append, fill]
    have continued := ih (start + head.length + 1) (Storage.write start (head.map fun operand => some operand.base) store)
    have equal : start + cells (head :: rest) + row.length = start + head.length + 1 + cells rest + row.length := by
      simp [cells]; omega
    rw [equal, continued]
    exact Storage.after _ start _ store (by simp; omega)

#print axioms symbol_at
#print axioms complete_at
end ParserEngine.ProducerAdmission.CoreSymbols

namespace ParserEngine.ProducerAdmission
/-- The source's reverse accumulator is exactly stable first-observation order,
not merely an arbitrary set enumerator. -/
theorem collect_reverse [DecidableEq Key] (values found : List Key) :
    (collect values found).reverse = values.foldl NativeActions.Conflict.insert found.reverse := by
  induction values generalizing found with
  | nil => rfl
  | cons head rest ih =>
    simp only [collect, List.foldl_cons]
    by_cases present : head ∈ found
    · simpa [present, NativeActions.Conflict.insert] using ih found
    · simpa [present, NativeActions.Conflict.insert] using ih (head :: found)

theorem catalogue_order (productions : List Production) :
    catalogue productions = (terminals productions).foldl NativeActions.Conflict.insert [.eof] := by
  exact collect_reverse _ _

theorem catalogue_roundtrip (productions : List Production) (prefixKeys suffixKeys : List Terminal) (key : Terminal)
    (generated : catalogue productions = prefixKeys ++ key :: suffixKeys)
    (oldIndex : Terminal → Option Nat) (oldVector : Nat → Option Terminal) :
    Storage.index 0 (catalogue productions) oldIndex key = some prefixKeys.length ∧
      Storage.write 0 (catalogue productions) oldVector prefixKeys.length = some key := by
  have unique := catalogue_unique productions
  rw [generated] at unique
  have absent : key ∉ suffixKeys := by simp only [List.nodup_append, List.nodup_cons] at unique; exact unique.2.1.1
  rw [generated]
  exact ⟨by simpa using Storage.index_at prefixKeys suffixKeys key 0 oldIndex absent,
    by simpa using Storage.write_at prefixKeys suffixKeys key 0 oldVector⟩

namespace Admission
open NativeActions
abbrev Actions := List (List (Terminal × Validator.Action))
abbrev Gotos := List (List (Nat × Int))
def normalized (encode : Terminal → Option Nat) (actions : Actions) : CompilerExecution.ValidatedGraph.ActionRows :=
  actions.map fun row => row.map fun entry => (encode entry.1, entry.2)
def actionRows (productions : List Production) (ceiling : Nat) (encode : Terminal → Option Nat) (actions : Actions) : Bool :=
  actions.all fun row => Validator.rows (actionKey productions)
    (fun value key => Validator.action ceiling actions.length productions.length (layout productions) (encode key) value) row []
def gotoRows (productions : List Production) (ceiling states : Nat) (gotos : Gotos) : Bool :=
  gotos.all fun row => Validator.rows (gotoKey productions) (fun target _ => Validator.bounded ceiling states target) row []

theorem declared_action {productions ceiling encode actions row entry}
    (checked : actionRows productions ceiling encode actions = true) (stored : row ∈ actions) (member : entry ∈ row) :
    actionKey productions entry.1 = true ∧
      Validator.action ceiling actions.length productions.length (layout productions) (encode entry.1) entry.2 = true := by
  have admitted := (List.all_eq_true.mp checked) row stored
  have result := (Validator.rows_admit _ _ row [] admitted).1 entry member
  exact ⟨result.1, result.2.1⟩

theorem normalized_actions (productions : List Production) (ceiling : Nat) (encode : Terminal → Option Nat) (actions : Actions)
    (checked : actionRows productions ceiling encode actions = true) :
    ∀ (state : Nat) (entry : Option Nat × Validator.Action), entry ∈ ((normalized encode actions)[state]?).getD [] →
      Validator.action ceiling (normalized encode actions).length productions.length (layout productions) entry.1 entry.2 = true := by
  intro state entry member
  cases selected : actions[state]? with
  | none => simp [normalized, selected] at member
  | some row =>
    have present : entry ∈ row.map (fun value => (encode value.1, value.2)) := by simpa [normalized, selected] using member
    obtain ⟨original, occurrence, same⟩ := List.mem_map.mp present
    subst entry
    simpa [normalized] using (declared_action checked (List.mem_of_getElem? selected) occurrence).2

theorem normalized_gotos (productions : List Production) (ceiling states : Nat) (gotos : Gotos)
    (checked : gotoRows productions ceiling states gotos = true) :
    ∀ (state : Nat) (target : Int), target ∈ ((gotos.map (fun row => row.map Prod.snd))[state]?).getD [] →
      Validator.bounded ceiling states target = true := by
  intro state target member
  cases selected : gotos[state]? with
  | none => simp [selected] at member
  | some row =>
    have present : target ∈ row.map Prod.snd := by simpa [selected] using member
    obtain ⟨entry, occurrence, same⟩ := List.mem_map.mp present
    have admitted := (List.all_eq_true.mp checked) row (List.mem_of_getElem? selected)
    simpa [same] using ((Validator.rows_admit _ _ row [] admitted).1 entry occurrence).2.1

/-- Dimensions and declaration callbacks precede graph admission. Rejection
from any earlier layer cannot be converted into queue success. -/
theorem queue_after_declarations (productions : List Production) (ceiling : Nat) (encode : Terminal → Option Nat)
    (actions : Actions) (gotos : Gotos) (positive : 0 < actions.length)
    (checkedActions : actionRows productions ceiling encode actions = true)
    (checkedGotos : gotoRows productions ceiling actions.length gotos = true) :
    let rows := normalized encode actions
    let targets := gotos.map (fun row => row.map Prod.snd)
    CompilerExecution.Queue.admitted (CompilerExecution.Queue.run (CompilerExecution.ValidatedGraph.edges rows targets)
      (CompilerExecution.ValidatedGraph.accepts rows) (CompilerExecution.Queue.initial (List.range rows.length))) = true ↔
      ∃ node, CompilerExecution.Queue.Reaches (CompilerExecution.ValidatedGraph.edges rows targets) node ∧
        CompilerExecution.ValidatedGraph.accepts rows node = true := by
  apply CompilerExecution.ValidatedGraph.outer_queue_exact _ _ ceiling productions.length (layout productions)
    (by simpa [normalized] using positive) (normalized_actions _ _ _ _ checkedActions)
  simpa [normalized] using normalized_gotos productions ceiling actions.length gotos checkedGotos
end Admission
#print axioms catalogue_roundtrip
#print axioms Admission.queue_after_declarations
end ParserEngine.ProducerAdmission

namespace ParserEngine.ProducerAdmission.Lookahead
/-- Final sentinel publication follows the source's first pass. It cannot
clobber any actual state offset, even when adjacent state rows are empty. -/
def offsets (rows : List Row) (old : Nat → Option Nat) : Nat → Option Nat :=
  Storage.update (starts 0 0 rows old) rows.length (rows.map List.length).sum

theorem offset_at (prefixRows suffixRows : List Row) (row : Row) (old : Nat → Option Nat) :
    offsets (prefixRows ++ row :: suffixRows) old prefixRows.length = some (prefixRows.map List.length).sum := by
  simp only [offsets, Storage.update]
  rw [ite_eq_right (by simp)]
  simpa using starts_at prefixRows suffixRows row 0 0 old

def publish (rows : List Row) (store : CompilerExecution.Records.Store) : CompilerExecution.Records.Store :=
  { store with
    stateItems := (Storage.write 0 (rows.map (fun row => row.map Prod.fst)) store.stateItems)
    stateOffsets := offsets rows store.stateOffsets
    lookaheads := masks rows store.lookaheads }

/-- Whole producer publication refines the record consumer's physical fields;
none of the three matching reads is supplied as a caller premise. -/
theorem published_record (prefixRows suffixRows : List Row) (prefixItems suffixItems : Row) (core mask : Nat)
    (before : CompilerExecution.Records.Store) :
    let row := prefixItems ++ (core, mask) :: suffixItems
    let rows := prefixRows ++ row :: suffixRows
    let store := publish rows before
    store.stateItems prefixRows.length = some (row.map Prod.fst) ∧
      store.stateOffsets prefixRows.length = some (prefixRows.map List.length).sum ∧
      store.lookaheads ((prefixRows.map List.length).sum + prefixItems.length) = some mask := by
  dsimp [publish]
  refine ⟨?_, offset_at prefixRows suffixRows _ _, (packed_pair prefixRows suffixRows prefixItems suffixItems core mask (fun _ => none) before.lookaheads).2⟩
  simpa only [List.map_append, List.map_cons, List.length_map, Nat.zero_add] using
    Storage.write_at (prefixRows.map fun row => row.map Prod.fst)
      (suffixRows.map fun row => row.map Prod.fst) ((prefixItems ++ (core, mask) :: suffixItems).map Prod.fst) 0 before.stateItems

/-- Logical bit interpretation transports through the actual source write and
read. This premise is on the producer mask (owned by the solver), not on the
final physical vector or the generated event list. -/
theorem published_bits (prefixRows suffixRows : List Row) (prefixItems suffixItems : Row) (core mask width key : Nat)
    (before : CompilerExecution.Records.Store) (meaning : Nat → Prop)
    (solver : ∀ terminal, terminal < width → (mask.testBit terminal = true ↔ meaning terminal)) :
    let store := publish (prefixRows ++ (prefixItems ++ (core, mask) :: suffixItems) :: suffixRows) before
    ∃ observed, store.lookaheads ((prefixRows.map List.length).sum + prefixItems.length) = some observed ∧
      (key ∈ CompilerExecution.Records.maskKeys width observed ↔ key < width ∧ meaning key) := by
  refine ⟨mask, (published_record prefixRows suffixRows prefixItems suffixItems core mask before).2.2, ?_⟩
  rw [CompilerExecution.Records.mask_keys]
  constructor
  · rintro ⟨bound, bit⟩; exact ⟨bound, (solver key bound).mp bit⟩
  · rintro ⟨bound, fact⟩; exact ⟨bound, (solver key bound).mpr fact⟩

#print axioms published_record
#print axioms published_bits
end ParserEngine.ProducerAdmission.Lookahead

namespace ParserEngine.ProducerAdmission.Admission
/-- Typed projections of the native dimension and callback gates. Malformed
host vectors/rows and canonical-symbol decoding are rejected before this view. -/
def structural (productions : List Production) (ceiling : Nat) (encode : Terminal → Option Nat)
    (actions : Actions) (gotos : Gotos) : Bool :=
  decide (productions.length ≤ ceiling ∧ 0 < actions.length ∧ actions.length = gotos.length) &&
    actionRows productions ceiling encode actions && gotoRows productions ceiling actions.length gotos

def tableCheck (productions : List Production) (ceiling : Nat) (encode : Terminal → Option Nat)
    (actions : Actions) (gotos : Gotos) : Bool :=
  structural productions ceiling encode actions gotos &&
    CompilerExecution.Queue.admitted (CompilerExecution.Queue.run
      (CompilerExecution.ValidatedGraph.edges (normalized encode actions) (gotos.map fun row => row.map Prod.snd))
      (CompilerExecution.ValidatedGraph.accepts (normalized encode actions))
      (CompilerExecution.Queue.initial (List.range actions.length)))

theorem table_check_exact (productions : List Production) (ceiling : Nat) (encode : Terminal → Option Nat)
    (actions : Actions) (gotos : Gotos) :
    tableCheck productions ceiling encode actions gotos = true ↔
      structural productions ceiling encode actions gotos = true ∧
        ∃ node, CompilerExecution.Queue.Reaches
          (CompilerExecution.ValidatedGraph.edges (normalized encode actions) (gotos.map fun row => row.map Prod.snd)) node ∧
          CompilerExecution.ValidatedGraph.accepts (normalized encode actions) node = true := by
  unfold tableCheck
  rw [Bool.and_eq_true]
  constructor
  · rintro ⟨checked, reached⟩
    have gates := checked
    simp only [structural, Bool.and_eq_true, decide_eq_true_eq] at gates
    exact ⟨checked, (queue_after_declarations productions ceiling encode actions gotos gates.1.1.2.1 gates.1.2 gates.2).mp
      (by simpa [normalized] using reached)⟩
  · rintro ⟨checked, reached⟩
    have gates := checked
    simp only [structural, Bool.and_eq_true, decide_eq_true_eq] at gates
    exact ⟨checked, by simpa [normalized] using
      (queue_after_declarations productions ceiling encode actions gotos gates.1.1.2.1 gates.1.2 gates.2).mpr reached⟩

#print axioms table_check_exact
end ParserEngine.ProducerAdmission.Admission

namespace ParserEngine.ProducerAdmission.Lookahead
/-- A completed, source-related worklist trace establishes the mask meaning.
No final semantic equality or closed-set certificate is supplied. The trace
and propagation-program construction remain explicit parser-owned premises. -/
theorem published_quiescent (prefixRows suffixRows : List Row) (prefixItems suffixItems : Row)
    (core mask width key : Nat) (before : CompilerExecution.Records.Store)
    (program : List (Worklist.Clause (Nat × Nat))) (coordinates delivered : List (Nat × Nat))
    (bound : key < width)
    (covered : ((prefixRows.map List.length).sum + prefixItems.length, key) ∈ coordinates)
    (history : Worklist.BatchTrace program (Worklist.initial program)
      ⟨Encoding.decode (fun node =>
        (masks (prefixRows ++ (prefixItems ++ (core, mask) :: suffixItems) :: suffixRows) before.lookaheads node).getD 0) coordinates,
        delivered, []⟩) :
    (key ∈ CompilerExecution.Records.maskKeys width mask ↔
      Worklist.Entails program ((prefixRows.map List.length).sum + prefixItems.length, key)) := by
  have exact := Worklist.batch_quiescent_exact program _ history rfl
    ((prefixRows.map List.length).sum + prefixItems.length, key)
  have written := (packed_pair prefixRows suffixRows prefixItems suffixItems core mask (fun _ => none) before.lookaheads).2
  simp only [Encoding.decode_member, covered, true_and, written, Option.getD_some] at exact
  simpa only [CompilerExecution.Records.mask_keys, bound, true_and] using exact
#print axioms published_quiescent
end ParserEngine.ProducerAdmission.Lookahead

namespace ParserEngine.ProducerAdmission
/-- A real RHS terminal occurrence produces the same base key in the physical
core catalogue, the stable terminal vector, its installed hash index, and the
validator vocabulary. All stores may have arbitrary prior contents. -/
theorem terminal_producer (prefixProductions suffixProductions : List Production)
    (prefixItems suffixItems : List Operand) (operand : Operand) (key : Terminal) (lhs : Nat) (action : Action)
    (base : operand.base = .terminal key)
    (oldSymbols : Nat → Option (Option Symbol)) (oldIndex : Terminal → Option Nat) (oldValues : Nat → Option Terminal) :
    let production : Production := ⟨lhs, prefixItems ++ operand :: suffixItems, action⟩
    let productions := prefixProductions ++ production :: suffixProductions
    ∃ ordinal,
      CoreSymbols.fill 0 (productions.map Production.rhs) oldSymbols
        (CoreSymbols.cells (prefixProductions.map Production.rhs) + prefixItems.length) = some (some (.terminal key)) ∧
      Storage.index 0 (catalogue productions) oldIndex key = some ordinal ∧
      Storage.write 0 (catalogue productions) oldValues ordinal = some key ∧ actionKey productions key = true := by
  dsimp
  let production : Production := ⟨lhs, prefixItems ++ operand :: suffixItems, action⟩
  let productions := prefixProductions ++ production :: suffixProductions
  have member : key ∈ terminals productions := by
    apply List.mem_filterMap.mpr
    refine ⟨operand, ?_, ?_⟩
    · apply List.mem_flatMap.mpr
      exact ⟨production, by simp [productions], by simp [production]⟩
    · cases operand with | mk symbol annotations => simp_all [terminalOf]
  have declared := (catalogue_members productions key).mpr (Or.inr member)
  obtain ⟨prefixKeys, suffixKeys, generated⟩ := List.mem_iff_append.mp declared
  have roundtrip := catalogue_roundtrip productions prefixKeys suffixKeys key generated oldIndex oldValues
  refine ⟨prefixKeys.length, ?_, roundtrip.1, roundtrip.2, catalogue_admitted productions key declared⟩
  simpa only [List.map_append, List.map_cons, Nat.zero_add, base] using
    CoreSymbols.symbol_at (prefixProductions.map Production.rhs) (suffixProductions.map Production.rhs)
      prefixItems suffixItems operand 0 oldSymbols
#print axioms terminal_producer
end ParserEngine.ProducerAdmission

namespace ParserEngine.ProducerAdmission.Source
open WalkSource NativeActions.Source
def marked : Datum := defn "marked-symbol?" ["value"] [app "and" [call "pair?" ["value"], app "eq?" [call "car" ["value"], quoted "marked"]]]
def base : Datum := defn "base-symbol" ["value"] [app "if" [call "marked-symbol?" ["value"], call "cadr" ["value"], s "value"]]
def terminalTag : Datum := defn "terminal-symbol?" ["value"] [app "and" [call "pair?" ["value"], app "eq?" [call "car" ["value"], quoted "terminal"]]]
def layoutEnd : Datum := defn "layout-end-action?" ["action"] [app "or" [app "eq?" [s "action", quoted "layout-end"],
  app "and" [call "pair?" ["action"], app "eq?" [call "car" ["action"], quoted "layout-end"]]]]
end ParserEngine.ProducerAdmission.Source
namespace ParserEngine.ProducerAdmissionWitness
open ProducerAdmission
set_option autoImplicit false in
theorem helpers_checked : baseSource = Source.base ∧ markedSource = Source.marked ∧
    terminalTagSource = Source.terminalTag ∧ layoutEndSource = Source.layoutEnd := ⟨rfl, rfl, rfl, rfl⟩
#print axioms helpers_checked
end ParserEngine.ProducerAdmissionWitness

namespace ParserEngine.ProducerAdmission
/-- Seen accumulation never drops the seeded tail, including the native EOF. -/
theorem collect_suffix [DecidableEq Key] (values found : List Key) :
    ∃ added, collect values found = added ++ found := by
  induction values generalizing found with
  | nil => exact ⟨[], rfl⟩
  | cons head rest ih =>
    by_cases present : head ∈ found
    · simpa [collect, present] using ih found
    · obtain ⟨added, generated⟩ := ih (head :: found)
      exact ⟨added ++ [head], by simpa [collect, present, List.append_assoc] using generated⟩

theorem eof_ordinal (productions : List Production) (oldIndex : Terminal → Option Nat) :
    ∃ tail, catalogue productions = .eof :: tail ∧
      Storage.index 0 (catalogue productions) oldIndex .eof = some 0 := by
  obtain ⟨added, generated⟩ := collect_suffix (terminals productions) [Terminal.eof]
  have ordered : catalogue productions = .eof :: added.reverse := by simp [catalogue, generated]
  exact ⟨added.reverse, ordered, (catalogue_roundtrip productions [] added.reverse .eof ordered oldIndex (fun _ => none)).1⟩

/-- Normalized logical keys are computed from the producer's installed index;
EOF alone maps to the optional EOF key. No user-provided ordinal map is needed. -/
def keyCode (productions : List Production) (key : Terminal) : Option Nat :=
  if key = .eof then none else Storage.index 0 (catalogue productions) (fun _ => none) key

theorem key_code_exact (productions : List Production) (prefixKeys suffixKeys : List Terminal) (key : Terminal)
    (generated : catalogue productions = prefixKeys ++ key :: suffixKeys) (nonEOF : key ≠ .eof) :
    keyCode productions key = some prefixKeys.length := by
  simp only [keyCode, ite_eq_right nonEOF]
  exact (catalogue_roundtrip productions prefixKeys suffixKeys key generated (fun _ => none) (fun _ => none)).1

example (productions : List Production) : keyCode productions .eof = none := by simp [keyCode]
#print axioms eof_ordinal
#print axioms key_code_exact
end ParserEngine.ProducerAdmission
