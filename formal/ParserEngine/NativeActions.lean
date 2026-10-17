import ParserEngine.TablePublication

/-! Native candidate extraction, conflict decisions and validation. Source
schemas are checked separately below. Compiler/host primitives, physical
catalog extraction and viable-state production are explicit premises. -/
namespace ParserEngine.NativeActions
open Grammar Construction

namespace Events
/-- A source observation keeps physical production identity. A missing shift
transition is failure; terminal zero remains a valid indexed terminal. -/
structure Observation where
  production : Nat
  dot : Nat
  symbol : Option Grammar.Symbol
  terminal : Option Nat
  target : Option Nat
  lookaheads : List Nat

def extract (observation : Observation) : Option (List (Nat × PhysicalEvent)) :=
  match observation.terminal with
  | some terminal => observation.target.map fun target =>
      [(terminal, .shift ⟨observation.production, observation.dot, none⟩ terminal target)]
  | none => if observation.symbol.isSome then some [] else
      some (observation.lookaheads.map fun key =>
        (key, if observation.production = 0 then .accept ⟨0, observation.dot, none⟩
              else .reduce ⟨observation.production, observation.dot, none⟩))

theorem shift_zero (observation : Observation) (target : Nat)
    (terminal : observation.terminal = some 0) (transition : observation.target = some target) :
    extract observation = some [(0, .shift ⟨observation.production, observation.dot, none⟩ 0 target)] := by
  simp [extract, terminal, transition]
theorem missing_shift (observation : Observation) (key : Nat)
    (terminal : observation.terminal = some key) (missing : observation.target = none) :
    extract observation = none := by simp [extract, terminal, missing]

/-- Semantic extraction replaces only the lookahead field on each completed
physical item. The source's production-zero branch is preserved before decoding. -/
def decode (rows : CFG) (keyOf : Nat → Option Nat) (entry : Nat × PhysicalEvent) : Option (Nat × RawEvent) := do
  let event ← match entry.2 with
    | .shift item terminal target => decodePhysicalEvent rows (.shift item terminal target)
    | .reduce item => decodePhysicalEvent rows (.reduce {item with lookahead := keyOf entry.1})
    | .accept item => decodePhysicalEvent rows (.accept {item with lookahead := keyOf entry.1})
  pure (entry.1, event)

/-- Actual observed-item facts, rather than desired final cell support. -/
structure Facts (g : CFG) (start : Nat) (augmented : Production) (table : Table)
    (state : Nat) (keyOf : Nat → Option Nat) (observation : Observation) : Prop where
  physical : ∃ production, (augmented :: g)[observation.production]? = some production ∧
    observation.symbol = production.rhs[observation.dot]?
  shift : ∀ terminal target, observation.terminal = some terminal → observation.target = some target →
    ∃ production, (augmented :: g)[observation.production]? = some production ∧
      (∃ lookahead, (⟨production, observation.dot, lookahead⟩ : Item) ∈ table.items state) ∧
      production.rhs[observation.dot]? = some (.terminal terminal) ∧
      keyOf terminal = some terminal ∧ table.goto state (.terminal terminal) = some target
  complete : observation.terminal = none → observation.symbol = none →
    ∀ key ∈ observation.lookaheads, ∃ production,
      (augmented :: g)[observation.production]? = some production ∧
      observation.dot = production.rhs.length ∧
      (⟨production, observation.dot, keyOf key⟩ : Item) ∈ table.items state ∧
      (if observation.production = 0 then
        observation.dot = 1 ∧ keyOf key = none ∧ augmented.rhs = [.nonterminal start]
       else True)

/-- Every physical branch generated from any observation decodes to an admitted
raw event. This is uniform in grammar size, item/dot and lookahead-list length. -/
theorem extracted_admitted (g : CFG) (start : Nat) (augmented : Production) (table : Table)
    (state : Nat) (keyOf : Nat → Option Nat) (observation : Observation)
    (facts : Facts g start augmented table state keyOf observation)
    (entries : List (Nat × PhysicalEvent)) (execution : extract observation = some entries)
    (entry : Nat × PhysicalEvent) (present : entry ∈ entries) :
    ∃ raw, decode (augmented :: g) keyOf entry = some (entry.1, raw) ∧
      admitEvent g start augmented table state (keyOf entry.1) raw = true := by
  cases terminal : observation.terminal with
  | some key =>
    cases transition : observation.target with
    | none => simp [extract, terminal, transition] at execution
    | some target =>
      simp only [extract, terminal, transition, Option.map_some, Option.some.injEq] at execution
      subst entries
      have equal := List.mem_singleton.mp present
      subst entry
      obtain ⟨production, row, member, symbol, spelling, edge⟩ := facts.shift key target terminal transition
      refine ⟨.shift ⟨production, observation.dot, none⟩ key target, ?_, ?_⟩
      · simp [decode, decodePhysicalEvent, decodePhysical, row]
      · obtain ⟨lookahead, present⟩ := member
        simp only [admitEvent, decide_eq_true_eq]
        exact ⟨⟨⟨production, observation.dot, lookahead⟩, present, rfl, rfl⟩, symbol, spelling, edge⟩
  | none =>
    by_cases symbol : observation.symbol.isSome
    · simp [extract, terminal, symbol] at execution
      subst entries
      simp at present
    · have empty : observation.symbol = none := by cases equal : observation.symbol <;> simp_all
      simp only [extract, terminal, empty, Option.isSome_none, Bool.false_eq_true, ↓reduceIte,
        Option.some.injEq] at execution
      subst entries
      obtain ⟨key, member, equation⟩ := List.mem_map.mp present
      subst entry
      obtain ⟨production, row, dot, contained, kind⟩ := facts.complete terminal empty key member
      by_cases zero : observation.production = 0
      · simp only [zero, ↓reduceIte] at kind ⊢
        obtain ⟨one, eof, shape⟩ := kind
        have same : production = augmented := by simpa [zero] using row.symm
        subst production
        refine ⟨.accept ⟨augmented, observation.dot, keyOf key⟩, ?_, ?_⟩
        · simp [decode, decodePhysicalEvent, decodePhysical]
        · simp only [admitEvent, decide_eq_true_eq]
          exact ⟨contained, True.intro, one, eof, eof, shape⟩
      · simp only [zero, ↓reduceIte] at kind ⊢
        have authored : production ∈ g := by
          cases id : observation.production with
          | zero => exact False.elim (zero id)
          | succ id => exact List.mem_of_getElem? (by simpa [id] using row)
        refine ⟨.reduce ⟨production, observation.dot, keyOf key⟩, ?_, ?_⟩
        · simp [decode, decodePhysicalEvent, decodePhysical, row, zero]
        · simp only [admitEvent, decide_eq_true_eq]
          exact ⟨contained, dot, True.intro, authored⟩
def plan (rows : CFG) (keyOf : Nat → Option Nat) : List (Choice × Observation) → Option TablePublication.Events
  | [] => some []
  | (choice, observation) :: rest => do
      let physical ← extract observation
      let tail ← plan rows keyOf rest
      pure (physical.filterMap (fun entry =>
        (decode rows keyOf entry).map fun decoded => (decoded.1, choice, decoded.2)) ++ tail)

/-- Composition consumes observed native records, not a final action-table
origin certificate. Every completed decode is justified by extracted_admitted. -/
theorem plan_admitted (g : CFG) (start : Nat) (augmented : Production) (table : Table)
    (state : Nat) (keyOf : Nat → Option Nat) (observations : List (Choice × Observation))
    (facts : ∀ entry ∈ observations, Facts g start augmented table state keyOf entry.2)
    (events : TablePublication.Events) (execution : plan (augmented :: g) keyOf observations = some events) :
    ∀ entry ∈ events, admitEvent g start augmented table state (keyOf entry.1) entry.2.2 = true := by
  induction observations generalizing events with
  | nil => simp [plan] at execution; subst events; simp
  | cons head rest ih =>
    rcases head with ⟨choice, observation⟩
    cases extracted : extract observation with
    | none => simp [plan, extracted] at execution
    | some physical =>
      cases continued : plan (augmented :: g) keyOf rest with
      | none => simp [plan, extracted, continued] at execution
      | some tail =>
        simp [plan, extracted, continued] at execution
        subst events
        intro entry member
        rcases List.mem_append.mp member with present | present
        · obtain ⟨original, physicalMember, equation⟩ := List.mem_filterMap.mp present
          obtain ⟨raw, decoded, admitted⟩ := extracted_admitted g start augmented table state keyOf observation
            (facts (choice, observation) (by simp)) physical extracted original physicalMember
          simp only [decoded, Option.map_some, Option.some.injEq] at equation
          subst entry
          exact admitted
        · exact ih (fun entry member => facts entry (List.mem_cons_of_mem _ member)) tail continued entry present
end Events

/-- This theorem replaces the final per-event admission assumption at the table
boundary with a source observation/execution relation and item/goto facts. -/
theorem published_from_extraction (input : TablePublication.Input) (g : CFG) (start : Nat) (augmented : Production)
    (observations : Nat → List (Choice × Events.Observation))
    (facts : ∀ state entry, entry ∈ observations state →
      Events.Facts g start augmented (TablePublication.skeleton input) state input.keyOf entry.2)
    (execution : ∀ state events, input.plans[state]? = some events →
      Events.plan (augmented :: g) input.keyOf (observations state) = some events)
    (bounds : ∀ (state : Nat) (events : TablePublication.Events), input.plans[state]? = some events →
      ∀ entry ∈ events, entry.1 < input.width) :
    TableSupported g start augmented (TablePublication.physicalTable input) := by
  rw [TablePublication.physical_table_exact]
  apply TablePublication.complete_table input g start augmented bounds
  intro state events stored
  exact Events.plan_admitted g start augmented (TablePublication.skeleton input) state input.keyOf
    (observations state) (facts state) events (execution state events stored)

namespace Conflict
/-- Native cond order, expressed independently of the abstract Choice match. -/
def dynamic (priority : Option Priority) : Bool := priority.any fun p => p.association == .dynamic

def shiftReduce (allowFork : Bool) (shift reduce : Option Priority) : Choice :=
  if dynamic reduce || dynamic shift then unresolved allowFork
  else match reduce, shift with
    | some r, some s =>
      if r.rank > s.rank then .right else if r.rank < s.rank then .left
      else if r.association = .left then .right
      else if r.association = .right then .left
      else if r.association = .none then .reject else .fail
    | _, _ => unresolved allowFork

def reduceReduce (allowFork : Bool) (left right : Option Priority) : Choice :=
  if dynamic left || dynamic right then unresolved allowFork
  else if (left.map Priority.rank).getD 0 > (right.map Priority.rank).getD 0 then .left
  else if (left.map Priority.rank).getD 0 < (right.map Priority.rank).getD 0 then .right
  else unresolved allowFork

theorem reduce_reduce_exact (allowFork : Bool) (left right : Option Priority) :
    reduceReduce allowFork left right = reduceReduceChoice allowFork left right := rfl

theorem shift_reduce_exact (allowFork : Bool) (shift reduce : Option Priority) :
    shiftReduce allowFork shift reduce = shiftReduceChoice allowFork shift reduce := by
  cases shift with
  | none => cases reduce <;> simp [shiftReduce, shiftReduceChoice, dynamic, unresolved]
  | some s =>
    cases reduce with
    | none => simp [shiftReduce, shiftReduceChoice, dynamic, unresolved]
    | some r =>
      cases s with | mk sa sr =>
        cases r with | mk ra rr =>
          cases sa <;> cases ra <;> simp [shiftReduce, shiftReduceChoice, dynamic]

/-- Same-target shifts retain a present priority and prefer the current cell
on equal ranks; association tags do not alter this native branch. -/
def sameShift : Option Priority → Option Priority → Bool
  | none, none => true
  | none, some _ => false
  | some _, none => true
  | some left, some right => decide (left.rank ≥ right.rank)

theorem same_shift_rank (left right : Priority) :
    (if sameShift (some left) (some right) then left.rank else right.rank) = max left.rank right.rank := by
  by_cases ranked : left.rank ≥ right.rank
  · simp [sameShift, ranked, Int.max_eq_left ranked]
  · simp [sameShift, ranked, Int.max_eq_right (by omega : left.rank ≤ right.rank)]

theorem same_shift_tie (left right : Priority) (equal : left.rank = right.rank) :
    sameShift (some left) (some right) = true := by simp [sameShift, equal]

/-- union-values preserves the entire left list and scans only the right list.
Existing left duplicates survive; new right values are inserted once. -/
def insert [DecidableEq α] (seen : List α) (value : α) : List α :=
  if value ∈ seen then seen else seen ++ [value]

theorem fold_members [DecidableEq α] (values seen : List α) (value : α) :
    value ∈ values.foldl insert seen ↔ value ∈ seen ∨ value ∈ values := by
  induction values generalizing seen with
  | nil => simp
  | cons head rest ih =>
    rw [List.foldl_cons, ih]
    by_cases present : head ∈ seen
    · simp only [insert, ite_eq_left present, List.mem_cons]
      constructor
      · intro member; exact member.elim (fun h => Or.inl h) (fun h => Or.inr (Or.inr h))
      · intro member
        rcases member with h | same | h
        · exact Or.inl h
        · exact Or.inl (same ▸ present)
        · exact Or.inr h
    · simp [insert, present, or_assoc]

def union [DecidableEq α] (left right : List α) : List α := right.foldl insert left

theorem append_unique_exact [DecidableEq α] (seen : List α) (value : α) :
    (if value ∈ seen then seen else seen.foldr List.cons [value]) = insert seen value := by
  have fold : seen.foldr List.cons [value] = seen ++ [value] := by
    induction seen with
    | nil => rfl
    | cons head tail ih => simp [ih]
  simp [fold, insert]

theorem union_members [DecidableEq α] (left right : List α) (value : α) :
    value ∈ union left right ↔ value ∈ left ∨ value ∈ right := fold_members right left value

theorem union_empty [DecidableEq α] (left : List α) : union left [] = left := rfl
end Conflict

namespace Validator
/-- Validation is shape/range admission, distinct from semantic LR item origin.
The fixnum ceiling is supplied by the installed host, never inferred from Nat. -/
def bounded (ceiling count : Nat) (value : Int) : Bool :=
  decide (0 ≤ value ∧ value ≤ ceiling ∧ value < count)

theorem bounded_exact (ceiling count : Nat) (value : Int) :
    bounded ceiling count value = true ↔ 0 ≤ value ∧ value ≤ ceiling ∧ value < count := by simp [bounded]

inductive Atom where
  | shift (target : Int) (precedence : Option Priority)
  | reduce (id : Int)
  | accept
  | reject (key : Option Nat) (rank : Int)
  deriving DecidableEq, Repr

inductive Action where
  | atomic (value : Atom)
  | fork (first second : Atom) (rest : List Atom)
  | layout (primary : Atom) (fallback : List Atom)
  deriving DecidableEq, Repr

def atomic (ceiling states productions : Nat) (key : Option Nat) : Atom → Bool
  | .shift target _ => bounded ceiling states target
  | .reduce id => bounded ceiling productions id
  | .accept => key == none
  | .reject actual _ => actual == key

def reduce (ceiling productions : Nat) : Atom → Bool
  | .reduce id => bounded ceiling productions id
  | _ => false

def shift (ceiling states : Nat) : Atom → Bool
  | .shift target _ => bounded ceiling states target
  | _ => false

def action (ceiling states productions : Nat) (layout : Bool) (key : Option Nat) : Action → Bool
  | .atomic value => atomic ceiling states productions key value
  | .fork first second rest =>
      atomic ceiling states productions key first && atomic ceiling states productions key second &&
        rest.all (atomic ceiling states productions key)
  | .layout primary fallback => layout && shift ceiling states primary &&
      !fallback.isEmpty && fallback.all (reduce ceiling productions)

def atoms : Action → List Atom
  | .atomic value => [value]
  | .fork first second rest => first :: second :: rest
  | .layout primary fallback => primary :: fallback

theorem reduce_is_atomic {ceiling states productions key value}
    (accepted : reduce ceiling productions value = true) : atomic ceiling states productions key value = true := by
  cases value <;> simp_all [reduce, atomic]
theorem shift_is_atomic {ceiling states productions key value}
    (accepted : shift ceiling states value = true) : atomic ceiling states productions key value = true := by
  cases value <;> simp_all [shift, atomic]

/-- Unbounded fork length and signed indices; every accepted nested member has
its own range/key check. Closed Action syntax excludes recursive wrappers. -/
theorem action_members {ceiling states productions layout key value}
    (accepted : action ceiling states productions layout key value = true) :
    ∀ atom ∈ atoms value, atomic ceiling states productions key atom = true := by
  cases value with
  | atomic value => simpa [action, atoms] using accepted
  | fork first second rest =>
    simp only [action, Bool.and_eq_true, List.all_eq_true] at accepted
    simp only [atoms, List.mem_cons]
    intro atom member
    rcases member with same | same | member
    · simpa [← same] using accepted.1.1
    · simpa [← same] using accepted.1.2
    · exact accepted.2 atom member
  | layout primary fallback =>
    simp only [action, Bool.and_eq_true, List.all_eq_true] at accepted
    intro atom member
    rcases List.mem_cons.mp member with same | present
    · subst atom; exact shift_is_atomic accepted.1.1.2
    · exact reduce_is_atomic (accepted.2 atom present)

/-- The seen-table loop is modeled in source order; duplicate keys are rejected
before insertion, including a valid action behind an earlier duplicate. -/
def rows [DecidableEq Key] (keyOK : Key → Bool) (valueOK : Value → Key → Bool) :
    List (Key × Value) → List Key → Bool
  | [], _ => true
  | (key, value) :: rest, seen =>
      keyOK key && decide (key ∉ seen) && valueOK value key && rows keyOK valueOK rest (key :: seen)

theorem rows_admit [DecidableEq Key] (keyOK : Key → Bool) (valueOK : Value → Key → Bool)
    (entries : List (Key × Value)) (seen : List Key) (accepted : rows keyOK valueOK entries seen = true) :
    (∀ entry ∈ entries, keyOK entry.1 = true ∧ valueOK entry.2 entry.1 = true ∧ entry.1 ∉ seen) ∧
      (entries.map Prod.fst).Nodup := by
  induction entries generalizing seen with
  | nil => simp
  | cons entry rest ih =>
    rcases entry with ⟨key, value⟩
    simp only [rows, Bool.and_eq_true, decide_eq_true_eq] at accepted
    obtain ⟨tail, unique⟩ := ih (key :: seen) accepted.2
    constructor
    · intro entry member
      rcases List.mem_cons.mp member with equal | member
      · subst entry; exact ⟨accepted.1.1.1, accepted.1.2, accepted.1.1.2⟩
      · have valid := tail entry member
        exact ⟨valid.1, valid.2.1, fun present => valid.2.2 (List.mem_cons_of_mem _ present)⟩
    · simp only [List.map_cons, List.nodup_cons]
      exact ⟨by intro present; obtain ⟨entry, member, same⟩ := List.mem_map.mp present
                exact (tail entry member).2.2 (by simp [same]), unique⟩
/-- Untyped runtime data is admitted only through the closed source shapes.
Other values stand for strings, vectors, procedures, non-integral floats and unsupported
host objects; they never acquire action semantics through fallback. -/
inductive Value where
  | symbol (name : String)
  | integer (value : Int)
  | integralFloat (value : Int)
  | falseValue
  | list (values : List Value)
  | other
  deriving Repr

/-- integer? admits integral flonums in ranks; fixnum? still rejects those
values in state/production IDs. Numeric normalization/order is a host premise. -/
def integerValue : Value → Option Int
  | .integer value | .integralFloat value => some value
  | _ => none

def priority : Value → Option (Option Priority)
  | .falseValue => some none
  | .list [.symbol association, rank] => do
      let rank ← integerValue rank
      let kind ← match association with
        | "none" => some Association.none | "left" => some Association.left
        | "right" => some Association.right | "dynamic" => some Association.dynamic | _ => none
      pure (some ⟨kind, rank⟩)
  | _ => none

def parseAtom (decodeKey : Value → Option (Option Nat)) : Value → Option Atom
  | .list [.symbol "shift", .integer target, precedence] => (priority precedence).map (Atom.shift target)
  | .list [.symbol "reduce", .integer id] => some (.reduce id)
  | .list [.symbol "accept"] => some .accept
  | .list [.symbol "reject-nonassoc", rawKey, rank] => do
      let key ← decodeKey rawKey
      let rank ← integerValue rank
      pure (.reject key rank)
  | _ => none

def parseFork (decodeKey : Value → Option (Option Nat)) : Value → Option (List Atom)
  | .list (.symbol "fork" :: first :: second :: rest) => do
      let a ← parseAtom decodeKey first
      let b ← parseAtom decodeKey second
      let tail ← rest.mapM (parseAtom decodeKey)
      pure (a :: b :: tail)
  | _ => none

def parse (decodeKey : Value → Option (Option Nat)) (value : Value) : Option Action :=
  match parseAtom decodeKey value with
  | some atom => some (.atomic atom)
  | none => match value with
    | .list (.symbol "fork" :: first :: second :: rest) => do
        let a ← parseAtom decodeKey first
        let b ← parseAtom decodeKey second
        let tail ← rest.mapM (parseAtom decodeKey)
        pure (.fork a b tail)
    | .list [.symbol "layout-guard", primary, fallback] => do
        let head ← parseAtom decodeKey primary
        let tail ← match parseAtom decodeKey fallback with
          | some atom => some [atom]
          | none => parseFork decodeKey fallback
        pure (.layout head tail)
    | _ => none

def check (ceiling states productions : Nat) (layout : Bool) (key : Option Nat)
    (decodeKey : Value → Option (Option Nat)) (value : Value) : Bool :=
  (parse decodeKey value).any (action ceiling states productions layout key)

theorem untyped_admission {ceiling states productions layout key decodeKey value}
    (accepted : check ceiling states productions layout key decodeKey value = true) :
    ∃ decoded, parse decodeKey value = some decoded ∧
      ∀ atom ∈ atoms decoded, atomic ceiling states productions key atom = true := by
  cases decoded : parse decodeKey value with
  | none => simp [check, decoded] at accepted
  | some value =>
    refine ⟨value, rfl, action_members (layout := layout) ?_⟩
    simpa [check, decoded] using accepted

example (decodeKey : Value → Option (Option Nat)) :
    parse decodeKey (.list [.symbol "shift", .integralFloat 0, .falseValue]) = none := rfl
example : priority (.list [.symbol "left", .integralFloat 1]) = some (some ⟨.left, 1⟩) := rfl

example (decodeKey : Value → Option (Option Nat)) :
    parse decodeKey (.list [.symbol "fork", .list [.symbol "reduce", .integer 0]]) = none := rfl
example (decodeKey : Value → Option (Option Nat)) :
    parse decodeKey (.list [.symbol "fork", .list [.symbol "reduce", .integer 0],
      .list [.symbol "fork", .list [.symbol "reduce", .integer 0], .list [.symbol "reduce", .integer 1]]]) = none := rfl

end Validator
#print axioms Events.extracted_admitted
#print axioms Events.plan_admitted
#print axioms published_from_extraction
#print axioms Conflict.shift_reduce_exact
#print axioms Conflict.same_shift_rank
#print axioms Validator.untyped_admission
#print axioms Conflict.append_unique_exact
#print axioms Conflict.union_members
#print axioms Validator.action_members
#print axioms Validator.rows_admit
end ParserEngine.NativeActions

namespace ParserEngine.NativeActions.Source
open WalkSource

def names (values : List String) : Datum := l (values.map s)
def defn (name : String) (parameters : List String) (body : List Datum) : Datum :=
  app "def" (names (name :: parameters) :: body)
def call (name : String) (parameters : List String) : Datum := app name (parameters.map s)
def pattern (tag : String) (fields : List String) : Datum := l (s "@list" :: quoted tag :: fields.map s)
def matchClause (test result : Datum) : Datum := l [test, result]

def dynamic : Datum := defn "dynamic-precedence?" ["precedence"]
  [app "and" [s "precedence", app "eq?" [call "car" ["precedence"], quoted "dynamic"]]]
def fork : Datum := defn "fork-action" ["left", "right"]
  [app "let*" [l [l [s "left-actions", app "if" [app "eq?" [call "car" ["left"], quoted "fork"], call "cdr" ["left"], call "list" ["left"]]],
                    l [s "right-actions", app "if" [app "eq?" [call "car" ["right"], quoted "fork"], call "cdr" ["right"], call "list" ["right"]]]],
    app "cons" [quoted "fork", call "union-values" ["left-actions", "right-actions"]]]]
def appendUnique : Datum := defn "append-unique" ["values", "value"]
  [app "if" [call "member" ["value", "values"], s "values",
    app "foldr" [s "cons", call "list" ["value"], s "values"]]]
def union : Datum := defn "union-values" ["left", "right"]
  [app "foldl" [app "lambda" [names ["value", "found"], call "append-unique" ["found", "value"]], s "left", s "right"]]
def unresolved : Datum := defn "fork-or-reject" ["conflict-policy", "shift", "reduce", "message", "evidence"]
  [app "if" [app "memq" [s "conflict-policy", app "quote" [names ["selective-glr", "probe"]]],
    call "fork-action" ["shift", "reduce"], call "error" ["message", "evidence"]]]

def shiftReduce : Datum := defn "resolve-shift-reduce" ["terminal", "shift", "reduce", "table", "conflict-policy"]
  [app "let*" [l [l [s "production", app "vector-ref" [s "table", call "cadr" ["reduce"]]],
                    l [s "reduce-precedence", call "production-precedence" ["production"]],
                    l [s "shift-precedence", call "caddr" ["shift"]]],
    app "cond" [
      l [app "or" [call "dynamic-precedence?" ["reduce-precedence"], call "dynamic-precedence?" ["shift-precedence"]],
        app "fork-or-reject" [s "conflict-policy", s "shift", s "reduce", .text "dynamic precedence requires selective GLR admission", s "terminal"]],
      l [app "not" [call "and" ["reduce-precedence", "shift-precedence"]],
        app "fork-or-reject" [s "conflict-policy", s "shift", s "reduce", .text "unresolved shift/reduce conflict requires precedence",
          call "list" ["terminal", "production"]]],
      l [s "else", app "let" [l [l [s "reduce-rank", call "cadr" ["reduce-precedence"]], l [s "shift-rank", call "cadr" ["shift-precedence"]]],
        app "cond" [l [call ">" ["reduce-rank", "shift-rank"], s "reduce"], l [call "<" ["reduce-rank", "shift-rank"], s "shift"],
          l [app "eq?" [call "car" ["reduce-precedence"], quoted "left"], s "reduce"],
          l [app "eq?" [call "car" ["reduce-precedence"], quoted "right"], s "shift"],
          l [app "eq?" [call "car" ["reduce-precedence"], quoted "none"], app "list" [quoted "reject-nonassoc", s "terminal", s "reduce-rank"]],
          l [s "else", app "error" [.text "unknown static associativity", s "terminal", s "reduce-precedence"]]]]]]]]

def reduceReduce : Datum := defn "resolve-reduce-reduce" ["terminal", "left", "right", "table", "conflict-policy"]
  [app "let*" [l [l [s "left-production", app "vector-ref" [s "table", call "cadr" ["left"]]],
                    l [s "right-production", app "vector-ref" [s "table", call "cadr" ["right"]]],
                    l [s "left-precedence", call "production-precedence" ["left-production"]],
                    l [s "right-precedence", call "production-precedence" ["right-production"]],
                    l [s "left-rank", app "if" [s "left-precedence", call "cadr" ["left-precedence"], n 0]],
                    l [s "right-rank", app "if" [s "right-precedence", call "cadr" ["right-precedence"], n 0]]],
    app "cond" [
      l [app "or" [call "dynamic-precedence?" ["left-precedence"], call "dynamic-precedence?" ["right-precedence"]],
        app "fork-or-reject" [s "conflict-policy", s "left", s "right", .text "dynamic precedence requires selective GLR admission",
          call "list" ["terminal", "left-production", "right-production"]]],
      l [call ">" ["left-rank", "right-rank"], s "left"], l [call "<" ["left-rank", "right-rank"], s "right"],
      l [app "memq" [s "conflict-policy", app "quote" [names ["selective-glr", "probe"]]], call "fork-action" ["left", "right"]],
      l [s "else", app "error" [.text "unresolved reduce/reduce conflict", s "terminal", s "left-production", s "right-production"]]]]]

def tagged (value tag : String) : Datum := app "eq?" [call "car" [value], quoted tag]
def dispatcher : Datum := defn "resolve-action" ["state", "terminal", "current", "action", "table", "conflict-policy"] [
  app "cond" [l [call "equal?" ["current", "action"], s "current"],
    l [app "or" [tagged "current" "fork", tagged "action" "fork"], call "fork-action" ["current", "action"]],
    l [tagged "current" "reject-nonassoc", s "current"], l [tagged "action" "reject-nonassoc", s "action"],
    l [app "and" [tagged "current" "shift", tagged "action" "shift", app "=" [call "cadr" ["current"], call "cadr" ["action"]]],
      app "let" [l [l [s "current-precedence", call "caddr" ["current"]], l [s "next-precedence", call "caddr" ["action"]]],
        app "cond" [l [app "and" [call "not" ["current-precedence"], call "not" ["next-precedence"]], s "current"],
          l [call "not" ["current-precedence"], s "action"], l [call "not" ["next-precedence"], s "current"],
          l [app ">=" [call "cadr" ["current-precedence"], call "cadr" ["next-precedence"]], s "current"], l [s "else", s "action"]]]],
    l [app "and" [tagged "current" "shift", tagged "action" "reduce"], call "resolve-shift-reduce" ["terminal", "current", "action", "table", "conflict-policy"]],
    l [app "and" [tagged "current" "reduce", tagged "action" "shift"], call "resolve-shift-reduce" ["terminal", "action", "current", "table", "conflict-policy"]],
    l [app "and" [tagged "current" "reduce", tagged "action" "reduce"], call "resolve-reduce-reduce" ["terminal", "current", "action", "table", "conflict-policy"]],
    l [s "else", app "error" [.text "unresolved LR action conflict", s "state", s "terminal", s "current", s "action"]]]]

def bounded : Datum := defn "bounded-index?" ["value", "count"]
  [app "and" [call "fixnum?" ["value"], app "fx>=" [s "value", n 0], call "fx<" ["value", "count"]]]
def precedence : Datum := defn "precedence?" ["value"]
  [app "or" [call "not" ["value"], app "match" [s "value",
    l [app "@list" [app "or" [quoted "none", quoted "left", quoted "right", quoted "dynamic"], call "?" ["integer?"]], b true],
    l [s "_", b false]]]]
def validator : Datum := defn "make-action-validator" ["states", "productions", "layout?"] [
  defn "reduce?" ["value"] [app "match" [s "value", matchClause (pattern "reduce" ["id"]) (call "bounded-index?" ["id", "productions"]), l [s "_", b false]]],
  defn "shift?" ["value"] [app "match" [s "value", matchClause (pattern "shift" ["target", "precedence"])
    (app "and" [call "bounded-index?" ["target", "states"], call "precedence?" ["precedence"]]), l [s "_", b false]]],
  defn "atomic?" ["value", "terminal"] [app "or" [call "shift?" ["value"], call "reduce?" ["value"],
    app "match" [s "value", matchClause (pattern "accept" []) (call "equal?" ["terminal", "+lr-eof+"]),
      l [l [s "@list", quoted "reject-nonassoc", s "key", call "?" ["integer?"]], call "equal?" ["key", "terminal"]], l [s "_", b false]]]],
  defn "fork?" ["value", "branch?"] [app "match" [s "value",
    matchClause (pattern "fork" ["first", "second", "rest", "..."])
      (app "and" [call "branch?" ["first"], call "branch?" ["second"], call "every" ["branch?", "rest"]]), l [s "_", b false]]],
  app "lambda" [names ["action", "terminal"], app "or" [call "atomic?" ["action", "terminal"],
    app "fork?" [s "action", app "lambda" [names ["branch"], call "atomic?" ["branch", "terminal"]]],
    app "match" [s "action", matchClause (pattern "layout-guard" ["primary", "fallback"])
      (app "and" [s "layout?", call "shift?" ["primary"], app "or" [call "reduce?" ["fallback"], call "fork?" ["fallback", "reduce?"]]]),
      l [s "_", b false]]]]]

def event : Datum := app "lambda" [names ["item"],
  app "set!" [s "processed-items", app "+" [s "processed-items", n 1]],
  app "let*" [l [l [s "production-id", call "core-item-production-id" ["item", "layout"]],
                    l [s "symbol", call "vector-ref" ["core-symbols", "item"]],
                    l [s "terminal-id", call "vector-ref" ["core-terminal-ids", "item"]]],
    app "cond" [l [s "terminal-id", app "install!" [s "terminal-id",
      app "list" [quoted "shift", app "let" [l [s "target", call "vector-ref" ["shift-targets", "terminal-id"]],
        app "unless" [s "target", app "error" [.text "LR shift transition is missing", s "state-id", s "symbol"]], s "target"],
        app "production-precedence" [call "vector-ref" ["table", "production-id"]]]]],
      l [call "not" ["symbol"], app "let" [l [s "action", app "if" [app "=" [s "production-id", n 0],
          app "quote" [names ["accept"]], app "list" [quoted "reduce", s "production-id"]]],
        app "compiler-index-set-for-each" [call "vector-ref" ["lookaheads", "lookahead-node"],
          app "lambda" [names ["lookahead"], call "install!" ["lookahead", "action"]]]]]]],
  app "set!" [s "lookahead-node", app "+" [s "lookahead-node", n 1]]]
def rows : Datum := defn "validate-rows" ["rows", "key?", "value?", "kind"] [
  app "for" [l [s "state", app "in-range" [call "vector-length" ["rows"]]],
    app "let" [l [l [s "row", call "vector-ref" ["rows", "state"]],
                    l [s "seen", app "make-table" [.keyword "test", s "equal?"]]],
      app "unless" [call "list?" ["row"], app "error" [.text "invalid LR table row", s "kind", s "state", s "row"]],
      app "for-each" [app "lambda" [names ["entry"],
        app "unless" [app "and" [call "pair?" ["entry"], app "key?" [call "car" ["entry"]],
          app "not" [app "table-ref" [s "seen", call "car" ["entry"], b false]],
          app "value?" [call "cdr" ["entry"], call "car" ["entry"]]],
          app "error" [.text "invalid LR table entry", s "kind", s "state", s "entry"]],
        app "table-set!" [s "seen", call "car" ["entry"], b true]], s "row"]]], s "rows"]

end ParserEngine.NativeActions.Source

namespace ParserEngine.NativeActionsWitness
open WalkSource NativeActions

set_option autoImplicit false in
theorem source_checked :
    dynamicSource = Source.dynamic ∧ forkSource = Source.fork ∧ unionSource = Source.union ∧ appendUniqueSource = Source.appendUnique ∧
    unresolvedSource = Source.unresolved ∧ dispatcherSource = Source.dispatcher ∧ shiftReduceSource = Source.shiftReduce ∧ reduceReduceSource = Source.reduceReduce ∧
    boundedSource = Source.bounded ∧ precedenceSource = Source.precedence ∧ validatorSource = Source.validator ∧
    rowsSource = Source.rows ∧
    eventSource = Source.event := by
  exact ⟨rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩

example : replace (s ">") (s "<") shiftReduceSource ≠ Source.shiftReduce := by decide +kernel
example : replace (s "every") (s "any") validatorSource ≠ Source.validator := by decide +kernel
example : replace (n 0) (n 1) boundedSource ≠ Source.bounded := by decide +kernel
example : replace (n 0) (n 1) eventSource ≠ Source.event := by decide +kernel
example : (.keyword "test" : Datum) ≠ s "test" := by decide +kernel
#print axioms source_checked
end ParserEngine.NativeActionsWitness
