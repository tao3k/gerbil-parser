import ParserEngine.HybridIterator

/-! Restricted source admission for the native walker. The reader serializes
syntax, not conclusions. Only this decoder assigns supported source idioms a
schedule. Iterative source extraction and macro/compiler correctness are separate. -/
namespace ParserEngine.WalkSource
open IteratorEncoding IteratorMachine HybridIterator
inductive Datum where
  | symbol (name : String)
  | keyword (name : String)
  | text (value : String)
  | integer (value : Nat)
  /-- Canonical negative scalar: magnitude zero denotes minus one. -/
  | negative (magnitude : Nat)
  | boolean (value : Bool)
  | list (items : List Datum)
  deriving Repr
mutual
  def datumEq (a b : Datum) : Decidable (a = b) :=
    match a, b with
    | .keyword x, .keyword y => decidable_of_iff (x = y) (by simp)
    | .symbol x, .symbol y => decidable_of_iff (x = y) (by simp)
    | .text x, .text y => decidable_of_iff (x = y) (by simp)
    | .negative x, .negative y => decidable_of_iff (x = y) (by simp)
    | .integer x, .integer y => decidable_of_iff (x = y) (by simp)
    | .boolean x, .boolean y => decidable_of_iff (x = y) (by simp)
    | .list x, .list y =>
      letI := datumListEq x y
      decidable_of_iff (x = y) (by simp)
    | .negative _, .symbol _ | .negative _, .keyword _ | .negative _, .text _
    | .negative _, .integer _ | .negative _, .boolean _ | .negative _, .list _
    | .symbol _, .negative _ | .keyword _, .negative _ | .text _, .negative _
    | .integer _, .negative _ | .boolean _, .negative _ | .list _, .negative _
    | .keyword _, .symbol _ | .keyword _, .text _ | .keyword _, .integer _ | .keyword _, .boolean _ | .keyword _, .list _
    | .symbol _, .keyword _ | .text _, .keyword _ | .integer _, .keyword _ | .boolean _, .keyword _ | .list _, .keyword _
    | .symbol _, .text _ | .symbol _, .integer _ | .symbol _, .boolean _ | .symbol _, .list _
    | .text _, .symbol _ | .text _, .integer _ | .text _, .boolean _ | .text _, .list _
    | .integer _, .symbol _ | .integer _, .text _ | .integer _, .boolean _ | .integer _, .list _
    | .boolean _, .symbol _ | .boolean _, .text _ | .boolean _, .integer _ | .boolean _, .list _
    | .list _, .symbol _ | .list _, .text _ | .list _, .integer _ | .list _, .boolean _ => isFalse (by intro h; cases h)
  termination_by sizeOf a + sizeOf b
  def datumListEq (a b : List Datum) : Decidable (a = b) :=
    match a, b with
    | [], [] => isTrue rfl
    | [], _ :: _ | _ :: _, [] => isFalse (by intro h; cases h)
    | x :: xs, y :: ys =>
      letI := datumEq x y
      letI := datumListEq xs ys
      decidable_of_iff (x = y ∧ xs = ys) (by simp)
  termination_by sizeOf a + sizeOf b
end
instance : DecidableEq Datum := datumEq
abbrev s := Datum.symbol
abbrev l := Datum.list
abbrev n := Datum.integer
abbrev b := Datum.boolean
def app (name : String) (args : List Datum) := l (s name :: args)
def quoted (name : String) := app "quote" [s name]
def get (name : String) := app name [s "code"]
def callback (operation : String) (name offset context : Datum) :=
  l [l [s "lambda", l [s "op", s "name", s "offset", s "shift", s "moved", s "context"],
        s "effect", s "..."], quoted operation, name, offset, s "delta", s "moved?", context]
inductive Route where
  | valueBody | fieldBody | viewBody | left | right | prefix | blocks | tail | items
  deriving DecidableEq, Repr
structure Call where
  route : Route
  shifted : Bool
  forceMoved : Bool
  increment : Nat
  many : Bool := false
  reversed : Bool := false
  deriving DecidableEq, Repr
structure Walker where
  cutoff : Nat
  value : List Call
  field : List Call
  view : List Call
  chunk : List Call
  append : List Call
  branch : List Call
  deriving DecidableEq, Repr

def selector : String → Option Route
  | "event-program-value-body" => some .valueBody
  | "event-field-frame-body" => some .fieldBody
  | "event-code-view-code" => some .viewBody
  | "event-code-branch-left" => some .left
  | "event-code-branch-right" => some .right
  | "event-code-append-prefix" => some .prefix
  | "event-code-append-tail" => some .tail
  | _ => none

def decodeCall (source : Datum) : Option Call := do
  match source with
  | .list [.symbol "walk", .list [.symbol accessor, .symbol "code"], delta, moved,
      .list [.symbol "+", .symbol "depth", .integer step]] =>
    let route ← selector accessor
    let shifted ← if delta = s "delta" then some false
      else if delta = app "+" [s "delta", get "event-code-view-delta"] then some true else none
    let forceMoved ← if moved = s "moved?" then some false else if moved = b true then some true else none
    pure ⟨route, shifted, forceMoved, step, false, false⟩
  | _ => none

def decodeMany (source : Datum) : Option Call := do
  match source with
  | .list [.symbol "for-each", .list [.symbol "lambda", .list [.symbol "block"], body], blocks] =>
    match body with
    | .list [.symbol "walk", .symbol "block", .symbol "delta", .symbol "moved?",
        .list [.symbol "+", .symbol "depth", .integer step]] =>
      if blocks = get "event-code-append-blocks" then pure ⟨.blocks, false, false, step, true, false⟩
      else if blocks = app "reverse" [get "event-code-append-blocks"] then pure ⟨.blocks, false, false, step, true, true⟩
      else none
    | _ => none
  | _ => none

def decodeChunk (source : Datum) : Option Call := do
  match source with
  | .list [.symbol "let*", bindings,
      .list [.symbol "let", .symbol "loop", seed,
        .list [.symbol "when", test, body, again]]] =>
    if bindings ≠ l [l [s "items", get "event-code-chunk-items"], l [s "n", app "vector-length" [s "items"]]] ∨
        seed ≠ l [l [s "i", n 0]] ∨ test ≠ app "<" [s "i", s "n"] ∨
        again ≠ app "loop" [app "+" [s "i", n 1]] then none
    else match body with
    | .list [.symbol "walk", .list [.symbol "vector-ref", .symbol "items", .symbol "i"],
        .symbol "delta", .symbol "moved?", .list [.symbol "+", .symbol "depth", .integer step]] =>
      pure ⟨.items, false, false, step, true, false⟩
    | _ => none
  | _ => none

/-- Every binding and callback operand is consumed, including the saved opening
result. Unknown syntax or extra body expressions fail closed. -/
def decodeValue (source : Datum) : Option Call := do
  match source with
  | .list [.symbol "let*", bindings, child, .list [.symbol "if", .symbol "kind", close, boundary]] =>
    if bindings ≠ l [l [s "kind", get "event-program-value-kind"],
        l [s "node-state", app "and" [s "kind", callback "open-node" (s "kind") (get "event-program-value-start") (b false)]]] ∨
        close ≠ callback "close-node" (s "kind") (get "event-program-value-end") (s "node-state") ∨
        boundary ≠ callback "boundary" (b false) (get "event-program-value-end") (b false) then none
    else decodeCall child
  | _ => none

def decode (source : Datum) : Option Walker := do
  match source with
  | .list [.symbol "def", .list [.symbol "walk", .symbol "code", .symbol "delta", .symbol "moved?", .symbol "depth"],
      .list [.symbol "cond", empty, fallback, value, field, token, view, chunk, append, branch, invalid]] =>
    if empty ≠ l [app "not" [s "code"], app "void" []] ∨
        invalid ≠ l [s "else", app "error" [.text "invalid event instruction"]] then none
    else
      let cutoff ← match fallback with
        | .list [.list [.symbol ">=", .symbol "depth", .integer cutoff], body] =>
          if body = app "iterative" [s "code", s "delta", s "moved?"] then some cutoff else none
        | _ => none
      let valueCall ← match value with
        | .list [test, body] => if test = get "event-program-value?" then decodeValue body else none
        | _ => none
      let fieldCall ← match field with
        | .list [test, start, child, finish] =>
          if test = get "event-field-frame?" ∧
              start = callback "open-field" (get "event-field-frame-name") (get "event-field-frame-start") (b false) ∧
              finish = callback "close-field" (get "event-field-frame-name") (get "event-field-frame-end") (b false)
          then decodeCall child else none
        | _ => none
      if token ≠ l [get "token?", callback "token" (s "code") (get "token-start") (b false)] then none
      else
        let viewCall ← match view with
          | .list [test, child] => if test = get "event-code-view?" then decodeCall child else none
          | _ => none
        let chunkCall ← match chunk with
          | .list [test, body] => if test = get "event-code-chunk?" then decodeChunk body else none
          | _ => none
        let appendCalls ← match append with
          | .list [test, leading, blocks, tail] =>
            if test = get "event-code-append?" then do
              let first ← decodeCall leading
              let middle ← decodeMany blocks
              let last ← decodeCall tail
              pure [first, middle, last]
            else none
          | _ => none
        let branchCalls ← match branch with
          | .list [test, left, right] =>
            if test = get "event-code-branch?" then do
              let first ← decodeCall left
              let last ← decodeCall right
              pure [first, last]
            else none
          | _ => none
        pure ⟨cutoff, [valueCall], [fieldCall], [viewCall], [chunkCall], appendCalls, branchCalls⟩
  | _ => none

def plain (route : Route) : Call := ⟨route, false, false, 1, false, false⟩
def contract (cutoff : Nat) : Walker :=
  ⟨cutoff, [plain .valueBody], [plain .fieldBody], [⟨.viewBody, true, true, 1, false, false⟩],
    [⟨.items, false, false, 1, true, false⟩],
    [plain .prefix, ⟨.blocks, false, false, 1, true, true⟩, plain .tail], [plain .left, plain .right]⟩
def admitted (program : Walker) : Bool := program == contract program.cutoff

def children (record : Record) (route : Route) : List Ref :=
  match record, route with
  | .value _ _ _ body, .valueBody | .field _ _ _ body, .fieldBody
  | .view body _, .viewBody => [body]
  | .branch left _ _ _ _, .left => [left]
  | .branch _ right _ _ _, .right => [right]
  | .append leading _ _ _ _ _, .prefix => [leading]
  | .append _ blocks _ _ _ _, .blocks => blocks
  | .append _ _ tail _ _ _, .tail => [tail]
  | .chunk items _ _, .items => items
  | _, _ => []
/-- Evaluate extracted call routes, parameters and loop order without invoking
HybridIterator.plan. A loop retains a sequence activation; a call retains a call. -/
def calls (program : List Call) (record : Record) (delta : Int) (moved : Bool) (depth : Nat) : List Activation :=
  program.flatMap fun call =>
    let items := children record call.route
    let items := if call.reversed then items.reverse else items
    let shift := if call.shifted then match record with | .view _ offset => delta + offset | _ => delta else delta
    let moved := if call.forceMoved then true else moved
    if call.many then [.sequence items shift moved (depth + call.increment)]
    else items.map fun child => .call child shift moved (depth + call.increment)

def planSource (program : Walker) (address : Ref) (record : Record) (delta : Int) (moved : Bool) (depth : Nat)
    (rest : List Activation) (out : List Observation) (counter : Nat) : Snapshot :=
  match record with
  | .empty => ⟨rest, out, counter⟩
  | .token value => ⟨rest, out ++ [observe (.token value) delta moved none], counter⟩
  | .branch .. => ⟨calls program.branch record delta moved depth ++ rest, out, counter⟩
  | .view .. => ⟨calls program.view record delta moved depth ++ rest, out, counter⟩
  | .append .. => ⟨calls program.append record delta moved depth ++ rest, out, counter⟩
  | .chunk .. => ⟨calls program.chunk record delta moved depth ++ rest, out, counter⟩
  | .value (some kind) start _ _ =>
    ⟨calls program.value record delta moved depth ++ .finish address delta moved (some (counter : Int)) :: rest,
      out ++ [observe (.openNode kind start) delta moved none], counter + 1⟩
  | .value none _ _ _ =>
    ⟨calls program.value record delta moved depth ++ .finish address delta moved none :: rest, out, counter⟩
  | .field name start _ _ =>
    ⟨calls program.field record delta moved depth ++ .finish address delta moved none :: rest,
      out ++ [observe (.openField name start) delta moved none], counter⟩

theorem admitted_plan (program : Walker) (checked : admitted program = true) :
    planSource program address record delta moved depth rest out counter =
      HybridIterator.plan address record delta moved depth rest out counter := by
  have equal : program = contract program.cutoff := by simpa [admitted] using checked
  rw [equal]
  cases record <;> simp [planSource, HybridIterator.plan, contract, calls, children, plain]
  case value kind start finish body => cases kind <;> rfl

theorem decoded_plan (source : Datum) (program : Walker) (decoded : decode source = some program)
    (checked : admitted program = true) :
    ∃ witness, decode source = some witness ∧
      planSource witness address record delta moved depth rest out counter =
        HybridIterator.plan address record delta moved depth rest out counter :=
  ⟨program, decoded, admitted_plan program checked⟩


/-- The native source plan replaces only the direct record dispatch. The packet
fallback is an explicit service obligation, not silently certified source here. -/
def advanceSource (program : Walker) (heap : Nat → Option (List Cell)) (budget : Nat)
    (state : Snapshot) : Option Snapshot :=
  match state.pending with
  | .call (some address) delta moved depth :: rest =>
    if program.cutoff ≤ depth then HybridIterator.advance heap program.cutoff budget state
    else do
      let record ← readRecord heap (some address)
      pure (planSource program (some address) record delta moved depth rest state.emitted state.nextNode)
  | _ => HybridIterator.advance heap program.cutoff budget state

theorem admitted_advance (program : Walker) (checked : admitted program = true) (state : Snapshot) :
    advanceSource program heap budget state = HybridIterator.advance heap program.cutoff budget state := by
  obtain ⟨pending, out, counter⟩ := state
  cases pending with
  | nil => rfl
  | cons task rest =>
    cases task with
    | sequence items delta moved depth => rfl
    | finish address delta moved context => rfl
    | call address delta moved depth =>
      cases address with
      | none => rfl
      | some address =>
        simp only [advanceSource]
        split
        · rfl
        · simp only [HybridIterator.advance]
          simp [‹¬program.cutoff ≤ depth›, admitted_plan program checked]


def runSource (program : Walker) (heap : Nat → Option (List Cell)) (budget : Nat) : Nat → Snapshot → Option Snapshot
  | 0, _ => none
  | fuel + 1, state =>
    if state.pending.isEmpty then some state else do
      let next ← advanceSource program heap budget state
      runSource program heap budget fuel next

theorem admitted_run (program : Walker) (checked : admitted program = true) (fuel : Nat) (state : Snapshot) :
    runSource program heap budget fuel state = HybridIterator.run heap program.cutoff budget fuel state := by
  induction fuel generalizing state with
  | zero => rfl
  | succ fuel ih =>
    simp only [runSource, HybridIterator.run, admitted_advance program checked]
    split
    · rfl
    · cases HybridIterator.advance heap program.cutoff budget state <;> simp [ih]

/-- A checked source datum and checked heap construct the extraction witness and
complete event/context result. No separately supplied direct-control trace. -/
theorem checked_source_execution (source : Datum) (program : Walker)
    (decoded : decode source = some program) (syntaxChecked : admitted program = true)
    (heap : Nat → Option (List Cell)) (domain : List Ref) (rank : Ref → Nat)
    (graphChecked : Extraction.verify heap domain rank = true) (address : Ref) (inside : address ∈ domain)
    (depth : Nat) :
    ∃ meaning budget fuel last,
      decode source = some program ∧ Extraction.generate heap (rank address + 1) address = some meaning ∧
      Extract heap address meaning ∧ runSource program heap budget fuel (HybridIterator.initial address depth) = some last ∧
      last.pending = [] ∧ last.emitted.map Observation.event = EventIterator.meaning meaning 0 false ∧
      runControl last.emitted ⟨0, []⟩ = some ⟨last.nextNode, []⟩ := by
  obtain ⟨meaning, budget, fuel, last, generated, extracted, executed, finished, events, contexts⟩ :=
    HybridIterator.checked_execution heap domain rank graphChecked address inside program.cutoff depth
  exact ⟨meaning, budget, fuel, last, decoded, generated, extracted,
    by simpa [admitted_run program syntaxChecked] using executed, finished, events, contexts⟩


/-- Admission of the raw tree constructs the decoded program as well as the
record extraction and completed callback witness. -/
theorem checked_syntax_execution (source : Datum)
    (syntaxChecked : (decode source).map admitted = some true)
    (heap : Nat → Option (List Cell)) (domain : List Ref) (rank : Ref → Nat)
    (graphChecked : Extraction.verify heap domain rank = true) (address : Ref) (inside : address ∈ domain)
    (depth : Nat) :
    ∃ program meaning budget fuel last,
      decode source = some program ∧ Extraction.generate heap (rank address + 1) address = some meaning ∧
      Extract heap address meaning ∧ runSource program heap budget fuel (HybridIterator.initial address depth) = some last ∧
      last.pending = [] ∧ last.emitted.map Observation.event = EventIterator.meaning meaning 0 false ∧
      runControl last.emitted ⟨0, []⟩ = some ⟨last.nextNode, []⟩ := by
  cases parsed : decode source with
  | none => simp [parsed] at syntaxChecked
  | some program =>
    have checked : admitted program = true := by simpa [parsed] using syntaxChecked
    obtain ⟨meaning, budget, fuel, last, evidence⟩ :=
      checked_source_execution source program parsed checked heap domain rank graphChecked address inside depth
    exact ⟨program, meaning, budget, fuel, last, rfl, evidence.2⟩

/-- Syntax mutation controls operate on the extracted tree, not sample parser
inputs. Replacement is structural, with no regular-expression source matching. -/
def replace (before after : Datum) (tree : Datum) : Datum :=
  if tree = before then after else
    match tree with
    | .list items => .list (items.map (replace before after))
    | atom => atom
termination_by sizeOf tree

def swapPriority : Datum → Datum
  | .list [.symbol "def", parameters, .list (.symbol "cond" :: empty :: fallback :: rest)] =>
    l [s "def", parameters, l (s "cond" :: fallback :: empty :: rest)]
  | other => other

def rejects (tree : Datum) : Bool := !((decode tree).map admitted).getD false

#print axioms admitted_advance
#print axioms admitted_run
#print axioms checked_source_execution
#print axioms checked_syntax_execution

#print axioms admitted_plan
#print axioms decoded_plan
end ParserEngine.WalkSource
