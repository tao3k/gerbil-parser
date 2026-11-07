import ParserEngine.SourceEffects

/-! Full restricted source schema for index-state-rows, including both mutation
passes, source for-each order, reversal and final publication. Pure key/value
callbacks, bounded row states and primitive vector semantics are premises. -/
namespace ParserEngine.RowSource
open WalkSource SourceEffects
structure Program where
  start : Nat
  increment : Nat
  reverse : Bool
  deriving DecidableEq, Repr

def decode (source : Datum) : Option Program := do
  match source with
  | .list [.symbol "def", signature,
      .list [.symbol "let", binding, accumulation,
        .list [.symbol "let", .symbol "loop", seed,
          .list [.symbol "when", test, update, again]], .symbol "indexed"]] =>
    if signature ≠ l [s "index-state-rows", s "rows", s "state-count", s "key-procedure", s "value-procedure"] ∨
        binding ≠ l [s "indexed", app "make-vector" [s "state-count", app "quote" [l []]]] ∨
        test ≠ app "<" [s "state", s "state-count"] then none
    else
      let start ← match seed with
        | .list [.list [.symbol "state", .integer initial]] => some initial
        | _ => none
      let increment ← match again with
        | .list [.symbol "loop", .list [.symbol "+", .symbol "state", .integer step]] => some step
        | _ => none
      let inserted := app "vector-set!" [s "indexed", s "state",
        app "cons" [app "cons" [app "key-procedure" [s "row"], app "value-procedure" [s "row"]],
          app "vector-ref" [s "indexed", s "state"]]]
      if accumulation ≠ app "for-each" [l [s "lambda", l [s "row"],
          app "let" [l [s "state", app "car" [s "row"]], inserted]], s "rows"] then none
      else
        let reverse ← if update = app "vector-set!" [s "indexed", s "state",
            app "reverse" [app "vector-ref" [s "indexed", s "state"]]] then some true
          else if update = app "vector-set!" [s "indexed", s "state", app "vector-ref" [s "indexed", s "state"]]
          then some false else none
        pure ⟨start, increment, reverse⟩
  | _ => none

def admitted (program : Program) : Bool := program == ⟨0, 1, true⟩

/-- Ascending physical vector writes. Earlier slots have already been reversed;
the next write reads the current store, not an assumed immutable copy. -/
def reverseSlots : Nat → (Nat → List Entry) → Nat → List Entry
  | 0, store => store
  | count + 1, store => fun index =>
    let current := reverseSlots count store
    if index = count then (current count).reverse else current index

theorem reverse_slots (count : Nat) (store : Nat → List Entry) (index : Nat) :
    reverseSlots count store index = if index < count then (store index).reverse else store index := by
  induction count with
  | zero => simp [reverseSlots]
  | succ count ih =>
    simp only [reverseSlots]
    by_cases equal : index = count
    · subst index
      simp [ih]
    · rw [ite_eq_right (by exact equal), ih]
      by_cases inside : index < count
      · simp [inside, show index < count + 1 by omega]
      · simp [inside, show ¬index < count + 1 by omega]

def execute (program : Program) (state : Row → Nat) (payload : Row → Entry) (rows : List Row) (count : Nat) :
    Option (Nat → List Entry) :=
  if admitted program && rows.all (fun row => state row < count) then
    some (reverseSlots count (accumulate state payload rows (fun _ => []))) else none

/-- All row lists and payloads; no fixed grammar or bounded sample enumeration.
The returned slot keeps every duplicate key in the original relative order. -/
theorem checked_rows (source : Datum) (checked : (decode source).map admitted = some true)
    (state : Row → Nat) (payload : Row → Entry) (rows : List Row) (count index : Nat) (inside : index < count)
    (bounded : ∀ row ∈ rows, state row < count) :
    ∃ program result, decode source = some program ∧ execute program state payload rows count = some result ∧
      result index = (rows.filter (fun row => index == state row)).map payload := by
  cases decoded : decode source with
  | none => simp [decoded] at checked
  | some program =>
    have accepted : admitted program = true := by simpa [decoded] using checked
    have allRows : rows.all (fun row => decide (state row < count)) = true := by
      simp only [List.all_eq_true, decide_eq_true_eq]
      exact bounded
    refine ⟨program, reverseSlots count (accumulate state payload rows (fun _ => [])), rfl, ?_, ?_⟩
    · simp [execute, accepted, allRows]
    · simp [reverse_slots, inside, accumulate_slot]

#print axioms reverse_slots
#print axioms checked_rows
end ParserEngine.RowSource
