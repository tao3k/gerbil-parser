import ParserEngine.LoweringClaimEvaluation
import ParserEngine.LoweringNames

/-! Recursive source fresh/claim calls share the captured counter cell. The
proof groups counter reservation and claim into one abstract allocation step;
individual Scheme assignments are not completed Names.Trace transitions. -/
namespace ParserEngine.LoweringFreshEvaluation
open WalkSource SchemeTableEvaluation LoweringClaimEvaluation
attribute [local simp] List.lookup

def candidate (owner : String) (counter : Nat) : String := "$" ++ owner ++ "." ++ toString counter
def nameExpression : Datum := app "string->symbol" [app "string-append" [
  .text "$", app "symbol->string" [s "owner"], .text ".", app "number->string" [s "next-synthetic"]]]
def loopBody : Datum := app "let" [.list [s "name", nameExpression],
  app "set!" [s "next-synthetic", app "+" [s "next-synthetic", n 1]],
  app "or" [app "claim-name" [s "name"], app "loop" []]]
def freshBody : Datum := app "let" [s "loop", .list [], loopBody]
def declarations : List (String × Procedure) := [
  ("claim-name", ⟨[s "name"], [claimBody]⟩), ("fresh", ⟨[s "owner"], [freshBody]⟩)]

def preludeNames : List String := ["table-ref", "table-set!", "not", "+",
  "symbol->string", "number->string", "string-append", "string->symbol"]

/-- Requirements on the real lexical module, not a replacement two-procedure
scope. Root storage and source extraction must construct this interface. -/
structure Scope where
  group : List (String × Procedure)
  captured : Environment
  counter : Nat
  table : Nat
  claimLookup : group.lookup "claim-name" = some ⟨[s "name"], [claimBody]⟩
  freshLookup : group.lookup "fresh" = some ⟨[s "owner"], [freshBody]⟩
  next : tie group captured "next-synthetic" = some (.cell counter)
  occupied : tie group captured "used-names" = some (.table table)
  prelude : ∀ operation ∈ preludeNames, tie group captured operation = none

attribute [local simp] Scope.claimLookup Scope.freshLookup Scope.next Scope.occupied
@[local simp] theorem Scope.claim (scope : Scope) :
    tie scope.group scope.captured "claim-name" = some (.recursive scope.group "claim-name" scope.captured) := by
  simp only [tie, scope.claimLookup]
@[local simp] theorem Scope.fresh (scope : Scope) :
    tie scope.group scope.captured "fresh" = some (.recursive scope.group "fresh" scope.captured) := by
  simp only [tie, scope.freshLookup]
@[local simp] theorem Scope.table_ref (scope : Scope) : tie scope.group scope.captured "table-ref" = none :=
  scope.prelude _ (by decide)
@[local simp] theorem Scope.table_set (scope : Scope) : tie scope.group scope.captured "table-set!" = none :=
  scope.prelude _ (by decide)
@[local simp] theorem Scope.negate (scope : Scope) : tie scope.group scope.captured "not" = none :=
  scope.prelude _ (by decide)
@[local simp] theorem Scope.add (scope : Scope) : tie scope.group scope.captured "+" = none :=
  scope.prelude _ (by decide)
@[local simp] theorem Scope.symbol_text (scope : Scope) : tie scope.group scope.captured "symbol->string" = none :=
  scope.prelude _ (by decide)
@[local simp] theorem Scope.natural_text (scope : Scope) : tie scope.group scope.captured "number->string" = none :=
  scope.prelude _ (by decide)
@[local simp] theorem Scope.append_text (scope : Scope) : tie scope.group scope.captured "string-append" = none :=
  scope.prelude _ (by decide)
@[local simp] theorem Scope.intern (scope : Scope) : tie scope.group scope.captured "string->symbol" = none :=
  scope.prelude _ (by decide)
@[local simp] theorem tie_singleton_self : tie [(name, definition)] captured name =
    some (.recursive [(name, definition)] name captured) := by simp [tie]
@[local simp] theorem tie_singleton_other (different : query ≠ name) :
    tie [(name, definition)] captured query = captured query := by
  have miss : (query == name) = false := Bool.eq_false_iff.mpr (fun equal => different (eq_of_beq equal))
  simp [tie, miss]

def outer (scope : Scope) (owner : String) : Environment :=
  extend (tie scope.group scope.captured) "owner" (.symbol owner)
def looping (scope : Scope) (owner : String) : Environment :=
  tie [("loop", ⟨[], [loopBody]⟩)] (outer scope owner)
def named (scope : Scope) (owner name : String) : Environment :=
  extend (looping scope owner) "name" (.symbol name)
def claiming (scope : Scope) (name : String) : Environment :=
  extend (tie scope.group scope.captured) "name" (.symbol name)

variable {scope : Scope}

theorem extracted_fresh : procedure LoweringNamesSource.freshSource =
    some ("fresh", ⟨[s "owner"], [freshBody]⟩) := rfl

theorem extracted_declarations :
    ([LoweringNamesSource.claimSource, LoweringNamesSource.freshSource].mapM procedure) = some declarations := rfl

theorem claim_ready : Ready (claiming scope name) heap scope.table name := by
  constructor <;> simp [claiming, extend, resolve]

theorem expression_eval (contents : heap scope.counter 0 = some (.natural count)) :
    Eval (looping scope owner) heap nameExpression (.symbol (candidate owner count)) heap := by
  have ownerRead : Eval (looping scope owner) heap (s "owner") (.symbol owner) heap := by
    apply Eval.variable
    simp [looping, outer, extend, resolve]
  have countRead : Eval (looping scope owner) heap (s "next-synthetic") (.natural count) heap := by
    apply Eval.variable
    simpa [looping, outer, extend, resolve] using contents
  exact .apply (.cons (.apply (.cons .text (.cons (.apply (.cons ownerRead .nil) rfl (by
    simp [looping, outer, extend])) (.cons .text
      (.cons (.apply (.cons countRead .nil) rfl (by simp [looping, outer, extend])) .nil))))
    rfl (by simp [looping, outer, extend])) .nil) rfl
      (by simp [looping, outer, extend])

theorem increment_eval (contents : heap scope.counter 0 = some (.natural count)) :
    Eval (named scope owner name) heap
      (app "set!" [s "next-synthetic", app "+" [s "next-synthetic", n 1]]) .unit
      (write heap scope.counter 0 (.natural (count + 1))) := by
  apply Eval.assignment (by simp [named, looping, outer, extend])
  apply Eval.apply (values := [.natural count, .natural 1])
  · refine .cons (.variable ?_) (.cons .natural .nil)
    simpa [named, looping, outer, extend, resolve] using contents
  · rfl
  · simp [named, looping, outer, extend]

theorem claim_call_occupied (present : heap scope.table (.symbol name) = some (.boolean true)) :
    Eval (named scope owner name) heap (app "claim-name" [s "name"]) (.boolean false) heap := by
  apply Eval.call (function := .recursive scope.group "claim-name" scope.captured)
    (values := [.symbol name]) rfl (.variable (by simp [named, looping, outer, extend, resolve]))
    (.cons (.variable (by simp [named, extend, resolve])) .nil)
  exact .recursive (definition := ⟨[s "name"], [claimBody]⟩) scope.claimLookup rfl
    (.single (occupied (claim_ready (scope := scope)) present))

theorem claim_call_available (absent : heap scope.table (.symbol name) = none) :
    Eval (named scope owner name) heap (app "claim-name" [s "name"]) (.symbol name)
      (writeSymbol heap scope.table name (.boolean true)) := by
  apply Eval.call (function := .recursive scope.group "claim-name" scope.captured)
    (values := [.symbol name]) rfl (.variable (by simp [named, looping, outer, extend, resolve]))
    (.cons (.variable (by simp [named, extend, resolve])) .nil)
  exact .recursive (definition := ⟨[s "name"], [claimBody]⟩) scope.claimLookup rfl (.single (available (claim_ready (scope := scope)) absent
    (argument_preserved (by simp [extend]))))

theorem write_overwrite : write (write heap counter 0 first) counter 0 second = write heap counter 0 second := by
  funext address key
  simp only [write]
  split <;> simp_all

def Occupied (heap : Heap) (table : Nat) (used : List String) : Prop :=
  ∀ name, heap table (.symbol name) = if name ∈ used then some (.boolean true) else none

theorem occupied_after_increment (valid : Occupied heap table used) :
    Occupied (write heap counter 0 value) table used := by
  intro name
  simpa [write] using valid name

/-- Arbitrary finite collision prefixes, with real recursive calls and no
assumption that the next candidate is free. The fuel belongs to the theorem. -/
theorem loop_eval (valid : Occupied heap scope.table used)
    (contents : heap scope.counter 0 = some (.natural count))
    (found : LoweringNames.search (candidate owner) used count fuel = some index) :
    Eval (looping scope owner) heap loopBody (.symbol (candidate owner index))
      (writeSymbol (write heap scope.counter 0 (.natural (index + 1))) scope.table (candidate owner index) (.boolean true)) := by
  induction fuel generalizing heap count with
  | zero => simp [LoweringNames.search] at found
  | succ fuel ih =>
    apply Eval.letBody (expression_eval contents)
    refine Sequence.cons (increment_eval contents) (.single ?_)
    simp only [LoweringNames.search] at found
    split at found
    next collision =>
      have present : write heap scope.counter 0 (.natural (count + 1)) scope.table (.symbol (candidate owner count)) =
          some (.boolean true) := by simpa [write, collision] using valid (candidate owner count)
      apply Eval.disjunctionFalse (claim_call_occupied present) rfl
      apply Eval.call (function := .recursive [("loop", ⟨[], [loopBody]⟩)] "loop" (outer scope owner))
        rfl (.variable (by simp [named, extend, looping, resolve])) .nil
      apply Invoke.recursive (definition := ⟨[], [loopBody]⟩) rfl rfl
      apply Sequence.single
      simpa only [write_overwrite, looping] using ih
        (occupied_after_increment (counter := scope.counter) (value := .natural (count + 1)) valid)
        (by simp [write]) found
    next free =>
      have same := Option.some.inj found
      subst index
      apply Eval.disjunctionTrue (claim_call_available ?_) rfl
      simpa [write, free] using valid (candidate owner count)

theorem fresh_eval (valid : Occupied heap scope.table used)
    (contents : heap scope.counter 0 = some (.natural count))
    (found : LoweringNames.search (candidate owner) used count fuel = some index) :
    Eval (outer scope owner) heap (app "fresh" [s "owner"])
      (.symbol (candidate owner index))
      (writeSymbol (write heap scope.counter 0 (.natural (index + 1))) scope.table (candidate owner index) (.boolean true)) := by
  apply Eval.call (function := .recursive scope.group "fresh" scope.captured)
    (values := [.symbol owner]) rfl
    (.variable (by simp [outer, extend, resolve]))
    (.cons (.variable (by simp [outer, extend, resolve])) .nil)
  apply Invoke.recursive (definition := ⟨[s "owner"], [freshBody]⟩) scope.freshLookup rfl
  apply Sequence.single
  apply Eval.namedLoop rfl Arguments.nil rfl
  exact .single (loop_eval valid contents found)

/-- Every returning evaluation agrees with the operational derivation, not
only a chosen witness run or a finite collection of grammars. -/
theorem fresh_unique (valid : Occupied heap scope.table used)
    (contents : heap scope.counter 0 = some (.natural count))
    (found : LoweringNames.search (candidate owner) used count fuel = some index)
    (execution : Eval (outer scope owner) heap (app "fresh" [s "owner"]) result after) :
    result = .symbol (candidate owner index) ∧
    after = writeSymbol (write heap scope.counter 0 (.natural (index + 1))) scope.table (candidate owner index) (.boolean true) :=
  execution.deterministic (fresh_eval valid contents found)

/-- The paper/protocol boundary is one complete allocation, including all
collisions. Encoding injectivity remains an explicit parser representation
obligation; it is not attributed to the external GC or compiler. -/
theorem allocation_simulation (valid : Occupied heap scope.table before.used)
    (contents : heap scope.counter 0 = some (.natural before.attempt))
    (injective : ∀ a b, candidate owner a = candidate owner b → a = b) (logicalOwner : Lowering.Owner) :
    ∃ index, candidate owner index ∉ before.used ∧ before.attempt ≤ index ∧
      Eval (outer scope owner) heap (app "fresh" [s "owner"])
        (.symbol (candidate owner index))
        (writeSymbol (write heap scope.counter 0 (.natural (index + 1))) scope.table (candidate owner index) (.boolean true)) ∧
      Names.Trace before (LoweringNames.claimed before (candidate owner) logicalOwner index) := by
  obtain ⟨index, found⟩ := LoweringNames.search_total (candidate owner) injective before.used before.attempt
  obtain ⟨free, increasing, history⟩ := LoweringNames.search_trace logicalOwner found
  exact ⟨index, free, increasing, fresh_eval valid contents found, history⟩

end ParserEngine.LoweringFreshEvaluation
