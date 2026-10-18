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
def group : List (String × Procedure) := [
  ("claim-name", ⟨[s "name"], [claimBody]⟩), ("fresh", ⟨[s "owner"], [freshBody]⟩)]
def captured (counter table : Nat) : Environment :=
  extend (extend (fun _ => none) "next-synthetic" (.cell counter)) "used-names" (.table table)
def outer (counter table : Nat) (owner : String) : Environment :=
  extend (tie group (captured counter table)) "owner" (.symbol owner)
def looping (counter table : Nat) (owner : String) : Environment :=
  tie [("loop", ⟨[], [loopBody]⟩)] (outer counter table owner)
def named (counter table : Nat) (owner name : String) : Environment :=
  extend (looping counter table owner) "name" (.symbol name)
def claiming (counter table : Nat) (name : String) : Environment :=
  extend (tie group (captured counter table)) "name" (.symbol name)

theorem extracted_fresh : procedure LoweringNamesSource.freshSource =
    some ("fresh", ⟨[s "owner"], [freshBody]⟩) := rfl

theorem extracted_group :
    ([LoweringNamesSource.claimSource, LoweringNamesSource.freshSource].mapM procedure) = some group := rfl

theorem claim_ready : Ready (claiming counter table name) heap table name := by
  constructor <;> simp [claiming, captured, group, tie, extend, resolve]

theorem expression_eval (contents : heap counter 0 = some (.natural count)) :
    Eval (looping counter table owner) heap nameExpression (.symbol (candidate owner count)) heap := by
  have ownerRead : Eval (looping counter table owner) heap (s "owner") (.symbol owner) heap := by
    apply Eval.variable
    simp [looping, outer, extend, tie, resolve]
  have countRead : Eval (looping counter table owner) heap (s "next-synthetic") (.natural count) heap := by
    apply Eval.variable
    simpa [looping, outer, captured, extend, tie, group, resolve] using contents
  exact .apply (.cons (.apply (.cons .text (.cons (.apply (.cons ownerRead .nil) rfl (by
    simp [looping, outer, captured, extend, tie, group])) (.cons .text
      (.cons (.apply (.cons countRead .nil) rfl (by simp [looping, outer, captured, extend, tie, group])) .nil))))
    rfl (by simp [looping, outer, captured, extend, tie, group])) .nil) rfl
      (by simp [looping, outer, captured, extend, tie, group])

theorem increment_eval (contents : heap counter 0 = some (.natural count)) :
    Eval (named counter table owner name) heap
      (app "set!" [s "next-synthetic", app "+" [s "next-synthetic", n 1]]) .unit
      (write heap counter 0 (.natural (count + 1))) := by
  apply Eval.assignment (by simp [named, looping, outer, captured, extend, tie, group])
  apply Eval.apply (values := [.natural count, .natural 1])
  · refine .cons (.variable ?_) (.cons .natural .nil)
    simpa [named, looping, outer, captured, extend, tie, group, resolve] using contents
  · rfl
  · simp [named, looping, outer, captured, extend, tie, group]

theorem claim_call_occupied (present : heap table (.symbol name) = some (.boolean true)) :
    Eval (named counter table owner name) heap (app "claim-name" [s "name"]) (.boolean false) heap := by
  apply Eval.call (function := .recursive group "claim-name" (captured counter table))
    (values := [.symbol name]) rfl (.variable (by simp [named, looping, outer, extend, tie, group, resolve]))
    (.cons (.variable (by simp [named, extend, resolve])) .nil)
  exact .recursive (definition := ⟨[s "name"], [claimBody]⟩) rfl rfl
    (.single (occupied (claim_ready (counter := counter)) present))

theorem claim_call_available (absent : heap table (.symbol name) = none) :
    Eval (named counter table owner name) heap (app "claim-name" [s "name"]) (.symbol name)
      (writeSymbol heap table name (.boolean true)) := by
  apply Eval.call (function := .recursive group "claim-name" (captured counter table))
    (values := [.symbol name]) rfl (.variable (by simp [named, looping, outer, extend, tie, group, resolve]))
    (.cons (.variable (by simp [named, extend, resolve])) .nil)
  exact .recursive (definition := ⟨[s "name"], [claimBody]⟩) rfl rfl (.single (available (claim_ready (counter := counter)) absent
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
theorem loop_eval (valid : Occupied heap table used)
    (contents : heap counter 0 = some (.natural count))
    (found : LoweringNames.search (candidate owner) used count fuel = some index) :
    Eval (looping counter table owner) heap loopBody (.symbol (candidate owner index))
      (writeSymbol (write heap counter 0 (.natural (index + 1))) table (candidate owner index) (.boolean true)) := by
  induction fuel generalizing heap count with
  | zero => simp [LoweringNames.search] at found
  | succ fuel ih =>
    apply Eval.letBody (expression_eval contents)
    refine Sequence.cons (increment_eval contents) (.single ?_)
    simp only [LoweringNames.search] at found
    split at found
    next collision =>
      have present : write heap counter 0 (.natural (count + 1)) table (.symbol (candidate owner count)) =
          some (.boolean true) := by simpa [write, collision] using valid (candidate owner count)
      apply Eval.disjunctionFalse (claim_call_occupied present) rfl
      apply Eval.call (function := .recursive [("loop", ⟨[], [loopBody]⟩)] "loop" (outer counter table owner))
        rfl (.variable (by simp [named, extend, looping, tie, resolve])) .nil
      apply Invoke.recursive (definition := ⟨[], [loopBody]⟩) rfl rfl
      apply Sequence.single
      simpa only [write_overwrite, looping] using ih
        (occupied_after_increment (counter := counter) (value := .natural (count + 1)) valid)
        (by simp [write]) found
    next free =>
      have same := Option.some.inj found
      subst index
      apply Eval.disjunctionTrue (claim_call_available ?_) rfl
      simpa [write, free] using valid (candidate owner count)

theorem fresh_eval (valid : Occupied heap table used)
    (contents : heap counter 0 = some (.natural count))
    (found : LoweringNames.search (candidate owner) used count fuel = some index) :
    Eval (outer counter table owner) heap (app "fresh" [s "owner"])
      (.symbol (candidate owner index))
      (writeSymbol (write heap counter 0 (.natural (index + 1))) table (candidate owner index) (.boolean true)) := by
  apply Eval.call (function := .recursive group "fresh" (captured counter table))
    (values := [.symbol owner]) rfl
    (.variable (by simp [outer, extend, tie, group, resolve]))
    (.cons (.variable (by simp [outer, extend, resolve])) .nil)
  apply Invoke.recursive (definition := ⟨[s "owner"], [freshBody]⟩) rfl rfl
  apply Sequence.single
  apply Eval.namedLoop rfl Arguments.nil rfl
  exact .single (loop_eval valid contents found)

/-- Every returning evaluation agrees with the operational derivation, not
only a chosen witness run or a finite collection of grammars. -/
theorem fresh_unique (valid : Occupied heap table used)
    (contents : heap counter 0 = some (.natural count))
    (found : LoweringNames.search (candidate owner) used count fuel = some index)
    (execution : Eval (outer counter table owner) heap (app "fresh" [s "owner"]) result after) :
    result = .symbol (candidate owner index) ∧
    after = writeSymbol (write heap counter 0 (.natural (index + 1))) table (candidate owner index) (.boolean true) :=
  execution.deterministic (fresh_eval valid contents found)

/-- The paper/protocol boundary is one complete allocation, including all
collisions. Encoding injectivity remains an explicit parser representation
obligation; it is not attributed to the external GC or compiler. -/
theorem allocation_simulation (valid : Occupied heap table before.used)
    (contents : heap counter 0 = some (.natural before.attempt))
    (injective : ∀ a b, candidate owner a = candidate owner b → a = b) (logicalOwner : Lowering.Owner) :
    ∃ index, candidate owner index ∉ before.used ∧ before.attempt ≤ index ∧
      Eval (outer counter table owner) heap (app "fresh" [s "owner"])
        (.symbol (candidate owner index))
        (writeSymbol (write heap counter 0 (.natural (index + 1))) table (candidate owner index) (.boolean true)) ∧
      Names.Trace before (LoweringNames.claimed before (candidate owner) logicalOwner index) := by
  obtain ⟨index, found⟩ := LoweringNames.search_total (candidate owner) injective before.used before.attempt
  obtain ⟨free, increasing, history⟩ := LoweringNames.search_trace logicalOwner found
  exact ⟨index, free, increasing, fresh_eval valid contents found, history⟩

end ParserEngine.LoweringFreshEvaluation
