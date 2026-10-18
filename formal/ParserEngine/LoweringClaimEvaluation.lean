import ParserEngine.LoweringNamesSource
import ParserEngine.SchemeEvaluationDeterminism

/-! Actual claim-name evaluation, including occupied and free branches. Source
extraction supplies a procedure body; no source hash selects an algorithm. -/
namespace ParserEngine.LoweringClaimEvaluation
open WalkSource SchemeTableEvaluation

def procedure : Datum → Option (String × Procedure)
  | .list [.symbol "def", .list (.symbol name :: parameters), body] =>
      some (name, ⟨parameters, [body]⟩)
  | _ => none

def claimBody : Datum := app "and" [
  app "not" [app "table-ref" [s "used-names", s "name", b false]],
  app "begin" [app "table-set!" [s "used-names", s "name", b true], s "name"]]

theorem extracted_claim : procedure LoweringNamesSource.claimSource =
    some ("claim-name", ⟨[s "name"], [claimBody]⟩) := rfl

structure Ready (env : Environment) (heap : Heap) (address : Nat) (name : String) : Prop where
  table : resolve env heap "used-names" = some (.table address)
  argument : resolve env heap "name" = some (.symbol name)
  lookup : env "table-ref" = none
  store : env "table-set!" = none
  negate : env "not" = none

theorem lookup_eval (ready : Ready env heap address name) :
    Eval env heap (app "table-ref" [s "used-names", s "name", b false])
      ((heap address (.symbol name)).getD (.boolean false)) heap :=
  .apply (.cons (.variable ready.table) (.cons (.variable ready.argument) (.cons .boolean .nil)))
    rfl ready.lookup

theorem occupied (ready : Ready env heap address name)
    (present : heap address (.symbol name) = some (.boolean true)) :
    Eval env heap claimBody (.boolean false) heap := by
  have read : Eval env heap (app "table-ref" [s "used-names", s "name", b false]) (.boolean true) heap := by
    simpa [present] using lookup_eval ready
  exact .conjunctionFalse (.apply (.cons read .nil) rfl ready.negate) rfl

theorem available (ready : Ready env heap address name)
    (absent : heap address (.symbol name) = none)
    (preserved : resolve env (writeSymbol heap address name (.boolean true)) "name" = some (.symbol name)) :
    Eval env heap claimBody (.symbol name) (writeSymbol heap address name (.boolean true)) := by
  have read : Eval env heap (app "table-ref" [s "used-names", s "name", b false]) (.boolean false) heap := by
    simpa [absent] using lookup_eval ready
  refine .conjunctionTrue (.apply (.cons read .nil) rfl ready.negate) rfl (.begin ?_)
  exact .cons (.apply (.cons (.variable ready.table) (.cons (.variable ready.argument)
    (.cons .boolean .nil))) rfl ready.store) (.single (.variable preserved))

/-- The lexical parameter binding is a value, so a table write cannot alter it.
This discharges preservation rather than assuming it at the invocation edge. -/
theorem argument_preserved (parameter : env "name" = some (.symbol name)) :
    resolve env (writeSymbol heap address name (.boolean true)) "name" = some (.symbol name) := by
  simp [resolve, parameter]

theorem occupied_unique (ready : Ready env heap address name)
    (present : heap address (.symbol name) = some (.boolean true))
    (execution : Eval env heap claimBody result after) : result = .boolean false ∧ after = heap := by
  exact (execution.deterministic (occupied ready present))

theorem available_unique (ready : Ready env heap address name)
    (absent : heap address (.symbol name) = none) (parameter : env "name" = some (.symbol name))
    (execution : Eval env heap claimBody result after) :
    result = .symbol name ∧ after = writeSymbol heap address name (.boolean true) :=
  execution.deterministic (available ready absent (argument_preserved parameter))

end ParserEngine.LoweringClaimEvaluation
