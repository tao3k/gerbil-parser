import ParserEngine.SchemeEvaluationDeterminism
import ParserEngine.LoweringProtocol

/-! Actual evaluation of the declaration collector, composed for arbitrary
callback streams. Full lower-to dispatch adequacy remains a separate obligation;
this does not replace it with a source-shape check. -/
namespace ParserEngine.LoweringDeclarationEvaluation
open WalkSource SchemeTableEvaluation

def slot : Datum := app "when" [s "declare-name", app "declare-name" [s "name"]]
def body : Datum := app "set!" [s "helpers", app "cons" [s "name", s "helpers"]]
def collector (captured : Environment) (address : Nat) : Value :=
  .closure [s "name"] [body] (extend captured "helpers" (.cell address))
def environment (captured : Environment) (address : Nat) (name : String) : Environment :=
  extend (extend captured "declare-name" (collector captured address)) "name" (.symbol name)

/-- Read the callback site from the real extracted lower-symbol declaration. -/
def extractedSlot : Datum :=
  match LoweringNamesSource.symbolSource with
  | .list [_, _, .list [_, _, _, _, _, .list [_, .list (_ :: _ :: callback :: _)]]] => callback
  | _ => .boolean false

theorem extracted : extractedSlot = slot := rfl

/-- The closure captures the mutable cell, not the list's earlier value. A
normal return invokes it once and conses the name before recursive lowering. -/
theorem slot_eval (unshadowed : captured "cons" = none) (contents : before address 0 = some (.list names)) :
    Eval (environment captured address name) before slot .unit
      (write before address 0 (.list (.symbol name :: names))) := by
  apply Eval.whenTrue (value := collector captured address)
    (.variable (by simp [environment, extend, resolve, collector])) rfl
  apply Sequence.single
  apply Eval.call (function := collector captured address) (values := [.symbol name]) rfl
    (.variable (by simp [environment, extend, resolve, collector]))
    (.cons (.variable (by simp [environment, extend, resolve])) .nil)
  apply Invoke.closure (entered := extend (extend captured "helpers" (.cell address)) "name" (.symbol name)) rfl
  apply Sequence.single
  apply Eval.assignment (by simp [extend])
  apply Eval.apply (values := [.symbol name, .list names])
  · exact .cons (.variable (by simp [extend, resolve]))
      (.cons (.variable (by simpa [extend, resolve] using contents)) .nil)
  · rfl
  · simpa [extend] using unshadowed

inductive Calls (captured : Environment) (address : Nat) : Heap → List String → Heap → Prop where
  | nil : Calls captured address heap [] heap
  | cons (head : Eval (environment captured address name) before slot .unit middle)
      (tail : Calls captured address middle names after) : Calls captured address before (name :: names) after

theorem overwrite : write (write heap address 0 first) address 0 last = write heap address 0 last := by
  funext other key
  simp only [write]
  split <;> simp_all

/-- No bound on nesting or callback count, and no supplied final heap relation. -/
theorem collect (names : List String) (unshadowed : captured "cons" = none) (contents : before address 0 = some (.list previous)) :
    Calls captured address before names
      (write before address 0 (.list ((names.map Value.symbol).reverse ++ previous))) := by
  induction names generalizing before previous with
  | nil =>
      have same : write before address 0 (.list previous) = before := by
        funext other key
        simp only [write]
        split
        next equal => obtain ⟨rfl, rfl⟩ := equal; exact contents.symm
        next => rfl
      simpa [same] using (Calls.nil (captured := captured) (address := address) (heap := before))
  | cons name names ih =>
      have tail := ih (before := write before address 0 (.list (.symbol name :: previous)))
        (previous := .symbol name :: previous) (by simp [write])
      have combined := Calls.cons (slot_eval (captured := captured) (name := name) unshadowed contents) tail
      simpa [List.map_cons, List.reverse_cons, List.append_assoc, overwrite] using combined

/-- Every returning execution has the generated result, including nonempty
initial collectors. This is determinism of Scheme evaluation, not a trace oracle. -/
theorem unique (run : Calls captured address before names after)
    (other : Calls captured address before names last) : after = last := by
  induction run generalizing last with
  | nil => cases other; rfl
  | cons head tail ih =>
      cases other with
      | cons otherHead otherTail =>
        obtain ⟨_, rfl⟩ := head.deterministic otherHead
        exact ih otherTail

/-- Compose arbitrary generated call trees with actual callback closure
execution. The parser's complete dispatcher-to-Eval simulation is not assumed. -/
theorem generated_collector
    (execution : LoweringCallStack.Execution (LoweringProtocol.Program rules root) (LoweringProtocol.Start rules root))
    (encode : Nat → String) (unshadowed : captured "cons" = none) (contents : before address 0 = some (.list previous)) :
    Calls captured address before
      ((LoweringCallStack.declarations execution.events).map (fun binding => encode binding.1))
      (write before address 0 (.list
        ((execution.after.names.bindings.reverse.map (fun binding => Value.symbol (encode binding.1))).reverse ++ previous))) := by
  rw [LoweringProtocol.callback_bindings]
  simpa [List.map_map, Function.comp_def] using
    collect (captured := captured) (execution.after.names.bindings.reverse.map (fun binding => encode binding.1)) unshadowed contents

end ParserEngine.LoweringDeclarationEvaluation
