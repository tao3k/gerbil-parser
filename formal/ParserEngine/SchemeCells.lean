import ParserEngine.SchemeTableEvaluation
import ParserEngine.Names

/-! A captured variable denotes a storage location. A call reads the current
contents, while unrelated caller bindings cannot change lexical resolution. -/
namespace ParserEngine.SchemeCells
open WalkSource SchemeTableEvaluation

def increment (name : String) : Datum := app "set!" [s name, app "+" [s name, n 1]]
def incrementBody (name : String) : List Datum := [increment name, s name]

theorem read_captured (held : captured name = some (.cell address))
    (contents : heap address 0 = some value) : Eval captured heap (s name) value heap := by
  apply Eval.variable
  simp only [resolve, held]
  exact contents

theorem increment_eval (held : captured name = some (.cell address))
    (addition : captured "+" = none) (contents : heap address 0 = some (.natural count)) :
    Eval captured heap (increment name) .unit (write heap address 0 (.natural (count + 1))) := by
  apply Eval.assignment held
  exact .apply (.cons (read_captured held contents) (.cons .natural .nil)) rfl addition

/-- The closure's saved environment is used; the caller does not supply its
counter value again. Every call reads the same cell in the new heap. -/
theorem closure_increment (held : captured name = some (.cell address))
    (addition : captured "+" = none) (contents : heap address 0 = some (.natural count)) :
    Invoke heap (.closure [] (incrementBody name) captured) [] (.natural (count + 1))
      (write heap address 0 (.natural (count + 1))) := by
  apply Invoke.closure rfl
  refine Sequence.cons (increment_eval held addition contents) (.single ?_)
  apply read_captured held
  simp [write]

/-- Any caller environment, including one shadowing the counter's name, invokes
the captured cell. This is lexical scope rather than dynamic scope. -/
theorem call_increment (procedure : resolve caller heap function = some (.closure [] (incrementBody name) captured))
    (allowed : callable (s function) = true) (held : captured name = some (.cell address))
    (addition : captured "+" = none) (contents : heap address 0 = some (.natural count)) :
    Eval caller heap (app function []) (.natural (count + 1))
      (write heap address 0 (.natural (count + 1))) :=
  .call allowed (.variable procedure) .nil (closure_increment held addition contents)

/-- Two separately constructed closures share an address, not a copied value. -/
theorem shared_counter (first : left name = some (.cell address)) (second : right name = some (.cell address))
    (leftAddition : left "+" = none) (rightAddition : right "+" = none)
    (contents : heap address 0 = some (.natural count)) :
    Invoke heap (.closure [] (incrementBody name) left) [] (.natural (count + 1))
      (write heap address 0 (.natural (count + 1))) ∧
    Invoke (write heap address 0 (.natural (count + 1))) (.closure [] (incrementBody name) right) []
      (.natural (count + 2)) (write (write heap address 0 (.natural (count + 1))) address 0 (.natural (count + 2))) := by
  constructor
  · exact closure_increment first leftAddition contents
  · simpa [Nat.add_assoc] using closure_increment second rightAddition
      (heap := write heap address 0 (.natural (count + 1))) (count := count + 1) (by simp [write])

theorem write_frame (different : other ≠ address) : (write heap address index value) other key = heap other key := by
  simp [write, different]

/-- The source increments before claim. That intermediate state cannot be
mistaken for a completed abstract retry or claim when the used set is empty. -/
theorem empty_trace_counter (history : Names.Trace before after) (empty : after.used = []) :
    after.attempt = before.attempt := by
  induction history with
  | refl => rfl
  | retry previous collision ih =>
    dsimp only at empty
    rw [empty] at collision
    cases collision
  | claim previous fresh ih => cases empty

theorem increment_not_atomic (counter : Nat) :
    ¬ Names.Trace (⟨[], [], counter⟩ : Names.State String) ⟨[], [], counter + 1⟩ := by
  intro history
  have unchanged := empty_trace_counter history rfl
  simp at unchanged
end ParserEngine.SchemeCells
