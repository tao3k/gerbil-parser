import ParserEngine.RuleIndexEvaluation
import ParserEngine.SchemeEvaluationDeterminism
import ParserEngine.CompilerSemanticsSource
import ParserEngine.LoweringTraversalData

/-! Source extraction supplies syntax only. Operational evaluation and the
all-declaration property are proved separately; no schema-to-algorithm shortcut. -/
namespace ParserEngine.RuleIndexSource
open WalkSource NativeActions.Source CompilerSemanticsSource RuleIndexEvaluation SchemeTableEvaluation

def definition : Datum := defn "rule-index-by-lhs" ["rules"] [
  app "let" [binding "rows" (app "make-table" [.keyword "test", s "eq?"]), loopSource, s "rows"]]

def extractLoop : Datum → Option Datum
  | .list [_, _, .list [_, _, loop, _]] => some loop
  | _ => none

theorem source_checked : definitionSource = definition := rfl

theorem loop_extracted : extractLoop definitionSource = some loopSource := rfl

/-- Every returning source-loop evaluation has the same exact physical heap,
not just a model-selected execution. Fresh-table allocation is external;
faithful symbol-key transport remains a parser representation premise. -/
theorem simulate {rows : List Row} (source : Datum) (extracted : extractLoop definitionSource = some source)
    (valid : Buckets address before)
    (evaluated : Eval (env address rows) before source result after) :
    result = .unit ∧ after = execute address before rows.reverse := by
  rw [loop_extracted] at extracted
  cases Option.some.inj extracted
  exact Eval.deterministic evaluated (source_execution before address rows valid)

/-- Arbitrary source rules, including duplicate equal bodies and cyclic
references represented as payloads: no matching declaration can disappear. -/
theorem complete {rows : List Row} (source : Datum) (extracted : extractLoop definitionSource = some source)
    (valid : Buckets address before)
    (evaluated : Eval (env address rows) before source result after)
    (empty : ∀ key, lookup before address key = .list []) :
    result = .unit ∧ (∀ key, lookup after address key = .list (selected key rows)) ∧
      (∀ other key, other ≠ address → after other key = before other key) := by
  obtain ⟨rfl, rfl⟩ := simulate source extracted valid evaluated
  exact ⟨rfl, fun key => by simpa using RuleIndexEvaluation.complete valid rows key (empty key),
    fun _ _ different => frame _ different⟩

end ParserEngine.RuleIndexSource
