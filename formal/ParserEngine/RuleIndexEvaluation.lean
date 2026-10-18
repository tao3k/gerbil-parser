import ParserEngine.SchemeTableEvaluation

/-! All same-owner declarations must survive mutable indexing. This source
execution obligation exposes an overwrite bug hidden by recognition controls. -/
namespace ParserEngine.RuleIndexEvaluation
open WalkSource SchemeTableEvaluation
abbrev Row := Nat × Value
def rowValue (row : Row) : Value := .list [.natural row.1, row.2]
def lookup (heap : Heap) (address key : Nat) : Value := (heap address key).getD (.list [])
def body : Datum := app "table-set!" [s "rows", app "car" [s "row"],
  app "cons" [app "cadr" [s "row"], app "table-ref" [s "rows", app "car" [s "row"], app "quote" [.list []]]]]
def loopSource : Datum := app "for-each" [app "lambda" [.list [s "row"], body], app "reverse" [s "rules"]]
def env (address : Nat) (rules : List Row) : Environment :=
  extend (extend (fun _ => none) "rows" (.table address)) "rules" (.list (rules.map rowValue))
def push (address : Nat) (row : Row) (heap : Heap) : Heap :=
  match lookup heap address row.1 with
  | .list values => write heap address row.1 (.list (row.2 :: values))
  | _ => heap -- Excluded by Buckets; not a native fallback.
def Buckets (address : Nat) (heap : Heap) : Prop :=
  ∀ key, ∃ values, lookup heap address key = .list values

theorem push_buckets (valid : Buckets address heap) (row : Row) : Buckets address (push address row heap) := by
  intro key
  obtain ⟨values, found⟩ := valid row.1
  by_cases same : key = row.1
  · subst key; exact ⟨row.2 :: values, by simp only [push, found]; simp [lookup, write]⟩
  · obtain ⟨other, read⟩ := valid key
    exact ⟨other, by simp only [push, found]; simpa [lookup, write, same] using read⟩

theorem push_eval (heap : Heap) (row : Row) (address : Nat) (rules : List Row)
    (valid : Buckets address heap) :
    Eval (extend (env address rules) "row" (rowValue row)) heap body .unit (push address row heap) := by
  obtain ⟨values, found⟩ := valid row.1
  have tableRead : Eval (extend (env address rules) "row" (rowValue row)) heap (s "rows") (.table address) heap :=
    .variable (by simp [resolve, env, extend])
  have rowRead : Eval (extend (env address rules) "row" (rowValue row)) heap (s "row") (rowValue row) heap :=
    .variable (by simp [resolve, extend, rowValue])
  have keyRead : Eval (extend (env address rules) "row" (rowValue row)) heap
      (app "car" [s "row"]) (.natural row.1) heap := .apply (.cons rowRead .nil) rfl (by simp [env, extend])
  have valueRead : Eval (extend (env address rules) "row" (rowValue row)) heap
      (app "cadr" [s "row"]) row.2 heap := .apply (.cons rowRead .nil) rfl (by simp [env, extend])
  have bucketRead : Eval (extend (env address rules) "row" (rowValue row)) heap
      (app "table-ref" [s "rows", app "car" [s "row"], app "quote" [.list []]]) (.list values) heap :=
    .apply (.cons tableRead (.cons keyRead (.cons .empty .nil)))
      (by simpa [primitive, lookup] using congrArg (fun value => some (value, heap)) found) (by simp [env, extend])
  have prepend : Eval (extend (env address rules) "row" (rowValue row)) heap
      (app "cons" [app "cadr" [s "row"], app "table-ref" [s "rows", app "car" [s "row"], app "quote" [.list []]]])
      (.list (row.2 :: values)) heap := .apply (.cons valueRead (.cons bucketRead .nil)) rfl (by simp [env, extend])
  exact .apply (.cons tableRead (.cons keyRead (.cons prepend .nil))) (by simp [primitive, push, found]) (by simp [env, extend])

def execute (address : Nat) (heap : Heap) : List Row → Heap
  | [] => heap
  | row :: rest => execute address (push address row heap) rest

theorem each_eval (heap : Heap) (address : Nat) (rules rows : List Row) (valid : Buckets address heap) :
    Each (env address rules) "row" [body] heap (rows.map rowValue) (execute address heap rows) := by
  induction rows generalizing heap with
  | nil => exact .nil
  | cons row rest ih =>
    exact .cons (.single (push_eval heap row address rules valid))
      (ih (push address row heap) (push_buckets valid row))

theorem source_execution (heap : Heap) (address : Nat) (rules : List Row) (valid : Buckets address heap) :
    Eval (env address rules) heap loopSource .unit (execute address heap rules.reverse) := by
  have input : Eval (env address rules) heap (s "rules") (.list (rules.map rowValue)) heap :=
    .variable (by simp [resolve, env, extend])
  have reversed : Eval (env address rules) heap (app "reverse" [s "rules"])
      (.list (rules.reverse.map rowValue)) heap := .apply (.cons input .nil) (by simp [primitive]) (by simp [env, extend])
  exact .forEach reversed (each_eval heap address rules rules.reverse valid) (by simp [env, extend])

def selected (key : Nat) (rows : List Row) : List Value :=
  rows.filterMap (fun row => if row.1 = key then some row.2 else none)

theorem execute_read (valid : Buckets address heap) (rows : List Row) (key : Nat)
    (initial : lookup heap address key = .list values) :
    lookup (execute address heap rows) address key = .list ((selected key rows).reverse ++ values) := by
  induction rows generalizing heap values with
  | nil => simpa [execute, selected] using initial
  | cons row rest ih =>
    obtain ⟨bucket, read⟩ := valid row.1
    by_cases same : row.1 = key
    · subst key
      have changed : lookup (push address row heap) address row.1 = .list (row.2 :: values) := by
        simp only [push, initial]
        simp [lookup, write]
      simpa [execute, selected, List.filterMap_cons, List.reverse_cons, List.append_assoc] using
        ih (push_buckets valid row) changed
    · have unchanged : lookup (push address row heap) address key = .list values := by
        simp only [push, read]
        simpa [lookup, write, Ne.symm same] using initial
      simpa [execute, selected, List.filterMap_cons, same] using ih (push_buckets valid row) unchanged

/-- Complete original order and multiplicity, including an arbitrary old tail. -/
theorem complete (valid : Buckets address heap) (rows : List Row) (key : Nat)
    (initial : lookup heap address key = .list values) :
    lookup (execute address heap rows.reverse) address key = .list (selected key rows ++ values) := by
  simpa [selected] using execute_read valid rows.reverse key initial

theorem frame (rows : List Row) (different : other ≠ address) :
    execute address heap rows other key = heap other key := by
  induction rows generalizing heap with
  | nil => rfl
  | cons row rest ih =>
    rw [execute, ih]
    cases read : lookup heap address row.1 <;> simp [push, read, write, different]

/-- Counterexample to using a scalar overwrite for a declaration multimap. -/
theorem overwrite_loses_first (first last : Value) (different : first ≠ last) :
    (write (write heap address key first) address key last) address key ≠ some first := by
  simp only [write, and_self, ↓reduceIte, ne_eq, Option.some.injEq]
  exact Ne.symm different
end ParserEngine.RuleIndexEvaluation
