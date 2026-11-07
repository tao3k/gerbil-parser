import ParserEngine.LoweringEntryEvaluation
import ParserEngine.LoweringPublicationWitness

/-! The native constructor is a five-field list, not an opaque replacement
record. Execute its actual body before relating emitter writes to publication. -/
namespace ParserEngine.LoweringProductionEvaluation
open WalkSource SchemeTableEvaluation

structure Payload where
  lhs : String
  rhs : List Value
  action : Datum
  precedence : Datum

def record (id : Nat) (value : Payload) : Value :=
  .list [.natural id, .symbol value.lhs, .list value.rhs, datumValue value.action, datumValue value.precedence]

def definition : Procedure :=
  ((LoweringClaimEvaluation.procedure LoweringPublicationWitness.constructorSource).map Prod.snd).getD ⟨[], []⟩

def group : List (String × Procedure) := [("make-production", definition)]
def function (captured : Environment) : Value := .recursive group "make-production" captured

def arguments (env : Environment) (id : Nat) (value : Payload) : Environment :=
  extend (extend (extend (extend (extend env "id" (.natural id)) "lhs" (.symbol value.lhs))
    "rhs" (.list value.rhs)) "action" (datumValue value.action)) "precedence" (datumValue value.precedence)

/-- Reader data never denotes the model's internal cell-reference binding. -/
theorem resolve_datum (read : env name = some (datumValue value)) :
    resolve env heap name = some (datumValue value) := by
  cases value <;> simp_all [resolve, datumValue]

set_option maxRecDepth 8192 in
theorem source_constructor : definition =
    ⟨[s "id", s "lhs", s "rhs", s "action", s "precedence"],
      [app "list" [s "id", s "lhs", s "rhs", s "action", s "precedence"]]⟩ := rfl

/-- A real declaration binding supplies the constructor value. Other module
bindings remain in captured; no independently specified constructor algorithm
or result-to-input equality is assumed. -/
theorem invoke (listPrimitive : captured "list" = none) (id : Nat) (value : Payload) :
    Invoke heap (function captured)
      [.natural id, .symbol value.lhs, .list value.rhs, datumValue value.action, datumValue value.precedence]
      (record id value) heap := by
  apply Invoke.recursive (definition := definition) (entered := arguments (tie group captured) id value) rfl rfl
  apply SchemeTableEvaluation.Sequence.single
  apply Eval.apply (values := [.natural id, .symbol value.lhs, .list value.rhs,
    datumValue value.action, datumValue value.precedence]) _ rfl _
  · refine .cons (.variable ?_) (.cons (.variable ?_) (.cons (.variable ?_)
      (.cons (.variable ?_) (.cons (.variable ?_) .nil))))
    · simp [arguments, extend, resolve]
    · simp [arguments, extend, resolve]
    · simp [arguments, extend, resolve]
    · apply resolve_datum; simp [arguments, extend]
    · apply resolve_datum; simp [arguments, extend]
  · simpa [arguments, extend, tie, group, List.lookup] using listPrimitive

end ParserEngine.LoweringProductionEvaluation
