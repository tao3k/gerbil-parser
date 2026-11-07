import ParserEngine.LoweringObserverEvaluation
import ParserEngine.SchemeMutationTrace

/-! The local emit closure evaluates its context before invoking the root
emitter. That context may affect the heap; allocation reads its resulting heap,
not a counter snapshot taken before argument evaluation. -/
namespace ParserEngine.LoweringObservedClosure
open WalkSource SchemeTableEvaluation LoweringProductionEvaluation
open LoweringObserverEvaluation

inductive Application : Callback → Callback → Datum → Heap → Datum → Heap → Prop where
  | disabled : Application .disabled context recipe heap (.boolean false) heap
  | enabled (truth : truthy observer.value = true)
      (execution : Invoke before context.value [datumValue recipe] (datumValue proof) after) :
      Application observer context recipe before proof after

theorem Application.evaluates (run : Application observer context recipe before proof after)
    (observerRead : resolve env before "observe-proof" = some observer.value)
    (contextRead : resolve env before "context" = some context.value)
    (recipeRead : resolve env before "proof" = some (datumValue recipe)) :
    Eval env before (app "and" [s "observe-proof", app "context" [s "proof"]]) (datumValue proof) after := by
  cases run with
  | disabled => exact .conjunctionFalse (.variable observerRead) rfl
  | enabled truth execution =>
      apply Eval.conjunctionTrue (.variable observerRead) truth
      exact .call rfl (.variable contextRead) (.cons (.variable recipeRead) .nil) execution

def caller (env : Environment) (base : Nat) (observers : Observers) (context : Callback)
    (owner : String) (path : Datum) (expression : Datum) (value : Payload) : Environment :=
  extend (extend (extend (extend (extend (extend (extend (extend
    (LoweringObserverEvaluation.root env base observers) "lhs" (.symbol value.lhs)) "owner" (.symbol owner))
    "expression" (datumValue expression)) "precedence" (datumValue value.precedence))
    "path" (datumValue path)) "isolated?" (.boolean false)) "context" context.value) "recurrence" (.boolean false)

def localGroup := [("emit", LoweringEntryEvaluation.emitter)]

def function (env : Environment) (base : Nat) (observers : Observers) (context : Callback)
    (owner : String) (path : Datum) (expression : Datum) (value : Payload) : Value :=
  .recursive localGroup "emit" (caller env base observers context owner path expression value)

def arguments (env : Environment) (base : Nat) (observers : Observers) (context : Callback)
    (owner : String) (path : Datum) (expression recipe : Datum) (value : Payload) : Environment :=
  extend (extend (extend (extend (extend
    (tie localGroup (caller env base observers context owner path expression value))
    "lhs" (.symbol value.lhs)) "rhs" (.list value.rhs)) "action" (datumValue value.action))
    "precedence" (datumValue value.precedence)) "proof" (datumValue recipe)

set_option maxRecDepth 16384 in
theorem caller_bound : bind LoweringEntryEvaluation.definition.parameters
    [.symbol value.lhs, .symbol owner, datumValue expression, datumValue value.precedence,
      datumValue path, .boolean false, context.value, .boolean false]
    (LoweringObserverEvaluation.root env base observers) =
    some (caller env base observers context owner path expression value) := rfl

/-- Actual source argument order: context first, constructor and observers next,
then the two publication writes. No unchanged-context-heap premise is required. -/
theorem invoke (observers : Observers) (context : Callback) (id next : Nat)
    (listPrimitive : env "list" = none) (consPrimitive : env "cons" = none)
    (plusPrimitive : env "+" = none) (reversePrimitive : truthy observers.origin.value = true → env "reverse" = none)
    (owner : String) (path : Datum) (expression recipe proof : Datum) (value : Payload)
    (applied : Application observers.proof context recipe before proof initialized)
    (count : initialized base 0 = some (.natural id))
    (origin : Origin id owner expression value observers.origin path initialized originResult middle)
    (observed : Notification observers.proof middle (proofValues id proof value) proofResult ready)
    (liveCount : ready base 0 = some (.natural next))
    (liveRecords : ready (base + 4) 0 = some (.list previous)) :
    Invoke before (function env base observers context owner path expression value)
      [.symbol value.lhs, .list value.rhs, datumValue value.action, datumValue value.precedence, datumValue recipe]
      .unit (published ready base next (record id value) previous) := by
  let named := arguments env base observers context owner path expression recipe value
  have lastArgument : Eval named before
      (app "and" [s "observe-proof", app "context" [s "proof"]]) (datumValue proof) initialized := by
    apply applied.evaluates
    · apply Callback.resolves; rfl
    · apply Callback.resolves; rfl
    · apply resolve_datum; rfl
  apply Invoke.recursive (definition := LoweringEntryEvaluation.emitter) (entered := named) rfl rfl
  apply Sequence.single
  apply Eval.call (function := .recursive LoweringRootScope.group "emit-production"
      (LoweringRootStorage.environment (LoweringObserverEvaluation.globals env observers) base))
    (middle := before) (ready := initialized)
    (values := [.symbol value.lhs, .list value.rhs, datumValue value.action, datumValue value.precedence,
      .symbol owner, datumValue path, datumValue expression, datumValue proof]) rfl
  · exact .variable (by rfl)
  · refine .cons (.variable (by rfl)) (.cons (.variable (by rfl))
      (.cons (.variable ?_) (.cons (.variable ?_) (.cons (.variable (by rfl))
      (.cons (.variable ?_) (.cons (.variable ?_) (.cons lastArgument .nil)))))))
    all_goals apply resolve_datum; rfl
  · exact LoweringObserverEvaluation.invoke observers id next listPrimitive consPrimitive plusPrimitive reversePrimitive count
      owner path expression proof value origin observed liveCount liveRecords

/-- This is a generated physical store history for the whole captured chain,
including recursive callback and context calls, not an externally supplied log. -/
theorem effects (execution : Invoke before (function env base observers context owner path expression value) argv result after) :
    SchemeMutationTrace.Generated before after := SchemeMutationTrace.invoke_generated execution

end ParserEngine.LoweringObservedClosure
