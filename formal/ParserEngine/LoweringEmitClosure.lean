import ParserEngine.LoweringEmitterEvaluation

/-! Every unobserved recursive lowering call binds the same local emit source.
Its caller metadata remains captured while the five emission arguments shadow
only their own names. The target is the real root emitter, not an abstract step. -/
namespace ParserEngine.LoweringEmitClosure
open WalkSource SchemeTableEvaluation LoweringProductionEvaluation LoweringEmitterEvaluation

/-- The actual lower-to parameter order, including isolated/default arguments. -/
def caller (env : Environment) (base : Nat) (context : LoweringEmitterEvaluation.Context) (value : Payload) (isolated : Bool) : Environment :=
  extend (extend (extend (extend (extend (extend (extend (extend (root env base)
    "lhs" (.symbol value.lhs)) "owner" (.symbol context.owner)) "expression" (datumValue context.expression))
    "precedence" (datumValue value.precedence)) "path" (datumValue context.path))
    "isolated?" (.boolean isolated)) "context" (.boolean false)) "recurrence" (.boolean false)

def callerValues (context : LoweringEmitterEvaluation.Context) (value : Payload) (isolated : Bool) : List Value :=
  [.symbol value.lhs, .symbol context.owner, datumValue context.expression, datumValue value.precedence,
    datumValue context.path, .boolean isolated, .boolean false, .boolean false]

def localGroup : List (String × Procedure) := [("emit", LoweringEntryEvaluation.emitter)]
def function (env : Environment) (base : Nat) (context : LoweringEmitterEvaluation.Context) (value : Payload) (isolated : Bool) : Value :=
  .recursive localGroup "emit" (caller env base context value isolated)

def arguments (env : Environment) (base : Nat) (context : LoweringEmitterEvaluation.Context) (value : Payload) (isolated : Bool) : Environment :=
  extend (extend (extend (extend (extend (tie localGroup (caller env base context value isolated))
    "lhs" (.symbol value.lhs)) "rhs" (.list value.rhs)) "action" (datumValue value.action))
    "precedence" (datumValue value.precedence)) "proof" (.boolean false)

set_option maxRecDepth 16384 in
theorem caller_bound : bind LoweringEntryEvaluation.definition.parameters (callerValues context value isolated)
    (root env base) = some (caller env base context value isolated) := rfl

/- No execution of context/proof closures is assumed: the real and form
short-circuits before reading or invoking them in the unobserved profile. -/
set_option maxRecDepth 16384 in
theorem invoke (id : Nat) (listPrimitive : env "list" = none) (consPrimitive : env "cons" = none)
    (plusPrimitive : env "+" = none)
    (count : before base 0 = some (.natural id))
    (contents : before (base + 4) 0 = some (.list previous))
    (context : LoweringEmitterEvaluation.Context) (value : Payload) (isolated : Bool) :
    Invoke before (function env base context value isolated)
      [.symbol value.lhs, .list value.rhs, datumValue value.action, datumValue value.precedence, .boolean false]
      .unit (committed before base id previous value) := by
  apply Invoke.recursive (definition := LoweringEntryEvaluation.emitter)
    (entered := arguments env base context value isolated) rfl rfl
  apply SchemeTableEvaluation.Sequence.single
  apply Eval.call (function := .recursive LoweringRootScope.group "emit-production"
    (LoweringRootStorage.environment (globals env) base)) (middle := before) (ready := before) (values := values context value) rfl
  · apply Eval.variable; rfl
  · refine .cons (.variable (by rfl)) (.cons (.variable (by rfl))
      (.cons (.variable ?_) (.cons (.variable ?_) (.cons (.variable (by rfl))
      (.cons (.variable ?_) (.cons (.variable ?_) (.cons ?_ .nil)))))))
    · apply resolve_datum; rfl
    · apply resolve_datum; rfl
    · apply resolve_datum; rfl
    · apply resolve_datum; rfl
    · exact .conjunctionFalse (.variable (by rfl)) rfl
  · exact LoweringEmitterEvaluation.invoke id listPrimitive consPrimitive plusPrimitive count contents context value

end ParserEngine.LoweringEmitClosure
