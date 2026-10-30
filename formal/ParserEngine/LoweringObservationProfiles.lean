import ParserEngine.LoweringObservedClosure

/-! A source-executed proof-only helper profile. The actual identity context
and a returning diagnostic callback establish their own frame behavior; no
callback result or unchanged store is assumed by this regression theorem. -/
namespace ParserEngine.LoweringObservationProfiles
open WalkSource SchemeTableEvaluation LoweringProductionEvaluation LoweringObserverEvaluation

private def observers : Observers := ⟨.disabled,
  .closure [s "kind", s "production", s "proof"] [quoted "observed"] (fun _ => none)⟩

private def context : Callback := .closure [s "proof"] [s "proof"] (fun _ => none)

/-- Any record payload, counter and existing prefix, with the native false path
used when only proof observation is enabled. The callback returns a symbol,
not void; the source discards it at the correct sequence boundary. -/
theorem helper_proof_only (id : Nat)
    (listPrimitive : env "list" = none) (consPrimitive : env "cons" = none)
    (plusPrimitive : env "+" = none)
    (owner : String) (expression recipe : Datum) (value : Payload)
    (count : before base 0 = some (.natural id))
    (contents : before (base + 4) 0 = some (.list previous)) :
    Invoke before (LoweringObservedClosure.function env base observers context owner (.boolean false) expression value)
      [.symbol value.lhs, .list value.rhs, datumValue value.action, datumValue value.precedence, datumValue recipe]
      .unit (LoweringEmitterEvaluation.committed before base id previous value) := by
  have applied : LoweringObservedClosure.Application observers.proof context recipe before recipe before := by
    apply LoweringObservedClosure.Application.enabled (by rfl)
    apply Invoke.closure (entered := extend (fun _ => none) "proof" (datumValue recipe)) rfl
    apply Sequence.single
    apply Eval.variable
    apply resolve_datum
    rfl
  have observed : Notification observers.proof before (proofValues id recipe value) (.symbol "observed") before := by
    apply Notification.returned (by rfl)
    apply Invoke.closure (entered := extend (extend (extend (fun _ => none)
      "kind" (.symbol "production")) "production" (record id value)) "proof" (datumValue recipe)) rfl
    exact .single .quoteSymbol
  exact LoweringObservedClosure.invoke observers context id id listPrimitive consPrimitive plusPrimitive (by intro impossible; cases impossible)
    owner (.boolean false) expression recipe recipe value applied count .disabled observed count contents

end ParserEngine.LoweringObservationProfiles
