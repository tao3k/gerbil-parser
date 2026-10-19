import ParserEngine.SignedSourceMetadata

/-! Execute the real observed emitter. Callback executions are operational
Invoke judgments, not boolean outcomes. Their heaps feed the subsequent source
reads, exposing the exact noninterference obligation for dense publication. -/
namespace ParserEngine.LoweringObserverEvaluation
open WalkSource SchemeTableEvaluation LoweringProductionEvaluation LoweringEmitterEvaluation

inductive Callback where
  | disabled
  | closure (parameters : List Datum) (body : List Datum) (captured : Environment)
  | recursive (group : List (String × Procedure)) (name : String) (captured : Environment)

def Callback.value : Callback → Value
  | .disabled => .boolean false
  | .closure parameters body captured => .closure parameters body captured
  | .recursive group name captured => .recursive group name captured

theorem Callback.resolves (cb : Callback) (bound : env name = some cb.value) :
    resolve env heap name = some cb.value := by
  unfold resolve
  rw [bound]
  cases cb <;> rfl

inductive Notification : Callback → Heap → List Value → Value → Heap → Prop where
  | disabled : Notification .disabled heap argv .unit heap
  | returned (enabled : truthy cb.value = true)
      (execution : Invoke before cb.value argv result after) :
      Notification cb before argv result after

/-- The native when/call path executes the actual callback exactly once. -/
theorem Notification.evaluates (run : Notification cb before argv result after)
    (condition : resolve env before name = some cb.value)
    (arguments : Arguments env before expressions argv before)
    (allowed : callable (s name) = true) :
    Eval env before (app "when" [s name, app name expressions]) result after := by
  cases run with
  | disabled => exact .whenFalse (.variable condition) rfl
  | @returned cb before argv result after enabled execution =>
      apply Eval.whenTrue (.variable condition) enabled
      apply Sequence.single
      exact .call allowed (.variable condition) arguments execution

structure Observers where
  origin : Callback
  proof : Callback

def globals (env : Environment) (observers : Observers) : Environment :=
  extend (extend (tie LoweringProductionEvaluation.group env) "observe-origin" observers.origin.value)
    "observe-proof" observers.proof.value

def root (env : Environment) (base : Nat) (observers : Observers) : Environment :=
  tie LoweringRootScope.group (LoweringRootStorage.environment (globals env observers) base)

def arguments (env : Environment) (base : Nat) (observers : Observers)
    (owner : String) (path : List Datum) (expression proof : Datum) (value : Payload) : Environment :=
  extend (extend (extend (extend (extend (extend (extend (extend (root env base observers)
    "lhs" (.symbol value.lhs)) "rhs" (.list value.rhs)) "action" (datumValue value.action))
    "precedence" (datumValue value.precedence)) "owner" (.symbol owner)) "path" (datumValue (.list path)))
    "expression" (datumValue expression)) "proof" (datumValue proof)

def originValues (id : Nat) (owner : String) (path : List Datum) (expression : Datum) (value : Payload) : List Value :=
  [record id value, .symbol owner, .list (datumValues path).reverse, datumValue expression]

def proofValues (id : Nat) (proof : Datum) (value : Payload) : List Value :=
  [.symbol "production", record id value, datumValue proof]

def published (heap : Heap) (base next : Nat) (production : Value) (previous : List Value) : Heap :=
  write (write heap base 0 (.natural (next + 1))) (base + 4) 0 (.list (production :: previous))

/- Neither the final heap nor observer noninterference is an input oracle.
Each cb executes against its predecessor's heap. The post-cb
counter and prefix are read where the actual source reads them. -/
set_option maxRecDepth 16384 in
theorem body_eval (observers : Observers) (id next : Nat)
    (listPrimitive : env "list" = none) (consPrimitive : env "cons" = none)
    (plusPrimitive : env "+" = none) (reversePrimitive : env "reverse" = none)
    (count : before base 0 = some (.natural id))
    (owner : String) (path : List Datum) (expression proof : Datum) (value : Payload)
    (origin : Notification observers.origin before (originValues id owner path expression value) originResult middle)
    (observed : Notification observers.proof middle (proofValues id proof value) proofResult ready)
    (liveCount : ready base 0 = some (.natural next))
    (liveRecords : ready (base + 4) 0 = some (.list previous)) :
    SchemeTableEvaluation.Sequence (arguments env base observers owner path expression proof value)
      before LoweringEmitterEvaluation.definition.body .unit
      (published ready base next (record id value) previous) := by
  let env' := arguments env base observers owner path expression proof value
  let named := extend env' "production" (record id value)
  have made : Eval env' before
      (app "make-production" [s "next-id", s "lhs", s "rhs", s "action", s "precedence"])
      (record id value) before := by
    apply Eval.call (function := LoweringProductionEvaluation.function env)
      (values := [.natural id, .symbol value.lhs, .list value.rhs,
        datumValue value.action, datumValue value.precedence]) rfl
    · apply Eval.variable; rfl
    · refine .cons (.variable ?_) (.cons (.variable (by rfl)) (.cons (.variable (by rfl))
        (.cons (.variable ?_) (.cons (.variable ?_) .nil))))
      · change before base 0 = _; exact count
      · apply resolve_datum; rfl
      · apply resolve_datum; rfl
    · exact LoweringProductionEvaluation.invoke listPrimitive id value
  have originRead : resolve named before "observe-origin" = some observers.origin.value := by
    apply Callback.resolves; rfl
  have proofRead : resolve named middle "observe-proof" = some observers.proof.value := by
    apply Callback.resolves; rfl
  have originArgs : Arguments named before
      [s "production", s "owner", app "reverse" [s "path"], s "expression"]
      (originValues id owner path expression value) before := by
    refine .cons (.variable (by rfl)) (.cons (.variable (by rfl)) (.cons ?_ (.cons (.variable ?_) .nil)))
    · apply Eval.apply (values := [.list (datumValues path)])
      · exact .cons (.variable (by rfl)) .nil
      · rfl
      · change env "reverse" = none; exact reversePrimitive
    · apply resolve_datum; rfl
  have proofArgs : Arguments named middle [quoted "production", s "production", s "proof"]
      (proofValues id proof value) middle := by
    refine .cons .quoteSymbol (.cons (.variable (by rfl)) (.cons (.variable ?_) .nil))
    apply resolve_datum; rfl
  have increment : Eval named ready (app "set!" [s "next-id", app "+" [s "next-id", n 1]]) .unit
      (write ready base 0 (.natural (next + 1))) := by
    apply Eval.assignment (by rfl)
    apply Eval.apply (values := [.natural next, .natural 1])
    · refine .cons (.variable ?_) (.cons .natural .nil)
      change ready base 0 = _; exact liveCount
    · rfl
    · change env "+" = none; exact plusPrimitive
  have store : Eval named (write ready base 0 (.natural (next + 1)))
      (app "set!" [s "productions", app "cons" [s "production", s "productions"]]) .unit
      (published ready base next (record id value) previous) := by
    apply Eval.assignment (by rfl)
    apply Eval.apply (values := [record id value, .list previous])
    · refine .cons (.variable (by rfl)) (.cons (.variable ?_) .nil)
      change write ready base 0 (.natural (next + 1)) (base + 4) 0 = _
      simpa [write] using liveRecords
    · rfl
    · change env "cons" = none; exact consPrimitive
  apply SchemeTableEvaluation.Sequence.single
  exact .letBody made (.cons (origin.evaluates originRead originArgs rfl)
    (.cons (observed.evaluates proofRead proofArgs rfl) (.cons increment (.single store))))

/-- Real root lookup and eight-argument binding; no source body execution
premise is supplied for the emitter itself. -/
theorem invoke (observers : Observers) (id next : Nat)
    (listPrimitive : env "list" = none) (consPrimitive : env "cons" = none)
    (plusPrimitive : env "+" = none) (reversePrimitive : env "reverse" = none)
    (count : before base 0 = some (.natural id))
    (owner : String) (path : List Datum) (expression proof : Datum) (value : Payload)
    (origin : Notification observers.origin before (originValues id owner path expression value) originResult middle)
    (observed : Notification observers.proof middle (proofValues id proof value) proofResult ready)
    (liveCount : ready base 0 = some (.natural next))
    (liveRecords : ready (base + 4) 0 = some (.list previous)) :
    Invoke before (.recursive LoweringRootScope.group "emit-production"
        (LoweringRootStorage.environment (globals env observers) base))
      [.symbol value.lhs, .list value.rhs, datumValue value.action, datumValue value.precedence,
        .symbol owner, datumValue (.list path), datumValue expression, datumValue proof]
      .unit (published ready base next (record id value) previous) :=
  .recursive LoweringEmitterEvaluation.root_lookup rfl
    (body_eval observers id next listPrimitive consPrimitive plusPrimitive reversePrimitive count
      owner path expression proof value origin observed liveCount liveRecords)

/-- This is the exact counter condition needed by dense publication. Callback
return alone does not entail it. Heap effects remain visible to the consumer. -/
theorem counter_agreement_iff (id : Nat) :
    published ready base next production previous base 0 = some (.natural (id + 1)) ↔ next = id := by
  simp [published, write]

end ParserEngine.LoweringObserverEvaluation
