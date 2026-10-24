import ParserEngine.LoweringProductionEvaluation

/-! Execute the actual unobserved root emitter: constructor call, disabled
observers, counter increment, then reverse publication. Both owned cells are
derived from the existing root layout; no final heap equality is an input. -/
namespace ParserEngine.LoweringEmitterEvaluation
open WalkSource SchemeTableEvaluation LoweringProductionEvaluation

structure Context where
  owner : String
  path : Datum
  expression : Datum

def globals (env : Environment) : Environment :=
  extend (extend (tie LoweringProductionEvaluation.group env) "observe-origin" (.boolean false))
    "observe-proof" (.boolean false)

def root (env : Environment) (base : Nat) : Environment :=
  tie LoweringRootScope.group (LoweringRootStorage.environment (globals env) base)

def arguments (env : Environment) (base : Nat) (context : Context) (value : Payload) : Environment :=
  extend (extend (extend (extend (extend (extend (extend (extend (root env base)
    "lhs" (.symbol value.lhs)) "rhs" (.list value.rhs)) "action" (datumValue value.action))
    "precedence" (datumValue value.precedence)) "owner" (.symbol context.owner))
    "path" (datumValue context.path)) "expression" (datumValue context.expression)) "proof" (.boolean false)

def definition : Procedure :=
  ((LoweringClaimEvaluation.procedure LoweringPublicationWitness.emitterSource).map Prod.snd).getD ⟨[], []⟩

def committed (before : Heap) (base id : Nat) (previous : List Value) (value : Payload) : Heap :=
  write (write before base 0 (.natural (id + 1))) (base + 4) 0 (.list (record id value :: previous))

set_option maxRecDepth 16384 in
theorem root_lookup : LoweringRootScope.group.lookup "emit-production" = some definition := rfl

/- The actual body is read from the same source witness as publication. -/
set_option maxRecDepth 16384 in
theorem body_eval (id : Nat) (listPrimitive : env "list" = none) (consPrimitive : env "cons" = none)
    (plusPrimitive : env "+" = none)
    (count : before base 0 = some (.natural id))
    (contents : before (base + 4) 0 = some (.list previous)) (context : Context) (value : Payload) :
    SchemeTableEvaluation.Sequence (arguments env base context value) before definition.body .unit
      (committed before base id previous value) := by
  let env' := arguments env base context value
  have made : Eval env' before
      (app "make-production" [s "next-id", s "lhs", s "rhs", s "action", s "precedence"])
      (record id value) before := by
    apply Eval.call (function := LoweringProductionEvaluation.function env)
      (values := [.natural id, .symbol value.lhs, .list value.rhs,
        datumValue value.action, datumValue value.precedence]) rfl
    · apply Eval.variable; rfl
    · refine .cons (.variable ?_) (.cons (.variable ?_) (.cons (.variable ?_)
        (.cons (.variable ?_) (.cons (.variable ?_) .nil))))
      · change before base 0 = _; exact count
      · rfl
      · rfl
      · apply resolve_datum; rfl
      · apply resolve_datum; rfl
    · exact LoweringProductionEvaluation.invoke listPrimitive id value
  let named := extend env' "production" (record id value)
  have origin : Eval named before (app "when" [s "observe-origin",
      app "observe-origin" [s "production", s "owner", app "reverse" [s "path"], s "expression"]]) .unit before :=
    .whenFalse (.variable (by rfl)) rfl
  have proof : Eval named before (app "when" [s "observe-proof",
      app "observe-proof" [quoted "production", s "production", s "proof"]]) .unit before :=
    .whenFalse (.variable (by rfl)) rfl
  have increment : Eval named before (app "set!" [s "next-id", app "+" [s "next-id", n 1]]) .unit
      (write before base 0 (.natural (id + 1))) := by
    apply Eval.assignment (by rfl)
    apply Eval.apply (values := [.natural id, .natural 1])
    · refine .cons (.variable ?_) (.cons .natural .nil)
      change before base 0 = _; exact count
    · rfl
    · change env "+" = none; exact plusPrimitive
  have store : Eval named (write before base 0 (.natural (id + 1)))
      (app "set!" [s "productions", app "cons" [s "production", s "productions"]]) .unit
      (committed before base id previous value) := by
    apply Eval.assignment (by rfl)
    apply Eval.apply (values := [record id value, .list previous])
    · refine .cons (.variable (by rfl)) (.cons (.variable ?_) .nil)
      change write before base 0 (.natural (id + 1)) (base + 4) 0 = _
      simpa [write] using contents
    · rfl
    · change env "cons" = none; exact consPrimitive
  apply SchemeTableEvaluation.Sequence.single
  exact .letBody made (.cons origin (.cons proof (.cons increment (.single store))))

def values (context : Context) (value : Payload) : List Value :=
  [.symbol value.lhs, .list value.rhs, datumValue value.action, datumValue value.precedence,
    .symbol context.owner, datumValue context.path, datumValue context.expression, .boolean false]

/-- The real root procedure lookup and its actual eight-argument binding. -/
theorem invoke (id : Nat) (listPrimitive : env "list" = none) (consPrimitive : env "cons" = none)
    (plusPrimitive : env "+" = none)
    (count : before base 0 = some (.natural id))
    (contents : before (base + 4) 0 = some (.list previous)) (context : Context) (value : Payload) :
    Invoke before (.recursive LoweringRootScope.group "emit-production"
      (LoweringRootStorage.environment (globals env) base)) (values context value) .unit
      (committed before base id previous value) :=
  .recursive root_lookup rfl (body_eval id listPrimitive consPrimitive plusPrimitive count contents context value)

theorem committed_counter (id : Nat) : committed before base id previous value base 0 = some (.natural (id + 1)) := by
  simp [committed, write]

theorem committed_records (id : Nat) : committed before base id previous value (base + 4) 0 =
    some (.list (record id value :: previous)) := by simp [committed, write]

/-- Allocation tables, the synthetic counter and the declaration collector are
not emission-owned cells. Callers compose this frame law with their own reads. -/
theorem committed_frame (id : Nat) (counter : address ≠ base ∨ key ≠ .index 0)
    (productions : address ≠ base + 4 ∨ key ≠ .index 0) :
    committed before base id previous value address key = before address key := by
  simp only [committed, write]
  split
  · grind
  · split <;> grind

/-- Root-layout ownership discharges the allocation frame without requiring
an independently supplied non-aliasing assertion from the caller. -/
theorem name_counter_preserved (id : Nat) :
    committed before base id previous value (base + 1) 0 = before (base + 1) 0 :=
  committed_frame id (Or.inl (by omega)) (Or.inl (by omega))

theorem names_preserved (id : Nat)
    (valid : LoweringFreshEvaluation.Occupied before (base + 2) used) :
    LoweringFreshEvaluation.Occupied (committed before base id previous value) (base + 2) used := by
  intro name
  exact (committed_frame id (Or.inr (by simp)) (Or.inr (by simp))).trans (valid name)

/-- Fixed argument values only: disjoint source writes commute as heap
operations. This does not authorize reordering data-dependent parsing calls
or assert anything about concurrent host memory ordering. -/
theorem allocation_emission_commute (id : Nat) :
    LoweringAllocationCalls.committed (committed before base id previous value) (base + 1) (base + 2) owner index =
      committed (LoweringAllocationCalls.committed before (base + 1) (base + 2) owner index) base id previous value := by
  funext address key
  simp only [committed, LoweringAllocationCalls.committed, write, writeSymbol]
  repeat' first | rfl | split
  all_goals grind

end ParserEngine.LoweringEmitterEvaluation