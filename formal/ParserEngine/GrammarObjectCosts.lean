import LeanPoo.Functional.StatefulValidation
import LeanPoo.Functional.ClassValidation

/-! Cost/refinement obligations for checked Grammar object construction, not LR
automaton construction. LeanPoo owns validation and state/error semantics.
Counts below are administrative operation counts, never milliseconds or a GC
model. No theorem certifies the Scheme adapter or its allocation behavior. -/
namespace ParserEngine.GrammarObjectCosts
open LeanPoo.Functional

/-- A pure recipe adapter may be fused with traversal after the complete
candidate-dependent presence/shape preflight. The state may contain receiver
updates, dependency observations and callback traces. Errors retain that state.
Do not move effectful metadata selection across the preflight boundary. -/
theorem fuse_plan
    (check : State → Recipe → Except Error Value × State)
    (prepare : Field → Recipe) (state : State) (fields : List Field) :
    Validation.traverseState check state (fields.map prepare) =
      Validation.traverseState (fun current field => check current (prepare field))
        state fields :=
  Validation.traverseState_map check prepare state fields

/-- Fusion preserves the exact first failing callback and its returned state;
the suffix does not run. This reuses the upstream first-error characterization. -/
theorem fused_failure
    (check : State → Recipe → Except Error Value × State)
    (prepare : Field → Recipe) (state : State) (fields : List Field)
    (error : Error) (final : State) :
    Validation.traverseState (fun current field => check current (prepare field))
        state fields = (.error error, final) ↔
      ∃ before item after values next, fields = before ++ item :: after ∧
        Validation.traverseState (fun current field => check current (prepare field))
          state before = (.ok values, next) ∧
        check next (prepare item) = (.error error, final) :=
  Validation.traverseState_error_iff _ state fields error final

/-- A successful Slot must install its actual normalizer result. Equality with
the input is not assumed by the cost model or by a proposed fused driver. -/
theorem normalized_child
    (slot : ClassValidation.Slot Value Error) (missing invalid : Error)
    (source checked : Value) (raw : slot.constant source = true)
    (called : slot.check source = .ok checked)
    (accepted : slot.constant checked = true) :
    ClassValidation.normalize slot missing invalid (some source) = .ok (some checked) :=
  ClassValidation.present_success slot missing invalid source checked raw called accepted

/-- Static accounting for a pure map spine: its output has one cons per field.
The fused traversal need not materialize this intermediate list. Scheme source
and compiler/allocation evidence must establish that this elimination occurs. -/
theorem recipe_spine (prepare : Field → Recipe) (fields : List Field) :
    (fields.map prepare).length = fields.length := List.length_map ..

/-- Per-pair *disjoint* work counters supplied by a host instrumentation plan.
Inclusive profiler stack counts cannot be added to populate these fields. -/
structure Work where
  prototype : Nat
  shape : Nat
  normalization : Nat
  contextual : Nat
  evidence : Nat
  administration : Nat

def Work.total (work : Work) : Nat :=
  work.prototype + work.shape + work.normalization + work.contextual +
    work.evidence + work.administration

/-- Removing only measured administrative units leaves semantic work intact.
This is an operation-count result; no elapsed-time improvement follows. -/
theorem administrative_saving (work : Work) (removed : Nat)
    (covered : removed ≤ work.administration) (pairs : Nat) :
    pairs * work.total =
      pairs * {work with administration := work.administration - removed}.total +
      pairs * removed := by
  have exact : work.total =
      {work with administration := work.administration - removed}.total + removed := by
    simp only [Work.total]
    omega
  rw [exact, Nat.mul_add]

/-- Preparing a descriptor once can only remove setup repetitions. Every
candidate still pays current admission. Descriptor validity and read dependencies
must be checked by the existing owner, not inferred from object identity. -/
theorem preparation_saving (setup admission pairs : Nat) :
    (pairs + 1) * (setup + admission) =
      (setup + (pairs + 1) * admission) + pairs * setup := by
  simp only [Nat.mul_add, Nat.add_mul, Nat.one_mul]
  omega

private def currentRole (valid : Bool) (_ : Unit) : Except Unit Unit × Bool :=
  (if valid then .ok () else .error (), valid)

/-- The same identity can have a different current list/descriptor obligation.
A cached accepted membership cannot replace the second current check. -/
theorem identity_cache_counterexample :
    Validation.traverseState currentRole true [()] = (.ok [()], true) ∧
    Validation.traverseState currentRole false [()] = (.error (), false) := by
  constructor <;> rfl

private def observedNormalizer (calls : Nat) (_ : Unit) : Except Unit Nat × Nat :=
  (.ok (calls + 1), calls + 1)

/-- Even an always-successful callback may have observable effects. Eliminating
one revalidation without a stronger purity/currentness contract changes state. -/
theorem repeated_callback_counterexample :
    Validation.traverseState observedNormalizer 0 [(), ()] = (.ok [1, 2], 2) ∧
    Validation.traverseState observedNormalizer 0 [()] = (.ok [1], 1) := by
  constructor <;> rfl

end ParserEngine.GrammarObjectCosts
