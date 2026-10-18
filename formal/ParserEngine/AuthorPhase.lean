import ParserEngine.TestPhase

/-! A compiled language interface and an authored declaration have different
compiler demands. SDK phase interpretation, export resolution and evaluation
are external premises; this does not model Gambit or prove the whole lowerer. -/
namespace ParserEngine.AuthorPhase
open LeanPoo.Functional
universe u v

inductive Use where | interface | declaration deriving DecidableEq
inductive Capability where | compiler deriving DecidableEq

def demand : Use → List Capability
  | .interface => []
  | .declaration => [.compiler]

variable {Context : Type u} {Value : Context → Capability → Type v}

/-- An unused compiler is not even requested. Provider equality is unnecessary,
including when one provider is missing or would select a failing factory. -/
theorem interface_independent
    (provider other : Provider Context Capability Value) :
    Requirements.prepare provider (demand .interface) =
      Requirements.prepare other (demand .interface) := rfl

theorem declaration_requires_compiler
    (provider : Provider Context Capability Value)
    (missing : provider .compiler = none) :
    Requirements.prepare provider (demand .declaration) = .error .compiler := by
  simp [demand, Requirements.prepare, missing]

/-- Stage separation changes admission, not the caller context supplied to the
retained expansion factory. Syntax and hygienic runtime operands belong there. -/
theorem expansion_context
    (factory : Factory Context (fun context => Value context Capability.compiler))
    (context : Context) :
    Requirements.build (keys := demand .declaration)
      (factory, PUnit.unit) context = (factory context, PUnit.unit) := rfl

/-- Reading a phase-minus-one service dependency does not request its runtime.
Its explicit declaration-time import/evaluation is a different operation. -/
theorem template_service_dormant (name : Module) :
    TestPhase.runtimeRequest (⟨-1, 0, some name⟩ : TestPhase.ResolvedImport Module) = none := by
  simp [TestPhase.runtimeRequest]

/-- Eager compiler admission can reject an interface that needs no compiler.
This is the design counterexample behind the native module-registry control. -/
theorem eager_admission_counterexample :
    Requirements.prepare (Context := Unit) (Value := fun _ _ => Unit)
      (fun (_ : Capability) => none) (demand .interface) = .ok PUnit.unit ∧
    Requirements.prepare (Context := Unit) (Value := fun _ _ => Unit)
      (fun (_ : Capability) => none) (demand .declaration) = .error .compiler := by
  simp [demand, Requirements.prepare]

end ParserEngine.AuthorPhase
