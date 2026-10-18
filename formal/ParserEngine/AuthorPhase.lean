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

/-- A shared interpreter's ambient bindings are not evidence that each source
owner declares its own requirements. This combines provider selection with
the concrete missing-source-binding failure found in independent CI imports. -/
def inherit (owned ambient : Provider Context Capability Value) : Provider Context Capability Value :=
  fun key => match owned key with
    | some factory => some factory
    | none => ambient key

theorem ambient_masks_missing
    (owned ambient : Provider Context Capability Value)
    (missing : owned .compiler = none) (available : ambient .compiler = some factory) :
    Requirements.prepare owned (demand .declaration) = .error .compiler ∧
    Requirements.prepare (inherit owned ambient) (demand .declaration) =
      .ok (factory, PUnit.unit) := by
  simp [demand, Requirements.prepare, inherit, missing, available]

/-- A retained factory receives the entire caller context, including arbitrary
syntax and runtime binding operands. Factoring the service adds no projection
or reconstruction of that context. -/
theorem retained_operands
    (factory : Factory Context (fun context => Value context Capability.compiler)) :
    Factory.map (fun _ value => value) factory = factory := Factory.map_identity factory

/-- Matchers compare transported literal identities, not just their spelling.
The SDK identifier relation is an explicit operand, not reimplemented here. -/
def literalMatch (same : Identifier → Identifier → Bool) : List Identifier → List Identifier → Bool
  | [], [] => true
  | actual :: rest, expected :: tail => same actual expected && literalMatch same rest tail
  | _, _ => false

theorem literal_transport
    (same : Identifier → Identifier → Bool) (other : Target → Target → Bool)
    (move : Identifier → Target)
    (preserved : ∀ actual expected, other (move actual) (move expected) = same actual expected)
    (actual expected : List Identifier) :
    literalMatch other (actual.map move) (expected.map move) = literalMatch same actual expected := by
  induction actual generalizing expected with
  | nil => cases expected <;> rfl
  | cons value rest ih =>
    cases expected with
    | nil => rfl
    | cons wanted tail => simp [literalMatch, preserved, ih]

/-- Recreating an expected keyword in a service namespace can reject the same
spelling that was accepted in the facade. Dropping binding identity would also
admit a keyword captured by the caller. Both are real phase-transport defects. -/
theorem literal_context_counterexample :
    literalMatch (fun (a b : String × Nat) => a == b) [("syntax", 0)] [("syntax", 0)] = true ∧
    literalMatch (fun (a b : String × Nat) => a == b) [("syntax", 0)] [("syntax", 1)] = false ∧
    literalMatch (fun (a b : String × Nat) => a.1 == b.1) [("syntax", 0)] [("syntax", 1)] = true := by
  decide

end ParserEngine.AuthorPhase
