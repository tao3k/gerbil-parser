import ParserEngine.LoweringBranchSource

/-! Full admitted lowering source, generated lexical call stack, physical name
store and retained LeanPoo construction. The physical/name origin proof is
generated; raw record encoding is checked separately without changing records.
This denotes the supported normal-return protocol, not arbitrary Scheme code. -/
namespace ParserEngine.LoweringProtocol
open Lowering LoweringScope LoweringNameStore LoweringCallStack

abbrev Program (rules : List (Nat × LoweringTraversal.Syntax)) (root : Nat) :=
  (LoweringProgram.compile rules root).code
abbrev Start (rules : List (Nat × LoweringTraversal.Syntax)) (root : Nat) :=
  initial (LoweringProgram.reserved rules root)

def run (source : WalkSource.Datum) (rules : List (Nat × LoweringTraversal.Syntax)) (root : Nat)
    (candidate : Nat → Nat → Nat)
    (injective : ∀ owner a b, candidate owner a = candidate owner b → a = b) :
    Option (Execution (Program rules root) (Start rules root)) :=
  if decide (source = LoweringBranchSource.root) then
    some (execute candidate injective (Program rules root) (Start rules root) .nil)
  else none

theorem generated_origin (rules : List (Nat × LoweringTraversal.Syntax)) (root : Nat)
    (execution : Execution (Program rules root) (Start rules root)) :
    Names.admitNative (LoweringTraversal.normalized rules) (decode execution.after.names)
      (emissions execution.events) ((LoweringTraversal.declarations rules).map LoweringBranches.Emission.recipe) = true := by
  have aligned := LoweringCallStack.realized (Program rules root) (Start rules root) execution
  rw [(LoweringProgram.compile rules root).erased] at aligned
  rw [execution.trajectory]
  exact LoweringBranches.realized_native aligned

def input (rules : List (Nat × LoweringTraversal.Syntax)) (root : Nat)
    (execution : Execution (Program rules root) (Start rules root)) (raw : ConstructionGrammar.Input) :
    ConstructionGrammar.Input :=
  ⟨raw.records, raw.augmented, root, root, LoweringTraversal.normalized rules,
    decode execution.after.names, (LoweringTraversal.declarations rules).map LoweringBranches.Emission.recipe⟩

theorem input_origin (rules : List (Nat × LoweringTraversal.Syntax)) (root : Nat)
    (execution : Execution (Program rules root) (Start rules root)) (raw : ConstructionGrammar.Input)
    (same : (ConstructionGrammar.grammar (input rules root execution raw)).tail = emissions execution.events) :
    Names.admitNative (input rules root execution raw).rules (input rules root execution raw).decode
      (ConstructionGrammar.grammar (input rules root execution raw)).tail (input rules root execution raw).recipes = true := by
  rw [same]
  exact generated_origin rules root execution

def construct (source : WalkSource.Datum) (rules : List (Nat × LoweringTraversal.Syntax)) (root : Nat)
    (candidate : Nat → Nat → Nat) (injective : ∀ owner a b, candidate owner a = candidate owner b → a = b)
    (sources : ConstructionConsumer.Sources) (raw : ConstructionGrammar.Input) (traversal : CachedConstruction.Traversal) :
    Option (Σ value, ConstructionGrammar.Output value traversal) := do
  let execution ← run source rules root candidate injective
  let value := input rules root execution raw
  if decide ((ConstructionGrammar.grammar value).tail = emissions execution.events) then
    (ConstructionGrammar.construct sources value traversal).map (fun output => ⟨value, output⟩)
  else none

theorem constructed (source : WalkSource.Datum) (rules : List (Nat × LoweringTraversal.Syntax)) (root : Nat)
    (candidate : Nat → Nat → Nat) (injective : ∀ owner a b, candidate owner a = candidate owner b → a = b)
    (sources : ConstructionConsumer.Sources) (raw : ConstructionGrammar.Input) (traversal : CachedConstruction.Traversal)
    (output : Σ value, ConstructionGrammar.Output value traversal)
    (built : construct source rules root candidate injective sources raw traversal = some output) :
    ∃ execution, run source rules root candidate injective = some execution ∧
      output.1 = input rules root execution raw ∧
      (ConstructionGrammar.grammar output.1).tail = emissions execution.events ∧
      ConstructionGrammar.construct sources output.1 traversal = some output.2 := by
  unfold construct at built
  obtain ⟨execution, ran, rest⟩ := Option.bind_eq_some_iff.mp built
  dsimp only at rest
  split at rest
  next matching =>
    obtain ⟨product, made, equal⟩ := Option.map_eq_some_iff.mp rest
    subst output
    exact ⟨execution, ran, rfl, of_decide_eq_true matching, made⟩
  next denied => cases rest

theorem complete (source : WalkSource.Datum) (rules : List (Nat × LoweringTraversal.Syntax)) (root : Nat)
    (candidate : Nat → Nat → Nat) (injective : ∀ owner a b, candidate owner a = candidate owner b → a = b)
    (sources : ConstructionConsumer.Sources) (raw : ConstructionGrammar.Input) (traversal : CachedConstruction.Traversal)
    (output : Σ value, ConstructionGrammar.Output value traversal)
    (built : construct source rules root candidate injective sources raw traversal = some output) :
    ConstructionFIFO.executeSource sources.builder sources.growth sources.append sources.template
      (ConstructionGrammar.grammar output.1) (ConstructionGrammar.nonempty output.1 output.2.receipt) traversal =
      some ((output.2.product.construction.1.machine, output.2.product.construction.2), output.2.product.actions) ∧
    ConstructionReadiness.Complete (ConstructionGrammar.grammar output.1) traversal output.2.product.layout ∧
    ConstructionReadiness.Visit (ConstructionGrammar.grammar output.1) traversal output.2.product.layout
      output.2.product.construction.1.machine output.2.product.construction.2 :=
  ConstructionGrammar.complete sources output.1 traversal output.2
    (by obtain ⟨_, _, _, _, made⟩ := constructed source rules root candidate injective sources raw traversal output built; exact made)

theorem unsupported_rejected (source : WalkSource.Datum) (changed : source ≠ LoweringBranchSource.root)
    (rules : List (Nat × LoweringTraversal.Syntax)) (root : Nat) (candidate : Nat → Nat → Nat)
    (injective : ∀ owner a b, candidate owner a = candidate owner b → a = b) :
    run source rules root candidate injective = none := by simp [run, changed]

set_option maxRecDepth 16384 in
theorem current_ready (rules : List (Nat × LoweringTraversal.Syntax)) (root : Nat)
    (candidate : Nat → Nat → Nat) (injective : ∀ owner a b, candidate owner a = candidate owner b → a = b) :
    (run LoweringBranchSource.rootSource rules root candidate injective).isSome = true := by
  have source : LoweringBranchSource.rootSource = LoweringBranchSource.root := rfl
  simp only [run, source, decide_true, ↓reduceIte, Option.isSome_some]

end ParserEngine.LoweringProtocol
