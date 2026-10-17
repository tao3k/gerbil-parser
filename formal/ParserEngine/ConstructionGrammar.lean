import ParserEngine.ConstructionConsumer
import ParserEngine.Names

/-! Admission of ordered native production records into the retained builder.
Names and terminal payloads use the existing normalized native IR identities;
host symbol/string encoding remains a separate representation boundary. -/
namespace ParserEngine.ConstructionGrammar
open ProducerAdmission

structure Record where
  id : Nat
  value : Production

structure Input where
  records : List Record
  augmented : Nat
  root : Nat
  source : Nat
  rules : Lowering.Rules
  decode : Nat → Lowering.Owner
  recipes : List Names.Recipe

def productions (input : Input) := input.records.map Record.value

def symbol (input : Input) (operand : ProducerAdmission.Operand) : Grammar.Symbol :=
  match operand.base with
  | .nonterminal name => .nonterminal name
  | .terminal key => .terminal ((keyCode (productions input) key).getD 0)

def row (input : Input) (record : Record) : Grammar.Production :=
  ⟨record.value.lhs, record.value.rhs.map (symbol input)⟩

def grammar (input : Input) : Grammar.CFG := input.records.map (row input)

def operandAllowed (input : Input) (operand : ProducerAdmission.Operand) : Bool :=
  match operand.base with
  | .nonterminal name => decide (name ∈ input.records.tail.map (fun record => record.value.lhs))
  | .terminal key => (keyCode (productions input) key).isSome

def checks (input : Input) : Bool :=
  decide (input.records.map Record.id = List.range input.records.length) &&
  decide ((grammar input).head? = some ⟨input.augmented, [.nonterminal input.root]⟩) &&
  input.records.all (fun record => record.value.rhs.all (operandAllowed input)) &&
  input.records.tail.all (fun record => record.value.lhs != input.augmented) &&
  decide (input.decode input.root = .named input.source) &&
  Names.admitNative input.rules input.decode (grammar input).tail input.recipes

structure Admitted (input : Input) : Prop where
  accepted : checks input = true

def prepare (input : Input) : Option (PLift (Admitted input)) :=
  if accepted : checks input = true then some ⟨⟨accepted⟩⟩ else none

theorem admitted_fields (input : Input) (receipt : Admitted input) :
    input.records.map Record.id = List.range input.records.length ∧
    (grammar input).head? = some ⟨input.augmented, [.nonterminal input.root]⟩ ∧
    input.records.all (fun record => record.value.rhs.all (operandAllowed input)) = true ∧
    input.records.tail.all (fun record => record.value.lhs != input.augmented) = true ∧
    input.decode input.root = .named input.source ∧
    Names.admitNative input.rules input.decode (grammar input).tail input.recipes = true := by
  have accepted := receipt.accepted
  simpa only [checks, Bool.and_eq_true, decide_eq_true_eq, and_assoc] using accepted

theorem nonempty (input : Input) (receipt : Admitted input) : grammar input ≠ [] := by
  intro empty
  have head := (admitted_fields input receipt).2.1
  rw [empty] at head
  cases head

/-- Metadata erasure preserves physical row positions, including duplicates. -/
theorem project_index (input : Input) (index : Nat) :
    (grammar input)[index]? = (input.records[index]?).map (row input) := by
  simp [grammar]

theorem id_index (input : Input) (receipt : Admitted input) (index : Nat) (record : Record)
    (read : input.records[index]? = some record) : record.id = index := by
  have bound := List.getElem?_eq_some_iff.mp read
  have field : (input.records.map Record.id)[index]? = some record.id := by
    rw [List.getElem?_map, read]
    rfl
  rw [(admitted_fields input receipt).1] at field
  simpa [bound.1] using field.symm

/-- No default terminal code can be observed on an admitted RHS. -/
theorem operand_exact (input : Input) (receipt : Admitted input) (record : Record)
    (member : record ∈ input.records) (operand : ProducerAdmission.Operand) (present : operand ∈ record.value.rhs) :
    match operand.base with
    | .nonterminal name => name ∈ input.records.tail.map (fun record => record.value.lhs)
    | .terminal key => ∃ code, keyCode (productions input) key = some code ∧ symbol input operand = .terminal code := by
  have allowed := List.all_eq_true.mp (List.all_eq_true.mp (admitted_fields input receipt).2.2.1 record member) operand present
  cases base : operand.base with
  | nonterminal name => simpa [operandAllowed, base] using allowed
  | terminal key =>
    simp only [operandAllowed, base] at allowed
    cases found : keyCode (productions input) key with
    | none => simp [found] at allowed
    | some ordinal => exact ⟨ordinal, found, by simp [symbol, base, found]⟩

theorem origins (input : Input) (receipt : Admitted input) :
    ∀ production ∈ (grammar input).tail, Names.OriginSupported input.rules input.decode production :=
  Names.admitted_native_sound (admitted_fields input receipt).2.2.2.2.2

theorem source_derivation (input : Input) (receipt : Admitted input) (word : List Nat)
    (derivation : Grammar.Derives (grammar input).tail [.nonterminal input.root] word) :
    Lowering.Eval input.rules (.reference input.source) word :=
  Names.admitted_native_root_sound (admitted_fields input receipt).2.2.2.2.2 input.root input.source word
    (admitted_fields input receipt).2.2.2.2.1 derivation

def context (input : Input) (receipt : Admitted input) (traversal : CachedConstruction.Traversal) :
    ConstructionConsumer.Context := ⟨grammar input, nonempty input receipt, traversal⟩

structure Output (input : Input) (traversal : CachedConstruction.Traversal) where
  receipt : Admitted input
  product : ConstructionConsumer.Product (context input receipt traversal)

/-- Neither a CFG nor its nonempty proof is supplied by the caller. The same
source-admitted LeanPoo factory consumes the internally projected grammar. -/
def construct (sources : ConstructionConsumer.Sources) (input : Input) (traversal : CachedConstruction.Traversal) :
    Option (Output input traversal) :=
  (prepare input).bind fun receipt =>
    (ConstructionConsumer.construct sources (context input receipt.down traversal)).map fun product => ⟨receipt.down, product⟩

theorem rejected (sources : ConstructionConsumer.Sources) (input : Input) (traversal : CachedConstruction.Traversal)
    (failure : checks input = false) : construct sources input traversal = none := by
  simp [construct, prepare, failure]

theorem consumer_success (sources : ConstructionConsumer.Sources) (input : Input)
    (traversal : CachedConstruction.Traversal) (output : Output input traversal)
    (success : construct sources input traversal = some output) :
    ConstructionConsumer.construct sources (context input output.receipt traversal) = some output.product := by
  unfold construct at success
  cases prepared : prepare input with
  | none => simp [prepared] at success
  | some receipt =>
    simp only [prepared, Option.bind_some] at success
    cases built : ConstructionConsumer.construct sources (context input receipt.down traversal) with
    | none => simp [built] at success
    | some product =>
      simp only [built, Option.map_some] at success
      cases Option.some.inj success
      rfl

theorem complete (sources : ConstructionConsumer.Sources) (input : Input)
    (traversal : CachedConstruction.Traversal) (output : Output input traversal)
    (success : construct sources input traversal = some output) :
    ConstructionFIFO.executeSource sources.builder sources.growth sources.append sources.template
      (grammar input) (nonempty input output.receipt) traversal =
      some ((output.product.construction.1.machine, output.product.construction.2), output.product.actions) ∧
    ConstructionReadiness.Complete (grammar input) traversal output.product.layout ∧
    ConstructionReadiness.Visit (grammar input) traversal output.product.layout
      output.product.construction.1.machine output.product.construction.2 :=
  ConstructionConsumer.source_readiness sources (context input output.receipt traversal) output.product
    (consumer_success sources input traversal output success)

end ParserEngine.ConstructionGrammar
