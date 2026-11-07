import ParserEngine.Reduction
import ParserEngine.TableBridge

/-! Decode native unfielded/labelled child storage before applying grammar
semantics. Inner Tree values are admitted decodings of native token/node/
fragment records; host heaps, relocation and event-program decoding are separate. -/
namespace ParserEngine.SemanticValues

/-- Grammar projection may erase field/alias distinctions. Production IDs
must therefore index grammar and semantic metadata together, never by body equality. -/
structure Rule where
  production : Grammar.Production
  actions : List (List Action)
  width : actions.length = production.rhs.length

def grammarView (rules : List Rule) : Grammar.CFG := rules.map Rule.production
def actionView (rules : List Rule) : List (List (List Action)) := rules.map Rule.actions

theorem same_id_views (rules : List Rule) (id : Nat) (rule : Rule)
    (found : rules[id]? = some rule) :
    TableBridge.decodeAction (grammarView rules) (.reduce id) = some (.reduce rule.production) ∧
      (actionView rules)[id]? = some rule.actions := by
  simp [TableBridge.decodeAction, grammarView, actionView, List.getElem?_map, found]

abbrev Stored := Sequence ChildStorage

def decode (stored : Stored) : Children := stored.flatten.map restoreChild

def fieldStored (name : String) (values : List ChildStorage) (offset : Nat) : List ChildStorage :=
  match values with
  | [] => []
  | [.unfielded value] => [.labelled name value]
  | _ => [.labelled name (.fragment (bounds (values.map restoreChild) offset)
      (values.map restoreChild))]

def aliasStored (kind : String) (values : List ChildStorage) (offset : Nat) : List ChildStorage :=
  [.unfielded (.node kind (bounds (values.map restoreChild) offset) (values.map restoreChild))]

theorem field_decodes (name : String) (values : List ChildStorage) (offset : Nat) :
    (fieldStored name values offset).map restoreChild = field name (values.map restoreChild) offset := by
  cases values with
  | nil => rfl
  | cons first rest =>
    cases rest with
    | nil => cases first <;> rfl
    | cons second rest => cases first <;> rfl

theorem alias_decodes (kind : String) (values : List ChildStorage) (offset : Nat) :
    (aliasStored kind values offset).map restoreChild = alias kind (values.map restoreChild) offset := rfl

/-- Actual materialization retains every child and any supplied suffix. -/
theorem materialization_decodes (stored : Stored) (suffix : List ChildStorage) :
    (stored.materializeInto suffix).1.map restoreChild = decode stored ++ suffix.map restoreChild := by
  rw [materialize_into_exact]
  simp [decode]

def applyStored : Action → List ChildStorage → Nat → List ChildStorage
  | .field name, values, offset => fieldStored name values offset
  | .alias kind, values, offset => aliasStored kind values offset

def runActions : List Action → List ChildStorage → Nat → List ChildStorage
  | [], values, _ => values
  | action :: rest, values, offset => runActions rest (applyStored action values offset) offset

theorem actions_decode (actions : List Action) (values : List ChildStorage) (offset : Nat) :
    (runActions actions values offset).map restoreChild =
      interpretActions actions ⟨values.map restoreChild, offset⟩ := by
  induction actions generalizing values with
  | nil => rfl
  | cons action rest ih =>
    rw [runActions, ih]
    cases action with
    | field name => simp [applyStored, field_decodes, interpretActions, executeAction]
    | alias kind => simp [applyStored, alias_decodes, interpretActions, executeAction]

structure Operand where
  actions : List Action
  children : Stored
  offset : Nat

def decodeOperand (operand : Operand) : ParserEngine.Operand :=
  ⟨operand.actions, decode operand.children, operand.offset⟩

def runOperand (operand : Operand) : List ChildStorage :=
  runActions operand.actions (operand.children.materializeInto []).1 operand.offset

theorem operand_decodes (operand : Operand) :
    (runOperand operand).map restoreChild =
      interpretActions operand.actions ⟨decode operand.children, operand.offset⟩ := by
  rw [runOperand, actions_decode]
  simp [materialize_into_exact, decode]

/-- Top-first LR values must be consumed in reverse source order, then joined
by prepending completed operands. Nullable anchor production remains explicit
in Operand.offset; this theorem does not invent native byte coordinates. -/
def reduce (stack : List Operand) : Stored :=
  stackSequence (stack.map runOperand) (.leaf [])

theorem reduction_decodes (stack : List Operand) :
    decode (reduce stack) = reduceSource (stack.reverse.map decodeOperand) := by
  simp only [decode, reduce, stack_sequence_order, Sequence.flatten, List.append_nil,
    reduceSource, List.map_map, List.map_reverse]
  rw [List.map_flatten]
  simp only [List.map_map, List.map_reverse]
  congr 1
  apply congrArg List.reverse
  apply List.map_congr_left
  intro operand _
  exact operand_decodes operand

/-- Compose storage decoding with the existing POO preparation provenance,
not an assumption that any installed callback implements the grammar action. -/
theorem prepared_operand_decodes (operand : Operand) (plan : Plan operand.actions)
    (ready : LeanPoo.Functional.Requirements.prepare provider operand.actions = .ok plan) :
    (runOperand operand).map restoreChild =
      executePlan plan ⟨decode operand.children, operand.offset⟩ := by
  rw [operand_decodes, prepared_plan_equivalent operand.actions plan ready]

/-- Shared source-order denotation connects arbitrary nonempty physical values,
reverse-stack execution and preparation-selected action factories. -/
theorem prepared_reduction_decodes (source : List Operand)
    (plans : (operand : ParserEngine.Operand) → Plan operand.actions)
    (ready : ∀ operand, LeanPoo.Functional.Requirements.prepare provider operand.actions =
      .ok (plans operand)) :
    decode (reduce source.reverse) =
      reducePreparedStack (source.map decodeOperand).reverse plans := by
  rw [reduction_decodes, List.reverse_reverse,
    prepared_reverse_reduction_equivalent (source.map decodeOperand) plans ready]

end ParserEngine.SemanticValues
