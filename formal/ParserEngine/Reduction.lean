import ParserEngine.Preparation
import ParserEngine.Sequence

namespace ParserEngine

structure Operand where
  actions : List Action
  children : Children
  offset : Nat

def reduceSource (operands : List Operand) : Children :=
  (operands.map fun operand => interpretActions operand.actions ⟨operand.children, operand.offset⟩).flatten

/-- A successful deterministic reduction; failure/admission precedes this operation. -/
def reduceStack (operands : List Operand) : Children :=
  (stackSequence
    (operands.map fun operand => interpretActions operand.actions ⟨operand.children, operand.offset⟩)
    (.leaf [])).flatten

theorem reverse_reduction_equivalent (source : List Operand) :
    reduceStack source.reverse = reduceSource source := by
  simp only [reduceStack, reduceSource, List.map_reverse]
  exact reverse_stack_source_order _

/-- Prepared operand factories retain operand-local nullable anchors. -/
def reducePreparedStack (operands : List Operand)
    (plans : (operand : Operand) → Plan operand.actions) : Children :=
  (stackSequence
    (operands.map fun operand => executePlan (plans operand) ⟨operand.children, operand.offset⟩)
    (.leaf [])).flatten

theorem prepared_reverse_reduction_equivalent (source : List Operand)
    (plans : (operand : Operand) → Plan operand.actions)
    (ready : ∀ operand, LeanPoo.Functional.Requirements.prepare provider operand.actions =
      .ok (plans operand)) :
    reducePreparedStack source.reverse plans = reduceSource source := by
  have same : (fun operand : Operand =>
      executePlan (plans operand) ⟨operand.children, operand.offset⟩) =
      (fun operand : Operand =>
        interpretActions operand.actions ⟨operand.children, operand.offset⟩) := by
    funext operand
    exact prepared_plan_equivalent operand.actions (plans operand) (ready operand) _
  simp only [reducePreparedStack, same]
  exact reverse_reduction_equivalent source

/-- LR holds one more state than semantic values. Width admission is explicit. -/
theorem reduction_preserves_depth (states : List Nat) (values : List Children)
    (width goto : Nat) (result : Children)
    (aligned : states.length = values.length + 1) (fits : width ≤ values.length) :
    (goto :: states.drop width).length = (result :: values.drop width).length + 1 := by
  simp only [List.length_cons, List.length_drop]
  omega

theorem shift_preserves_depth (states : List Nat) (values : List Children)
    (next : Nat) (token : Children) (aligned : states.length = values.length + 1) :
    (next :: states).length = (token :: values).length + 1 := by
  simp only [List.length_cons]
  omega

/-- A full flat publication charges every source byte and every event emission. -/
def publicationWork (sourceBytes : Nat) (events : List α) : Nat := sourceBytes + events.length

theorem complete_publication_lower_bound (bytes : Nat) (events : List α) :
    bytes ≤ publicationWork bytes events ∧ events.length ≤ publicationWork bytes events := by
  simp [publicationWork]

end ParserEngine
