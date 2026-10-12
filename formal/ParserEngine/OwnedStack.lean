import ParserEngine.Reduction

namespace ParserEngine

/-- The private buffer's active prefix has the same newest-first order as an
    immutable checkpoint stack. Capacity and inactive cells are not public data. -/
def freezeStack (storage : Array α) (depth : Nat) : List α :=
  (storage.toList.take depth).reverse

theorem freeze_stack_length (storage : Array α) (depth : Nat)
    (fits : depth ≤ storage.size) :
    (freezeStack storage depth).length = depth := by
  simp [freezeStack, Nat.min_eq_left fits]

/-- Growth/clearing may alter inactive cells only. This condition is discharged
    by Scheme storage tests; it is not a proof of the native bulk primitives. -/
theorem freeze_stack_ext (left right : Array α) (depth : Nat)
    (same : left.toList.take depth = right.toList.take depth) :
    freezeStack left depth = freezeStack right depth := by
  exact congrArg List.reverse same

theorem freeze_stack_roundtrip (stack : List α) :
    freezeStack stack.reverse.toArray stack.length = stack := by
  have all : stack.reverse.take stack.length = stack.reverse := by
    simpa only [List.length_reverse] using (List.take_length : List.take stack.reverse.length stack.reverse = stack.reverse)
  simp [freezeStack, all]

/-- A checked LR frame owns one more active state than semantic values. -/
theorem frozen_frame_aligned (states : Array Nat) (values : Array Children) (top : Nat)
    (stateFits : top + 1 ≤ states.size) (valueFits : top ≤ values.size) :
    (freezeStack states (top + 1)).length = (freezeStack values top).length + 1 := by
  rw [freeze_stack_length states (top + 1) stateFits,
      freeze_stack_length values top valueFits]

/-- Writing one admitted value after the retained prefix publishes a cons onto
    its immutable reverse-order frontier. No heap-safety claim is implied. -/
theorem freeze_stack_push (before after : Array α) (top : Nat) (value : α)
    (retained : after.toList.take (top + 1) = before.toList.take top ++ [value]) :
    freezeStack after (top + 1) = value :: freezeStack before top := by
  simp [freezeStack, retained, List.reverse_append]

/-- Array indices in the private transition loop remain in the admitted capacity.
    These arithmetic obligations do not certify native allocation or fixnum limits. -/
theorem owned_reduction_index_fits (top width capacity : Nat)
    (fits : top < capacity) (admitted : width ≤ top) :
    top - width < capacity := by omega

theorem owned_operand_index_fits (top width index capacity : Nat)
    (fits : top < capacity) (admitted : width ≤ top) (operand : index < width) :
    top - index - 1 < capacity := by omega

theorem owned_shift_index_fits (top capacity : Nat) (fits : top < capacity) :
    top + 1 < (if top + 1 < capacity then capacity else capacity * 2) := by
  split <;> omega

end ParserEngine
