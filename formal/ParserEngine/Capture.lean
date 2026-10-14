namespace ParserEngine.Capture

/-- Reverse the used stack prefix directly into source-order children while
retaining its suffix. Child values and their ownership are not changed. -/
def splitPrefix (width : Nat) (stack acc : List α) : List α × List α :=
  match width, stack with
  | 0, _ => (acc, stack)
  | _ + 1, [] => (acc, [])
  | n + 1, head :: tail => splitPrefix n tail (head :: acc)

theorem split_prefix_exact (width : Nat) (stack acc : List α) :
    splitPrefix width stack acc = ((stack.take width).reverse ++ acc, stack.drop width) := by
  induction width generalizing stack acc with
  | zero => simp [splitPrefix]
  | succ n ih =>
    cases stack with
    | nil => simp [splitPrefix]
    | cons head tail => simp [splitPrefix, ih, List.reverse_cons, List.append_assoc]

theorem source_order_preserved (width : Nat) (stack : List α) :
    (splitPrefix width stack []).1 = (stack.take width).reverse := by
  rw [split_prefix_exact]
  simp

theorem suffix_preserved (width : Nat) (stack : List α) :
    (splitPrefix width stack []).2 = stack.drop width := by rw [split_prefix_exact]

/-- Counts are additive metadata. The fused stack-order sum equals the
source-order sum; arbitrary effectful observers are not covered. -/
theorem token_count_order_independent (stack : List α) (width : Nat) (count : α → Nat) :
    (((stack.take width).reverse).map count).sum = ((stack.take width).map count).sum := by
  simp [List.map_reverse]

/-- Count list-cell inspections for take/drop/reverse, last, count fold and yield
fold. States, nested child getters, constructor and publication costs are separate.
For width zero the last traversal is omitted. -/
def formerVisits (width : Nat) := 5 * width + (width - 1)
def fusedVisits (width : Nat) := if width = 1 then 1 else 2 * width

theorem nullable_split_exact (stack : List α) : splitPrefix 0 stack [] = ([], stack) := rfl

theorem unary_split_exact (head : α) (tail : List α) :
    splitPrefix 1 (head :: tail) [] = ([head], tail) := rfl

theorem capture_visits_not_more (width : Nat) : fusedVisits width ≤ formerVisits width := by
  unfold fusedVisits formerVisits
  split <;> omega

theorem capture_visits_strict (width : Nat) (positive : 0 < width) :
    fusedVisits width < formerVisits width := by
  unfold fusedVisits formerVisits
  split <;> omega

end ParserEngine.Capture
