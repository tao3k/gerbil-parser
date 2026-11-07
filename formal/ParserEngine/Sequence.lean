import Lean

namespace ParserEngine

/-- Logical proper-list leaves and concatenation records from runtime/funcs.ss. -/
inductive Sequence (α : Type) where
  | leaf (values : List α)
  | branch (left right : Sequence α)

def Sequence.flatten : Sequence α → List α
  | .leaf values => values
  | .branch left right => left.flatten ++ right.flatten

/-- One structural visit per record/leaf, plus one emission per element.
This models a builder traversal, not the append-based reference above. -/
def Sequence.visits : Sequence α → Nat
  | .leaf values => 1 + values.length
  | .branch left right => 1 + left.visits + right.visits

def Sequence.records : Sequence α → Nat
  | .leaf _ => 1
  | .branch left right => 1 + left.records + right.records

theorem sequence_visits_exact (seq : Sequence α) :
    seq.visits = seq.records + seq.flatten.length := by
  induction seq with
  | leaf values => rfl
  | branch left right ihLeft ihRight =>
    simp only [Sequence.visits, Sequence.records, Sequence.flatten, List.length_append]
    omega

/-- Copy each leaf cell once into a suffix, counting the actual recursive emissions. -/
def copyLeaf (values tail : List α) : List α × Nat :=
  match values with
  | [] => (tail, 0)
  | head :: rest =>
    let result := copyLeaf rest tail
    (head :: result.1, result.2 + 1)

theorem copy_leaf_exact (values tail : List α) :
    copyLeaf values tail = (values ++ tail, values.length) := by
  induction values with
  | nil => rfl
  | cons head rest ih => simp [copyLeaf, ih]

/-- Executable suffix builder: traversal never copies an accumulated emitted.
This recursive specification does not model Scheme's bounded-stack visitor. -/
def Sequence.materializeInto (seq : Sequence α) (tail : List α) : List α × Nat :=
  match seq with
  | .leaf values =>
    let result := copyLeaf values tail
    (result.1, result.2 + 1)
  | .branch left right =>
    let suffix := right.materializeInto tail
    let emitted :=  left.materializeInto suffix.1
    (emitted.1, 1 + emitted.2 + suffix.2)

theorem materialize_into_exact (seq : Sequence α) (tail : List α) :
    seq.materializeInto tail = (seq.flatten ++ tail, seq.records + seq.flatten.length) := by
  induction seq generalizing tail with
  | leaf values => simp [Sequence.materializeInto, copy_leaf_exact,
      Sequence.flatten, Sequence.records, Nat.add_comm]
  | branch left right ihLeft ihRight =>
    simp [Sequence.materializeInto, ihLeft, ihRight, Sequence.flatten,
      Sequence.records, List.append_assoc]
    omega

/-- The output accumulator restores source order when consuming the reverse LR stack. -/
def stackSequence (stack : List (List α)) (acc : Sequence α) : Sequence α :=
  match stack with
  | [] => acc
  | head :: rest => stackSequence rest (.branch (.leaf head) acc)

theorem stack_sequence_order (stack : List (List α)) (acc : Sequence α) :
    (stackSequence stack acc).flatten = stack.reverse.flatten ++ acc.flatten := by
  induction stack generalizing acc with
  | nil => simp [stackSequence]
  | cons head rest ih =>
    simp [stackSequence, ih, Sequence.flatten, List.reverse_cons, List.append_assoc]

theorem reverse_stack_source_order (source : List (List α)) :
    (stackSequence source.reverse (.leaf [])).flatten = source.flatten := by
  simp [stack_sequence_order, Sequence.flatten]

/-- Copies made by repeatedly appending a singleton to an ordinary accumulated list. -/
def copiedPrefix : Nat → Nat
  | 0 => 0
  | n + 1 => copiedPrefix n + n

theorem copied_prefix_triangular (n : Nat) : 2 * copiedPrefix n + n = n * n := by
  induction n with
  | zero => rfl
  | succ n ih =>
    simp only [copiedPrefix, Nat.mul_add, Nat.mul_succ, Nat.succ_mul]
    omega

/-- A persistent join allocates one branch without copying either child list. -/
def joinAllocations : Nat → Nat
  | 0 => 0
  | n + 1 => joinAllocations n + 1

theorem join_allocations_linear (n : Nat) : joinAllocations n = n := by
  induction n with
  | zero => rfl
  | succ n ih => simp [joinAllocations, ih]

end ParserEngine
