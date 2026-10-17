import ParserEngine.DiscoveryOrder
import ParserEngine.KernelConstruction

/-! Native earliest-item merge order, constructed template order, and the
precise sorted-permutation contract required of the host list-sort primitive. -/
namespace ParserEngine.GroupOrder
open SparseGroupMasks

def ordered (touched : Nat → Bool) : List Row → List Row → List Row
  | left, [] => left
  | [], right :: rest => if touched right.key then ordered touched [] rest else right :: ordered touched [] rest
  | head :: tail, right :: rest =>
    if touched right.key then ordered touched (head :: tail) rest
    else if head.first < right.first then head :: ordered touched tail (right :: rest)
      else right :: ordered touched (head :: tail) rest
termination_by left right => left.length + right.length

theorem accumulator (touched : Nat → Bool) (left right found : List Row) :
    merge touched left right found = found.reverse ++ ordered touched left right := by
  cases right with
  | nil => simp [merge, ordered]
  | cons row rest =>
    cases left with
    | nil =>
      rw [merge, ordered]
      split
      · exact accumulator touched [] rest found
      · rw [accumulator touched [] rest (row :: found)]
        simp [List.reverse_cons, List.append_assoc]
    | cons head tail =>
      rw [merge, ordered]
      split
      · exact accumulator touched (head :: tail) rest found
      · split
        · rw [accumulator touched tail (row :: rest) (head :: found)]
          simp [List.reverse_cons, List.append_assoc]
        · rw [accumulator touched (head :: tail) rest (row :: found)]
          simp [List.reverse_cons, List.append_assoc]
termination_by left.length + right.length

theorem member (touched : Nat → Bool) (left right : List Row) (row : Row) :
    row ∈ ordered touched left right ↔ row ∈ left ∨ row ∈ right ∧ touched row.key = false := by
  have found := merge_member touched left right [] row
  simpa [accumulator] using found

theorem sorted (touched : Nat → Bool) (left right : List Row)
    (leftSorted : left.Pairwise (fun a b => a.first ≤ b.first))
    (rightSorted : right.Pairwise (fun a b => a.first ≤ b.first)) :
    (ordered touched left right).Pairwise (fun a b => a.first ≤ b.first) := by
  cases right with
  | nil => simpa [ordered] using leftSorted
  | cons row rest =>
    obtain ⟨rightHead, rightTail⟩ := List.pairwise_cons.mp rightSorted
    cases left with
    | nil =>
      rw [ordered]
      split
      · exact sorted touched [] rest leftSorted rightTail
      · apply List.pairwise_cons.mpr
        refine ⟨?_, sorted touched [] rest leftSorted rightTail⟩
        intro other belongs
        obtain absent | ⟨present, _⟩ := (member touched [] rest other).mp belongs
        · cases absent
        · exact rightHead other present
    | cons head tail =>
      obtain ⟨leftHead, leftTail⟩ := List.pairwise_cons.mp leftSorted
      rw [ordered]
      split
      · exact sorted touched (head :: tail) rest leftSorted rightTail
      · split
        next before =>
          apply List.pairwise_cons.mpr
          refine ⟨?_, sorted touched tail (row :: rest) leftTail rightSorted⟩
          intro other belongs
          obtain own | ⟨shared, _⟩ := (member touched tail (row :: rest) other).mp belongs
          · exact leftHead other own
          · rcases List.mem_cons.mp shared with rfl | later
            · omega
            · have bound := rightHead other later; omega
        next after =>
          apply List.pairwise_cons.mpr
          refine ⟨?_, sorted touched (head :: tail) rest leftSorted rightTail⟩
          intro other belongs
          obtain own | ⟨shared, _⟩ := (member touched (head :: tail) rest other).mp belongs
          · rcases List.mem_cons.mp own with rfl | later
            · omega
            · have bound := leftHead other later; omega
          · exact rightHead other shared
termination_by left.length + right.length

theorem own_sorted (symbol : Nat → Option Nat) (kernel predictions : List Nat) :
    (ownRows symbol kernel predictions).Pairwise (fun a b => a.first ≤ b.first) := by
  have all := List.pairwise_mergeSort (le := fun a b : Row => decide (a.first ≤ b.first))
    (by intro a b c ab bc; simp only [decide_eq_true_eq] at *; omega)
    (by intro a b; simp only [Bool.or_eq_true, decide_eq_true_eq]; omega)
  exact (all _).imp (by intro a b relation; exact of_decide_eq_true relation)

theorem groups_sorted (symbol : Nat → Option Nat) (kernel predictions : List Nat)
    (predictionOrder : predictions.Pairwise (· < ·)) :
    (groups symbol kernel predictions).Pairwise (fun a b => a.first ≤ b.first) := by
  simp only [groups, accumulator, List.reverse_nil, List.nil_append]
  exact sorted _ _ _ (own_sorted symbol kernel predictions)
    ((DiscoveryOrder.shared_sorted symbol predictions predictionOrder).imp (by intro a b lower; omega))

theorem offset_strict (widths : List Nat) (left right : Nat) (lower : left < right) (inside : right < widths.length) :
    Viable.denseOffset widths left < Viable.denseOffset widths right := by
  induction widths generalizing left right with
  | nil => simp at inside
  | cons width rest ih =>
    cases left with
    | zero =>
      cases right with
      | zero => omega
      | succ right => simp only [Viable.denseOffset]; omega
    | succ left =>
      cases right with
      | zero => omega
      | succ right =>
        have smaller := ih left right (by omega) (by simpa using inside)
        simp only [Viable.denseOffset]
        omega

theorem materialized_sorted (g : Grammar.CFG) (mask : Nat) :
    (PredictionMaterialization.materialize g mask).Pairwise (· < ·) := by
  apply List.pairwise_map.mpr
  apply ((List.pairwise_lt_range (n := g.length)).filter _).imp_of_mem
  intro left right _ rightMem lower
  exact offset_strict _ left right lower (by simpa using (List.mem_range.mp (List.mem_filter.mp rightMem).1))

theorem generated_sorted (g : Grammar.CFG) (mask : Nat) :
    (KernelConstruction.rows g mask).Pairwise (fun a b => a.first ≤ b.first) := by
  exact groups_sorted _ _ _ (materialized_sorted g _)

theorem row_origin (symbol : Nat → Option Nat) (kernel predictions : List Nat) (row : Row)
    (belongs : row ∈ groups symbol kernel predictions) : symbol row.first = some row.key := by
  obtain impossible | own | ⟨shared, _⟩ := (merge_member _ _ _ [] row).mp belongs
  · cases impossible
  · obtain ⟨key, keyMem, same⟩ := List.mem_map.mp (List.mem_mergeSort.mp own)
    cases same
    simp only
    split
    next shared =>
      rw [Nat.min_def]
      split
      · exact (DiscoveryOrder.first_origin symbol kernel key keyMem).2
      · exact (DiscoveryOrder.first_origin symbol predictions key shared).2
    next own => exact (DiscoveryOrder.first_origin symbol kernel key keyMem).2
  · obtain ⟨key, keyMem, same⟩ := List.mem_map.mp shared
    cases same
    exact (DiscoveryOrder.first_origin symbol predictions key keyMem).2

theorem row_ties (symbol : Nat → Option Nat) (kernel predictions : List Nat) (left right : Row)
    (leftMem : left ∈ groups symbol kernel predictions) (rightMem : right ∈ groups symbol kernel predictions)
    (same : left.first = right.first) : left = right := by
  have leftRead := row_origin symbol kernel predictions left leftMem
  have rightRead := row_origin symbol kernel predictions right rightMem
  rw [same] at leftRead
  have keys := Option.some.inj (leftRead.symm.trans rightRead)
  have masks : left.mask = right.mask := by rw [row_mask _ _ _ left leftMem, row_mask _ _ _ right rightMem, keys]
  cases left; cases right; simp_all

/-- Any primitive sort satisfying sortedness and permutation has the same
answer here. There is no hidden dependency on sorting stability or algorithm. -/
theorem sort_unique (source actual expected : List Row)
    (actualPerm : actual.Perm source) (expectedPerm : expected.Perm source)
    (actualSorted : actual.Pairwise (fun a b => a.first ≤ b.first))
    (expectedSorted : expected.Pairwise (fun a b => a.first ≤ b.first))
    (ties : ∀ left ∈ source, ∀ right ∈ source, left.first = right.first → left = right) : actual = expected := by
  apply List.Perm.eq_of_pairwise _ actualSorted expectedSorted (actualPerm.trans expectedPerm.symm)
  intro left right leftMem rightMem forward backward
  exact ties left (actualPerm.mem_iff.mp leftMem) right (expectedPerm.mem_iff.mp rightMem) (by omega)

theorem native_own_sort (symbol : Nat → Option Nat) (kernel predictions : List Nat) (actual : List Row)
    (permutation : actual.Perm (ownUnsorted symbol kernel predictions))
    (orderedRows : actual.Pairwise (fun a b => a.first ≤ b.first)) : actual = ownRows symbol kernel predictions := by
  have expected := List.mergeSort_perm (ownUnsorted symbol kernel predictions) (fun a b => decide (a.first ≤ b.first))
  apply sort_unique (ownUnsorted symbol kernel predictions) actual (ownRows symbol kernel predictions)
    permutation expected orderedRows (own_sorted symbol kernel predictions)
  intro left leftMem right rightMem same
  apply row_ties symbol kernel predictions left right _ _ same
  · exact (merge_member _ _ _ [] left).mpr (Or.inr (Or.inl (List.mem_mergeSort.mpr leftMem)))
  · exact (merge_member _ _ _ [] right).mpr (Or.inr (Or.inl (List.mem_mergeSort.mpr rightMem)))

end ParserEngine.GroupOrder
