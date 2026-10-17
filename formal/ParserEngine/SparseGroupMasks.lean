import ParserEngine.PhysicalCoreExtraction
import Init.Data.List.Sort.Lemmas

/-! Native sparse grouping accumulates advanced core bits, combines shared
prediction groups once, and filters touched shared groups during the merge.
Meaning is independent of row order; earliest-item ordering is a separate law. -/
namespace ParserEngine.SparseGroupMasks
abbrev Store := Nat → Nat

def add (symbol : Nat → Option Nat) (store : Store) (item : Nat) : Store :=
  match symbol item with
  | none => store
  | some key => fun target => if target = key then store target ||| 2 ^ (item + 1) else store target

def accumulate (symbol : Nat → Option Nat) : List Nat → Store → Store
  | [], store => store
  | item :: rest, store => accumulate symbol rest (add symbol store item)

theorem add_bit (symbol : Nat → Option Nat) (store : Store) (item key bit : Nat) :
    (add symbol store item key).testBit bit = true ↔
      (store key).testBit bit = true ∨ symbol item = some key ∧ bit = item + 1 := by
  cases source : symbol item with
  | none => simp [add, source]
  | some owner =>
    by_cases same : key = owner
    · subst owner
      simp only [add, source, ↓reduceIte, Nat.testBit_or, Bool.or_eq_true,
        Nat.testBit_two_pow, decide_eq_true_eq, true_and]
      exact or_congr Iff.rfl eq_comm
    · simp [add, source, same, Ne.symm same]

theorem accumulated_bit (symbol : Nat → Option Nat) (items : List Nat) (store : Store) (key bit : Nat) :
    (accumulate symbol items store key).testBit bit = true ↔
      (store key).testBit bit = true ∨ ∃ item ∈ items, symbol item = some key ∧ bit = item + 1 := by
  induction items generalizing store with
  | nil => simp [accumulate]
  | cons item rest ih => simp [accumulate, ih, add_bit, or_assoc]

def mask (symbol : Nat → Option Nat) (items : List Nat) (key : Nat) : Nat := accumulate symbol items (fun _ => 0) key

theorem mask_bit (symbol : Nat → Option Nat) (items : List Nat) (key bit : Nat) :
    (mask symbol items key).testBit bit = true ↔ ∃ item ∈ items, symbol item = some key ∧ bit = item + 1 := by
  simp [mask, accumulated_bit]

def keys (symbol : Nat → Option Nat) (items : List Nat) : List Nat := (items.filterMap symbol).eraseDups

theorem key_present (symbol : Nat → Option Nat) (items : List Nat) (key : Nat) :
    key ∈ keys symbol items ↔ ∃ item ∈ items, symbol item = some key := by simp [keys]

theorem mask_nonzero (symbol : Nat → Option Nat) (items : List Nat) (key : Nat) :
    mask symbol items key ≠ 0 ↔ key ∈ keys symbol items := by
  constructor
  · intro nonzero
    by_cases selected : key ∈ keys symbol items
    · exact selected
    apply False.elim
    apply nonzero
    apply (Encoding.mask_zero_iff _).mpr
    intro bit
    cases value : (mask symbol items key).testBit bit with
    | false => rfl
    | true =>
      obtain ⟨item, member, source, _⟩ := (mask_bit symbol items key bit).mp value
      exact False.elim (selected ((key_present symbol items key).mpr ⟨item, member, source⟩))
  · intro present zero
    obtain ⟨item, member, source⟩ := (key_present symbol items key).mp present
    have bit := (mask_bit symbol items key (item + 1)).mpr ⟨item, member, source, rfl⟩
    simp [zero] at bit

structure Row where
  first : Nat
  key : Nat
  mask : Nat
  deriving DecidableEq, Repr

def merge (touched : Nat → Bool) : List Row → List Row → List Row → List Row
  | left, [], found => found.reverse ++ left
  | [], right :: rest, found => if touched right.key then merge touched [] rest found
      else merge touched [] rest (right :: found)
  | head :: tail, right :: rest, found =>
    if touched right.key then merge touched (head :: tail) rest found
    else if head.first < right.first then merge touched tail (right :: rest) (head :: found)
      else merge touched (head :: tail) rest (right :: found)
termination_by left right _ => left.length + right.length

theorem merge_member (touched : Nat → Bool) (left right found : List Row) (row : Row) :
    row ∈ merge touched left right found ↔
      row ∈ found ∨ row ∈ left ∨ row ∈ right ∧ touched row.key = false := by
  cases right with
  | nil => simp [merge]
  | cons head rest =>
    cases left with
    | nil =>
      rw [merge]
      split
      next skip =>
        have absent : ¬(row = head ∧ touched row.key = false) := by
          rintro ⟨rfl, bad⟩; simp [skip] at bad
        simp [merge_member touched [] rest found row, or_and_right, absent]
      next keep =>
        have retained : touched head.key = false := Bool.eq_false_iff.mpr keep
        have visible : (row = head ∧ touched row.key = false) ↔ row = head := by
          constructor
          · exact And.left
          · rintro rfl; exact ⟨rfl, retained⟩
        simp [merge_member touched [] rest (head :: found) row, or_and_right, visible, or_assoc, or_left_comm]
    | cons first tail =>
      rw [merge]
      split
      next skip =>
        have absent : ¬(row = head ∧ touched row.key = false) := by
          rintro ⟨rfl, bad⟩; simp [skip] at bad
        simp [merge_member touched (first :: tail) rest found row, or_and_right, absent, or_assoc]
      next keep =>
        have retained : touched head.key = false := Bool.eq_false_iff.mpr keep
        split
        next before => simp [merge_member touched tail (head :: rest) (first :: found) row, or_assoc, or_left_comm]
        next after =>
          have visible : (row = head ∧ touched row.key = false) ↔ row = head := by
            constructor
            · exact And.left
            · rintro rfl; exact ⟨rfl, retained⟩
          simp [merge_member touched (first :: tail) rest (head :: found) row, or_and_right, visible, or_assoc, or_left_comm]
termination_by left.length + right.length

def first (symbol : Nat → Option Nat) (items : List Nat) (key : Nat) : Nat :=
  ((items.find? fun item => symbol item == some key)).getD 0

def sharedRows (symbol : Nat → Option Nat) (predictions : List Nat) : List Row :=
  (keys symbol predictions).map fun key => ⟨first symbol predictions key, key, mask symbol predictions key⟩
def ownRows (symbol : Nat → Option Nat) (kernel predictions : List Nat) : List Row :=
  ((keys symbol kernel).map fun key =>
    ⟨if key ∈ keys symbol predictions then min (first symbol kernel key) (first symbol predictions key)
      else first symbol kernel key, key, mask symbol kernel key ||| mask symbol predictions key⟩).mergeSort (fun a b => a.first ≤ b.first)

def groups (symbol : Nat → Option Nat) (kernel predictions : List Nat) : List Row :=
  merge (fun key => mask symbol kernel key != 0) (ownRows symbol kernel predictions) (sharedRows symbol predictions) []

def combined (symbol : Nat → Option Nat) (kernel predictions : List Nat) (key : Nat) : Nat :=
  mask symbol kernel key ||| mask symbol predictions key

theorem own_at (symbol : Nat → Option Nat) (kernel predictions : List Nat) (key : Nat)
    (present : key ∈ keys symbol kernel) :
    ∃ row ∈ ownRows symbol kernel predictions, row.key = key ∧ row.mask = combined symbol kernel predictions key := by
  refine ⟨_, List.mem_mergeSort.mpr (List.mem_map.mpr ⟨key, present, rfl⟩), rfl, rfl⟩

theorem shared_at (symbol : Nat → Option Nat) (predictions : List Nat) (key : Nat)
    (present : key ∈ keys symbol predictions) :
    ∃ row ∈ sharedRows symbol predictions, row.key = key ∧ row.mask = mask symbol predictions key :=
  ⟨_, List.mem_map.mpr ⟨key, present, rfl⟩, rfl, rfl⟩

theorem row_mask (symbol : Nat → Option Nat) (kernel predictions : List Nat) (row : Row)
    (member : row ∈ groups symbol kernel predictions) : row.mask = combined symbol kernel predictions row.key := by
  have chosen := (merge_member _ _ _ [] row).mp member
  rcases chosen with impossible | own | ⟨shared, untouched⟩
  · cases impossible
  · obtain ⟨key, _, same⟩ := List.mem_map.mp (List.mem_mergeSort.mp own)
    cases same; rfl
  · obtain ⟨key, _, same⟩ := List.mem_map.mp shared
    cases same
    have zero : mask symbol kernel key = 0 := by simpa using untouched
    simp [combined, zero]

theorem row_bit (symbol : Nat → Option Nat) (kernel predictions : List Nat) (key bit : Nat) :
    (∃ row ∈ groups symbol kernel predictions, row.key = key ∧ row.mask.testBit bit = true) ↔
      (combined symbol kernel predictions key).testBit bit = true := by
  constructor
  · rintro ⟨row, member, same, present⟩
    simpa [row_mask symbol kernel predictions row member, same] using present
  · intro present
    by_cases own : mask symbol kernel key = 0
    · have bit : (mask symbol predictions key).testBit bit = true := by simpa [combined, own] using present
      have nonzero : mask symbol predictions key ≠ 0 := by intro zero; simp [zero] at bit
      obtain ⟨row, member, same, maskEq⟩ := shared_at symbol predictions key ((mask_nonzero _ _ _).mp nonzero)
      refine ⟨row, (merge_member _ _ _ [] row).mpr (Or.inr (Or.inr ⟨member, ?_⟩)), same, ?_⟩
      · simp [same, own]
      · simpa [maskEq] using bit
    · obtain ⟨row, member, same, maskEq⟩ := own_at symbol kernel predictions key ((mask_nonzero _ _ _).mp own)
      exact ⟨row, (merge_member _ _ _ [] row).mpr (Or.inr (Or.inl member)), same, by simpa [maskEq] using present⟩

theorem groups_bit (symbol : Nat → Option Nat) (kernel predictions : List Nat) (key bit : Nat) :
    (∃ row ∈ groups symbol kernel predictions, row.key = key ∧ row.mask.testBit bit = true) ↔
      (∃ item ∈ kernel, symbol item = some key ∧ bit = item + 1) ∨
      (∃ item ∈ predictions, symbol item = some key ∧ bit = item + 1) := by
  rw [row_bit]
  simp [combined, Nat.testBit_or, Bool.or_eq_true, mask_bit]

end ParserEngine.SparseGroupMasks
