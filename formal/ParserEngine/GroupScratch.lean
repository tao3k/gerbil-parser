import ParserEngine.SparseGroupMasks

/-! Borrowed grouping storage follows the real zero-test/write/reset order.
Masks are restored globally; first-item slots are intentionally not cleared. -/
namespace ParserEngine.GroupScratch
open SparseGroupMasks

structure State where
  masks : Store
  firsts : Store
  order : List Nat

def record (symbol : Nat → Option Nat) (before : State) (item : Nat) : State :=
  match symbol item with
  | none => before
  | some key =>
    { masks := add symbol before.masks item
      firsts := if before.masks key = 0 then fun target => if target = key then item else before.firsts target else before.firsts
      order := if before.masks key = 0 then key :: before.order else before.order }

def scan (symbol : Nat → Option Nat) : List Nat → State → State
  | [], before => before
  | item :: rest, before => scan symbol rest (record symbol before item)

def clear : List Nat → Store → Store
  | [], masks => masks
  | key :: rest, masks => clear rest (fun target => if target = key then 0 else masks target)

def reset (before : State) : State := {before with masks := clear before.order before.masks, order := []}
def borrow (firsts : Store) : State := ⟨fun _ => 0, firsts, []⟩

def Covered (before : State) : Prop := ∀ key, before.masks key ≠ 0 → key ∈ before.order

theorem record_masks (symbol : Nat → Option Nat) (before : State) (item : Nat) :
    (record symbol before item).masks = add symbol before.masks item := by
  cases source : symbol item <;> simp [record, add, source]

theorem scan_masks (symbol : Nat → Option Nat) (items : List Nat) (before : State) :
    (scan symbol items before).masks = accumulate symbol items before.masks := by
  induction items generalizing before with
  | nil => rfl
  | cons item rest ih => simp only [scan, ih, record_masks, accumulate]

theorem record_covered (symbol : Nat → Option Nat) (before : State) (covered : Covered before) (item : Nat) :
    Covered (record symbol before item) := by
  intro key nonzero
  cases source : symbol item with
  | none => simpa only [record, source] using covered key (by simpa [record, source] using nonzero)
  | some owner =>
    by_cases empty : before.masks owner = 0
    · by_cases same : key = owner
      · simp [record, source, empty, same]
      · have old : before.masks key ≠ 0 := by simpa [record, add, source, same] using nonzero
        simpa only [record, source, empty, ↓reduceIte] using List.mem_cons_of_mem owner (covered key old)
    · by_cases same : key = owner
      · simpa [record, source, empty, same] using covered owner empty
      · have old : before.masks key ≠ 0 := by simpa [record, add, source, same] using nonzero
        simpa [record, source, empty] using covered key old

theorem scan_covered (symbol : Nat → Option Nat) (items : List Nat) (before : State) (covered : Covered before) :
    Covered (scan symbol items before) := by
  induction items generalizing before with
  | nil => exact covered
  | cons item rest ih => exact ih _ (record_covered symbol before covered item)

theorem clear_read (order : List Nat) (masks : Store) (key : Nat) :
    clear order masks key = if key ∈ order then 0 else masks key := by
  induction order generalizing masks with
  | nil => simp [clear]
  | cons owner rest ih => simp [clear, ih]; split <;> simp_all

theorem reset_zero (before : State) (covered : Covered before) : (reset before).masks = fun _ => 0 := by
  funext key
  simp only [reset, clear_read]
  split
  · rfl
  next absent =>
    by_cases zero : before.masks key = 0
    · exact zero
    · exact False.elim (absent (covered key zero))

/-- A completed call restores all masks, including keys never touched by it.
No claim is made that stale first-item slots are zeroed. -/
theorem borrowed_reset (symbol : Nat → Option Nat) (items : List Nat) (firsts : Store) :
    (reset (scan symbol items (borrow firsts))).masks = (borrow firsts).masks := by
  apply reset_zero
  apply scan_covered
  intro key nonzero
  exact False.elim (nonzero rfl)

/-- Arbitrary stale first-item slots cannot affect mask accumulation. -/
theorem masks_independent (symbol : Nat → Option Nat) (items : List Nat) (left right : Store) :
    (scan symbol items (borrow left)).masks = (scan symbol items (borrow right)).masks := by
  simp only [scan_masks, borrow]

theorem first_write (symbol : Nat → Option Nat) (before : State) (item key : Nat)
    (selected : symbol item = some key) (empty : before.masks key = 0) :
    (record symbol before item).firsts key = item := by simp [record, selected, empty]

theorem first_retained (symbol : Nat → Option Nat) (before : State) (item key : Nat)
    (occupied : before.masks key ≠ 0) :
    (record symbol before item).firsts key = before.firsts key := by
  cases source : symbol item with
  | none => simp [record, source]
  | some owner =>
    by_cases same : key = owner
    · subst owner; simp [record, source, occupied]
    · by_cases empty : before.masks owner = 0 <;> simp [record, source, empty, same]

theorem record_nonzero (symbol : Nat → Option Nat) (before : State) (item key : Nat)
    (occupied : before.masks key ≠ 0) : (record symbol before item).masks key ≠ 0 := by
  rw [record_masks]
  cases source : symbol item with
  | none => simpa [add, source] using occupied
  | some owner =>
    by_cases same : key = owner
    · simp only [add, source, same, ↓reduceIte]
      intro zero
      exact occupied (by simpa [same] using (Nat.or_eq_zero_iff.mp zero).1)
    · simpa [add, source, same] using occupied

theorem scan_retained (symbol : Nat → Option Nat) (items : List Nat) (before : State) (key : Nat)
    (occupied : before.masks key ≠ 0) : (scan symbol items before).firsts key = before.firsts key := by
  induction items generalizing before with
  | nil => rfl
  | cons item rest ih =>
    rw [scan, ih _ (record_nonzero symbol before item key occupied)]
    exact first_retained symbol before item key occupied

theorem first_ready (symbol : Nat → Option Nat) (items : List Nat) (before : State) (key : Nat)
    (empty : before.masks key = 0) (present : key ∈ keys symbol items) :
    (scan symbol items before).firsts key = first symbol items key := by
  induction items generalizing before with
  | nil => simp [keys] at present
  | cons item rest ih =>
    by_cases selected : symbol item = some key
    · have occupied : (record symbol before item).masks key ≠ 0 := by
        rw [record_masks]
        simp [add, selected, empty]
      rw [scan, scan_retained symbol rest _ key occupied, first_write symbol before item key selected empty]
      simp [first, selected]
    · have unchanged : (record symbol before item).masks key = before.masks key := by
        rw [record_masks]
        cases source : symbol item with
        | none => simp [add, source]
        | some owner =>
          have distinct : key ≠ owner := by intro same; subst owner; exact selected source
          simp [add, source, distinct]
      have tail : key ∈ keys symbol rest := by
        obtain ⟨origin, member, source⟩ := (key_present _ _ _).mp present
        rcases List.mem_cons.mp member with rfl | later
        · exact False.elim (selected source)
        · exact (key_present _ _ _).mpr ⟨origin, later, source⟩
      rw [scan, ih _ (unchanged.trans empty) tail]
      simp [first, selected]

def Recorded (before : State) : Prop :=
  before.order.Nodup ∧ ∀ key, key ∈ before.order ↔ before.masks key ≠ 0

theorem record_recorded (symbol : Nat → Option Nat) (before : State) (recorded : Recorded before) (item : Nat) :
    Recorded (record symbol before item) := by
  cases source : symbol item with
  | none => simpa [record, source] using recorded
  | some owner =>
    by_cases empty : before.masks owner = 0
    · have absent : owner ∉ before.order := by intro member; exact ((recorded.2 owner).mp member) empty
      refine ⟨by simpa [record, source, empty] using List.nodup_cons.mpr ⟨absent, recorded.1⟩, ?_⟩
      intro key
      by_cases same : key = owner
      · subst key; simp [record, add, source, empty]
      · simpa [record, add, source, empty, same] using recorded.2 key
    · refine ⟨by simpa [record, source, empty] using recorded.1, ?_⟩
      intro key
      by_cases same : key = owner
      · subst key
        have member := (recorded.2 owner).mpr empty
        have occupied := record_nonzero symbol before item owner empty
        exact iff_of_true (by simpa [record, source, empty] using member) occupied
      · simpa [record, add, source, empty, same] using recorded.2 key

theorem scan_recorded (symbol : Nat → Option Nat) (items : List Nat) (before : State) (recorded : Recorded before) :
    Recorded (scan symbol items before) := by
  induction items generalizing before with
  | nil => exact recorded
  | cons item rest ih => exact ih _ (record_recorded symbol before recorded item)

/-- The mutable touched list is a permutation of the discovered keys, not
an unchecked second symbol catalogue. Native sorting removes its reversal. -/
theorem order_permutation (symbol : Nat → Option Nat) (items : List Nat) (firsts : Store) :
    (scan symbol items (borrow firsts)).order.Perm (keys symbol items) := by
  have recorded := scan_recorded symbol items (borrow firsts) (by constructor; simp [borrow]; intro key; simp [borrow])
  apply (List.perm_ext_iff_of_nodup recorded.1 (keys_unique symbol items)).mpr
  intro key
  rw [recorded.2, scan_masks]
  exact mask_nonzero symbol items key

end ParserEngine.GroupScratch
