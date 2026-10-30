import ParserEngine.GroupScratch

/-! The native shared-template table is populated by first-row creation and
subsequent mask writes. Its query law is derived from that recurrence, never
supplied as a certificate for a final table. -/
namespace ParserEngine.TemplateIndex
open SparseGroupMasks
abbrev Index := Nat → Option Row

def view (state : GroupScratch.State) : Index := fun key =>
  if state.masks key = 0 then none else some ⟨state.firsts key, key, state.masks key⟩

def record (symbol : Nat → Option Nat) (before : Index) (item : Nat) : Index :=
  match symbol item with
  | none => before
  | some key =>
    let prior := (before key).getD ⟨item, key, 0⟩
    ProducerAdmission.Storage.update before key ⟨prior.first, key, prior.mask ||| 2 ^ (item + 1)⟩

def scan (symbol : Nat → Option Nat) : List Nat → Index → Index
  | [], before => before
  | item :: rest, before => scan symbol rest (record symbol before item)

theorem record_view (symbol : Nat → Option Nat) (state : GroupScratch.State) (item : Nat) :
    record symbol (view state) item = view (GroupScratch.record symbol state item) := by
  funext query
  cases selected : symbol item with
  | none => simp [record, GroupScratch.record, selected]
  | some key =>
    by_cases same : query = key
    · subst query
      by_cases zero : state.masks key = 0
      · simp [record, view, selected, GroupScratch.record, add, ProducerAdmission.Storage.update, zero]
      · have occupied : state.masks key ||| 2 ^ (item + 1) ≠ 0 := by simp
        simp [record, view, selected, GroupScratch.record, add, ProducerAdmission.Storage.update, zero, occupied]

    · by_cases zero : state.masks key = 0 <;>
        simp [record, view, selected, GroupScratch.record, add, ProducerAdmission.Storage.update, same, zero]

theorem scan_view (symbol : Nat → Option Nat) (items : List Nat) (state : GroupScratch.State) :
    scan symbol items (view state) = view (GroupScratch.scan symbol items state) := by
  induction items generalizing state with
  | nil => rfl
  | cons item rest ih => simp only [scan, record_view, ih, GroupScratch.scan]

def build (symbol : Nat → Option Nat) (items : List Nat) : Index := scan symbol items (fun _ => none)

theorem lookup (symbol : Nat → Option Nat) (items : List Nat) (key : Nat) :
    build symbol items key = if key ∈ keys symbol items then some ⟨first symbol items key, key, mask symbol items key⟩ else none := by
  have empty : (fun _ => (none : Option Row)) = view (GroupScratch.borrow (fun _ => 0)) := by funext key; simp [view, GroupScratch.borrow]
  unfold build
  rw [empty, scan_view]
  simp only [view, GroupScratch.scan_masks]
  change (if mask symbol items key = 0 then none else some
    (⟨(GroupScratch.scan symbol items (GroupScratch.borrow (fun _ => 0))).firsts key, key, mask symbol items key⟩ : Row)) = _
  by_cases present : key ∈ keys symbol items
  · have nonzero := (mask_nonzero symbol items key).mpr present
    have initialized := GroupScratch.first_ready symbol items (GroupScratch.borrow (fun _ => 0)) key rfl present
    simp only [nonzero, present, ↓reduceIte]
    rw [initialized]
  · have zero : mask symbol items key = 0 := by
      by_cases zero : mask symbol items key = 0
      · exact zero
      · exact False.elim (present ((mask_nonzero symbol items key).mp zero))
    simp [zero, present]

def own (index : Index) (scratch : GroupScratch.State) : List Row := scratch.order.map fun key =>
  match index key with
  | none => ⟨scratch.firsts key, key, scratch.masks key⟩
  | some shared => ⟨min (scratch.firsts key) shared.first, key, scratch.masks key ||| shared.mask⟩

theorem own_exact (symbol : Nat → Option Nat) (items : List Nat) (scratch : GroupScratch.State) :
    own (build symbol items) scratch = scratch.order.map (fun key =>
      (⟨if key ∈ keys symbol items then min (scratch.firsts key) (first symbol items key) else scratch.firsts key,
        key, scratch.masks key ||| mask symbol items key⟩ : Row)) := by
  apply List.map_congr_left
  intro key _
  rw [lookup]
  by_cases present : key ∈ keys symbol items
  · simp [present]
  · have zero : mask symbol items key = 0 := by
      by_cases zero : mask symbol items key = 0
      · exact zero
      · exact False.elim (present ((mask_nonzero symbol items key).mp zero))
    simp [present, zero]

end ParserEngine.TemplateIndex
