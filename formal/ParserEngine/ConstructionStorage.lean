import ParserEngine.PlanStorage

/-! Three owned vectors follow one append/publication history. A missing row
is the unpublished ghost tag of the native empty-list initializer; decoding
either missing or published empty rows yields the same physical empty list. -/
namespace ParserEngine.ConstructionStorage
open ProducerAdmission
variable {Value : Type}
abbrev Row := List (Nat × Nat)

structure Heap where
  states : List (Option (List Nat))
  plans : List (Option PlanStorage.Plan)
  rows : List (Option Row)

def empty (layout : PlanStorage.Layout) : Heap :=
  ⟨List.replicate layout.initial none, List.replicate layout.initial none,
    List.replicate layout.initial none⟩

def prepare (slots : List (Option Value)) (count : Nat) :=
  if count = slots.length then VectorStorage.grow slots else slots

def copy (source target : Heap) : Nat → Heap
  | 0 => target
  | count + 1 =>
    let prior := copy source target count
    let states := prior.states.set count (source.states[count]?.join)
    let rows := prior.rows.set count (source.rows[count]?.join)
    let plans := prior.plans.set count (source.plans[count]?.join)
    ⟨states, plans, rows⟩

theorem copy_fields (source target : Heap) (count : Nat) :
    (copy source target count).states = VectorStorage.copy source.states target.states count ∧
      (copy source target count).rows = VectorStorage.copy source.rows target.rows count ∧
      (copy source target count).plans = VectorStorage.copy source.plans target.plans count := by
  induction count with
  | zero => exact ⟨rfl, rfl, rfl⟩
  | succ count ih => simp [copy, VectorStorage.copy, ih]

def grow (heap : Heap) : Heap :=
  copy heap ⟨List.replicate (2 * heap.states.length) none, List.replicate (2 * heap.states.length) none,
    List.replicate (2 * heap.states.length) none⟩ heap.states.length

def append (heap : Heap) (count : Nat) (state : List Nat) (plan : PlanStorage.Plan) : Heap :=
  let prepared := if count = heap.states.length then grow heap else heap
  {prepared with states := prepared.states.set count (some state), plans := prepared.plans.set count (some plan)}

def publish (heap : Heap) (node : Nat) (row : Row) : Heap :=
  {heap with rows := heap.rows.set node (some row)}

/-- Erase publication ownership to the native empty-list representation. -/
def rowAt (heap : Heap) (node : Nat) : Row := (heap.rows[node]?.join).getD []

theorem empty_row_encoding : (none : Option Row).getD [] = (some ([] : Row)).getD [] := rfl

theorem prepare_length (slots : List (Option Value)) (count : Nat) :
    (prepare slots count).length = VectorStorage.capacity count slots.length :=
  VectorStorage.prepared_length slots count

theorem prepare_read (slots : List (Option Value)) (count node : Nat) :
    (prepare slots count)[node]?.join = slots[node]?.join := by
  unfold prepare
  split
  · rw [VectorStorage.grow_exact]
    by_cases inside : node < slots.length
    · rw [StateInterning.grow_retains slots node inside]
    · unfold StateInterning.grow
      rw [List.getElem?_append_right (by omega : slots.length ≤ node)]
      simp only [List.getElem?_replicate, Option.join]
      split <;> simp [List.getElem?_eq_none (by omega : slots.length ≤ node)]
  · rfl

theorem append_read (slots : List (Option Value)) (count : Nat) (value : Value)
    (bounded : count ≤ slots.length) (positive : 0 < slots.length) (node : Nat) :
    (VectorStorage.append slots count value)[node]?.join =
      Storage.update (fun query => slots[query]?.join) count value node := by
  by_cases same : node = count
  · subst node
    simp [VectorStorage.append_read slots count value bounded positive, Storage.update]
  · unfold VectorStorage.append
    rw [List.getElem?_set_ne (Ne.symm same)]
    exact (prepare_read slots count node).trans (by simp [Storage.update, same])

structure Shape (heap : Heap) (count : Nat) : Prop where
  plans : heap.plans.length = heap.states.length
  rows : heap.rows.length = heap.states.length
  bounded : count ≤ heap.states.length
  positive : 0 < heap.states.length

/-- One common copy loop refines all three projections; no independent growth
choice for any vector is permitted by the append implementation. -/
theorem append_exact (heap : Heap) (count : Nat) (state : List Nat) (plan : PlanStorage.Plan)
    (shape : Shape heap count) : append heap count state plan =
      ⟨VectorStorage.append heap.states count state, VectorStorage.append heap.plans count plan,
        prepare heap.rows count⟩ := by
  unfold append VectorStorage.append prepare
  by_cases full : count = heap.states.length
  · simp only [shape.rows, shape.plans, full, ↓reduceIte, grow, (copy_fields _ _ _).1,
      (copy_fields _ _ _).2.1, (copy_fields _ _ _).2.2]
    simp only [VectorStorage.grow, shape.rows, shape.plans]
  · have rowRoom : count ≠ heap.rows.length := by simpa only [shape.rows] using full
    have planRoom : count ≠ heap.plans.length := by simpa only [shape.plans] using full
    simp only [full, rowRoom, planRoom, ↓reduceIte]

theorem empty_shape (layout : PlanStorage.Layout) : Shape (empty layout) 0 := by
  exact ⟨by simp [empty], by simp [empty], by simp [empty], by simpa [empty] using layout.positive⟩

theorem append_shape (heap : Heap) (count : Nat) (state : List Nat) (plan : PlanStorage.Plan)
    (shape : Shape heap count) : Shape (append heap count state plan) (count + 1) := by
  have room := VectorStorage.room count heap.states.length shape.bounded shape.positive
  rw [append_exact heap count state plan shape]
  refine ⟨?_, ?_, ?_, ?_⟩
  · simp only [VectorStorage.append_length, shape.plans]
  · simp only [VectorStorage.append_length, prepare_length, shape.rows]
  · simpa only [VectorStorage.append_length] using Nat.succ_le_of_lt room
  · simp only [VectorStorage.append_length]; omega

theorem publish_shape (heap : Heap) (count node : Nat) (row : Row)
    (shape : Shape heap count) : Shape (publish heap node row) count := by
  exact ⟨shape.plans, by simpa [publish] using shape.rows, shape.bounded, shape.positive⟩

structure Refines (heap : Heap) (states : Nat → Option (List Nat))
    (plans : Nat → Option PlanStorage.Plan) (rows : Nat → Option Row) : Prop where
  states : ∀ node, heap.states[node]?.join = states node
  plans : ∀ node, heap.plans[node]?.join = plans node
  rows : ∀ node, heap.rows[node]?.join = rows node

theorem empty_refines (layout : PlanStorage.Layout) :
    Refines (empty layout) (fun _ => none) (fun _ => none) (fun _ => none) := by
  have missing {Value : Type} (node : Nat) : ((List.replicate layout.initial (none : Option Value))[node]?).join = none := by
    simp only [List.getElem?_replicate, Option.join]; split <;> rfl
  exact ⟨missing, missing, missing⟩

theorem append_refines (heap : Heap) (count : Nat) (state : List Nat) (plan : PlanStorage.Plan)
    (shape : Shape heap count) (before : Refines heap states plans rows) :
    Refines (append heap count state plan) (Storage.update states count state)
      (Storage.update plans count plan) rows := by
  rw [append_exact heap count state plan shape]
  refine ⟨?_, ?_, ?_⟩
  · intro node
    exact (append_read heap.states count state shape.bounded shape.positive node).trans
      (by simp only [before.states])
  · intro node
    exact (append_read heap.plans count plan (by simpa [shape.plans] using shape.bounded)
      (by simpa [shape.plans] using shape.positive) node).trans (by simp only [before.plans])
  · intro node
    exact (prepare_read heap.rows count node).trans (before.rows node)

theorem publish_refines (heap : Heap) (count node : Nat) (row : Row)
    (shape : Shape heap count) (inside : node < count) (before : Refines heap states plans rows) :
    Refines (publish heap node row) states plans (Storage.update rows node row) := by
  refine ⟨before.states, before.plans, ?_⟩
  intro query
  by_cases same : query = node
  · subst query
    have bound : node < heap.rows.length := by rw [shape.rows]; exact Nat.lt_of_lt_of_le inside shape.bounded
    simp only [publish, List.getElem?_set_self bound, Option.join_some, Storage.update, ↓reduceIte]
  · simp only [publish, List.getElem?_set_ne (Ne.symm same), before.rows, Storage.update, same, ↓reduceIte]

inductive Trace (layout : PlanStorage.Layout) : Nat → Heap → Prop where
  | start : Trace layout 0 (empty layout)
  | append {count heap} (prior : Trace layout count heap) (state : List Nat) (plan : PlanStorage.Plan) :
      Trace layout (count + 1) (append heap count state plan)
  | publish {count heap} (prior : Trace layout count heap) (node : Nat) (row : Row) (inside : node < count) :
      Trace layout count (publish heap node row)

theorem trace_shape {layout : PlanStorage.Layout} (trace : Trace layout count heap) : Shape heap count := by
  induction trace with
  | start => exact empty_shape layout
  | append _ state plan ih => exact append_shape _ _ state plan ih
  | publish _ node row _ ih => exact publish_shape _ _ node row ih

end ParserEngine.ConstructionStorage
