import ParserEngine.LookaheadExecution

/-! Reconstruct node coordinates from the actual two-pass LR0 item layout.
Empty state rows retain their state IDs. Zero core IDs are ordinary values;
range premises, rather than a zero sentinel, justify every physical read. -/
namespace ParserEngine.NodeCoordinates
open ProducerAdmission
abbrev Rows := List (List Nat)

def width (rows : Rows) : Nat := (rows.map List.length).sum

def offsets (base : Nat) : Rows → List Nat
  | [] => [base]
  | row :: rest => base :: offsets (base + row.length) rest

def coordinates (state : Nat) : Rows → List (Nat × Nat)
  | [] => []
  | row :: rest => row.map (fun core => (state, core)) ++ coordinates (state + 1) rest

def readItems (rows : Rows) (node : Nat) : Nat := (rows.flatten[node]?).getD 0

theorem offset_at (before after : Rows) (base : Nat) :
    (offsets base (before ++ after))[before.length]? = some (base + width before) := by
  induction before generalizing base with
  | nil => cases after <;> simp [offsets, width]
  | cons row rest ih => simpa [offsets, width, Nat.add_assoc, Nat.add_comm, Nat.add_left_comm] using ih (base + row.length)

theorem coordinate_length (state : Nat) (rows : Rows) : (coordinates state rows).length = width rows := by
  induction rows generalizing state with
  | nil => rfl
  | cons row rest ih => simp [coordinates, width, ih]

theorem row_coordinate (before after : Rows) (row : List Nat) (state position : Nat)
    (inside : position < row.length) :
    (coordinates state (before ++ row :: after))[width before + position]? =
      (row[position]?).map (fun core => (state + before.length, core)) := by
  induction before generalizing state with
  | nil =>
    simp only [List.nil_append, coordinates, width, List.map_nil, List.sum_nil, Nat.zero_add, List.length_nil, Nat.add_zero]
    rw [List.getElem?_append_left (by simpa using inside), List.getElem?_map]
  | cons head before ih =>
    simp only [List.cons_append, coordinates]
    rw [List.getElem?_append_right (by simp [width]; omega)]
    have shifted : width (head :: before) + position - (head.map (fun core => (state, core))).length =
        width before + position := by simp [width]; omega
    rw [shifted]
    simpa [Nat.add_assoc, Nat.add_comm, Nat.add_left_comm] using ih (state + 1)

theorem row_item (before after : Rows) (row : List Nat) (position : Nat) (inside : position < row.length) :
    (before ++ row :: after).flatten[width before + position]? = row[position]? := by
  induction before with
  | nil => simp [width, List.getElem?_append_left inside]
  | cons head before ih =>
    simp only [List.cons_append, List.flatten_cons]
    rw [List.getElem?_append_right (by simp [width]; omega)]
    have shifted : width (head :: before) + position - head.length = width before + position := by
      simp [width]; omega
    rw [shifted]
    exact ih

/-- Both offset sentinels and the flattened core vector come from the same row
slice. This is the input protocol of lookahead-node-index, not a caller-supplied
(state,core) pairing for every final node. -/
theorem slice_coordinate (before after : Rows) (row : List Nat) (node : Nat)
    (inside : width before ≤ node ∧ node < width before + row.length) :
    (coordinates 0 (before ++ row :: after))[node]? =
      some (before.length, readItems (before ++ row :: after) node) := by
  let position := node - width before
  have bound : position < row.length := by dsimp [position]; omega
  have address : width before + position = node := by dsimp [position]; omega
  have raw := row_item before after row position bound
  have paired := row_coordinate before after row 0 position bound
  rw [address] at raw paired
  have value : row[position]? = some row[position] := by simp [bound]
  rw [value] at paired raw
  simp only [readItems, raw, Option.getD_some]
  simpa only [Nat.zero_add, Option.map_some] using paired

/-- Native binary-search success yields an extracted coordinate in the same
state slice, even when equal core IDs occur in different states. -/
theorem lookup_sound (before after : Rows) (row : List Nat) (core node : Nat)
    (found : LookaheadExecution.Search.find (readItems (before ++ row :: after)) core
      (width before) (width before + row.length) = some node) :
    (coordinates 0 (before ++ row :: after))[node]? = some (before.length, core) := by
  obtain ⟨low, high, value⟩ := LookaheadExecution.Search.sound _ _ _ _ _ found
  simpa [value] using slice_coordinate before after row node ⟨low, high⟩

theorem slice_sorted (before after : Rows) (row : List Nat) (sorted : row.Pairwise (· < ·)) :
    ∀ i j, width before ≤ i → j < width before + row.length → i < j →
      readItems (before ++ row :: after) i < readItems (before ++ row :: after) j := by
  intro i j low high earlier
  have iBound : i - width before < row.length := by omega
  have jBound : j - width before < row.length := by omega
  have first := row_item before after row (i - width before) iBound
  have second := row_item before after row (j - width before) jBound
  have firstAddress : width before + (i - width before) = i := by omega
  have secondAddress : width before + (j - width before) = j := by omega
  rw [firstAddress] at first
  rw [secondAddress] at second
  have ordered := List.pairwise_iff_getElem.mp sorted (i - width before) (j - width before) iBound jBound (by omega)
  simp only [readItems, first, second, List.getElem?_eq_getElem iBound, List.getElem?_eq_getElem jBound, Option.getD_some]
  exact ordered

theorem lookup_complete (before after : Rows) (row : List Nat) (sorted : row.Pairwise (· < ·))
    (position core : Nat) (entry : row[position]? = some core) :
    LookaheadExecution.Search.find (readItems (before ++ row :: after)) core
      (width before) (width before + row.length) = some (width before + position) := by
  have inside : position < row.length := by
    by_cases good : position < row.length
    · exact good
    · simp [List.getElem?_eq_none (by omega : row.length ≤ position)] at entry
  apply LookaheadExecution.Search.complete
  · omega
  · simp only [readItems, row_item before after row position inside, entry, Option.getD_some]
  · exact slice_sorted before after row sorted

/-- The physical vector fill has the same read protocol as the flattening
above. Writes into later slots do not change a previously written slot. -/
theorem written_at (values : List Nat) (start index : Nat) (old : Nat → Option Nat)
    (inside : index < values.length) : Storage.write start values old (start + index) = values[index]? := by
  induction values generalizing start index old with
  | nil => simp at inside
  | cons head rest ih =>
    cases index with
    | zero =>
      simp only [Nat.add_zero, Storage.write]
      rw [Storage.before rest (start + 1) start _ (by omega)]
      simp [Storage.update]
    | succ index =>
      change Storage.write (start + 1) rest (Storage.update old start head) (start + (index + 1)) = rest[index]?
      rw [show start + (index + 1) = (start + 1) + index by omega]
      exact ih (start + 1) index (Storage.update old start head) (by simpa using inside)

end ParserEngine.NodeCoordinates
