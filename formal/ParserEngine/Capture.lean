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

/-- Scalar metadata with an opaque terminal yield. Yield relocation is supplied
by the existing owner; no associativity or canonicalization is assumed. -/
structure Metadata (Yield : Type) where
  start : Int
  finish : Int
  tokens : Nat
  yield : Yield

inductive Piece (Yield : Type) where
  | token (start finish : Int) (yield : Yield)
  | fragment (metadata : Metadata Yield)
  | view (delta : Int) (base : Piece Yield)

def pieceStart : Piece Yield → Int
  | .token start _ _ => start
  | .fragment m => m.start
  | .view delta base => delta + pieceStart base

def pieceEnd : Piece Yield → Int
  | .token _ finish _ => finish
  | .fragment m => m.finish
  | .view delta base => delta + pieceEnd base

def pieceTokens : Piece Yield → Nat
  | .token _ _ _ => 1
  | .fragment m => m.tokens
  | .view _ base => pieceTokens base

def pieceYield (relocate : Yield → Int → Yield) : Piece Yield → Yield
  | .token _ _ yield => yield
  | .fragment m => m.yield
  | .view delta base => relocate (pieceYield relocate base) delta

def separateMetadata (relocate : Yield → Int → Yield) (piece : Piece Yield) : Metadata Yield :=
  ⟨pieceStart piece, pieceEnd piece, pieceTokens piece, pieceYield relocate piece⟩

/-- Reuse the existing four projections for a view, preserving its exact
relocation nesting. Closed base representations can be classified once. -/
def classifiedMetadata (relocate : Yield → Int → Yield) : Piece Yield → Metadata Yield
  | .token start finish yield => ⟨start, finish, 1, yield⟩
  | .fragment m => m
  | .view delta base => separateMetadata relocate (.view delta base)

theorem classified_metadata_exact (relocate : Yield → Int → Yield) (piece : Piece Yield) :
    classifiedMetadata relocate piece = separateMetadata relocate piece := by
  cases piece <;> rfl

/-- The consumer receives the identical fields exactly once. The function is
pure; Scheme must establish stable local reads and absence of callbacks. -/
theorem classified_consumer_exact (relocate : Yield → Int → Yield) (piece : Piece Yield)
    (consume : Metadata Yield → Value) :
    consume (classifiedMetadata relocate piece) = consume (separateMetadata relocate piece) := by
  rw [classified_metadata_exact]

/-- Count complete piece-kind decisions, not accessor checks, native instructions
or allocations. Each relocated root adds one decision before the old path. -/
def separateDecisions (ordinary views : Nat) := 4 * (ordinary + views)
def classifiedDecisions (ordinary views : Nat) := ordinary + 5 * views

theorem classification_break_even (ordinary views : Nat) :
    classifiedDecisions ordinary views ≤ separateDecisions ordinary views ↔ views ≤ 3 * ordinary := by
  unfold classifiedDecisions separateDecisions
  omega

-- Fresh capture benefits in this model; a view-only workload regresses.
example : classifiedDecisions 193 0 < separateDecisions 193 0 := by decide
example : separateDecisions 0 64 < classifiedDecisions 0 64 := by decide

/-- Dense execution exposes every immutable frontier. Sparse execution constructs
only frontiers requested by the same shift-index predicate. Frame includes all
states, semantic values, recognition, mode and source coordinates. -/
def numberedFrames (next : Nat) : List Frame → List (Nat × Frame)
  | [] => []
  | frame :: rest => (next, frame) :: numberedFrames (next + 1) rest

def selectedFrames (spacing next : Nat) : List Frame → List Frame
  | [] => []
  | frame :: rest =>
    if next % spacing = 0 then frame :: selectedFrames spacing (next + 1) rest
    else selectedFrames spacing (next + 1) rest

def denseSelection (spacing next : Nat) (frames : List Frame) : List Frame :=
  (numberedFrames next frames).filterMap
    (fun pair => if pair.1 % spacing = 0 then some pair.2 else none)

theorem sparse_frontiers_exact (spacing next : Nat) (frames : List Frame) :
    selectedFrames spacing next frames = denseSelection spacing next frames := by
  induction frames generalizing next with
  | nil => rfl
  | cons frame rest ih =>
    simp only [selectedFrames, denseSelection, numberedFrames, List.filterMap_cons]
    split <;> simp_all [denseSelection]

theorem sparse_frontiers_not_more (spacing next : Nat) (frames : List Frame) :
    (selectedFrames spacing next frames).length ≤ frames.length := by
  induction frames generalizing next with
  | nil => simp [selectedFrames]
  | cons frame rest ih =>
    simp only [selectedFrames]
    split <;> simp only [List.length_cons]
    · exact Nat.add_le_add_right (ih (next + 1)) 1
    · exact Nat.le_trans (ih (next + 1)) (Nat.le_succ _)

theorem first_shift_skipped (spacing : Nat) (wide : 1 < spacing)
    (frame : Frame) (rest : List Frame) :
    (selectedFrames spacing 1 (frame :: rest)).length < (frame :: rest).length := by
  have nonzero : 1 % spacing ≠ 0 := by rw [Nat.mod_eq_of_lt wide]; decide
  simp only [selectedFrames, nonzero, ↓reduceIte, List.length_cons]
  change (selectedFrames spacing 2 rest).length < rest.length + 1
  exact Nat.lt_succ_of_le (sparse_frontiers_not_more spacing 2 rest)

/-- Fusing driver calls preserves ordered transitions. State must include error
and observable callback effects; this is not a Scheme refinement theorem. -/
theorem fused_driver_order (step : State → Action → State)
    (chunks : List (List Action)) (initial : State) :
    chunks.foldl (fun state chunk => chunk.foldl step state) initial =
      chunks.flatten.foldl step initial := by
  induction chunks generalizing initial with
  | nil => rfl
  | cons chunk rest ih => simp [List.foldl_cons, List.flatten_cons, List.foldl_append, ih]

end ParserEngine.Capture
