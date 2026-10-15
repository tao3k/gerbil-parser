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

/-- A capture yield is opaque execution metadata. Only its ordered terminal
sequence is observed by transfer admission; binary association is not an ABI. -/
inductive YieldTree (α : Type) where
  | empty
  | leaf (value : α)
  | branch (left right : YieldTree α)

def YieldTree.terminals : YieldTree α → List α
  | .empty => []
  | .leaf value => [value]
  | .branch left right => left.terminals ++ right.terminals

def yieldJoin (left right : YieldTree α) : YieldTree α :=
  match left, right with
  | .empty, right => right
  | left, .empty => left
  | left, right => .branch left right

theorem yield_join_order (left right : YieldTree α) :
    (yieldJoin left right).terminals = left.terminals ++ right.terminals := by
  cases left <;> cases right <;> simp [yieldJoin, YieldTree.terminals]

theorem left_yield_fold_order (pieces : List (YieldTree α)) (initial : YieldTree α) :
    (pieces.foldl yieldJoin initial).terminals =
      initial.terminals ++ (pieces.map YieldTree.terminals).flatten := by
  induction pieces generalizing initial with
  | nil => simp
  | cons first rest ih =>
    simp [List.foldl_cons, ih, yield_join_order, List.append_assoc]

theorem right_yield_fold_order (pieces : List (YieldTree α)) :
    (pieces.foldr yieldJoin .empty).terminals = (pieces.map YieldTree.terminals).flatten := by
  induction pieces with
  | nil => rfl
  | cons first rest ih => simp [List.foldr_cons, yield_join_order, ih]

/-- Reverse-stack accumulation prepends each complete piece yield. It can fuse
capture's count/children traversal without changing any ordered terminal leaf.
Existing relocation subtrees are opaque pieces and are not distributed/reboxed. -/
theorem fused_yield_observation (pieces : List (YieldTree α)) :
    (pieces.foldr yieldJoin .empty).terminals =
      (pieces.foldl yieldJoin .empty).terminals := by
  rw [right_yield_fold_order, left_yield_fold_order]
  rfl

/-- This counts logical piece classifications, not native heap bytes or GC.
Published retained fragments, prefix frames and complete artifacts remain paid.
For positive widths the old algorithm inspects endpoints plus count/yield. -/
def separateWideDecisions (width : Nat) := 2 * width + 2
def fusedWideDecisions (width : Nat) := width

theorem fused_wide_decisions_strict (width : Nat) :
    fusedWideDecisions width < separateWideDecisions width := by
  unfold fusedWideDecisions separateWideDecisions
  omega

/-- A whole-request cost model must retain allocation and tracing charges.
A reduction in logical visits alone places no bound on these supplied costs. -/
structure CaptureCost where
  transitions : Nat
  allocated : Nat
  retained : Nat
  gc : Nat

def CaptureCost.total (cost : CaptureCost) : Nat :=
  cost.transitions + cost.allocated + cost.retained + cost.gc

theorem capture_cost_requires_all_charges (before after : CaptureCost)
    (allocation : after.allocated ≤ before.allocated)
    (retention : after.retained ≤ before.retained)
    (collection : after.gc ≤ before.gc)
    (execution : after.transitions < before.transitions) :
    after.total < before.total := by
  unfold CaptureCost.total
  omega

/-- A capture signature is a live reuse premise: identical consumed yield
alone does not certify an unchanged exit lexical boundary. -/
structure BoundarySignature where
  kind : Nat
  lexeme : List Nat
  deriving DecidableEq

def boundaryAccepts (expected actual : Option BoundarySignature) : Bool := decide (expected = actual)

theorem changed_boundary_rejected (expected actual : Option BoundarySignature)
    (changed : expected ≠ actual) : boundaryAccepts expected actual = false := by
  simp [boundaryAccepts, changed]

example : boundaryAccepts (some ⟨1, [97]⟩) (some ⟨1, [98]⟩) = false := by decide

/-- A memo belongs to one immutable grammar fragment. Token leaves are retained
by identity; relocation wrappers keep their original nesting. The producer
premise is essential: arbitrary writes to topology invalidate the memo. -/
inductive YieldMemo (α : Type) where
  | pending
  | ready (value : α)

def forceYield (produce : Unit → α) : YieldMemo α → α × YieldMemo α
  | .pending => let value := produce (); (value, .ready value)
  | .ready value => (value, .ready value)

theorem delayed_yield_first_exact (produce : Unit → α) :
    forceYield produce .pending = (produce (), .ready (produce ())) := rfl

theorem delayed_yield_warm_exact (produce : Unit → α) :
    forceYield produce (forceYield produce .pending).2 = forceYield produce .pending := rfl

/-- Deferral removes fresh capture's parallel yield-tree allocations only when
no consumer requests the yield. Forced reuse still pays its full construction,
plus any memo traversal overhead; this is not free work or a GC-time proof. -/
def eagerYieldAllocations (branches : Nat) := branches
def delayedYieldAllocations (demand : Bool) (branches : Nat) := if demand then branches else 0

theorem fresh_yield_allocations_eliminated (branches : Nat) :
    delayedYieldAllocations false branches = 0 := rfl

theorem forced_yield_allocations_retained (branches : Nat) :
    delayedYieldAllocations true branches = eagerYieldAllocations branches := rfl

/-- Fresh capture needs span/count metadata, not the deferred terminal index.
A span consumer must not accidentally force the derived yield while classifying. -/
def captureBounds : Piece Yield → Int × Int × Nat
  | .token start finish _ => (start, finish, 1)
  | .fragment metadata => (metadata.start, metadata.finish, metadata.tokens)
  | .view delta base => (delta + pieceStart base, delta + pieceEnd base, pieceTokens base)

theorem capture_bounds_exact (piece : Piece Yield) :
    captureBounds piece = (pieceStart piece, pieceEnd piece, pieceTokens piece) := by
  cases piece <;> rfl

/-- A prepared reduction context contains only transition identity. Payload
includes ordered children, exact lookahead, semantic value, bounds/count, anchor
and derived-index memo; none of those request-local values are interned. -/
structure TransitionContext (Runtime Executor : Type) where
  runtime : Runtime
  executor : Executor
  production : Nat
  entry : Nat
  target : Nat
  lookaheadKind : Option Nat

structure FlatCapture (Runtime Executor Payload : Type) where
  runtime : Runtime
  executor : Executor
  production : Nat
  entry : Nat
  target : Nat
  lookaheadKind : Option Nat
  payload : Payload

structure SharedCapture (Runtime Executor Payload : Type) where
  context : TransitionContext Runtime Executor
  payload : Payload

def shareCapture (value : FlatCapture Runtime Executor Payload) : SharedCapture Runtime Executor Payload :=
  ⟨⟨value.runtime, value.executor, value.production, value.entry, value.target, value.lookaheadKind⟩, value.payload⟩

def expandCapture (value : SharedCapture Runtime Executor Payload) : FlatCapture Runtime Executor Payload :=
  ⟨value.context.runtime, value.context.executor, value.context.production,
   value.context.entry, value.context.target, value.context.lookaheadKind, value.payload⟩

theorem shared_context_roundtrip (value : FlatCapture Runtime Executor Payload) :
    expandCapture (shareCapture value) = value := by cases value; rfl

theorem shared_context_consumer (value : FlatCapture Runtime Executor Payload)
    (consume : FlatCapture Runtime Executor Payload → Result) :
    consume (expandCapture (shareCapture value)) = consume value := by rw [shared_context_roundtrip]

/-- Coordinate admission includes runtime, production and entry identity.
Mutable executor/target changes must replace a cached descriptor. -/
theorem cached_context_admission (wanted cached : TransitionContext Runtime Executor)
    (coordinates : cached.runtime = wanted.runtime ∧ cached.production = wanted.production ∧
      cached.entry = wanted.entry)
    (live : cached.executor = wanted.executor ∧ cached.target = wanted.target ∧
      cached.lookaheadKind = wanted.lookaheadKind) : cached = wanted := by
  cases wanted; cases cached
  simp_all

/-- Hoisted directory admission requires a row owned by this runtime and
production. Executor and target remain live checks on every reduction. -/
theorem prepared_row_context_admission
    (row : Nat → Option (TransitionContext Runtime Executor))
    (runtime : Runtime) (production : Nat)
    (owned : ∀ entry context, row entry = some context →
      context.runtime = runtime ∧ context.production = production ∧ context.entry = entry)
    (wanted cached : TransitionContext Runtime Executor)
    (found : row wanted.entry = some cached)
    (sameRuntime : wanted.runtime = runtime) (sameProduction : wanted.production = production)
    (live : cached.executor = wanted.executor ∧ cached.target = wanted.target ∧
      cached.lookaheadKind = wanted.lookaheadKind) : cached = wanted := by
  apply cached_context_admission wanted cached
  · obtain ⟨hr, hp, he⟩ := owned wanted.entry cached found
    exact ⟨hr.trans sameRuntime.symm, hp.trans sameProduction.symm, he⟩
  · exact live

/-- Engine-produced unary child storage is unambiguous: a token vector or
final fragment/view is not a Scheme list. The first observation materializes
its stable list; ordinary multi-child lists are already ready. -/
inductive ChildStore (α : Type) where
  | unary (child : α)
  | ready (children : List α)

def forceChildren : ChildStore α → List α × ChildStore α
  | .unary child => ([child], .ready [child])
  | .ready children => (children, .ready children)

theorem unary_children_exact (child : α) :
    (forceChildren (.unary child)).1 = [child] := rfl

theorem unary_children_warm (child : α) :
    forceChildren (forceChildren (.unary child)).2 = forceChildren (.unary child) := rfl

/-- Only a request with no child-list demand avoids the unary cons. A first
query pays one cell and memo publication; concurrent first queries may duplicate
construction. This is not a whole-request CPU or exactly-once theorem. -/
def unaryChildrenCells (demand : Bool) : Nat := if demand then 1 else 0

theorem unary_children_no_demand : unaryChildrenCells false = 0 := rfl

theorem unary_children_first_demand : unaryChildrenCells true = 1 := rfl

/-- Snapshot the live kind and lexeme before deferring their pair allocation.
Retaining a mutable token vector instead would not preserve this obligation. -/
structure BoundarySnapshot (Lexeme : Type) where
  kind : Nat
  lexeme : Lexeme

def expandBoundary (kind : Nat) (lexeme : Lexeme) : BoundarySnapshot Lexeme := ⟨kind, lexeme⟩

theorem boundary_snapshot_exact (snapshot : BoundarySnapshot Lexeme) :
    expandBoundary snapshot.kind snapshot.lexeme = snapshot := by cases snapshot; rfl

/-- Static capture input is prepared with the runtime's immutable grammar.
The row and every context retain the same runtime/production coordinate. -/
structure CapturePlan (Runtime : Type) where
  runtime : Runtime
  production : Nat
  width : Nat

def preparedCapturePrefix (plan : CapturePlan Runtime) (stack : List α) :=
  splitPrefix plan.width stack []

theorem prepared_capture_prefix_exact (plan : CapturePlan Runtime) (width : Nat)
    (ready : plan.width = width) (stack : List α) :
    preparedCapturePrefix plan stack = ((stack.take width).reverse, stack.drop width) := by
  unfold preparedCapturePrefix
  rw [ready, split_prefix_exact]
  simp

/-- Explicit call-word accounting only. Compiler registers, continuation
frames, plan headers and native CPU require separate Scheme measurements. -/
def formerCaptureArguments (nodes : Nat) := 10 * nodes
def preparedCaptureArguments (nodes : Nat) := 8 * nodes

theorem prepared_capture_arguments_saved (nodes : Nat) :
    formerCaptureArguments nodes = preparedCaptureArguments nodes + 2 * nodes := by
  unfold formerCaptureArguments preparedCaptureArguments
  omega

/-- Only canonical transitions enter this cache. The compiled grammar's goto
mapping is immutable for this runtime/production/entry; arbitrary direct
executors create uncached descriptors and cannot violate these premises. -/
theorem canonical_context_admission (wanted cached : TransitionContext Runtime Executor)
    (canonical : Executor) (target : Nat)
    (coordinates : cached.runtime = wanted.runtime ∧ cached.production = wanted.production ∧
      cached.entry = wanted.entry)
    (cachedExecutor : cached.executor = canonical) (wantedExecutor : wanted.executor = canonical)
    (cachedTarget : cached.target = target) (wantedTarget : wanted.target = target)
    (kind : cached.lookaheadKind = wanted.lookaheadKind) : cached = wanted := by
  apply cached_context_admission wanted cached coordinates
  exact ⟨cachedExecutor.trans wantedExecutor.symm, cachedTarget.trans wantedTarget.symm, kind⟩

/-- Canonical and custom entries occupy disjoint halves of one bounded row.
The hot canonical index remains unchanged; the custom half retains exact checks. -/
theorem capture_row_partitions_disjoint (states canonical custom : Nat)
    (canonicalBound : canonical < states) : canonical ≠ states + custom := by
  omega

theorem capture_row_custom_index_bounded (states entry : Nat)
    (entryBound : entry < states) : states + entry < 2 * states := by
  omega

/-- Logical data slots only. Cold descriptor objects and the bounded directory
are charged; object headers, lookup work, allocator and GC are separate costs. -/
def flatCaptureSlots (nodes : Nat) := 13 * nodes
def sharedCaptureSlots (nodes contexts plans cells : Nat) := 9 * nodes + 6 * contexts + 4 * plans + cells

theorem shared_context_slot_break_even (nodes contexts plans cells : Nat) :
    sharedCaptureSlots nodes contexts plans cells < flatCaptureSlots nodes ↔
      6 * contexts + 4 * plans + cells < 4 * nodes := by
  unfold sharedCaptureSlots flatCaptureSlots
  omega

end ParserEngine.Capture
