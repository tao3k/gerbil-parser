import ParserEngine.Construction

/-! Operational adapters for Scheme object storage, finite native scheduling
and dynamically scoped layout. These semantics describe the used primitives,
not an interpreter or a correctness theorem for Gambit/GCC. -/
namespace ParserEngine.Operational

namespace Heap

inductive Cell (Atom : Type) where
  | atom (value : Atom)
  | pair (first rest : Nat)
  | vector (references : List Nat)
  deriving DecidableEq, Repr

abbrev Store (Atom : Type) := List (Cell Atom)

inductive Tree (Atom : Type) where
  | atom (value : Atom)
  | pair (first rest : Tree Atom)
  deriving DecidableEq, Repr

/-- A published list recursively contains immutable pairs and atoms, including
shared subgraphs. Mutable vectors cannot masquerade as frozen list objects. -/
inductive Frozen (heap : Store Atom) : Nat → Tree Atom → Prop where
  | atom {address value} (read : heap[address]? = some (.atom value)) :
      Frozen heap address (.atom value)
  | pair {address first rest left right} (read : heap[address]? = some (.pair first rest))
      (car : Frozen heap first left) (cdr : Frozen heap rest right) :
      Frozen heap address (.pair left right)

def allocate (heap : Store Atom) (cell : Cell Atom) : Store Atom × Nat :=
  (heap ++ [cell], heap.length)

def writeVector (heap : Store Atom) (owner index value : Nat) : Store Atom × Bool :=
  match heap[owner]? with
  | some (.vector references) =>
      if index < references.length then
        (heap.set owner (.vector (references.set index value)), true)
      else (heap, false)
  | _ => (heap, false)

theorem allocation_identity (heap : Store Atom) (cell : Cell Atom) :
    (allocate heap cell).1[(allocate heap cell).2]? = some cell ∧
      (allocate heap cell).2 = heap.length := by simp [allocate]

theorem allocation_is_fresh (heap : Store Atom) (cell : Cell Atom) :
    heap[(allocate heap cell).2]? = none := by simp [allocate]

theorem frozen_transport {before after : Store Atom} {root tree}
    (retains : ∀ (address : Nat) (cell : Cell Atom), before[address]? = some cell → after[address]? = some cell)
    (published : Frozen before root tree) : Frozen after root tree := by
  induction published with
  | atom read => exact Frozen.atom (retains _ _ read)
  | pair read car cdr ihCar ihCdr => exact Frozen.pair (retains _ _ read) ihCar ihCdr

theorem allocation_retains {heap : Store Atom} {root tree}
    (published : Frozen heap root tree) (cell : Cell Atom) :
    Frozen (allocate heap cell).1 root tree := by
  apply frozen_transport (published := published)
  intro address value read
  by_cases bound : address < heap.length
  · simpa only [allocate, List.getElem?_append_left bound] using read
  · have missing := List.getElem?_eq_none (Nat.le_of_not_gt bound)
    rw [missing] at read
    cases read

theorem vector_update_retains {heap : Store Atom} {owner references root tree}
    (vectorAt : heap[owner]? = some (.vector references))
    (published : Frozen heap root tree) (replacement : List Nat) :
    Frozen (heap.set owner (.vector replacement)) root tree := by
  induction published with
  | @atom address value read =>
    have different : owner ≠ address := by
      intro same
      subst owner
      rw [vectorAt] at read
      cases read
    apply Frozen.atom
    rw [List.getElem?_set_ne different]
    exact read
  | @pair address first rest left right read car cdr ihCar ihCdr =>
    have different : owner ≠ address := by
      intro same
      subst owner
      rw [vectorAt] at read
      cases read
    apply Frozen.pair _ ihCar ihCdr
    rw [List.getElem?_set_ne different]
    exact read

theorem vector_write_retains {heap : Store Atom} {root tree}
    (published : Frozen heap root tree) (owner index value : Nat) :
    Frozen (writeVector heap owner index value).1 root tree := by
  unfold writeVector
  split
  · rename_i references vectorAt
    split
    · exact vector_update_retains vectorAt published _
    · exact published
  · exact published

theorem failed_write_atomic (heap : Store Atom) (owner index value : Nat)
    (failed : (writeVector heap owner index value).2 = false) :
    (writeVector heap owner index value).1 = heap := by
  cases read : heap[owner]? with
  | none => simp [writeVector, read]
  | some cell =>
    cases cell with
    | atom atom => simp [writeVector, read]
    | pair first rest => simp [writeVector, read]
    | vector references =>
      by_cases bound : index < references.length
      · simp [writeVector, read, bound] at failed
      · simp [writeVector, read, bound]

inductive Trace : Store Atom → Store Atom → Prop where
  | initial (heap : Store Atom) : Trace heap heap
  | allocate {before after} (earlier : Trace before after) (cell : Cell Atom) :
      Trace before (Heap.allocate after cell).1
  | write {before after} (earlier : Trace before after) (owner index value : Nat) :
      Trace before (writeVector after owner index value).1

theorem publication_survives_trace {before after : Store Atom} {root tree}
    (published : Frozen before root tree) (execution : Trace before after) : Frozen after root tree := by
  induction execution with
  | initial => exact published
  | allocate earlier cell ih => exact allocation_retains ih cell
  | write earlier owner index value ih => exact vector_write_retains ih owner index value

/-- A vector alias is observably mutable, unlike the pair/atom representation. -/
theorem vector_alias_counterexample :
    let heap : Store Nat := [.atom 7, .vector [0], .atom 9]
    (writeVector heap 1 0 2).1[1]? ≠ heap[1]? := by decide

theorem allocation_preserves_read {heap : Store Atom} {address : Nat} {value : Cell Atom}
    (read : heap[address]? = some value) (cell : Cell Atom) :
    (allocate heap cell).1[address]? = some value := by
  by_cases bound : address < heap.length
  · simpa only [allocate, List.getElem?_append_left bound] using read
  · have missing := List.getElem?_eq_none (Nat.le_of_not_gt bound)
    rw [missing] at read
    cases read

theorem other_vector_write_preserves_read {heap : Store Atom} {address owner index value : Nat}
    {cell : Cell Atom} (different : owner ≠ address) (read : heap[address]? = some cell) :
    (writeVector heap owner index value).1[address]? = some cell := by
  unfold writeVector
  split
  · split
    · rw [List.getElem?_set_ne different]; exact read
    · exact read
  · exact read

/-- A request owns allocations at or beyond its initial heap frontier. Writes
through an alias into an older request are excluded by this explicit protocol. -/
inductive PrivateTrace (frontier : Nat) : Store Atom → Store Atom → Prop where
  | initial (heap : Store Atom) : PrivateTrace frontier heap heap
  | allocate {before after} (earlier : PrivateTrace frontier before after) (cell : Cell Atom) :
      PrivateTrace frontier before (Heap.allocate after cell).1
  | write {before after} (earlier : PrivateTrace frontier before after) (owner index value : Nat)
      (owned : frontier ≤ owner) : PrivateTrace frontier before (writeVector after owner index value).1
  | replace {before after} (earlier : PrivateTrace frontier before after) (owner : Nat)
      (cell : Cell Atom) (owned : frontier ≤ owner) :
      PrivateTrace frontier before (after.set owner cell)

theorem private_trace_retains {before after : Store Atom} {frontier address : Nat} {cell : Cell Atom}
    (older : address < frontier) (read : before[address]? = some cell)
    (execution : PrivateTrace frontier before after) : after[address]? = some cell := by
  induction execution with
  | initial => exact read
  | allocate earlier cell ih => exact allocation_preserves_read ih cell
  | write earlier owner index value owned ih =>
    exact other_vector_write_preserves_read (by omega) ih
  | replace earlier owner cell owned ih =>
    rw [List.getElem?_set_ne (by omega : owner ≠ address)]
    exact ih

end Heap

namespace Scheduling

def eraseMany [BEq Fact] : List Fact → List Fact → List Fact
  | remaining, [] => remaining
  | remaining, fact :: rest => eraseMany (remaining.erase fact) rest

theorem erase_many_length_le [BEq Fact] (remaining incoming : List Fact) :
    (eraseMany remaining incoming).length ≤ remaining.length := by
  induction incoming generalizing remaining with
  | nil => exact Nat.le_refl _
  | cons fact rest ih => exact Nat.le_trans (ih _) List.length_erase_le

theorem erase_many_fresh_decreases [BEq Fact] [LawfulBEq Fact]
    (remaining incoming : List Fact) (fresh : ∃ fact ∈ incoming, fact ∈ remaining) :
    (eraseMany remaining incoming).length < remaining.length := by
  induction incoming generalizing remaining with
  | nil => simp at fresh
  | cons first rest ih =>
    by_cases present : first ∈ remaining
    · have shorter := List.length_erase_of_mem present
      have bounded := erase_many_length_le (remaining.erase first) rest
      simp only [eraseMany]
      have nonempty : 0 < remaining.length := List.length_pos_of_mem present
      omega
    · have unchanged := List.erase_eq_self_iff.mpr present
      simp only [eraseMany, unchanged]
      apply ih
      obtain ⟨fact, member, known⟩ := fresh
      rcases List.mem_cons.mp member with rfl | member
      · exact False.elim (present known)
      · exact ⟨fact, member, known⟩

structure Budget (Fact Owner : Type) where
  remaining : List Fact
  queued : List Owner

def rank (state : Budget Fact Owner) : Nat := 2 * state.remaining.length + state.queued.length

def publish [BEq Fact] [LawfulBEq Fact] [BEq Owner] (state : Budget Fact Owner) (owner : Owner)
    (incoming : List Fact) : Budget Fact Owner :=
  if incoming.any (fun fact => state.remaining.contains fact) then
    ⟨eraseMany state.remaining incoming,
      if state.queued.contains owner then state.queued else owner :: state.queued⟩
  else state

theorem stale_publication_noop [BEq Fact] [LawfulBEq Fact] [BEq Owner]
    (state : Budget Fact Owner) (owner : Owner) (incoming : List Fact)
    (old : ∀ fact ∈ incoming, fact ∉ state.remaining) : publish state owner incoming = state := by
  unfold publish
  split
  · rename_i live
    obtain ⟨fact, member, known⟩ := List.any_eq_true.mp live
    exact False.elim (old fact member (List.contains_iff_mem.mp known))
  · rfl

inductive Step [BEq Fact] [LawfulBEq Fact] [BEq Owner] :
    Budget Fact Owner → Budget Fact Owner → Prop where
  | publish {state owner incoming} (fresh : ∃ fact ∈ incoming, fact ∈ state.remaining) :
      Step state (Scheduling.publish state owner incoming)
  | drain (remaining : List Fact) (owner : Owner) (rest : List Owner) :
      Step ⟨remaining, owner :: rest⟩ ⟨remaining, rest⟩

theorem step_decreases [BEq Fact] [LawfulBEq Fact] [BEq Owner]
    {before after : Budget Fact Owner} (step : Step before after) : rank after < rank before := by
  cases step with
  | @publish state owner incoming fresh =>
    have shorter := erase_many_fresh_decreases _ _ fresh
    have live : incoming.any (fun fact => before.remaining.contains fact) = true := by
      obtain ⟨fact, member, known⟩ := fresh
      exact List.any_eq_true.mpr ⟨fact, member, List.contains_iff_mem.mpr known⟩
    simp only [publish, live, ite_true, rank]
    split <;> (try simp only [List.length_cons]) <;> omega
  | drain => simp [rank]

/-- Only progress steps recurse. Empty/old evidence is a finite callback
return, not an allowed infinitely repeated scheduler transition. -/
def finish [BEq Fact] [LawfulBEq Fact] [BEq Owner]
    (choose : (state : Budget Fact Owner) → Option {next // Step state next})
    (state : Budget Fact Owner) : Budget Fact Owner :=
  match choose state with
  | none => state
  | some next => finish choose next.1
termination_by rank state
decreasing_by exact step_decreases next.2

theorem finish_stopped [BEq Fact] [LawfulBEq Fact] [BEq Owner]
    (choose : (state : Budget Fact Owner) → Option {next // Step state next})
    (state : Budget Fact Owner) : choose (finish choose state) = none := by
  rw [finish]
  split
  next stopped => exact stopped
  next later equation => exact finish_stopped choose later.1
termination_by rank state
decreasing_by
  rename_i later _ _
  exact step_decreases later.2

theorem finish_quiescent [BEq Fact] [LawfulBEq Fact] [BEq Owner]
    (choose : (state : Budget Fact Owner) → Option {next // Step state next})
    (progress : ∀ state, state.queued ≠ [] → choose state ≠ none)
    (state : Budget Fact Owner) : (finish choose state).queued = [] := by
  by_cases empty : (finish choose state).queued = []
  · exact empty
  · exact False.elim (progress _ empty (finish_stopped choose state))

/-- Exact native partition recursion guard. Even the round producer is
arbitrary; recursion itself requires a strictly larger count below the limit. -/
def partitionRounds (nodes : Nat) (groups : State → Nat) (round : State → State)
    (state : State) : Nat :=
  if groups state < groups (round state) ∧ groups (round state) < nodes then
    1 + partitionRounds nodes groups round (round state)
  else 1
termination_by nodes - groups state
decreasing_by omega

theorem partition_round_bound (nodes : Nat) (groups : State → Nat) (round : State → State)
    (state : State) : partitionRounds nodes groups round state ≤ nodes - groups state + 1 := by
  rw [partitionRounds]
  split
  · have later := partition_round_bound nodes groups round (round state)
    omega
  · omega
termination_by nodes - groups state
decreasing_by omega

/-- A nonzero native delta supplies the fresh finite-domain witness needed by
Step.publish. The terminal bound and current mask/catalog relation are explicit. -/
theorem native_delta_progress {remaining : List (Nat × Nat)} {owner terminalCount incoming known : Nat}
    (bounded : ∀ bit, incoming.testBit bit = true → bit < terminalCount)
    (representation : ∀ bit, bit < terminalCount →
      ((owner, bit) ∈ remaining ↔ known.testBit bit = false))
    (novel : Encoding.difference incoming known ≠ 0) :
    ∃ fact ∈ ((List.range terminalCount).filter (fun bit => incoming.testBit bit)).map (fun bit => (owner, bit)),
      fact ∈ remaining := by
  obtain ⟨bit, present⟩ := Nat.exists_testBit_of_ne_zero novel
  have fresh := (Encoding.difference_fresh incoming known bit).mp present
  have bound := bounded bit fresh.1
  refine ⟨(owner, bit), ?_, (representation bit bound).mpr fresh.2⟩
  apply List.mem_map.mpr
  exact ⟨bit, List.mem_filter.mpr ⟨List.mem_range.mpr bound, fresh.1⟩, rfl⟩

inductive Trace [BEq Fact] [LawfulBEq Fact] [BEq Owner] :
    Nat → Budget Fact Owner → Budget Fact Owner → Prop where
  | initial (state : Budget Fact Owner) : Trace 0 state state
  | next {count before middle after} (step : Step before middle)
      (later : Trace count middle after) : Trace (count + 1) before after

theorem trace_length_bound [BEq Fact] [LawfulBEq Fact] [BEq Owner]
    {count : Nat} {before after : Budget Fact Owner} (execution : Trace count before after) :
    count + rank after ≤ rank before := by
  induction execution with
  | initial => omega
  | next step later ih => have decrease := step_decreases step; omega

end Scheduling

namespace Layout

structure Frame where
  column : Nat
  marker : String
  deriving DecidableEq, Repr

structure Token where
  lexeme : String
  start : Nat
  finish : Nat
  deriving DecidableEq, Repr

/-- Native column reads require a nonempty admitted token; EOF is represented
separately. Its final byte is within the source, without truncated subtraction. -/
theorem token_final_byte_in_bounds (token : Token) (bytes : Nat)
    (nonempty : token.start < token.finish) (bounded : token.finish ≤ bytes) :
    token.start ≤ token.finish - 1 ∧ token.finish - 1 < bytes := by omega

inductive Role where
  | ordinary | start | next
  deriving DecidableEq, Repr

def shiftAllowed (columns : Nat → Nat) (frames : List Frame) (token : Token) : Bool :=
  match frames with
  | [] => true
  | frame :: _ => decide (frame.column < columns token.start)

def nextEligible (columns : Nat → Nat) (frames : List Frame) (token : Token) : Bool :=
  match frames with
  | [] => false
  | frame :: _ => decide (columns (token.finish - 1) = frame.column ∧ token.lexeme = frame.marker)

def afterShift (columns : Nat → Nat) (frames : List Frame) (role : Role) (token : Token) : List Frame :=
  match role with
  | .start => ⟨columns (token.finish - 1), token.lexeme⟩ :: frames
  | _ => frames

def afterEnd (columns : Nat → Nat) (frames : List Frame)
    (token : Option Token) (boundaries : List String) : Option (List Frame) :=
  match frames with
  | [] => none
  | frame :: tail =>
      match token with
      | none => some tail
      | some next =>
          if !nextEligible columns frames next &&
              decide (columns next.start ≤ frame.column ∨ next.lexeme ∈ boundaries)
            then some tail else none

theorem start_frame_uses_final_byte (columns : Nat → Nat) (frames : List Frame) (token : Token) :
    afterShift columns frames .start token = ⟨columns (token.finish - 1), token.lexeme⟩ :: frames := rfl

theorem non_start_retains_frames (columns : Nat → Nat) (frames : List Frame) (token : Token)
    (role : Role) (other : role ≠ .start) : afterShift columns frames role token = frames := by
  cases role <;> simp_all [afterShift]

theorem end_requires_owned_frame {columns frames token boundaries tail}
    (closed : afterEnd columns frames token boundaries = some tail) :
    ∃ frame, frames = frame :: tail := by
  cases frames with
  | nil => simp [afterEnd] at closed
  | cons head rest =>
    cases token with
    | none => simp only [afterEnd, Option.some.injEq] at closed; exact ⟨head, by rw [closed]⟩
    | some token =>
      simp only [afterEnd] at closed
      split at closed
      · cases closed; exact ⟨head, rfl⟩
      · contradiction

theorem eligible_next_cannot_close {columns frames token boundaries}
    (eligible : nextEligible columns frames token = true) :
    afterEnd columns frames (some token) boundaries = none := by
  cases frames with
  | nil => simp [afterEnd]
  | cons head tail => simp [afterEnd, eligible]

theorem last_frame_eof_closes (columns : Nat → Nat) (frame : Frame) (boundaries : List String) :
    afterEnd columns [frame] none boundaries = some [] := rfl

theorem declared_boundary_closes {columns frame tail token boundaries}
    (ordinary : nextEligible columns (frame :: tail) token = false)
    (declared : token.lexeme ∈ boundaries) :
    afterEnd columns (frame :: tail) (some token) boundaries = some tail := by
  simp [afterEnd, ordinary, declared]

/-- The previous fallback origin theorem now receives the actual frame/column
eligibility computation instead of an unexplained Boolean. -/
theorem context_selection_origin {origin : Grammar.Action → Prop} {primary reductions}
    (primarySource : ∀ action ∈ Construction.selected primary, origin action)
    (fallbackSource : ∀ action ∈ reductions, origin action)
    (columns : Nat → Nat) (frames : List Frame) (token : Token) (enabled : Bool) :
    ∀ action ∈ Construction.selectLayout (shiftAllowed columns frames token)
      (Construction.publishLayout enabled primary reductions), origin action :=
  Construction.layout_selection_origin primarySource fallbackSource enabled _

/-- Column objects retain heap identity across dynamic scope restoration.
Frames describe the current branch; they are not global parser state. -/
structure Context where
  columns : Option Nat
  frames : List Frame
  deriving DecidableEq, Repr

structure Dynamic where
  current : Context
  saved : List Context
  deriving DecidableEq, Repr

def enter (state : Dynamic) (inner : Context) : Dynamic :=
  ⟨inner, state.current :: state.saved⟩

def unwind (depth : Nat) (state : Dynamic) : Dynamic :=
  if state.saved.length ≤ depth then state else
    match _stack : state.saved with
    | [] => state
    | prior :: rest => unwind depth ⟨prior, rest⟩
termination_by state.saved.length
decreasing_by simp_all

theorem unwind_owned_scope (outer : Dynamic) (nested : List Context) (current : Context) :
    unwind outer.saved.length ⟨current, nested ++ outer.current :: outer.saved⟩ = outer := by
  induction nested generalizing current with
  | nil =>
    rw [unwind]
    simp only [List.nil_append, List.length_cons, Nat.add_one_le_iff, Nat.lt_irrefl, ite_false]
    rw [unwind]
    simp
  | cons head rest ih =>
    rw [unwind]
    have deeper : ¬ (head :: (rest ++ outer.current :: outer.saved)).length ≤ outer.saved.length := by simp; omega
    simp only [List.cons_append]
    rw [ite_eq_right deeper]
    exact ih head

inductive Exit (Value : Type) where
  | returned (value : Value)
  | failed (reason : String)
  | cancelled
  deriving DecidableEq, Repr

def leave (depth : Nat) (result : Exit Value) (state : Dynamic) : Exit Value × Dynamic :=
  (result, unwind depth state)

theorem every_exit_restores (outer : Dynamic) (nested : List Context) (current : Context)
    (result : Exit Value) :
    leave outer.saved.length result ⟨current, nested ++ outer.current :: outer.saved⟩ = (result, outer) := by
  simp only [leave, unwind_owned_scope]

/-- Dynamic restoration and private heap ownership compose: restoring the outer
column reference also restores access to its unchanged mutable column object. -/
theorem outer_columns_survive {before after : Heap.Store Atom} {frontier address : Nat}
    {cell : Heap.Cell Atom} (outer : Dynamic) (nested : List Context) (current : Context)
    (result : Exit Value) (columnOwner : outer.current.columns = some address)
    (older : address < frontier) (read : before[address]? = some cell)
    (execution : Heap.PrivateTrace frontier before after) :
    (leave outer.saved.length result ⟨current, nested ++ outer.current :: outer.saved⟩).2.current.columns =
      some address ∧ after[address]? = some cell := by
  rw [every_exit_restores]
  exact ⟨columnOwner, Heap.private_trace_retains older read execution⟩

end Layout

namespace Collection
open Heap

inductive Reachable (heap : Store Atom) (roots : List Nat) : Nat → Prop where
  | root {address} (member : address ∈ roots) : Reachable heap roots address
  | first {owner first rest} (live : Reachable heap roots owner)
      (read : heap[owner]? = some (.pair first rest)) : Reachable heap roots first
  | rest {owner first rest} (live : Reachable heap roots owner)
      (read : heap[owner]? = some (.pair first rest)) : Reachable heap roots rest
  | slot {owner references address} (live : Reachable heap roots owner)
      (read : heap[owner]? = some (.vector references)) (member : address ∈ references) :
      Reachable heap roots address

def renameCell (relocate : Nat → Nat) : Cell Atom → Cell Atom
  | .atom value => .atom value
  | .pair first rest => .pair (relocate first) (relocate rest)
  | .vector references => .vector (references.map relocate)

/-- Collector contract on reachable objects, not a claim about Gambit's copying
algorithm. Captured continuation roots must be included alongside active roots. -/
structure Relocation (before after : Store Atom) (roots : List Nat) where
  address : Nat → Nat
  unique : ∀ left right, Reachable before roots left → Reachable before roots right →
    address left = address right → left = right
  copied : ∀ owner cell, Reachable before roots owner → before[owner]? = some cell →
    after[address owner]? = some (renameCell address cell)

theorem frozen_survives {before after : Store Atom} {roots root tree}
    (collection : Relocation before after roots) (live : Reachable before roots root)
    (published : Frozen before root tree) : Frozen after (collection.address root) tree := by
  induction published with
  | atom read => exact Frozen.atom (collection.copied _ _ live read)
  | pair read car cdr ihCar ihCdr =>
    exact Frozen.pair (collection.copied _ _ live read)
      (ihCar (Reachable.first live read)) (ihCdr (Reachable.rest live read))

theorem identity_survives {before after : Store Atom} {roots left right}
    (collection : Relocation before after roots)
    (l : Reachable before roots left) (r : Reachable before roots right) :
    collection.address left = collection.address right ↔ left = right :=
  ⟨collection.unique left right l r, fun same => congrArg collection.address same⟩

theorem private_owner_survives {before after : Store Atom} {roots : List Nat} {retained : List Nat} {owner : Nat}
    (collection : Relocation before after roots)
    (ownerLive : Reachable before roots owner)
    (protectedLive : ∀ address ∈ retained, Reachable before roots address)
    (owned : owner ∉ retained) : collection.address owner ∉ retained.map collection.address := by
  intro aliased
  obtain ⟨address, member, equal⟩ := List.mem_map.mp aliased
  have same := collection.unique address owner (protectedLive address member) ownerLive equal
  exact owned (same ▸ member)

/-- Both active and suspended roots are relocated. The theorem cannot be used
for a continuation root omitted from the collector's root set. -/
theorem suspended_publication_survives {before after : Store Atom} {active suspended root tree}
    (collection : Relocation before after (active ++ suspended))
    (captured : root ∈ suspended) (published : Frozen before root tree) :
    Frozen after (collection.address root) tree :=
  frozen_survives collection (Reachable.root (List.mem_append.mpr (Or.inr captured))) published

end Collection

namespace Reentry

/-- Wind activation identities are distinct even if parameter values agree.
Paths are outermost first; the native dynamic stack is innermost first. -/
structure Crossing where
  shared : List Nat
  departed : List Nat
  arrived : List Nat
  deriving DecidableEq, Repr

def commonPath : List Nat → List Nat → Crossing
  | [], target => ⟨[], [], target⟩
  | current, [] => ⟨[], current, []⟩
  | left :: current, right :: target =>
      if left = right then
        let later := commonPath current target
        ⟨left :: later.shared, later.departed, later.arrived⟩
      else ⟨[], left :: current, right :: target⟩

theorem crossing_decomposes (current target : List Nat) :
    (commonPath current target).shared ++ (commonPath current target).departed = current ∧
    (commonPath current target).shared ++ (commonPath current target).arrived = target := by
  induction current generalizing target with
  | nil => simp [commonPath]
  | cons left current ih =>
    cases target with
    | nil => simp [commonPath]
    | cons right target =>
      by_cases same : left = right
      · subst right
        simpa [commonPath] using ih target
      · simp [commonPath, same]

def runExits : List Nat → List Nat → Option (List Nat)
  | stack, [] => some stack
  | [], _ :: _ => none
  | current :: stack, owner :: rest => if current = owner then runExits stack rest else none

def runEnters : List Nat → List Nat → List Nat
  | stack, [] => stack
  | stack, owner :: rest => runEnters (owner :: stack) rest

theorem exit_prefix (departed shared : List Nat) :
    runExits (departed ++ shared) departed = some shared := by
  induction departed with
  | nil => simp [runExits]
  | cons owner rest ih => simpa [runExits] using ih

theorem enter_path (stack arrived : List Nat) : runEnters stack arrived = arrived.reverse ++ stack := by
  induction arrived generalizing stack with
  | nil => rfl
  | cons owner rest ih => simp [runEnters, ih, List.reverse_cons, List.append_assoc]

def resume (active captured : List Nat) : Option (List Nat) :=
  let crossing := commonPath active.reverse captured.reverse
  (runExits active crossing.departed.reverse).map (fun shared => runEnters shared crossing.arrived)

/-- Reentry runs after hooks innermost first and before hooks outermost first.
An already-shared wind extent is retained, not exited and entered again. -/
theorem resume_captured_path (active captured : List Nat) : resume active captured = some captured := by
  have split := crossing_decomposes active.reverse captured.reverse
  have current := congrArg List.reverse split.1
  have target := congrArg List.reverse split.2
  simp only [List.reverse_append, List.reverse_reverse] at current target
  have exited := exit_prefix (commonPath active.reverse captured.reverse).departed.reverse
    (commonPath active.reverse captured.reverse).shared.reverse
  rw [current] at exited
  dsimp only [resume]
  rw [exited]
  simp only [Option.map_some, enter_path]
  exact congrArg some target

theorem same_path_has_no_crossings (path : List Nat) :
    (commonPath path path).departed = [] ∧ (commonPath path path).arrived = [] := by
  induction path with
  | nil => simp [commonPath]
  | cons owner rest ih => simpa [commonPath] using ih

/-- The same captured continuation can be resumed after any intervening caller
path. No consumed flag or one-shot token is introduced by this semantics. -/
theorem repeated_resume (caller firstCaller captured : List Nat) :
    resume firstCaller captured = some captured ∧ resume caller captured = some captured :=
  ⟨resume_captured_path _ _, resume_captured_path _ _⟩

def contextAt (base : Layout.Context) (environment : Nat → Layout.Context) (stack : List Nat) : Layout.Context :=
  match stack with
  | [] => base
  | owner :: _ => environment owner

theorem captured_context_restored (base : Layout.Context) (environment : Nat → Layout.Context)
    (caller captured : List Nat) :
    (resume caller captured).map (contextAt base environment) = some (contextAt base environment captured) := by
  rw [resume_captured_path]
  rfl

def roots (base : Layout.Context) (environment : Nat → Layout.Context) (captured : List Nat) : List Nat :=
  base.columns.toList ++ captured.flatMap (fun owner => (environment owner).columns.toList)

theorem captured_column_is_root {base environment captured address}
    (column : (contextAt base environment captured).columns = some address) :
    address ∈ roots base environment captured := by
  cases captured with
  | nil => simp [roots, contextAt] at *; exact column
  | cons owner rest =>
    simp only [contextAt] at column
    simp [roots, column]

/-- GC and reentry compose by relocating captured roots as well as active ones.
Restored context references read the corresponding relocated heap objects. -/
theorem captured_column_survives {before after : Heap.Store Atom} {active base environment captured address cell}
    (collection : Collection.Relocation before after (active ++ roots base environment captured))
    (column : (contextAt base environment captured).columns = some address)
    (read : before[address]? = some cell) :
    after[collection.address address]? = some (Collection.renameCell collection.address cell) :=
  collection.copied _ _
    (Collection.Reachable.root (List.mem_append.mpr (Or.inr (captured_column_is_root column)))) read

end Reentry

namespace Backward

abbrev Buckets := List (Nat × List Nat)

def insert (key vertex : Nat) : Buckets → Buckets
  | [] => [(key, [vertex])]
  | (label, members) :: rest =>
      if key = label then (label, vertex :: members) :: rest
      else (label, members) :: insert key vertex rest

def buckets (signature : Nat → Nat) : List Nat → Buckets
  | [] => []
  | vertex :: rest => insert (signature vertex) vertex (buckets signature rest)

def mass (groups : Buckets) : Nat := (groups.map (fun group => group.2.length)).sum

theorem insert_mass (key vertex : Nat) (groups : Buckets) : mass (insert key vertex groups) = mass groups + 1 := by
  induction groups with
  | nil => simp [insert, mass]
  | cons group rest ih =>
    rcases group with ⟨label, members⟩
    by_cases same : key = label
    · simp [insert, same, mass]; omega
    · simp only [insert, same, ite_false, mass, List.map_cons, List.sum_cons] at *
      omega

theorem buckets_mass (signature : Nat → Nat) (vertices : List Nat) :
    mass (buckets signature vertices) = vertices.length := by
  induction vertices with
  | nil => rfl
  | cons vertex rest ih => simp only [buckets, insert_mass, List.length_cons, ih]

theorem insert_nonempty {groups : Buckets}
    (valid : ∀ group ∈ groups, group.2 ≠ []) (key vertex : Nat) :
    ∀ group ∈ insert key vertex groups, group.2 ≠ [] := by
  induction groups with
  | nil => simp [insert]
  | cons group rest ih =>
    rcases group with ⟨label, members⟩
    by_cases same : key = label
    · simp only [insert, same, ite_true, List.mem_cons]
      intro candidate member
      rcases member with rfl | previous
      · simp
      · exact valid candidate (List.mem_cons_of_mem _ previous)
    · simp only [insert, same, ite_false, List.mem_cons]
      intro candidate member
      rcases member with rfl | previous
      · exact valid _ (by simp)
      · exact ih (fun other member => valid other (List.mem_cons_of_mem _ member)) candidate previous

theorem buckets_nonempty (signature : Nat → Nat) (vertices : List Nat) :
    ∀ group ∈ buckets signature vertices, group.2 ≠ [] := by
  induction vertices with
  | nil => simp [buckets]
  | cons vertex rest ih => exact insert_nonempty ih _ _

theorem nonempty_bucket_count {signature : Nat → Nat} {vertices : List Nat} (nonempty : vertices ≠ []) :
    1 ≤ (buckets signature vertices).length := by
  have same := buckets_mass signature vertices
  cases equation : buckets signature vertices with
  | nil => simp [equation, mass] at same; exact False.elim (nonempty (List.eq_nil_of_length_eq_zero same.symm))
  | cons head tail => simp

structure Partition where
  blocks : List (List Nat)
  nonempty : ∀ block ∈ blocks, block ≠ []

def vertices (partition : Partition) : Nat := (partition.blocks.map List.length).sum

def refine (signature : List Nat → Nat → Nat) (partition : Partition) : Partition :=
  ⟨partition.blocks.flatMap (fun block => (buckets (signature block) block).map Prod.snd), by
    intro block member
    obtain ⟨old, present, nested⟩ := List.mem_flatMap.mp member
    obtain ⟨bucket, inBucket, same⟩ := List.mem_map.mp nested
    subst block
    exact buckets_nonempty _ _ bucket inBucket⟩

theorem refined_vertices (signature : List Nat → Nat → Nat) (partition : Partition) :
    vertices (refine signature partition) = vertices partition := by
  have helper : ∀ blocks : List (List Nat),
      ((blocks.flatMap (fun block => (buckets (signature block) block).map Prod.snd)).map List.length).sum =
        (blocks.map List.length).sum := by
    intro blocks
    induction blocks with
    | nil => rfl
    | cons head rest ih =>
      simp only [List.flatMap_cons, List.map_append, List.sum_append, List.map_cons, List.sum_cons, ih]
      have total := buckets_mass (signature head) head
      simpa only [mass, List.map_map, Function.comp_def] using congrArg (fun n => n + (rest.map List.length).sum) total
  exact helper partition.blocks

theorem partition_density (partition : Partition) : partition.blocks.length ≤ vertices partition := by
  have helper : ∀ blocks : List (List Nat), (∀ block ∈ blocks, block ≠ []) →
      blocks.length ≤ (blocks.map List.length).sum := by
    intro blocks valid
    induction blocks with
    | nil => simp
    | cons head rest ih =>
      have positive : 0 < head.length := List.length_pos_iff.mpr (valid head (by simp))
      have tail := ih (fun block member => valid block (List.mem_cons_of_mem _ member))
      simp only [List.length_cons, List.map_cons, List.sum_cons]
      omega
  exact helper _ partition.nonempty

def changed (signature : List Nat → Nat → Nat) (partition : Partition) : Bool :=
  partition.blocks.any (fun block => decide (1 < (buckets (signature block) block).length))

theorem refine_count_ge (signature : List Nat → Nat → Nat) (blocks : List (List Nat))
    (valid : ∀ block ∈ blocks, block ≠ []) :
    blocks.length ≤ (blocks.flatMap (fun block => (buckets (signature block) block).map Prod.snd)).length := by
  induction blocks with
  | nil => simp
  | cons head rest ih =>
    have positive := nonempty_bucket_count (signature := signature head) (valid head (by simp))
    have later := ih (fun b m => valid b (List.mem_cons_of_mem _ m))
    simp only [List.flatMap_cons, List.length_append, List.length_map, List.length_cons]
    omega

theorem changed_grows (signature : List Nat → Nat → Nat) (partition : Partition)
    (split : changed signature partition = true) :
    partition.blocks.length < (refine signature partition).blocks.length := by
  have witness : ∃ block ∈ partition.blocks, 1 < (buckets (signature block) block).length := by
    obtain ⟨block, member, split⟩ := List.any_eq_true.mp split
    exact ⟨block, member, of_decide_eq_true split⟩
  have helper : ∀ blocks : List (List Nat), (∀ block ∈ blocks, block ≠ []) →
      (∃ block ∈ blocks, 1 < (buckets (signature block) block).length) →
      blocks.length < (blocks.flatMap (fun block => (buckets (signature block) block).map Prod.snd)).length := by
    intro blocks valid hasSplit
    induction blocks with
    | nil => simp at hasSplit
    | cons head rest ih =>
      have headBound := nonempty_bucket_count (signature := signature head) (valid head (by simp))
      have restBound := refine_count_ge signature rest (fun b m => valid b (List.mem_cons_of_mem _ m))
      obtain ⟨block, member, growth⟩ := hasSplit
      simp only [List.mem_cons] at member
      simp only [List.flatMap_cons, List.length_append, List.length_map, List.length_cons]
      rcases member with rfl | member
      · omega
      · have later := ih (fun b m => valid b (List.mem_cons_of_mem _ m)) ⟨block, member, growth⟩
        omega
  exact helper partition.blocks partition.nonempty witness

/-- Backward continuation is the actual multi-bucket predicate. Growth is
proved from bucket construction, rather than supplied as a changed-flag axiom. -/
def rounds (signature : Partition → List Nat → Nat → Nat) (partition : Partition) : Nat :=
  if changed (signature partition) partition then
    1 + rounds signature (refine (signature partition) partition)
  else 1
termination_by vertices partition - partition.blocks.length
decreasing_by
  have growth := changed_grows (signature partition) partition (by assumption)
  have density := partition_density (refine (signature partition) partition)
  have conserved := refined_vertices (signature partition) partition
  omega

theorem round_bound (signature : Partition → List Nat → Nat → Nat) (partition : Partition) :
    rounds signature partition ≤ vertices partition - partition.blocks.length + 1 := by
  rw [rounds]
  split
  · have later := round_bound signature (refine (signature partition) partition)
    have growth := changed_grows (signature partition) partition (by assumption)
    have conserved := refined_vertices (signature partition) partition
    have density := partition_density (refine (signature partition) partition)
    omega
  · omega
termination_by vertices partition - partition.blocks.length
decreasing_by
  have growth := changed_grows (signature partition) partition (by assumption)
  have conserved := refined_vertices (signature partition) partition
  have density := partition_density (refine (signature partition) partition)
  omega

end Backward

namespace Canonical

/-- Dense masks have one row per physical core, including zero rows. Sparse
native keys emit nonzero rows in physical-core order. -/
def sparse (core : Nat) : List Nat → List (Nat × Nat)
  | [] => []
  | mask :: rest => if mask = 0 then sparse (core + 1) rest
      else (core, mask) :: sparse (core + 1) rest

def matrices : Nat → Nat → List (List Nat)
  | 0, _ => [[]]
  | cores + 1, terminals =>
      (matrices cores terminals).flatMap (fun tail =>
        (List.range (2 ^ terminals)).map (fun mask => mask :: tail))

theorem matrix_is_enumerated {cores terminals : Nat} {masks : List Nat}
    (width : masks.length = cores) (bounded : ∀ mask ∈ masks, mask < 2 ^ terminals) :
    masks ∈ matrices cores terminals := by
  induction cores generalizing masks with
  | zero => have empty := List.eq_nil_of_length_eq_zero width; simp [empty, matrices]
  | succ cores ih =>
    cases masks with
    | nil => simp at width
    | cons mask tail =>
      have restWidth : tail.length = cores := by simpa using width
      have restBound := fun value member => bounded value (List.mem_cons_of_mem mask member)
      apply List.mem_flatMap.mpr
      refine ⟨tail, ih restWidth restBound, ?_⟩
      exact List.mem_map.mpr ⟨mask, List.mem_range.mpr (bounded mask (by simp)), rfl⟩

def catalogue (cores terminals : Nat) : List (List (Nat × Nat)) :=
  (matrices cores terminals).map (sparse 0)

theorem matrix_count (cores terminals : Nat) :
    (matrices cores terminals).length = (2 ^ terminals) ^ cores := by
  induction cores with
  | zero => simp [matrices]
  | succ cores ih =>
    simp only [matrices, List.length_flatMap, List.length_map, List.length_range]
    rw [List.map_const', List.sum_replicate_nat, ih, Nat.pow_succ]

theorem catalogue_count (cores terminals : Nat) :
    (catalogue cores terminals).length = (2 ^ terminals) ^ cores := by
  simp [catalogue, matrix_count]

theorem native_key_admitted {cores terminals : Nat} {masks : List Nat}
    (width : masks.length = cores) (bounded : ∀ mask ∈ masks, mask < 2 ^ terminals) :
    sparse 0 masks ∈ catalogue cores terminals :=
  List.mem_map.mpr ⟨masks, matrix_is_enumerated width bounded, rfl⟩

theorem intern_catalogue_bound [BEq Key] [LawfulBEq Key]
    (domain keys : List Key) (key : Key) (unique : keys.Nodup)
    (included : ∀ old ∈ keys, old ∈ domain) (admitted : key ∈ domain) :
    (Construction.intern keys key).1.length ≤ domain.length := by
  apply List.Nodup.length_le_of_subset (Construction.intern_unique keys key unique)
  intro old member
  unfold Construction.intern at member
  split at member
  · exact included old member
  · rcases List.mem_append.mp member with earlier | new
    · exact included old earlier
    · have same := List.mem_singleton.mp new
      simpa only [same] using admitted

/-- Native canonical closure may enqueue duplicate owners: every enqueue is
still charged to a strict mask growth. This differs from LALR coalescing. -/
def closurePublish [BEq Fact] [LawfulBEq Fact]
    (state : Scheduling.Budget Fact Nat) (owner : Nat) (incoming : List Fact) : Scheduling.Budget Fact Nat :=
  if incoming.any (fun fact => state.remaining.contains fact) then
    ⟨Scheduling.eraseMany state.remaining incoming, owner :: state.queued⟩ else state

theorem closure_enqueue_decreases [BEq Fact] [LawfulBEq Fact]
    (state : Scheduling.Budget Fact Nat) (owner : Nat) (incoming : List Fact)
    (fresh : ∃ fact ∈ incoming, fact ∈ state.remaining) :
    Scheduling.rank (closurePublish state owner incoming) < Scheduling.rank state := by
  have shorter := Scheduling.erase_many_fresh_decreases _ _ fresh
  have live : incoming.any (fun fact => state.remaining.contains fact) = true := by
    obtain ⟨fact, member, known⟩ := fresh
    exact List.any_eq_true.mpr ⟨fact, member, List.contains_iff_mem.mpr known⟩
  simp only [closurePublish, live, ite_true, Scheduling.rank, List.length_cons]
  omega

structure Queue (Key : Type) where
  keys : List Key
  pending : List Nat
  processed : Nat

def intern [BEq Key] [LawfulBEq Key] (state : Queue Key) (key : Key) : Queue Key :=
  if key ∈ state.keys then state else
    ⟨state.keys ++ [key], state.pending ++ [state.keys.length], state.processed⟩

def rank (domain : List Key) (state : Queue Key) : Nat :=
  2 * (domain.length - state.keys.length) + state.pending.length

theorem existing_never_requeues [BEq Key] [LawfulBEq Key] (state : Queue Key) (key : Key)
    (existing : key ∈ state.keys) : intern state key = state := by simp [intern, existing]

theorem novel_intern_decreases [BEq Key] [LawfulBEq Key]
    (domain : List Key) (state : Queue Key) (key : Key)
    (unique : state.keys.Nodup) (included : ∀ old ∈ state.keys, old ∈ domain)
    (admitted : key ∈ domain) (novel : key ∉ state.keys) : rank domain (intern state key) < rank domain state := by
  have bound := intern_catalogue_bound domain state.keys key unique included admitted
  simp only [Construction.intern, novel, ite_false, List.length_append, List.length_singleton] at bound
  simp only [rank, intern, novel, ite_false, List.length_append, List.length_singleton]
  omega

theorem intern_accounting [BEq Key] [LawfulBEq Key] (state : Queue Key) (key : Key)
    (balanced : state.pending.length + state.processed = state.keys.length) :
    (intern state key).pending.length + (intern state key).processed = (intern state key).keys.length := by
  unfold intern
  split
  · exact balanced
  · simp only [List.length_append, List.length_singleton]
    omega

def drain (state : Queue Key) : Queue Key :=
  match state.pending with
  | [] => state
  | _ :: rest => {state with pending := rest, processed := state.processed + 1}

theorem drain_decreases (domain : List Key) (state : Queue Key)
    (work : state.pending ≠ []) : rank domain (drain state) < rank domain state := by
  cases pending : state.pending with
  | nil => exact False.elim (work pending)
  | cons head rest => simp [drain, pending, rank]

theorem completed_processes_catalogue (state : Queue Key)
    (balanced : state.pending.length + state.processed = state.keys.length)
    (finished : state.pending = []) : state.processed = state.keys.length := by
  simpa [finished] using balanced

theorem intern_keys [BEq Key] [LawfulBEq Key] (state : Queue Key) (key : Key) :
    (intern state key).keys = (Construction.intern state.keys key).1 := by
  unfold intern Construction.intern
  split <;> rfl

theorem drain_keys (state : Queue Key) : (drain state).keys = state.keys := by
  unfold drain
  split <;> rfl

theorem drain_accounting (state : Queue Key)
    (balanced : state.pending.length + state.processed = state.keys.length) :
    (drain state).pending.length + (drain state).processed = (drain state).keys.length := by
  unfold drain
  split
  · exact balanced
  · rename_i head rest equation
    simp only [equation, List.length_cons] at balanced
    dsimp only
    omega

structure Admitted (domain : List Key) where
  queue : Queue Key
  unique : queue.keys.Nodup
  included : ∀ key ∈ queue.keys, key ∈ domain
  balanced : queue.pending.length + queue.processed = queue.keys.length

def admit [BEq Key] [LawfulBEq Key] {domain : List Key}
    (state : Admitted domain) (key : Key) (member : key ∈ domain) : Admitted domain :=
  ⟨intern state.queue key, by rw [intern_keys]; exact Construction.intern_unique _ _ state.unique,
    by
      intro old present
      unfold intern at present
      split at present
      · exact state.included old present
      · rcases List.mem_append.mp present with previous | added
        · exact state.included old previous
        · have same := List.mem_singleton.mp added
          simpa only [same] using member,
    intern_accounting _ _ state.balanced⟩

def removeHead {domain : List Key} (state : Admitted domain) : Admitted domain :=
  ⟨drain state.queue, by rw [drain_keys]; exact state.unique,
    by rw [drain_keys]; exact state.included, drain_accounting _ state.balanced⟩

def expand [BEq Key] [LawfulBEq Key] {domain : List Key} (state : Admitted domain)
    (targets : List Key) (valid : ∀ key ∈ targets, key ∈ domain) : Admitted domain :=
  match targets with
  | [] => state
  | key :: rest => expand (admit state key (valid key (by simp))) rest
      (fun other member => valid other (List.mem_cons_of_mem key member))

theorem admit_rank_le [BEq Key] [LawfulBEq Key] {domain : List Key}
    (state : Admitted domain) (key : Key) (member : key ∈ domain) :
    rank domain (admit state key member).queue ≤ rank domain state.queue := by
  by_cases existing : key ∈ state.queue.keys
  · simp [admit, existing_never_requeues _ _ existing]
  · exact Nat.le_of_lt (novel_intern_decreases domain state.queue key state.unique state.included member existing)

theorem expand_rank_le [BEq Key] [LawfulBEq Key] {domain : List Key}
    (state : Admitted domain) (targets : List Key) (valid : ∀ key ∈ targets, key ∈ domain) :
    rank domain (expand state targets valid).queue ≤ rank domain state.queue := by
  induction targets generalizing state with
  | nil => exact Nat.le_refl _
  | cons key rest ih =>
    exact Nat.le_trans (ih _ _) (admit_rank_le state key (valid key (by simp)))

/-- One real drain is followed by a finite symbol traversal. Old states do not
requeue; every fresh target is admitted into the grammar-derived catalogue. -/
def finish [BEq Key] [LawfulBEq Key] {domain : List Key}
    (targets : Admitted domain → Nat → List Key)
    (accepted : ∀ state owner key, key ∈ targets state owner → key ∈ domain)
    (state : Admitted domain) : Admitted domain :=
  match equation : state.queue.pending with
  | [] => state
  | owner :: _ =>
      let next := expand (removeHead state) (targets state owner) (accepted state owner)
      have progress : rank domain next.queue < rank domain state.queue := by
        have drained := drain_decreases domain state.queue (by rw [equation]; simp)
        have expanded := expand_rank_le (removeHead state) (targets state owner) (accepted state owner)
        exact Nat.lt_of_le_of_lt expanded drained
      finish targets accepted next
termination_by rank domain state.queue
decreasing_by exact progress

theorem finish_quiescent [BEq Key] [LawfulBEq Key] {domain : List Key}
    (targets : Admitted domain → Nat → List Key)
    (accepted : ∀ state owner key, key ∈ targets state owner → key ∈ domain)
    (state : Admitted domain) : (finish targets accepted state).queue.pending = [] := by
  rw [finish]
  split
  next stopped => exact stopped
  next owner rest equation => exact finish_quiescent targets accepted _
termination_by rank domain state.queue
decreasing_by
  rename_i owner _ equation _ _
  have drained := drain_decreases domain state.queue (by rw [equation]; simp)
  have expanded := expand_rank_le (removeHead state) (targets state owner) (accepted state owner)
  exact Nat.lt_of_le_of_lt expanded drained

theorem finish_processes_all_keys [BEq Key] [LawfulBEq Key] {domain : List Key}
    (targets : Admitted domain → Nat → List Key)
    (accepted : ∀ state owner key, key ∈ targets state owner → key ∈ domain)
    (state : Admitted domain) :
    (finish targets accepted state).queue.processed = (finish targets accepted state).queue.keys.length :=
  completed_processes_catalogue _ (finish targets accepted state).balanced (finish_quiescent targets accepted state)

end Canonical

end ParserEngine.Operational
