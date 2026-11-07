import ParserEngine.SourceEffects

/-! Logical identities for parser-owned physical continuation vectors. Fresh,
stable object identity and primitive vector operations are host contracts;
nonduplication of identities in the pending list is a parser invariant. -/
namespace ParserEngine.FrameOwnership
open IteratorEncoding

structure Store where
  heap : Nat → List Cell
  frontier : Nat
  pending : List Nat

def Owned (store : Store) : Prop :=
  store.pending.Nodup ∧ ∀ pointer ∈ store.pending, pointer < store.frontier

def project (heap : Nat → List Cell) : List Nat → Option (List Frame)
  | [] => some []
  | pointer :: rest => do
    let frame ← decodeFrame (heap pointer)
    let tail ← project heap rest
    pure (frame :: tail)

def push (store : Store) (frame : Frame) : Store :=
  ⟨fun pointer => if pointer = store.frontier then encodeFrame frame else store.heap pointer,
    store.frontier + 1, store.frontier :: store.pending⟩
def pop (store : Store) : Store := ⟨store.heap, store.frontier, store.pending.tail⟩
def cursorWrite (store : Store) (pointer : Nat) (index : Int) : Store :=
  ⟨SourceEffects.write store.heap pointer 1 (.number (index + 1)), store.frontier, store.pending⟩

theorem push_owned (owned : Owned store) : Owned (push store frame) := by
  obtain ⟨unique, below⟩ := owned
  constructor
  · simp only [push, List.nodup_cons]
    exact ⟨fun member => Nat.lt_irrefl _ (below _ member), unique⟩
  · intro pointer member
    rcases List.mem_cons.mp member with rfl | member
    · exact Nat.lt_succ_self _
    · exact Nat.lt_trans (below _ member) (Nat.lt_succ_self _)

theorem pop_owned (owned : Owned store) : Owned (pop store) := by
  obtain ⟨unique, below⟩ := owned
  exact ⟨unique.tail, fun pointer member => below pointer (List.mem_of_mem_tail member)⟩

theorem cursor_owned (owned : Owned store) : Owned (cursorWrite store pointer index) := owned

theorem project_agrees (pointers : List Nat) (agrees : ∀ pointer ∈ pointers, first pointer = second pointer) :
    project first pointers = project second pointers := by
  induction pointers with
  | nil => rfl
  | cons pointer rest ih =>
    simp only [project, agrees pointer (by simp)]
    rw [ih (fun p inside => agrees p (List.mem_cons_of_mem _ inside))]

theorem push_project (owned : Owned store) (decoded : project store.heap store.pending = some frames) :
    project (push store frame).heap (push store frame).pending = some (frame :: frames) := by
  have tail := project_agrees store.pending (first := (push store frame).heap) (second := store.heap)
    (fun pointer member => by
      have different : pointer ≠ store.frontier := Nat.ne_of_lt (owned.2 pointer member)
      simp [push, different])
  change ((decodeFrame ((push store frame).heap store.frontier)).bind fun first =>
    (project (push store frame).heap store.pending).bind fun more => some (first :: more)) = _
  rw [tail, decoded]
  simp [push, frame_roundtrip]

theorem cursor_tail_unchanged (owned : Owned store) (pending : store.pending = pointer :: rest) :
    project (cursorWrite store pointer index).heap rest = project store.heap rest := by
  have unique : pointer ∉ rest := (List.nodup_cons.mp (by simpa [pending] using owned.1)).1
  apply project_agrees
  intro other member
  have different : other ≠ pointer := by intro same; subst other; exact unique member
  exact SourceEffects.write_other store.heap pointer 1 other (.number (index + 1)) different

/-- Derive disjointness from parser ownership rather than assuming that the
physical tail happens not to alias the vector currently being mutated. -/
theorem cursor_project (owned : Owned store) (pending : store.pending = pointer :: rest)
    (head : decodeFrame (store.heap pointer) = some (.chunk code index delta moved))
    (tail : project store.heap rest = some frames) :
    project (cursorWrite store pointer index).heap store.pending =
      some (.chunk code (index + 1) delta moved :: frames) := by
  have exactHead : store.heap pointer = encodeFrame (.chunk code index delta moved) :=
    (frame_exact _ _ head).symm
  have changed := SourceEffects.chunk_write_decodes code index delta moved
  have tailSame := cursor_tail_unchanged owned pending (index := index)
  simp only [pending, project]
  have read : decodeFrame ((cursorWrite store pointer index).heap pointer) =
      some (.chunk code (index + 1) delta moved) := by
    simpa [cursorWrite, SourceEffects.write_same, exactHead] using changed
  simp [read, tailSame, tail]

inductive Action where
  | allocate (frame : Frame)
  | pop
  | advanceCursor
  | retain

def step (action : Action) (store : Store) : Option Store :=
  match action with
  | .allocate frame => some (push store frame)
  | .pop => if store.pending.isEmpty then none else some (pop store)
  | .retain => some store
  | .advanceCursor => do
    let pointer ← store.pending.head?
    match decodeFrame (store.heap pointer) with
    | some (.chunk _ index _ _) => some (cursorWrite store pointer index)
    | _ => none

theorem step_owned (owned : Owned before) (executed : step action before = some after) : Owned after := by
  cases action with
  | allocate frame => cases Option.some.inj executed; exact push_owned owned
  | retain => cases Option.some.inj executed; exact owned
  | pop =>
    simp only [step] at executed
    split at executed
    · contradiction
    · cases Option.some.inj executed; exact pop_owned owned
  | advanceCursor =>
    cases pending : before.pending with
    | nil => simp [step, pending] at executed
    | cons pointer rest =>
      cases decoded : decodeFrame (before.heap pointer) with
      | none => simp [step, pending, decoded] at executed
      | some frame =>
        cases frame <;> simp [step, pending, decoded] at executed
        cases executed
        exact owned

def run : List Action → Store → Option Store
  | [], store => some store
  | action :: rest, store => do
    let next ← step action store
    run rest next

theorem run_owned (owned : Owned before) (executed : run actions before = some after) : Owned after := by
  induction actions generalizing before with
  | nil => cases Option.some.inj executed; exact owned
  | cons action rest ih =>
    cases advanced : step action before with
    | none => simp [run, advanced] at executed
    | some next => exact ih (step_owned owned advanced) (by simpa [run, advanced] using executed)

def allocateMany : List Frame → Store → Store
  | [], store => store
  | frame :: rest, store => allocateMany rest (push store frame)

theorem allocate_many_owned (frames : List Frame) (owned : Owned store) : Owned (allocateMany frames store) := by
  induction frames generalizing store with
  | nil => exact owned
  | cons frame rest ih => exact ih (push_owned owned)

theorem allocate_many_project (created : List Frame) (owned : Owned store)
    (decoded : project store.heap store.pending = some frames) :
    project (allocateMany created store).heap (allocateMany created store).pending = some (created.reverse ++ frames) := by
  induction created generalizing store frames with
  | nil => exact decoded
  | cons frame rest ih =>
    simpa [allocateMany, List.reverse_cons, List.append_assoc] using
      ih (push_owned owned) (push_project owned decoded (frame := frame))

theorem allocate_many_executes (created : List Frame) (store : Store) :
    run (created.map Action.allocate) store = some (allocateMany created store) := by
  induction created generalizing store with
  | nil => rfl
  | cons frame rest ih => simpa [run, step, allocateMany] using ih (push store frame)

/-- Every reachable frame in the source action vocabulary has a unique pending
identity. A fresh allocation never requires a global heap rebuild. -/
theorem initial_owned (heap : Nat → List Cell) (frontier : Nat) : Owned ⟨heap, frontier, []⟩ := by
  simp [Owned]

#print axioms push_owned
#print axioms push_project
#print axioms cursor_project
#print axioms run_owned
#print axioms allocate_many_project
end ParserEngine.FrameOwnership
