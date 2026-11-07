import ParserEngine.Host.LivenessSlots
import ParserEngine.Representation
import Init.Data.List.Nat.TakeDrop

/-! Source adapters for Gambit frontend liveness and GCC chunk publication.
Varsets are lists modulo membership. Physical placement and compiler refinement
remain separate obligations. No C sequential-consistency axiom is introduced. -/
namespace ParserEngine.Host
open ParserEngine.Representation
namespace Cache
structure Entry where
  location : Nat
  dirty : Bool
  stamp : Nat
  payload : Nat
  deriving DecidableEq, Repr

def clean (entry : Entry) : Entry := {entry with dirty := false}

@[simp] theorem clean_location (entry : Entry) : (clean entry).location = entry.location := rfl

theorem clean_of_clean (entry : Entry) (already : entry.dirty = false) : clean entry = entry := by
  cases entry; simp_all [clean]

def lookup (location : Nat) : List (Option Entry) → Option Entry
  | [] => none
  | none :: rest => lookup location rest
  | some entry :: rest => if entry.location = location then some entry else lookup location rest

def Unique : List (Option Entry) → Prop
  | [] => True
  | none :: rest => Unique rest
  | some entry :: rest => lookup entry.location rest = none ∧ Unique rest

/-- Exact early-return scan of targ-fp-cache-write-if-dirty. The optional
emitted entry is the F64BOX writeback event, not an assumed boxed-memory value. -/
def flush (location : Nat) : List (Option Entry) → List (Option Entry) × Option Entry
  | [] => ([], none)
  | none :: rest => let (updated, event) := flush location rest; (none :: updated, event)
  | some entry :: rest =>
      if entry.location = location ∧ entry.dirty = true then (some (clean entry) :: rest, some entry)
      else let (updated, event) := flush location rest; (some entry :: updated, event)

theorem flush_absent {location : Nat} (rows : List (Option Entry))
    (absent : lookup location rows = none) : flush location rows = (rows, none) := by
  induction rows with
  | nil => rfl
  | cons row rest ih =>
    cases row with
    | none => simpa [flush] using congrArg (fun result => (none :: result.1, result.2)) (ih absent)
    | some entry =>
      by_cases hit : entry.location = location
      · simp [lookup, hit] at absent
      · have restAbsent : lookup location rest = none := by simpa [lookup, hit] using absent
        simp [flush, hit, ih restAbsent]

/-- Uniqueness connects the probe's first entry to the first dirty write. A
clean alias preceding a dirty alias would violate this correspondence. -/
theorem flush_refines {location : Nat} (rows : List (Option Entry)) (unique : Unique rows) :
    (flush location rows).2 = (lookup location rows).filter (fun entry => entry.dirty) ∧
    lookup location (flush location rows).1 = (lookup location rows).map clean := by
  induction rows with
  | nil => simp [flush, lookup]
  | cons row rest ih =>
    cases row with
    | none => simpa [flush, lookup] using ih unique
    | some entry =>
      obtain ⟨absent, uniqueRest⟩ := unique
      by_cases hit : entry.location = location
      · have noRest : lookup location rest = none := by simpa [hit] using absent
        cases dirty : entry.dirty with
        | false => simp [flush, lookup, hit, dirty, Option.filter, flush_absent rest noRest,
            clean_of_clean entry dirty]
        | true => simp [flush, lookup, hit, dirty, Option.filter]
      · have result := ih uniqueRest
        simpa [flush, lookup, hit] using result

/-- Invalidation scans every cell, unlike the early-return writeback scan. -/
def invalidate (location : Nat) : List (Option Entry) → List (Option Entry)
  | [] => []
  | none :: rest => none :: invalidate location rest
  | some entry :: rest =>
      (if entry.location = location then none else some entry) :: invalidate location rest

theorem invalidate_absent (location : Nat) (rows : List (Option Entry)) :
    lookup location (invalidate location rows) = none := by
  induction rows with
  | nil => rfl
  | cons row rest ih =>
    cases row with
    | none => simpa [invalidate, lookup] using ih
    | some entry =>
      by_cases hit : entry.location = location <;> simp [invalidate, lookup, hit, ih]

/-- First free cell reuse, with the original capacity as the terminal growth
padding: a full vector of n cells becomes 2*n+1, not merely n+1. -/
def insert (extra : Nat) (entry : Entry) : List (Option Entry) → List (Option Entry)
  | [] => some entry :: List.replicate extra none
  | none :: rest => some entry :: rest
  | some old :: rest => some old :: insert extra entry rest

theorem insert_lookup {location : Nat} (rows : List (Option Entry)) (entry : Entry) (extra : Nat)
    (fresh : lookup location rows = none) (key : entry.location = location) :
    lookup location (insert extra entry rows) = some entry := by
  induction rows with
  | nil => simp [insert, lookup, key]
  | cons row rest ih =>
    cases row with
    | none => simp [insert, lookup, key]
    | some old =>
      by_cases hit : old.location = location
      · simp [lookup, hit] at fresh
      · simp [insert, lookup, hit, ih (by simpa [lookup, hit] using fresh)]

def keys : List (Option Entry) → List Nat
  | [] => []
  | none :: rest => keys rest
  | some entry :: rest => entry.location :: keys rest

theorem lookup_none_iff (location : Nat) (rows : List (Option Entry)) :
    lookup location rows = none ↔ location ∉ keys rows := by
  induction rows with
  | nil => simp [lookup, keys]
  | cons row rest ih =>
    cases row with
    | none => simpa [lookup, keys] using ih
    | some entry => by_cases hit : entry.location = location <;> simp [lookup, keys, hit, ih, Ne.symm]

theorem unique_iff_keys (rows : List (Option Entry)) : Unique rows ↔ (keys rows).Nodup := by
  induction rows with
  | nil => simp [Unique, keys]
  | cons row rest ih => cases row <;> simp [Unique, keys, ih, List.nodup_cons, lookup_none_iff]

theorem flush_keys (location : Nat) (rows : List (Option Entry)) : keys (flush location rows).1 = keys rows := by
  induction rows with
  | nil => rfl
  | cons row rest ih =>
    cases row with
    | none => simpa [flush, keys] using ih
    | some entry => by_cases hit : entry.location = location ∧ entry.dirty = true <;> simp [flush, keys, hit, ih]

theorem flush_unique {rows : List (Option Entry)} (unique : Unique rows) (location : Nat) :
    Unique (flush location rows).1 := by
  rw [unique_iff_keys, flush_keys]
  exact (unique_iff_keys rows).mp unique

theorem invalidate_keys (location : Nat) (rows : List (Option Entry)) :
    keys (invalidate location rows) = (keys rows).filter (fun key => key != location) := by
  induction rows with
  | nil => rfl
  | cons row rest ih =>
    cases row with
    | none => simpa [invalidate, keys] using ih
    | some entry => by_cases hit : entry.location = location <;> simp [invalidate, keys, hit, ih]

theorem invalidate_unique {rows : List (Option Entry)} (unique : Unique rows) (location : Nat) :
    Unique (invalidate location rows) := by
  rw [unique_iff_keys, invalidate_keys]
  exact List.Pairwise.filter _ ((unique_iff_keys rows).mp unique)

theorem insert_keys (rows : List (Option Entry)) (entry : Entry) (extra : Nat) :
    keys (insert extra entry rows) = entry.location :: keys rows ∨
      (keys (insert extra entry rows)).Perm (entry.location :: keys rows) := by
  induction rows with
  | nil =>
    have padding : keys (List.replicate extra (none : Option Entry)) = [] := by
      induction extra with
      | zero => rfl
      | succ n ih => simpa only [List.replicate_succ, keys] using ih
    exact Or.inl (by simp [insert, keys, padding])
  | cons row rest ih =>
    cases row with
    | none => exact Or.inl (by simp [insert, keys])
    | some old =>
      have perm : (keys (insert extra entry rest)).Perm (entry.location :: keys rest) :=
        ih.elim (fun same => same ▸ List.Perm.refl _) id
      exact Or.inr ((List.Perm.cons old.location perm).trans (List.Perm.swap _ _ _))

theorem insert_unique {rows : List (Option Entry)} (unique : Unique rows) (entry : Entry) (extra : Nat)
    (fresh : lookup entry.location rows = none) : Unique (insert extra entry rows) := by
  have desired : (entry.location :: keys rows).Nodup :=
    List.nodup_cons.mpr ⟨(lookup_none_iff _ _).mp fresh, (unique_iff_keys rows).mp unique⟩
  rw [unique_iff_keys]
  rcases insert_keys rows entry extra with same | perm
  · simpa [same] using desired
  · exact perm.nodup_iff.mpr desired

/-- Source invalidate-before-dirty-enter establishes the uniqueness needed by
future writebacks; it is not an additional caller certificate after every step. -/
def replaceDirty (rows : List (Option Entry)) (location counter payload : Nat) : List (Option Entry) :=
  insert rows.length ⟨location, true, counter + 1, payload⟩ (invalidate location rows)

theorem replace_dirty_unique {rows : List (Option Entry)} (unique : Unique rows)
    (location counter payload : Nat) : Unique (replaceDirty rows location counter payload) :=
  insert_unique (invalidate_unique unique location) _ _ (invalidate_absent location rows)

theorem replace_dirty_lookup (rows : List (Option Entry)) (location counter payload : Nat) :
    lookup location (replaceDirty rows location counter payload) = some ⟨location, true, counter + 1, payload⟩ :=
  insert_lookup _ _ _ (invalidate_absent location rows) rfl

theorem full_growth (rows : List (Option Entry)) (entry : Entry) (extra : Nat)
    (full : none ∉ rows) : (insert extra entry rows).length = rows.length + extra + 1 := by
  induction rows with
  | nil => simp [insert]
  | cons row rest ih =>
    cases row with
    | none => simp at full
    | some old =>
      simp only [insert, List.length_cons]
      have next := ih (by simpa using full)
      omega

def StampBound (counter : Nat) : List (Option Entry) → Prop
  | [] => True
  | none :: rest => StampBound counter rest
  | some entry :: rest => entry.stamp ≤ counter ∧ StampBound counter rest

theorem bound_mono {rows : List (Option Entry)} {before after : Nat}
    (bound : StampBound before rows) (grows : before ≤ after) : StampBound after rows := by
  induction rows with
  | nil => trivial
  | cons row rest ih => cases row <;> simp_all [StampBound]; omega

theorem invalidate_bound {rows : List (Option Entry)} {counter : Nat}
    (bound : StampBound counter rows) (location : Nat) : StampBound counter (invalidate location rows) := by
  induction rows with
  | nil => trivial
  | cons row rest ih =>
    cases row with
    | none => exact ih bound
    | some entry =>
      by_cases hit : entry.location = location <;> simp_all [invalidate, StampBound]

theorem flush_bound {rows : List (Option Entry)} {counter : Nat}
    (bound : StampBound counter rows) (location : Nat) : StampBound counter (flush location rows).1 := by
  induction rows with
  | nil => trivial
  | cons row rest ih =>
    cases row with
    | none => exact ih bound
    | some entry =>
      by_cases hit : entry.location = location ∧ entry.dirty = true
      · simpa only [flush, ite_eq_left hit, StampBound, clean] using bound
      · simpa only [flush, ite_eq_right hit, StampBound] using And.intro bound.1 (ih bound.2)

theorem insert_bound {rows : List (Option Entry)} {counter : Nat} (bound : StampBound counter rows)
    (entry : Entry) (fresh : entry.stamp ≤ counter) (extra : Nat) : StampBound counter (insert extra entry rows) := by
  have padding : StampBound counter (List.replicate extra (none : Option Entry)) := by
    induction extra with
    | zero => trivial
    | succ n ih => simpa only [List.replicate_succ, StampBound] using ih
  induction rows with
  | nil => exact ⟨fresh, padding⟩
  | cons row rest ih => cases row <;> simp_all [insert, StampBound]

/-- Procedure-local monotone stamp counter: allocation increments it; invalidation
and writeback do not reset it. The initializer is the only reset boundary. -/
inductive Trace : Nat → List (Option Entry) → Prop where
  | initial : Trace 0 []
  | invalidate {rows counter} : Trace counter rows → (location : Nat) → Trace counter (invalidate location rows)
  | dirty {rows counter} : Trace counter rows → (location payload : Nat) →
      Trace (counter + 1) (replaceDirty rows location counter payload)
  | cold {rows counter} : Trace counter rows → (location payload : Nat) → (fresh : lookup location rows = none) →
      Trace (counter + 1) (insert rows.length ⟨location, false, counter + 1, payload⟩ rows)
  | flush {rows counter} : Trace counter rows → (location : Nat) → Trace counter (flush location rows).1

theorem trace_unique {rows : List (Option Entry)} {counter : Nat} (trace : Trace counter rows) : Unique rows := by
  induction trace with
  | initial => trivial
  | invalidate _ location ih => exact invalidate_unique ih location
  | dirty _ location payload ih => exact replace_dirty_unique ih location _ payload
  | cold _ location payload fresh ih => exact insert_unique ih _ _ fresh
  | flush _ location ih => exact flush_unique ih location

theorem trace_bound {rows : List (Option Entry)} {counter : Nat} (trace : Trace counter rows) : StampBound counter rows := by
  induction trace with
  | initial => trivial
  | invalidate _ location ih => exact invalidate_bound ih location
  | dirty _ location payload ih =>
    exact insert_bound (bound_mono (invalidate_bound ih location) (by omega)) _ (by simp) _
  | cold _ location payload fresh ih => exact insert_bound (bound_mono ih (by omega)) _ (by simp) _
  | flush _ location ih => exact flush_bound ih location

def writeback (box : Nat → Roots.Word) (memory : Nat → Roots.Word) : Option Entry → Nat → Roots.Word
  | none => memory
  | some entry => fun location => if location = entry.location then box entry.payload else memory location

theorem dirty_read_materialized {rows : List (Option Entry)} (unique : Unique rows) {location : Nat}
    {entry : Entry} (selected : lookup location rows = some entry) (dirty : entry.dirty = true)
    (box : Nat → Roots.Word) (memory : Nat → Roots.Word) :
    writeback box memory (flush location rows).2 entry.location = box entry.payload := by
  have event := (flush_refines (location := location) rows unique).1
  simp [selected, dirty, Option.filter] at event
  simp [event, writeback]
theorem lookup_bound {rows : List (Option Entry)} {counter location : Nat}
    (bound : StampBound counter rows) {entry : Entry} (selected : lookup location rows = some entry) :
    entry.stamp ≤ counter := by
  induction rows with
  | nil => simp [lookup] at selected
  | cons row rest ih =>
    cases row with
    | none => exact ih bound selected
    | some old =>
      by_cases hit : old.location = location
      · simp only [lookup, ite_eq_left hit, Option.some.injEq] at selected
        subst entry
        exact bound.1
      · exact ih bound.2 (by simpa [lookup, hit] using selected)

theorem new_stamp_fresh {rows : List (Option Entry)} {counter location : Nat} (history : Trace counter rows)
    {entry : Entry} (selected : lookup location rows = some entry) : entry.stamp ≠ counter + 1 := by
  have bounded := lookup_bound (trace_bound history) selected
  omega

theorem history_dirty_read {rows : List (Option Entry)} {counter location : Nat} (trace : Trace counter rows)
    {entry : Entry} (selected : lookup location rows = some entry) (dirty : entry.dirty = true)
    (box : Nat → Roots.Word) (memory : Nat → Roots.Word) :
    writeback box memory (flush location rows).2 entry.location = box entry.payload :=
  dirty_read_materialized (trace_unique trace) selected dirty box memory

/-- One chain from an admitted cache history through the emitted F64BOX event
and physical slot write into the existing signed map reader/root scanner. The
box allocator's pointer result and compiler live decision remain explicit. -/
theorem dirty_spill_signed {rows : List (Option Entry)} {counter location index root size width : Nat}
    (history : Trace counter rows) {entry : Entry} (selected : lookup location rows = some entry)
    (dirty : entry.dirty = true) (box : Nat → Roots.Word) (boxed : box entry.payload = .reference root)
    (memory : Nat → Roots.Word) (heap : Int → Roots.Word) (base fp : Int) {flags : List Bool}
    (positive : 0 < width) (live : flags[index]? = some true) (inside : index < size) :
    let mask := FrameMaps.generate 1 0 flags
    let payload := writeback box memory (flush location rows).2 entry.location
    let written := Slots.store heap (Slots.emittedAddress base fp (index + 1 : Nat)) payload
    ((BitVec.ofNat width (Encoding.field mask width (width * (index / width)))).sshiftRight
      (index % width)).getLsbD 0 = true ∧ root ∈ Roots.frame mask 0 (Slots.scanWords written base size) := by
  have value := history_dirty_read history selected dirty box memory
  dsimp
  rw [value, boxed]
  exact (FrameMaps.emitted_signed_root positive live (Slots.stored_word_read heap base fp size index inside _)).2

end Cache

namespace Acquire
/-- A location's version records all writes observed there. Mutex release
publishes the owner view; a successful acquire imports that released view. -/
abbrev View := Nat → Nat

def join (left right : View) : View := fun location => max (left location) (right location)
def put (view : View) (location value : Nat) : View :=
  fun target => if target = location then value else view target

structure State where
  current : View
  released : View
  views : Nat → View
  owner : Option Nat

def Safe (s : State) : Prop :=
  (s.owner = none → ∀ location, s.current location ≤ s.released location) ∧
  (∀ worker, s.owner = some worker → ∀ location, s.current location ≤ s.views worker location)

def initial : State := ⟨fun _ => 0, fun _ => 0, fun _ _ => 0, none⟩

theorem initial_safe : Safe initial := by simp [Safe, initial]

def write (s : State) (worker location : Nat) : State :=
  let next := s.current location + 1
  { s with
    current := put s.current location next
    views := fun w => if w = worker then put (s.views w) location next else s.views w }

def release (s : State) (worker : Nat) : State :=
  {s with owner := none, released := join s.released (s.views worker)}

def mutexAcquire (s : State) (worker : Nat) : State :=
  { s with
    owner := some worker
    views := fun w => if w = worker then join (s.views w) s.released else s.views w }

theorem write_safe {s : State} (safe : Safe s) {worker location : Nat}
    (owns : s.owner = some worker) : Safe (write s worker location) := by
  constructor
  · simp [write, owns]
  · intro w owner target
    have same : w = worker := by simpa [write, owns] using owner.symm
    subst w
    by_cases hit : target = location
    · simp [write, put, hit]
    · simpa [write, put, hit] using safe.2 worker owns target

theorem release_safe {s : State} (safe : Safe s) {worker : Nat}
    (owns : s.owner = some worker) : Safe (release s worker) := by
  constructor
  · intro _ location
    exact Nat.le_trans (safe.2 worker owns location) (Nat.le_max_right _ _)
  · simp [release]

theorem acquire_safe {s : State} (safe : Safe s) {worker : Nat}
    (free : s.owner = none) : Safe (mutexAcquire s worker) := by
  constructor
  · simp [mutexAcquire]
  · intro w owner location
    have same : worker = w := by simpa [mutexAcquire] using owner
    subst w
    apply Nat.le_trans (safe.1 free location)
    simpa [mutexAcquire, join] using (Nat.le_max_right (s.views worker location) (s.released location))

inductive Step : State → State → Prop where
  | write (s worker location) (owns : s.owner = some worker) : Step s (write s worker location)
  | release (s worker) (owns : s.owner = some worker) : Step s (release s worker)
  | acquire (s worker) (free : s.owner = none) : Step s (mutexAcquire s worker)
inductive Trace : State → State → Prop where
  | refl (s) : Trace s s
  | next {a b c} : Trace a b → Step b c → Trace a c

theorem step_safe {a b : State} (safe : Safe a) (step : Step a b) : Safe b := by
  cases step with
  | write _ _ owns => exact write_safe safe owns
  | release _ owns => exact release_safe safe owns
  | acquire _ free => exact acquire_safe safe free

theorem trace_safe {a b : State} (safe : Safe a) (trace : Trace a b) : Safe b := by
  induction trace with
  | refl => exact safe
  | next _ step ih => exact step_safe ih step

/-- Any protected location, arbitrary workers and admitted finite histories. -/
theorem owner_observes {a b : State} (safe : Safe a) (trace : Trace a b)
    {worker : Nat} (owns : b.owner = some worker) (location : Nat) :
    b.current location ≤ b.views worker location :=
  (trace_safe safe trace).2 worker owns location

/-- Complete admitted histories start from a derived valid empty state. No
final visibility invariant is supplied by the consumer. -/
theorem complete_handoff {s : State} (history : Trace initial s)
    {worker location epoch : Nat} (owns : s.owner = some worker)
    (published : epoch ≤ s.current location) : epoch ≤ s.views worker location :=
  Nat.le_trans published (owner_observes initial_safe history owns location)

/-- Plain polling only changes ownership in this weak observation projection.
It has no rule importing the released view. -/
def plainEnter (s : State) (worker : Nat) : State := {s with owner := some worker}

theorem plain_poll_counterexample :
    ∃ s : State, Safe s ∧ s.owner = none ∧ ¬ Safe (plainEnter s 1) := by
  refine ⟨⟨fun _ => 1, fun _ => 1, fun _ _ => 0, none⟩, ?_, rfl, ?_⟩
  · simp [Safe]
  · simp [Safe, plainEnter]

/-- Metadata classification is not a proof of architecture-specific lowering.
Single-thread execution has no cross-worker acquire obligation; it is never
reported as a successful SMP handoff. -/
inductive Backend where
  | singleThread
  | posixMutex
  | ticketPoll
  deriving DecidableEq, Repr

def acquireQualified : Backend → Bool
  | .posixMutex => true
  | _ => false

theorem no_smp_inference : acquireQualified .singleThread = false ∧
    acquireQualified .ticketPoll = false := by decide
end Acquire



namespace Publication
/-- Epochs count complete immutable chunks, including end marker and link.
This is an admitted snapshot interface: fence imports completed visibility,
not merely execution of a writer-side C barrier. ChunkPublication refines
writer/reader ordering before any native execution can admit this interface. -/
structure State where
  initialized : Nat
  visible : Nat
  tail : Nat
  deriving Repr, DecidableEq

def Safe (s : State) : Prop := s.tail ≤ s.visible ∧ s.visible ≤ s.initialized
def writeChunk (s : State) : State := {s with initialized := s.initialized + 1}
def fence (s : State) : State := {s with visible := s.initialized}
def publishTail (s : State) : State := {s with tail := s.initialized}

theorem write_safe {s : State} (h : Safe s) : Safe (writeChunk s) := by
  simp only [Safe, writeChunk] at *; omega

theorem fence_safe {s : State} (h : Safe s) : Safe (fence s) := by
  simp only [Safe, fence] at *; omega

theorem fenced_publication_safe (s : State) : Safe (publishTail (fence (writeChunk s))) := by
  simp [Safe, publishTail, fence, writeChunk]

/-- Abstract complete snapshot publication. Native end_heap_chunk has two
writer barriers, but they alone do not construct this visibility admission;
the reader must import the link before decoding it. Wakeup is not modeled. -/
def endChunk (s : State) : State := fence (publishTail (fence (writeChunk s)))

theorem end_chunk_safe (s : State) : Safe (endChunk s) :=
  fence_safe (fenced_publication_safe s)

inductive Step : State → State → Prop where
  | write (s) : Step s (writeChunk s)
  | flush (s) : Step s (fence s)
  | publish (s) (ready : s.visible = s.initialized) : Step s (publishTail s)
inductive Trace : State → State → Prop where
  | refl (s) : Trace s s
  | next {a b c} : Trace a b → Step b c → Trace a c

theorem step_safe {a b : State} (h : Safe a) (step : Step a b) : Safe b := by
  cases step with
  | write => exact write_safe h
  | flush => exact fence_safe h
  | publish ready => simp [Safe, publishTail, ready]

theorem trace_safe {a b : State} (h : Safe a) (trace : Trace a b) : Safe b := by
  induction trace with
  | refl => exact h
  | next _ step ih => exact step_safe ih step

theorem observed_payload {a b : State} (h : Safe a) (trace : Trace a b)
    {chunk : Nat} (observed : chunk ≤ b.tail) : chunk ≤ b.visible :=
  Nat.le_trans observed (trace_safe h trace).1

theorem unfenced_counterexample : ¬ Safe (publishTail (writeChunk ⟨0, 0, 0⟩)) := by simp [Safe, publishTail, writeChunk]

/-- Locked recheck reads current queue, not an optimistic pre-lock head. -/
def take : List Nat → Option Nat × List Nat
  | [] => (none, [])
  | head :: rest => (some head, rest)

theorem distinct_reservations {queue : List Nat} (unique : queue.Nodup)
    {left right : Nat} (first : (take queue).1 = some left)
    (second : (take (take queue).2).1 = some right) : left ≠ right := by
  cases queue with
  | nil => simp [take] at first
  | cons head rest =>
    simp only [take, Option.some.injEq] at first
    subst left
    cases rest with
    | nil => simp [take] at second
    | cons next more =>
      simp only [take, Option.some.injEq] at second
      subst right
      have h := (List.nodup_cons.mp unique).1
      simp only [List.mem_cons, not_or] at h
      exact h.1
/-- Bridge visibility to the existing logical queued/held ownership transfer. -/
theorem reservation_visible {a b : State} (safe : Safe a) (history : Trace a b)
    {worker : CollectorSchedule.Worker} {object : Nat} {rest : List Nat}
    (head : worker.queued = object :: rest)
    (published : ∀ chunk ∈ worker.queued, chunk ≤ b.tail) :
    object ≤ b.visible ∧
      List.Perm (CollectorSchedule.work (CollectorSchedule.reserve worker object rest))
        (CollectorSchedule.work worker) := by
  refine ⟨observed_payload safe history (published object ?_),
    CollectorSchedule.reservation_retains_work worker object rest head⟩
  simp [head]
end Publication

namespace Atomic
/-- Logical decoded ticket counts. Physical fixnum wrap requires a bound. -/
def fetchAdd (current increment : Nat) : Nat × Nat := (current, current + increment)
def compareSwap (current expected replacement : Nat) : Nat × Nat :=
  (current, if current = expected then replacement else current)

theorem tickets_unique (initial first second : Nat) (different : first ≠ second) :
    (fetchAdd (initial + first) 1).1 ≠ (fetchAdd (initial + second) 1).1 := by
  simp only [fetchAdd]; omega

theorem compare_swap_success (current expected replacement : Nat) :
    (compareSwap current expected replacement).1 = expected ↔ current = expected := by rfl

theorem compare_swap_failed {current expected replacement : Nat} (h : current ≠ expected) :
    (compareSwap current expected replacement).2 = current := by simp [compareSwap, h]

/-- Physical modulo tickets are unique only within an admitted no-wrap window. -/
theorem bounded_tickets_unique {initial first second modulus : Nat}
    (left : initial + first < modulus) (right : initial + second < modulus)
    (different : first ≠ second) :
    (initial + first) % modulus ≠ (initial + second) % modulus := by
  rw [Nat.mod_eq_of_lt left, Nat.mod_eq_of_lt right]
  omega
end Atomic
namespace Ticket
/-- Decoded no-wrap ticket state. The existing Atomic bounds admit physical
fixnum encoding separately. A held ticket excludes other successful try-CAS. -/
structure State where
  nextTicket : Nat
  currentTicket : Nat
  memory : Acquire.State

def Valid (s : State) : Prop := Acquire.Safe s.memory ∧ s.currentTicket ≤ s.nextTicket ∧
  (s.currentTicket = s.nextTicket → s.memory.owner = none)

/-- TRYLOCK2 reads current into temp then CASes next. Successful CAS is a
GCC full-barrier RMW, so it imports the released view, unlike a plain poll. -/
def tryAcquire (s : State) (worker : Nat) : Bool × State :=
  if s.nextTicket = s.currentTicket then
    (true, ⟨s.nextTicket + 1, s.currentTicket, Acquire.mutexAcquire s.memory worker⟩)
  else (false, s)

theorem try_success_safe {s : State} (valid : Valid s) {worker : Nat}
    (equal : s.nextTicket = s.currentTicket) :
    Acquire.Safe (tryAcquire s worker).2.memory := by
  have free : s.memory.owner = none := valid.2.2 equal.symm
  simpa [tryAcquire, equal] using Acquire.acquire_safe valid.1 free

theorem try_failed_unchanged {s : State} {worker : Nat} (busy : s.nextTicket ≠ s.currentTicket) :
    tryAcquire s worker = (false, s) := by simp [tryAcquire, busy]

theorem try_success_observes {s : State} (valid : Valid s) {worker location : Nat}
    (equal : s.nextTicket = s.currentTicket) :
    s.memory.current location ≤ (tryAcquire s worker).2.memory.views worker location := by
  have safe := try_success_safe valid (worker := worker) equal
  have observed := safe.2 worker (by simp [tryAcquire, equal, Acquire.mutexAcquire]) location
  simpa [tryAcquire, equal, Acquire.mutexAcquire] using observed
/-- Current may advance between the initial volatile read and the CAS. Admit
any past decoded observation, not just a snapshot of the latest current value. -/
def tryObserved (s : State) (worker observed : Nat) : Bool × State :=
  let receipt := Atomic.compareSwap s.nextTicket observed (observed + 1)
  if receipt.1 = observed then
    (true, ⟨receipt.2, s.currentTicket, Acquire.mutexAcquire s.memory worker⟩)
  else (false, s)

theorem stale_cas_success_safe {s : State} (valid : Valid s) {worker observed : Nat}
    (past : observed ≤ s.currentTicket) (won : s.nextTicket = observed) :
    Acquire.Safe (tryObserved s worker observed).2.memory := by
  have equal : s.currentTicket = s.nextTicket := by have order := valid.2.1; omega
  have free := valid.2.2 equal
  simpa [tryObserved, Atomic.compareSwap, won] using Acquire.acquire_safe valid.1 free

theorem stale_cas_fails {s : State} {worker observed : Nat}
    (past : observed < s.nextTicket) : tryObserved s worker observed = (false, s) := by
  have unequal : s.nextTicket ≠ observed := by omega
  simp [tryObserved, Atomic.compareSwap, unequal]

/-- Unlock may leave pending tickets and no current owner. Valid deliberately
uses only empty-queue -> unowned, never the unsound converse. -/
def unlock (s : State) (worker : Nat) : State :=
  ⟨s.nextTicket, s.currentTicket + 1, Acquire.release s.memory worker⟩

theorem unlock_valid {s : State} (valid : Valid s) {worker : Nat}
    (owns : s.memory.owner = some worker) : Valid (unlock s worker) := by
  have strict : s.currentTicket < s.nextTicket := by
    have order := valid.2.1
    by_cases equal : s.currentTicket = s.nextTicket
    · have free := valid.2.2 equal
      rw [owns] at free
      cases free
    · omega
  refine ⟨Acquire.release_safe valid.1 owns, ?_, ?_⟩
  · simp only [unlock]; omega
  · simp [unlock, Acquire.release]

theorem try_preserves_valid {s : State} (valid : Valid s) (worker : Nat) : Valid (tryAcquire s worker).2 := by
  by_cases equal : s.nextTicket = s.currentTicket
  · refine ⟨try_success_safe valid equal, ?_, ?_⟩
    · simp [tryAcquire, equal]
    · simp [tryAcquire, equal]
  · simpa [tryAcquire, equal] using valid

end Ticket

end ParserEngine.Host
