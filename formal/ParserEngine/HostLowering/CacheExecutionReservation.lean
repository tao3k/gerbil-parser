import ParserEngine.HostLowering.OperandBoxing
import ParserEngine.Host

/-! Concrete operand tags and heap-flonum writes at the reported Gambit boundary.
HostLowering composes the host producer models with C storage; it does not assert
that an arbitrary compiler or weak-memory backend refines these transitions. -/
namespace ParserEngine.Host
open ParserEngine.Representation

namespace CacheExecution
/-- The actual cache vector stores only location, dirty flag and stamp. Payload
bits live in F64V<stamp> in the active fresh-variable target mode. The
inactive reused-register/stack mode requires a different index adapter. -/
def decodeEntry (unboxed : Nat → Nat) : List Operand.Table.Atom → Option Cache.Entry
  | [.number location, .boolean dirty, .number stamp] =>
      some ⟨location, dirty, stamp, unboxed stamp⟩
  | _ => none

theorem decoded_payload (unboxed : Nat → Nat) (location stamp : Nat) (dirty : Bool) :
    decodeEntry unboxed [.number location, .boolean dirty, .number stamp] =
      some ⟨location, dirty, stamp, unboxed stamp⟩ := rfl

/-- False capacity slots stay false; each actual entry must have all three
native fields before it can enter an execution snapshot. -/
def decodeRows (unboxed : Nat → Nat) : List (Option (List Operand.Table.Atom)) → Option (List (Option Cache.Entry))
  | [] => some []
  | none :: rest => do let rows ← decodeRows unboxed rest; some (none :: rows)
  | some atoms :: rest => do
      let entry ← decodeEntry unboxed atoms
      let rows ← decodeRows unboxed rest
      some (some entry :: rows)

theorem decoded_shape (unboxed : Nat → Nat) {atoms : List Operand.Table.Atom} {entry : Cache.Entry}
    (decoded : decodeEntry unboxed atoms = some entry) :
    ∃ location dirty stamp, atoms = [.number location, .boolean dirty, .number stamp] ∧
      entry = ⟨location, dirty, stamp, unboxed stamp⟩ := by
  unfold decodeEntry at decoded
  split at decoded
  next location dirty stamp => exact ⟨location, dirty, stamp, rfl, Option.some.inj decoded.symm⟩
  next => cases decoded

/-- Exact physical dirty-field mutation and early return of the native loop. -/
def flushRows (location : Nat) : List (Option (List Operand.Table.Atom)) →
    List (Option (List Operand.Table.Atom)) × Option (List Operand.Table.Atom)
  | [] => ([], none)
  | some [.number key, .boolean true, .number stamp] :: rest =>
      if key = location then
        (some [.number key, .boolean false, .number stamp] :: rest,
          some [.number key, .boolean true, .number stamp])
      else let (rows, event) := flushRows location rest
           (some [.number key, .boolean true, .number stamp] :: rows, event)
  | first :: rest => let (rows, event) := flushRows location rest; (first :: rows, event)

theorem physical_flush (unboxed : Nat → Nat) (location : Nat)
    {physical : List (Option (List Operand.Table.Atom))} {rows : List (Option Cache.Entry)}
    (snapshot : decodeRows unboxed physical = some rows) :
    decodeRows unboxed (flushRows location physical).1 = some (Cache.flush location rows).1 ∧
      (flushRows location physical).2.bind (decodeEntry unboxed) = (Cache.flush location rows).2 := by
  induction physical generalizing rows with
  | nil => simp [decodeRows] at snapshot; subst rows; simp [flushRows, decodeRows, Cache.flush]
  | cons first rest ih =>
    cases first with
    | none =>
      cases tailSnapshot : decodeRows unboxed rest with
      | none => simp [decodeRows, tailSnapshot] at snapshot
      | some tailRows =>
        simp [decodeRows, tailSnapshot] at snapshot; subst rows
        obtain ⟨updated, event⟩ := ih tailSnapshot
        simp [flushRows, decodeRows, Cache.flush, updated, event]
    | some atoms =>
      cases entryDecoded : decodeEntry unboxed atoms with
      | none => simp [decodeRows, entryDecoded] at snapshot
      | some entry =>
        cases tailSnapshot : decodeRows unboxed rest with
        | none => simp [decodeRows, entryDecoded, tailSnapshot] at snapshot
        | some tailRows =>
          simp [decodeRows, entryDecoded, tailSnapshot] at snapshot; subst rows
          obtain ⟨key, dirty, stamp, shape, payload⟩ := decoded_shape unboxed entryDecoded
          subst atoms; subst entry
          obtain ⟨updated, event⟩ := ih tailSnapshot
          cases dirty with
          | false => simp [flushRows, decodeRows, decodeEntry, Cache.flush, updated, event]
          | true =>
            by_cases hit : key = location
            · simp [flushRows, decodeRows, decodeEntry, Cache.flush, hit, Cache.clean, tailSnapshot]
            · simp [flushRows, decodeRows, decodeEntry, Cache.flush, hit, updated, event]

def invalidateRows (location : Nat) : List (Option (List Operand.Table.Atom)) →
    List (Option (List Operand.Table.Atom))
  | [] => []
  | some (.number key :: fields) :: rest =>
      (if key = location then none else some (.number key :: fields)) :: invalidateRows location rest
  | first :: rest => first :: invalidateRows location rest

theorem physical_invalidate (unboxed : Nat → Nat) (location : Nat)
    {physical : List (Option (List Operand.Table.Atom))} {rows : List (Option Cache.Entry)}
    (snapshot : decodeRows unboxed physical = some rows) :
    decodeRows unboxed (invalidateRows location physical) = some (Cache.invalidate location rows) := by
  induction physical generalizing rows with
  | nil => simp [decodeRows] at snapshot; subst rows; rfl
  | cons first rest ih =>
    cases first with
    | none =>
      cases tailSnapshot : decodeRows unboxed rest with
      | none => simp [decodeRows, tailSnapshot] at snapshot
      | some tailRows =>
        simp [decodeRows, tailSnapshot] at snapshot; subst rows
        simp [invalidateRows, decodeRows, Cache.invalidate, ih tailSnapshot]
    | some atoms =>
      cases entryDecoded : decodeEntry unboxed atoms with
      | none => simp [decodeRows, entryDecoded] at snapshot
      | some entry =>
        cases tailSnapshot : decodeRows unboxed rest with
        | none => simp [decodeRows, entryDecoded, tailSnapshot] at snapshot
        | some tailRows =>
          simp [decodeRows, entryDecoded, tailSnapshot] at snapshot; subst rows
          obtain ⟨key, dirty, stamp, shape, payload⟩ := decoded_shape unboxed entryDecoded
          subst atoms; subst entry
          by_cases hit : key = location <;>
            simp [invalidateRows, decodeRows, decodeEntry, Cache.invalidate, hit, ih tailSnapshot]

/-- The record's reported size is updated only on physical vector growth in
this native producer. Hole reuse does not increment it. Do not identify it
with live occupancy when importing heap-reservation bounds. -/
def enterReported (state : Nat × List (Option Cache.Entry)) (entry : Cache.Entry) :
    Nat × List (Option Cache.Entry) :=
  (if none ∈ state.2 then state.1 else state.2.length + 1,
    Cache.insert state.2.length entry state.2)

theorem reported_size_is_not_occupancy_bound :
    let first := enterReported (0, []) ⟨0, true, 1, 11⟩
    let second := enterReported first ⟨8, true, 2, 22⟩
    let third := enterReported second ⟨16, true, 3, 33⟩
    third.1 = 2 ∧ (third.2.filterMap id).length = 3 := by decide

structure State where
  rows : List (Option Cache.Entry)
  memory : Nat → Roots.Word
  heap : Boxing.Heap

def put (memory : Nat → Roots.Word) (location : Nat) (value : Roots.Word) : Nat → Roots.Word :=
  fun query => if query = location then value else memory query

/-- Runtime denotation of emitted dirty writeback before targ-opnd's read. -/
def read (abi : Boxing.ABI) (state : State) (location : Nat) : State :=
  let (rows, event) := Cache.flush location state.rows
  match event with
  | none => { state with rows := rows }
  | some entry =>
      { rows := rows
        memory := put state.memory entry.location (.reference state.heap.hp)
        heap := Boxing.allocate abi state.heap (BitVec.ofNat 64 entry.payload) }

/-- Only emitted backend copies execute here; frontend self-copy elision is a
separate step. The destination invalidation follows the materialized source. -/
def copy (abi : Boxing.ABI) (state : State) (source target : Nat) : State :=
  let fetched := read abi state source
  { fetched with rows := Cache.invalidate target fetched.rows
                 memory := put fetched.memory target (fetched.memory source) }

theorem read_rows (abi : Boxing.ABI) (state : State) (location : Nat) :
    (read abi state location).rows = (Cache.flush location state.rows).1 := by
  cases flushed : Cache.flush location state.rows with
  | mk rows event => cases event <;> simp [read, flushed]

theorem physical_copy_rows (abi : Boxing.ABI) (unboxed : Nat → Nat) (state : State)
    (source target : Nat) {physical : List (Option (List Operand.Table.Atom))}
    (snapshot : decodeRows unboxed physical = some state.rows) :
    decodeRows unboxed (invalidateRows target (flushRows source physical).1) =
      some (copy abi state source target).rows := by
  simpa [copy, read_rows] using physical_invalidate unboxed target (physical_flush unboxed source snapshot).1

theorem lookup_location {rows : List (Option Cache.Entry)} {location : Nat} {entry : Cache.Entry}
    (selected : Cache.lookup location rows = some entry) : entry.location = location := by
  induction rows with
  | nil => simp [Cache.lookup] at selected
  | cons cell rest ih =>
    cases cell with
    | none => exact ih selected
    | some old =>
      by_cases hit : old.location = location
      · simp [Cache.lookup, hit] at selected; subst entry; exact hit
      · exact ih (by simpa [Cache.lookup, hit] using selected)

theorem dirty_copy (abi : Boxing.ABI) (state : State) {counter source : Nat} (target : Nat)
    (history : Cache.Trace counter state.rows) {entry : Cache.Entry}
    (selected : Cache.lookup source state.rows = some entry) (dirty : entry.dirty = true)
    (ready : Boxing.Ready state.heap) (fresh : Boxing.Fresh state.heap) :
    let after := copy abi state source target
    after.memory target = .reference state.heap.hp ∧
      after.heap.cells (state.heap.hp + 1) = some (.bits (BitVec.ofNat 64 entry.payload)) ∧
      after.heap.hp ≤ state.heap.limit ∧ Boxing.Fresh after.heap ∧
      Cache.lookup target after.rows = none ∧ Cache.Trace counter after.rows := by
  have event := (Cache.flush_refines (location := source) state.rows (Cache.trace_unique history)).1
  simp [selected, dirty, Option.filter] at event
  have location := lookup_location selected
  dsimp only [copy, read]
  simp only [event]
  refine ⟨?_, ?_, Boxing.allocation_within_segment abi ready _, Boxing.allocated_fresh abi fresh _,
    Cache.invalidate_absent _ _, Cache.Trace.invalidate (Cache.Trace.flush history source) target⟩
  · simp [put, location]
  · simpa [Boxing.body, Boxing.tagged] using Boxing.allocated_payload abi state.heap (BitVec.ofNat 64 entry.payload)

theorem clean_read_no_allocation (abi : Boxing.ABI) (state : State) (location : Nat)
    (noEvent : (Cache.flush location state.rows).2 = none) :
    (read abi state location).heap = state.heap ∧ (read abi state location).memory = state.memory := by
  simp [read, noEvent]

/-- The writes of a copy preserve every location other than its source and
its destination. Aliasing source/destination is allowed. -/
theorem copy_other (abi : Boxing.ABI) (state : State) (source target query : Nat)
    (history : Cache.Unique state.rows) (notSource : query ≠ source) (notTarget : query ≠ target) :
    (copy abi state source target).memory query = state.memory query := by
  have event := (Cache.flush_refines (location := source) state.rows history).1
  cases selected : Cache.lookup source state.rows with
  | none => simp [selected] at event; simp [copy, read, event, put, notTarget]
  | some entry =>
    have location := lookup_location selected
    cases dirty : entry.dirty with
    | false => simp [selected, dirty, Option.filter] at event; simp [copy, read, event, put, notTarget]
    | true =>
      simp [selected, dirty, Option.filter] at event
      simp [copy, read, event, put, location, notSource, notTarget]
/-- One chain from a history-derived dirty source, through emitted backend copy,
to an exact-bit collector snapshot rooted by the copied register. -/
theorem copy_collection (abi : Boxing.ABI) (state : State) {counter source : Nat} (target : Nat)
    (history : Cache.Trace counter state.rows) {entry : Cache.Entry}
    (selected : Cache.lookup source state.rows = some entry) (dirty : entry.dirty = true)
    (ready : Boxing.Ready state.heap) (fresh : Boxing.Fresh state.heap)
    (stack : List Roots.Word) {after : Operational.Heap.Store (BitVec 64)}
    (collection : Operational.Collection.Relocation (HeapCheck.snapshot (copy abi state source target).heap)
      after (HeapCheck.savedRoots [(copy abi state source target).memory target] stack)) :
    after[collection.address state.heap.hp]? = some (.atom (BitVec.ofNat 64 entry.payload)) := by
  have value := (dirty_copy abi state target history selected dirty ready fresh).1
  have event := (Cache.flush_refines (location := source) state.rows (Cache.trace_unique history)).1
  simp [selected, dirty, Option.filter] at event
  have heapEq : (copy abi state source target).heap = Boxing.allocate abi state.heap (BitVec.ofNat 64 entry.payload) := by
    simp [copy, read, event]
  have saved : state.heap.hp ∈ HeapCheck.savedRoots [(copy abi state source target).memory target] stack :=
    HeapCheck.saved_register (index := 0) (by simpa using value)
  have objectRead : (HeapCheck.snapshot (copy abi state source target).heap)[state.heap.hp]? =
      some (.atom (BitVec.ofNat 64 entry.payload)) := by
    rw [heapEq]; exact HeapCheck.allocated_object abi state.heap _
  simpa [Operational.Collection.renameCell] using collection.copied state.heap.hp _
    (Operational.Collection.Reachable.root saved) objectRead

end CacheExecution

namespace Reservation
/-- Physical occupancy and dirty potential are separate from the native
record's reported size and its vector capacity. -/
def occupancy : List (Option Cache.Entry) → Nat
  | [] => 0
  | none :: rest => occupancy rest
  | some _ :: rest => occupancy rest + 1

def dirtyCount : List (Option Cache.Entry) → Nat
  | [] => 0
  | none :: rest => dirtyCount rest
  | some entry :: rest => dirtyCount rest + if entry.dirty then 1 else 0

def eventCount : Option Cache.Entry → Nat
  | none => 0
  | some _ => 1

theorem dirty_le_occupancy (rows : List (Option Cache.Entry)) : dirtyCount rows ≤ occupancy rows := by
  induction rows with
  | nil => exact Nat.le_refl 0
  | cons first rest ih => cases first with
    | none => exact ih
    | some entry => cases flag : entry.dirty <;> simp [dirtyCount, occupancy, flag] <;> omega

theorem occupancy_le_capacity (rows : List (Option Cache.Entry)) : occupancy rows ≤ rows.length := by
  induction rows with
  | nil => exact Nat.le_refl 0
  | cons first rest ih => cases first <;> simp_all [occupancy]; omega

theorem insert_hole_length {rows : List (Option Cache.Entry)} (entry : Cache.Entry) (extra : Nat)
    (hole : none ∈ rows) : (Cache.insert extra entry rows).length = rows.length := by
  induction rows with
  | nil => simp at hole
  | cons first rest ih => cases first with
    | none => simp [Cache.insert]
    | some old => simp [Cache.insert, ih (by simpa using hole)]

theorem invalidate_length (location : Nat) (rows : List (Option Cache.Entry)) :
    (Cache.invalidate location rows).length = rows.length := by
  induction rows with
  | nil => rfl
  | cons first rest ih => cases first with
    | none => simpa [Cache.invalidate] using congrArg Nat.succ ih
    | some entry => by_cases hit : entry.location = location <;> simp [Cache.invalidate, hit, ih]

theorem flush_length (location : Nat) (rows : List (Option Cache.Entry)) :
    (Cache.flush location rows).1.length = rows.length := by
  induction rows with
  | nil => rfl
  | cons first rest ih => cases first with
    | none => simpa [Cache.flush] using congrArg Nat.succ ih
    | some entry =>
      by_cases hit : entry.location = location ∧ entry.dirty = true <;> simp [Cache.flush, hit, ih]

/-- Nonempty native vectors have capacity 2*reported-1; invalidation does not
shrink them or their reported field. -/
def Shape (reported : Nat) (rows : List (Option Cache.Entry)) : Prop :=
  (reported = 0 ∧ rows = []) ∨ rows.length + 1 = 2 * reported

theorem enter_shape {reported : Nat} {rows : List (Option Cache.Entry)} (shape : Shape reported rows)
    (entry : Cache.Entry) : Shape (CacheExecution.enterReported (reported, rows) entry).1
      (CacheExecution.enterReported (reported, rows) entry).2 := by
  by_cases hole : none ∈ rows
  · have length := insert_hole_length entry rows.length hole
    rcases shape with ⟨zero, empty⟩ | shaped
    · simp [empty] at hole
    · exact Or.inr (by simpa [CacheExecution.enterReported, hole, length] using shaped)
  · exact Or.inr (by simp [CacheExecution.enterReported, hole, Cache.full_growth rows entry rows.length hole]; omega)

theorem same_capacity_shape {reported : Nat} {rows after : List (Option Cache.Entry)}
    (shape : Shape reported rows) (length : after.length = rows.length) : Shape reported after := by
  rcases shape with ⟨zero, empty⟩ | shaped
  · exact Or.inl ⟨zero, List.eq_nil_of_length_eq_zero (by simp [length, empty])⟩
  · exact Or.inr (by omega)

inductive Trace : Nat → List (Option Cache.Entry) → Prop where
  | initial : Trace 0 []
  | enter {reported rows} : Trace reported rows → (entry : Cache.Entry) →
      Trace (CacheExecution.enterReported (reported, rows) entry).1 (CacheExecution.enterReported (reported, rows) entry).2
  | invalidate {reported rows} : Trace reported rows → (location : Nat) → Trace reported (Cache.invalidate location rows)
  | flush {reported rows} : Trace reported rows → (location : Nat) → Trace reported (Cache.flush location rows).1

theorem trace_shape {reported : Nat} {rows : List (Option Cache.Entry)} (history : Trace reported rows) :
    Shape reported rows := by
  induction history with
  | initial => exact Or.inl ⟨rfl, rfl⟩
  | enter _ entry ih => exact enter_shape ih entry
  | invalidate _ location ih => exact same_capacity_shape ih (invalidate_length location _)
  | flush _ location ih => exact same_capacity_shape ih (flush_length location _)

theorem reported_bound {reported : Nat} {rows : List (Option Cache.Entry)} (history : Trace reported rows) :
    dirtyCount rows ≤ occupancy rows ∧ occupancy rows ≤ rows.length ∧ rows.length ≤ 2 * reported := by
  refine ⟨dirty_le_occupancy rows, occupancy_le_capacity rows, ?_⟩
  rcases trace_shape history with ⟨zero, empty⟩ | shaped
  · simp [empty]
  · omega

/-- Every emitted dirty writeback consumes exactly one unit of dirty potential.
Uniqueness is not needed for this count, including malformed duplicate keys. -/
theorem flush_conservation (location : Nat) (rows : List (Option Cache.Entry)) :
    dirtyCount (Cache.flush location rows).1 + eventCount (Cache.flush location rows).2 = dirtyCount rows := by
  induction rows with
  | nil => rfl
  | cons first rest ih => cases first with
    | none => simpa [Cache.flush, dirtyCount] using ih
    | some entry =>
      by_cases hit : entry.location = location ∧ entry.dirty = true
      · simp [Cache.flush, hit, dirtyCount, Cache.clean, eventCount]
      · simp only [Cache.flush, ite_eq_right hit, dirtyCount]
        cases entry.dirty <;> simp <;> omega

def flushMany : List Nat → List (Option Cache.Entry) → List (Option Cache.Entry) × Nat
  | [], rows => (rows, 0)
  | location :: rest, rows =>
      let (updated, event) := Cache.flush location rows
      let (after, count) := flushMany rest updated
      (after, eventCount event + count)

theorem batch_conservation (locations : List Nat) (rows : List (Option Cache.Entry)) :
    dirtyCount (flushMany locations rows).1 + (flushMany locations rows).2 = dirtyCount rows := by
  induction locations generalizing rows with
  | nil => simp [flushMany]
  | cons location rest ih =>
    have one := flush_conservation location rows
    have later := ih (Cache.flush location rows).1
    simp only [flushMany]
    omega

theorem batch_shape (locations : List Nat) {reported : Nat} {rows : List (Option Cache.Entry)}
    (history : Trace reported rows) : Trace reported (flushMany locations rows).1 := by
  induction locations generalizing rows with
  | nil => exact history
  | cons location rest ih => exact ih (Trace.flush history location)

/-- Reserved emitted words plus remaining dirty boxes is conserved by flushing.
No reported-size/occupancy equality is used. -/
def potential (reserved wordsPerBox : Nat) (rows : List (Option Cache.Entry)) : Nat :=
  reserved + dirtyCount rows * wordsPerBox

theorem batch_potential (locations : List Nat) (rows : List (Option Cache.Entry)) (reserved wordsPerBox : Nat) :
    potential (reserved + (flushMany locations rows).2 * wordsPerBox) wordsPerBox (flushMany locations rows).1 =
      potential reserved wordsPerBox rows := by
  have conservation := batch_conservation locations rows
  unfold potential
  calc
    reserved + (flushMany locations rows).2 * wordsPerBox + dirtyCount (flushMany locations rows).1 * wordsPerBox =
        reserved + (dirtyCount (flushMany locations rows).1 + (flushMany locations rows).2) * wordsPerBox := by
          rw [Nat.add_mul]; omega
    _ = reserved + dirtyCount rows * wordsPerBox := by rw [conservation]

theorem capacity_budget {reported : Nat} {rows : List (Option Cache.Entry)} (history : Trace reported rows)
    (reserved wordsPerBox fudge : Nat) (admitted : reserved + rows.length * wordsPerBox ≤ fudge) :
    potential reserved wordsPerBox rows ≤ fudge := by
  have bounded : dirtyCount rows ≤ rows.length := Nat.le_trans (reported_bound history).1 (reported_bound history).2.1
  exact Nat.le_trans (Nat.add_le_add_left (Nat.mul_le_mul_right wordsPerBox bounded) reserved) admitted

theorem doubled_report_budget {reported : Nat} {rows : List (Option Cache.Entry)} (history : Trace reported rows)
    (reserved wordsPerBox fudge : Nat) (admitted : reserved + 2 * reported * wordsPerBox ≤ fudge) :
    potential reserved wordsPerBox rows ≤ fudge :=
  capacity_budget history reserved wordsPerBox fudge
    (Nat.le_trans (Nat.add_le_add_left (Nat.mul_le_mul_right wordsPerBox (reported_bound history).2.2) reserved) admitted)

/-- This is the native comparison, retained verbatim in the model, not a
replacement runtime gate. Its failure-to-check does not prove the capacity bound. -/
def nativeChecks (reserved reported wordsPerBox fudge : Nat) : Bool :=
  decide (reserved + reported * wordsPerBox > fudge)

theorem equal_charge_can_exceed_potential :
    let first := CacheExecution.enterReported (0, []) ⟨0, true, 1, 11⟩
    let second := CacheExecution.enterReported first ⟨8, true, 2, 22⟩
    let third := CacheExecution.enterReported second ⟨16, true, 3, 33⟩
    nativeChecks 0 third.1 2 4 = false ∧ 4 < potential 0 2 third.2 := by decide

/-- prepare_mem_pstate computes a soft threshold using min(available, physical
remaining minus fudge). Natural subtraction is admitted only with physical
room, corresponding to the C signed-capacity premise. -/
def threshold (hp physical fudge available : Nat) : Nat :=
  hp + min available (physical - hp - fudge)

theorem prepared_bounds (hp physical fudge available : Nat) (room : hp + fudge ≤ physical) :
    hp ≤ threshold hp physical fudge available ∧ threshold hp physical fudge available + fudge ≤ physical := by
  have choice := Nat.min_le_right available (physical - hp - fudge)
  unfold threshold
  omega

theorem pending_fits (hp soft physical reserved wordsPerBox : Nat) (rows : List (Option Cache.Entry))
    (before : hp ≤ soft) (margin : soft + potential reserved wordsPerBox rows ≤ physical)
    (locations : List Nat) : hp + reserved + (flushMany locations rows).2 * wordsPerBox ≤ physical := by
  have count := batch_conservation locations rows
  have bound : (flushMany locations rows).2 ≤ dirtyCount rows := by omega
  have words := Nat.mul_le_mul_right wordsPerBox bound
  unfold potential at margin
  omega
/-- The compiler charges a portable upper bound, not the native object's exact
word count. In the qualified non-handle 64-bit branch these are six and two. -/
def compilerFlonumCharge (minWordBytes headerWords : Nat) : Nat :=
  (((8 + minWordBytes + headerWords * minWordBytes + 7) / 8) * 8) / minWordBytes

theorem reported_actual_bound {reported : Nat} {rows : List (Option Cache.Entry)}
    (history : Trace reported rows) (actualWords chargedWords : Nat)
    (ratio : 2 * actualWords ≤ chargedWords) :
    dirtyCount rows * actualWords ≤ reported * chargedWords := by
  have count : dirtyCount rows ≤ 2 * reported :=
    Nat.le_trans (reported_bound history).1 (Nat.le_trans (reported_bound history).2.1 (reported_bound history).2.2)
  calc
    dirtyCount rows * actualWords ≤ (2 * reported) * actualWords := Nat.mul_le_mul_right actualWords count
    _ = reported * (2 * actualWords) := by simp [Nat.mul_assoc, Nat.mul_left_comm]
    _ ≤ reported * chargedWords := Nat.mul_le_mul_left reported ratio

theorem native_no_check_safe {reported : Nat} {rows : List (Option Cache.Entry)}
    (history : Trace reported rows) (actualUsed reserved actualWords chargedWords fudge : Nat)
    (emitted : actualUsed ≤ reserved) (ratio : 2 * actualWords ≤ chargedWords)
    (noCheck : nativeChecks reserved reported chargedWords fudge = false) :
    actualUsed + dirtyCount rows * actualWords ≤ fudge := by
  have bounded := reported_actual_bound history actualWords chargedWords ratio
  have gate : reserved + reported * chargedWords ≤ fudge := by simp [nativeChecks] at noCheck; omega
  exact Nat.le_trans (Nat.add_le_add emitted bounded) gate

theorem qualified_charge : compilerFlonumCharge 4 2 = 6 ∧ 2 * 2 ≤ compilerFlonumCharge 4 2 := by decide

theorem qualified_no_check_safe {reported : Nat} {rows : List (Option Cache.Entry)}
    (history : Trace reported rows) (actualUsed reserved : Nat) (emitted : actualUsed ≤ reserved)
    (noCheck : nativeChecks reserved reported (compilerFlonumCharge 4 2) 4096 = false) :
    actualUsed + dirtyCount rows * 2 ≤ 4096 :=
  native_no_check_safe history actualUsed reserved 2 (compilerFlonumCharge 4 2) 4096 emitted qualified_charge.2 noCheck

/-- A heap check is emitted after frame materialization. All remaining dirty
boxes fit after a bounded potential increase by the triggering instruction,
using the C physical fudge rather than the compiler's smaller comparison. -/
theorem triggering_flush_fits (before after : List (Option Cache.Entry))
    (actualBefore actualAfter soft physical compilerFudge physicalFudge stepCost : Nat)
    (locations : List Nat) (prior : actualBefore + dirtyCount before * 2 ≤ compilerFudge)
    (step : actualAfter + dirtyCount after * 2 ≤ actualBefore + dirtyCount before * 2 + stepCost)
    (slack : compilerFudge + stepCost ≤ physicalFudge) (margin : soft + physicalFudge ≤ physical) :
    soft + actualAfter + (flushMany locations after).2 * 2 ≤ physical := by
  have events := batch_conservation locations after
  omega

/-- The actual prepare_mem_pstate geometry composes with an admitted compiler
budget. physical has already excluded end-of-chunk marker space. -/
theorem prepared_no_check_fits {reported : Nat} {rows : List (Option Cache.Entry)}
    (history : Trace reported rows) (start physical physicalFudge available actualUsed reserved : Nat)
    (room : start + physicalFudge ≤ physical) (fudge : 4096 ≤ physicalFudge)
    (emitted : actualUsed ≤ reserved)
    (noCheck : nativeChecks reserved reported (compilerFlonumCharge 4 2) 4096 = false)
    (locations : List Nat) :
    start + actualUsed + (flushMany locations rows).2 * 2 ≤ physical := by
  have budget := qualified_no_check_safe history actualUsed reserved emitted noCheck
  have bounds := prepared_bounds start physical physicalFudge available room
  have events := batch_conservation locations rows
  omega

def executeReads (abi : Boxing.ABI) : List Nat → CacheExecution.State → CacheExecution.State
  | [], state => state
  | location :: rest, state => executeReads abi rest (CacheExecution.read abi state location)

def ReadSafe (state : CacheExecution.State) : Prop :=
  state.heap.hp + dirtyCount state.rows * 2 ≤ state.heap.limit

theorem read_hp (abi : Boxing.ABI) (state : CacheExecution.State) (location : Nat) :
    (CacheExecution.read abi state location).heap.hp =
      state.heap.hp + eventCount (Cache.flush location state.rows).2 * 2 := by
  cases flushed : Cache.flush location state.rows with
  | mk rows event => cases event <;> simp [CacheExecution.read, flushed, eventCount, Boxing.allocate]

theorem read_limit (abi : Boxing.ABI) (state : CacheExecution.State) (location : Nat) :
    (CacheExecution.read abi state location).heap.limit = state.heap.limit := by
  cases flushed : Cache.flush location state.rows with
  | mk rows event => cases event <;> simp [CacheExecution.read, flushed, Boxing.allocate]

theorem read_preserves_potential (abi : Boxing.ABI) (state : CacheExecution.State) (location : Nat) :
    (CacheExecution.read abi state location).heap.hp + dirtyCount (CacheExecution.read abi state location).rows * 2 =
      state.heap.hp + dirtyCount state.rows * 2 := by
  have conserved := flush_conservation location state.rows
  rw [read_hp, CacheExecution.read_rows]
  omega

theorem read_preserves_safe (abi : Boxing.ABI) (state : CacheExecution.State) (location : Nat)
    (safe : ReadSafe state) : ReadSafe (CacheExecution.read abi state location) := by
  unfold ReadSafe
  rw [read_preserves_potential, read_limit]
  exact safe

theorem dirty_event_ready (state : CacheExecution.State) (location : Nat) (safe : ReadSafe state)
    {entry : Cache.Entry} (emitted : (Cache.flush location state.rows).2 = some entry) : Boxing.Ready state.heap := by
  have conserved := flush_conservation location state.rows
  simp only [emitted, eventCount] at conserved
  unfold ReadSafe at safe
  unfold Boxing.Ready
  omega

theorem read_heap_trace (abi : Boxing.ABI) (state : CacheExecution.State) (location : Nat) (safe : ReadSafe state) :
    Boxing.Trace abi state.heap (CacheExecution.read abi state location).heap := by
  cases flushed : Cache.flush location state.rows with
  | mk rows event =>
    cases event with
    | none => simpa [CacheExecution.read, flushed] using Boxing.Trace.refl (abi := abi) state.heap
    | some entry =>
      have ready := dirty_event_ready state location safe (by rw [flushed])
      simpa [CacheExecution.read, flushed] using
        Boxing.Trace.box (Boxing.Trace.refl state.heap) ready (BitVec.ofNat 64 entry.payload)

theorem heap_trace_trans {abi : Boxing.ABI} {before middle after : Boxing.Heap}
    (first : Boxing.Trace abi before middle) (second : Boxing.Trace abi middle after) : Boxing.Trace abi before after := by
  induction second with
  | refl => exact first
  | box earlier ready bits ih => exact Boxing.Trace.box ih ready bits
  | switchSection earlier start stop free ih => exact Boxing.Trace.switchSection ih start stop free

theorem execute_reads_safe (abi : Boxing.ABI) (locations : List Nat) (state : CacheExecution.State)
    (safe : ReadSafe state) :
    Boxing.Trace abi state.heap (executeReads abi locations state).heap ∧ ReadSafe (executeReads abi locations state) := by
  induction locations generalizing state with
  | nil => exact ⟨Boxing.Trace.refl _, safe⟩
  | cons location rest ih =>
    obtain ⟨later, laterSafe⟩ := ih (CacheExecution.read abi state location) (read_preserves_safe abi state location safe)
    exact ⟨heap_trace_trans (read_heap_trace abi state location safe) later, laterSafe⟩

theorem native_budget_admits_reads (abi : Boxing.ABI) (state : CacheExecution.State)
    {reported : Nat} (history : Trace reported state.rows) (start actualUsed reserved : Nat)
    (atCursor : state.heap.hp = start + actualUsed) (emitted : actualUsed ≤ reserved)
    (noCheck : nativeChecks reserved reported (compilerFlonumCharge 4 2) 4096 = false)
    (margin : start + 4096 ≤ state.heap.limit) (locations : List Nat) (fresh : Boxing.Fresh state.heap) :
    Boxing.Trace abi state.heap (executeReads abi locations state).heap ∧
      Boxing.Fresh (executeReads abi locations state).heap ∧
      (∀ address cell, state.heap.cells address = some cell →
        (executeReads abi locations state).heap.cells address = some cell) := by
  have budget := qualified_no_check_safe history actualUsed reserved emitted noCheck
  have safe : ReadSafe state := by unfold ReadSafe; omega
  have execution := (execute_reads_safe abi locations state safe).1
  exact ⟨execution, Boxing.trace_fresh fresh execution,
    fun _ _ read => Boxing.trace_retains fresh execution read⟩

/-- Reserve counters are generated by actual emitted operations, not supplied
as an arbitrary inequality after every writeback. Other object kinds retain
their own native size-bound premise. -/
structure Counters where
  actual : Nat
  reserved : Nat
  floating : Nat

def record (before : Counters) (actual charged : Nat) (floating : Bool) : Counters :=
  ⟨before.actual + actual, before.reserved + charged, before.floating + if floating then charged else 0⟩

inductive Accounting : Counters → Prop where
  | initial : Accounting ⟨0, 0, 0⟩
  | box {before} : Accounting before → Accounting (record before 2 (compilerFlonumCharge 4 2) true)
  | other {before} : Accounting before → (actual charged : Nat) → actual ≤ charged → (floating : Bool) →
      Accounting (record before actual charged floating)
  | checked {before} : Accounting before → Accounting ⟨0, 0, 0⟩

theorem accounting_bounds {counters : Counters} (history : Accounting counters) :
    counters.actual ≤ counters.reserved ∧ counters.floating ≤ counters.reserved := by
  induction history with
  | initial => simp
  | box _ ih => dsimp [record, compilerFlonumCharge]; omega
  | other _ actual charged bound floating ih => cases floating <;> dsimp [record] <;> omega
  | checked _ _ => simp


/-- Compose generated accounting, the reported-size producer, C's prepared
threshold and executed F64BOX events. The caller supplies neither an occupancy
identity nor a post-hoc emitted/reserved inequality. -/
theorem prepared_accounted_reads (abi : Boxing.ABI) (state : CacheExecution.State)
    {reported : Nat} (cacheHistory : Trace reported state.rows) (counters : Counters)
    (accounting : Accounting counters) (anchor reset available : Nat)
    (room : anchor + 8194 ≤ state.heap.limit)
    (resetBeforeThreshold : reset ≤ threshold anchor state.heap.limit 8194 available)
    (cursor : state.heap.hp = reset + counters.actual)
    (noCheck : nativeChecks counters.reserved reported (compilerFlonumCharge 4 2) 4096 = false)
    (locations : List Nat) (fresh : Boxing.Fresh state.heap) :
    Boxing.Trace abi state.heap (executeReads abi locations state).heap ∧
      Boxing.Fresh (executeReads abi locations state).heap ∧
      (∀ address cell, state.heap.cells address = some cell →
        (executeReads abi locations state).heap.cells address = some cell) := by
  have bounds := prepared_bounds anchor state.heap.limit 8194 available room
  have margin : reset + 4096 ≤ state.heap.limit := by omega
  exact native_budget_admits_reads abi state cacheHistory reset counters.actual counters.reserved cursor
    (accounting_bounds accounting).1 noCheck margin locations fresh
theorem invalidate_dirty_le (location : Nat) (rows : List (Option Cache.Entry)) :
    dirtyCount (Cache.invalidate location rows) ≤ dirtyCount rows := by
  induction rows with
  | nil => exact Nat.le_refl 0
  | cons first rest ih => cases first with
    | none => exact ih
    | some entry =>
      by_cases hit : entry.location = location <;> cases flag : entry.dirty <;>
        simp [Cache.invalidate, hit, dirtyCount, flag] <;> omega

theorem insert_dirty_count (rows : List (Option Cache.Entry)) (entry : Cache.Entry) (extra : Nat) :
    dirtyCount (Cache.insert extra entry rows) = dirtyCount rows + if entry.dirty then 1 else 0 := by
  have padding : dirtyCount (List.replicate extra (none : Option Cache.Entry)) = 0 := by
    induction extra with
    | zero => rfl
    | succ n ih => simpa only [List.replicate_succ, dirtyCount] using ih
  induction rows with
  | nil => simp [Cache.insert, dirtyCount, padding]
  | cons first rest ih => cases first with
    | none => simp [Cache.insert, dirtyCount]
    | some old => simp only [Cache.insert, dirtyCount, ih]; omega

theorem dirty_replacement_growth (rows : List (Option Cache.Entry)) (location counter payload : Nat) :
    dirtyCount (Cache.replaceDirty rows location counter payload) ≤ dirtyCount rows + 1 := by
  have discarded := invalidate_dirty_le location rows
  simpa [Cache.replaceDirty, insert_dirty_count] using Nat.add_le_add_right discarded 1

/-- The triggering unboxed assignment creates at most one new dirty result.
Its two-word potential increase fits the difference between source C fudge
and the smaller compiler threshold; all live flush selections are admitted. -/
theorem triggering_dirty_reads (abi : Boxing.ABI) (before : CacheExecution.State)
    {reported : Nat} (history : Trace reported before.rows) (counters : Counters)
    (accounting : Accounting counters) (reset location stamp payload : Nat)
    (cursor : before.heap.hp = reset + counters.actual)
    (noCheck : nativeChecks counters.reserved reported (compilerFlonumCharge 4 2) 4096 = false)
    (physicalMargin : reset + 8194 ≤ before.heap.limit)
    (locations : List Nat) (fresh : Boxing.Fresh before.heap) :
    let after := { before with rows := Cache.replaceDirty before.rows location stamp payload }
    Boxing.Trace abi before.heap (executeReads abi locations after).heap ∧
      Boxing.Fresh (executeReads abi locations after).heap ∧
      (∀ address cell, before.heap.cells address = some cell →
        (executeReads abi locations after).heap.cells address = some cell) := by
  let after := { before with rows := Cache.replaceDirty before.rows location stamp payload }
  have budget := qualified_no_check_safe history counters.actual counters.reserved (accounting_bounds accounting).1 noCheck
  have growth := dirty_replacement_growth before.rows location stamp payload
  have safe : ReadSafe after := by dsimp [ReadSafe, after]; omega
  have execution := (execute_reads_safe abi locations after safe).1
  exact ⟨execution, Boxing.trace_fresh fresh execution,
    fun _ _ read => Boxing.trace_retains fresh execution read⟩

end Reservation


end ParserEngine.Host
