import ParserEngine.HostLowering.SwitchEmissionIfJumpEmission
import ParserEngine.Host

/-! Concrete operand tags and heap-flonum writes at the reported Gambit boundary.
HostLowering composes the host producer models with C storage; it does not assert
that an arbitrary compiler or weak-memory backend refines these transitions. -/
namespace ParserEngine.Host
open ParserEngine.Representation

namespace ObjectClassification
/-- Qualified non-NaN-boxed ABI with a low-bit memory predicate. The native control
rejects other SDK configurations instead of reading immediate bits as pointers. -/
inductive Kind where
  | immediate | movable | still | forwarded | permanent | invalid | unmapped
  deriving DecidableEq, Repr

def fromHeader (tagBits : Nat) (head : BitVec 64) : Kind :=
  let kind := head.toNat % 8
  if kind = 0 then .movable else if kind = 5 then .still
  else if kind % 2^tagBits = 3 then .forwarded
  else if kind = 6 then .permanent else .invalid

def bodyAddress (tagBits : Nat) (referenceBytes word : BitVec 64) : BitVec 64 :=
  (word &&& ~~~(BitVec.ofNat 64 (2^tagBits - 1))) + referenceBytes

def classify (tagBits : Nat) (referenceBytes word : BitVec 64)
    (load : BitVec 64 → Option (BitVec 64)) : Kind :=
  if word.toNat % 2 = 0 then .immediate else
    match load (bodyAddress tagBits referenceBytes word - (8 : BitVec 64)) with
    | none => .unmapped
    | some head => fromHeader tagBits head

theorem immediate_never_reads_header (tagBits : Nat) (referenceBytes word : BitVec 64)
    (even : word.toNat % 2 = 0) (left right : BitVec 64 → Option (BitVec 64)) :
    classify tagBits referenceBytes word left = .immediate ∧
    classify tagBits referenceBytes word left = classify tagBits referenceBytes word right := by
  simp [classify, even]

theorem missing_reference_header_rejected (tagBits : Nat) (referenceBytes word : BitVec 64)
    (odd : word.toNat % 2 ≠ 0) (load : BitVec 64 → Option (BitVec 64))
    (missing : load (bodyAddress tagBits referenceBytes word - (8 : BitVec 64)) = none) :
    classify tagBits referenceBytes word load = .unmapped := by
  unfold classify
  rw [ite_eq_right odd, missing]

/-- Release builds do not necessarily diagnose invalid allocated headers:
mark_array's else branch reports only under enabled consistency diagnostics.
An unmapped address is outside C dereference admission, never PERM. -/
inductive Effect where
  | unchanged | copy | scheduleStill | redirect | reportInvalid | ignoreInvalid | unadmittedDereference
  deriving DecidableEq, Repr

def effect (diagnostics : Bool) : Kind → Effect
  | .immediate | .permanent => .unchanged
  | .movable => .copy
  | .still => .scheduleStill
  | .forwarded => .redirect
  | .invalid => if diagnostics then .reportInvalid else .ignoreInvalid
  | .unmapped => .unadmittedDereference

theorem invalid_is_not_permanent (head : BitVec 64) (tagBits : Nat)
    (invalid : fromHeader tagBits head = .invalid) :
    fromHeader tagBits head ≠ .permanent ∧ effect false (fromHeader tagBits head) = .ignoreInvalid := by
  simp [invalid, effect]

theorem native_header_dispatch (tagBits : Nat) (admitted : tagBits = 2 ∨ tagBits = 3)
    (head : BitVec 64) :
    (fromHeader tagBits head = .movable ↔ head.toNat % 8 = 0) ∧
    (fromHeader tagBits head = .still ↔ head.toNat % 8 = 5) ∧
    (fromHeader tagBits head = .forwarded ↔ head.toNat % 2^tagBits = 3) ∧
    (fromHeader tagBits head = .permanent ↔ head.toNat % 8 = 6) := by
  have bounds := Nat.mod_lt head.toNat (by decide : 0 < 8)
  have cases : head.toNat % 8 = 0 ∨ head.toNat % 8 = 1 ∨ head.toNat % 8 = 2 ∨
      head.toNat % 8 = 3 ∨ head.toNat % 8 = 4 ∨ head.toNat % 8 = 5 ∨
      head.toNat % 8 = 6 ∨ head.toNat % 8 = 7 := by omega
  rcases admitted with rfl | rfl <;>
    rcases cases with h | h | h | h | h | h | h | h <;>
      simp [fromHeader, h] <;> omega

/-- MAKE_HD uses five subtype bits above three header bits. This mathematical
packing corresponds to unsigned C only while the addition does not overflow. -/
def packed (bytes subtype kind : Nat) : Nat := bytes * 256 + subtype * 8 + kind

theorem packed_decode (bytes subtype kind : Nat) (subtypeBound : subtype < 32)
    (kindBound : kind < 8) :
    packed bytes subtype kind % 8 = kind ∧
    (packed bytes subtype kind / 8) % 32 = subtype ∧
    (packed bytes subtype kind + 7 * 256) / 2048 = (bytes + 7) / 8 := by
  unfold packed
  omega

theorem header_mask (head : BitVec 64) : (head &&& 7).toNat = head.toNat % 8 := by
  rw [BitVec.toNat_and]
  exact Nat.and_two_pow_sub_one_eq_mod head.toNat 3

/-- Positive signed-header admission avoids undefined signed construction and
also proves the collector's unsigned rounding addition does not wrap. -/
theorem packed_word_decode (bytes subtype kind : Nat) (subtypeBound : subtype < 32)
    (kindBound : kind < 8) (signedFits : packed bytes subtype kind + 1792 < 2^63) :
    let head := BitVec.ofNat 64 (packed bytes subtype kind)
    (head &&& 7).toNat = kind ∧
    (head.toNat / 8) % 32 = subtype ∧
    (head + (1792 : BitVec 64)).toNat / 2048 = (bytes + 7) / 8 := by
  have fits : packed bytes subtype kind < 2^64 := by omega
  have roundedFits : packed bytes subtype kind + 1792 < 2^64 := by omega
  have value : (BitVec.ofNat 64 (packed bytes subtype kind)).toNat = packed bytes subtype kind := by
    simp [BitVec.toNat_ofNat, Nat.mod_eq_of_lt fits]
  dsimp only
  obtain ⟨tag, subtypeRead, countRead⟩ := packed_decode bytes subtype kind subtypeBound kindBound
  refine ⟨?_, ?_, ?_⟩
  · rw [header_mask, value]; exact tag
  · rw [value]; exact subtypeRead
  · rw [BitVec.toNat_add, value]
    change (packed bytes subtype kind + 1792) % 2^64 / 2048 = (bytes + 7) / 8
    rw [Nat.mod_eq_of_lt roundedFits]
    exact countRead

/-- The native low-bit MEM_ALLOCATED predicate is exactly the arithmetic
predicate used before any header access, for every 64-bit object word. -/
theorem memory_mask (word : BitVec 64) : (word &&& 1).toNat = word.toNat % 2 := by
  rw [BitVec.toNat_and]
  exact Nat.and_two_pow_sub_one_eq_mod word.toNat 1

structure ScanContext where
  traverseWeak : Bool
  allocatedKeys : Bool
  weakKeys : Bool
  weakValues : Bool
  closureFreeStart : Nat
  frameSlots : List Nat

/-- The branch table is extracted from scan, not the earlier mark_array
header dispatch. STILL bodies are scanned but do not take the MOVABLE0 tail
shortcut. PERM bodies never enter this scan through the marker's worklist. -/
def shape (subtype words headerKind : Nat) (context : ScanContext) : SpecialScan.Shape :=
  if 18 ≤ subtype ∧ subtype ≤ 31 then .opaque
  else if subtype = 8 ∨ subtype = 9 then .name
  else if subtype = 13 then
    if words = 3 then .will context.traverseWeak
    else .hashTable context.allocatedKeys context.weakKeys context.weakValues 5 6
  else if subtype = 11 then .continuation
  else if subtype = 10 then .frame context.frameSlots
  else if subtype = 14 then .closure context.closureFreeStart
  else .ordinary (headerKind == 0 && decide (subtype ≤ 5))

theorem still_pair_scans_both_fields (context : ScanContext) :
    SpecialScan.offsets (shape 1 2 5 context) 2 = [0, 1] := by simp [shape, SpecialScan.offsets, List.range_succ]

theorem movable_pair_tail_requires_first_mark (context : ScanContext) :
    SpecialScan.offsets (shape 1 2 0 context) 2 = [1] := by simp [shape, SpecialScan.offsets, List.range_succ]

theorem decoded_fields_in_bounds (subtype words headerKind index : Nat) (context : ScanContext)
    (valid : SpecialScan.Valid (shape subtype words headerKind context) words)
    (selected : index ∈ SpecialScan.offsets (shape subtype words headerKind context) words) :
    index < words := SpecialScan.selected_fields_in_bounds _ _ _ valid selected

/-- Nonmoving references keep their raw object tag/address. A STILL mark may
schedule a scan once; the one-way mark state is per collection cycle. -/
inductive StillMark where
  | unmarked | queued
  deriving DecidableEq, Repr
structure Marker where
  mark : StillMark
  scheduled : Nat
  deriving DecidableEq, Repr

def visitStill (state : Marker) : Marker :=
  match state.mark with
  | .unmarked => ⟨.queued, state.scheduled + 1⟩
  | .queued => state

def visitMany : Nat → Marker → Marker
  | 0, state => state
  | n + 1, state => visitStill (visitMany n state)

theorem repeated_still_visit_once (visits : Nat) :
    visitMany (visits + 1) ⟨.unmarked, 0⟩ = ⟨.queued, 1⟩ := by
  induction visits with
  | zero => rfl
  | succ n ih => rw [visitMany, ih]; rfl

/-- Source-to-selector composition for bodies the marker actually schedules.
MOVABLE0 and STILL can reach scan; permanent/forwarded/invalid headers cannot
be silently admitted by this theorem. Header load and physical layout remain
explicit inputs rather than a final scan-correctness certificate. -/
theorem classification_to_scan (tagBits : Nat) (tagAdmission : tagBits = 2 ∨ tagBits = 3)
    (referenceBytes word head : BitVec 64) (load : BitVec 64 → Option (BitVec 64))
    (allocated : word.toNat % 2 ≠ 0)
    (read : load (bodyAddress tagBits referenceBytes word - (8 : BitVec 64)) = some head)
    (scheduled : head.toNat % 8 = 0 ∨ head.toNat % 8 = 5)
    (context : ScanContext) (words : Nat)
    (valid : SpecialScan.Valid (shape ((head.toNat / 8) % 32) words (head.toNat % 8) context) words) :
    (classify tagBits referenceBytes word load = .movable ∨
     classify tagBits referenceBytes word load = .still) ∧
    ∀ index, index ∈ SpecialScan.offsets (shape ((head.toNat / 8) % 32) words (head.toNat % 8) context) words →
      index < words := by
  constructor
  · unfold classify
    rw [ite_eq_right allocated, read]
    dsimp only
    obtain ⟨movable, still, _, _⟩ := native_header_dispatch tagBits tagAdmission head
    rcases scheduled with moving | nonmoving
    · exact Or.inl (movable.mpr moving)
    · exact Or.inr (still.mpr nonmoving)
  · intro index selected
    exact SpecialScan.selected_fields_in_bounds _ _ _ valid selected

end ObjectClassification

namespace ChunkPublication
/-- The end marker is written only after the full movable copy loop. An
explicit fresh marker cell follows the body, not a Boolean readiness flag. -/
def endMemory (memory : CollectorCopy.Memory) (oldHeader newHeader count : Nat)
    (header forwarding marker : BitVec 64) : CollectorCopy.Memory :=
  CollectorCopy.write (CollectorCopy.move memory oldHeader newHeader header forwarding count)
    (newHeader + count + 1) (some marker)

theorem copied_body_and_marker (memory : CollectorCopy.Memory) (oldHeader newHeader count : Nat)
    (header forwarding marker : BitVec 64)
    (separate : oldHeader + count + 1 ≤ newHeader ∨ newHeader + count + 1 ≤ oldHeader) :
    endMemory memory oldHeader newHeader count header forwarding marker (newHeader + count + 1) = some marker ∧
    ∀ index, index < count →
      endMemory memory oldHeader newHeader count header forwarding marker (newHeader + 1 + index) =
        memory (oldHeader + 1 + index) := by
  constructor
  · simp [endMemory, CollectorCopy.write]
  · intro index inside
    have different : newHeader + 1 + index ≠ newHeader + count + 1 := by omega
    simp only [endMemory, CollectorCopy.write, ite_eq_right different]
    exact CollectorCopy.moved_payload memory oldHeader newHeader count header forwarding separate index inside

/-- end_heap_chunk tests emptiness after incrementing past its marker. Even an
empty payload has a marker; currently scanned chunks must not also be queued. -/
def enqueue (start cursor scan : Nat) : Bool :=
  decide (cursor + 1 ≠ start) && !(decide (start ≤ scan) && decide (scan < cursor + 1))

theorem enqueue_exact (start cursor scan : Nat) (bounds : start ≤ cursor) :
    enqueue start cursor scan = true ↔ scan < start ∨ cursor + 1 ≤ scan := by
  simp [enqueue]
  omega

/-- Explicit projection of both writer barriers and the reader's operations.
The reader fence occurs after dereferencing the queue link. Its ordering
contract cannot be used retroactively for that link load. -/
inductive Event where
  | copyBody | endMarker | linkWrite | writerFence | tailWrite | unlock
  | tailRead | linkRead | headWrite | readerFence | scanBody
  deriving DecidableEq, Repr

def writer : List Event := [.copyBody, .endMarker, .linkWrite, .writerFence,
  .tailWrite, .writerFence, .unlock]
def reader : List Event := [.tailRead, .linkRead, .headWrite, .readerFence, .unlock, .scanBody]

theorem reader_link_precedes_fence : reader[1]? = some .linkRead ∧
    reader[3]? = some .readerFence ∧ reader[5]? = some .scanBody := by decide

/-- Epochs denote observations, not pointer addresses. Capturing a link value
is separate from subsequently improving the observer's view. Location 0 is the
queue link; location 1 is the copied payload. -/
structure Reader where
  observedTail : Nat
  view : Acquire.View
  sampledLink : Option Nat

def readLink (state : Reader) : Reader := {state with sampledLink := some (state.view 0)}

/-- A deliberately strong fence admission imports the entire published view.
Even granting this stronger effect cannot repair an already decoded link.
A native compiler/ISA fence law is not assumed by this definition. -/
def importAfter (published : Acquire.View) (state : Reader) : Reader :=
  {state with view := Acquire.join state.view published}

def GoodLink (state : Reader) : Prop := state.sampledLink = some state.observedTail

theorem late_import_retains_link (published : Acquire.View) (state : Reader) :
    (importAfter published (readLink state)).sampledLink = some (state.view 0) := rfl

theorem link_requires_prior_visibility (published : Acquire.View) (state : Reader) :
    GoodLink (importAfter published (readLink state)) ↔ state.view 0 = state.observedTail := by
  simp [GoodLink, importAfter, readLink]

theorem ordinary_reader_gap (epoch : Nat) (positive : 0 < epoch) :
    let published : Acquire.View := fun _ => epoch
    let before : Reader := ⟨epoch, fun _ => 0, none⟩
    let after := importAfter published (readLink before)
    after.observedTail = epoch ∧ after.view 0 = epoch ∧ after.view 1 = epoch ∧ ¬ GoodLink after := by
  simp [importAfter, readLink, Acquire.join, GoodLink]
  omega

/-- Successful admitted ownership histories provide the missing import before
link decoding. This theorem consumes the host acquire protocol, not a Boolean
backend name or the fact that an uncontended test happened to pass. -/
theorem acquire_before_link {state : Acquire.State} (history : Acquire.Trace Acquire.initial state)
    (worker epoch : Nat) (owns : state.owner = some worker) (published : epoch ≤ state.current 0)
    (bounded : state.views worker 0 ≤ epoch) :
    GoodLink (readLink ⟨epoch, state.views worker, none⟩) := by
  have sees := Acquire.complete_handoff history owns published
  simp only [GoodLink, readLink, Option.some.injEq]
  omega

/-- Prior snapshot readiness remains an explicit premise at the C boundary.
The old scalar publication model may be admitted only after the actual reader
has imported the copied data, marker and link before their respective reads. -/
def AdmitsSnapshot (chunk : Nat) (beforeLink beforeBody : Acquire.View) : Prop :=
  chunk ≤ beforeLink 0 ∧ chunk ≤ beforeBody 1 ∧ chunk ≤ beforeBody 2

theorem snapshot_needs_link (chunk : Nat) (beforeLink beforeBody : Acquire.View)
    (admitted : AdmitsSnapshot chunk beforeLink beforeBody) : chunk ≤ beforeLink 0 := admitted.1
end ChunkPublication

namespace ChunkOrder
/-- Four-role message-passing slice of the paper's external ordered-before
relation. publish is writer bob, observed is external rf, and stale is external
fr (the data read chose the initialization preceding the published write).
Other ARM relations and C data-race admission are not erased by this slice. -/
inductive Event where
  | writeData | writeTail | readTail | readData
  deriving DecidableEq, Repr

inductive Edge (readerOrdered : Bool) : Event → Event → Prop where
  | publish : Edge readerOrdered .writeData .writeTail
  | observed : Edge readerOrdered .writeTail .readTail
  | stale : Edge readerOrdered .readData .writeData
  | reader (ordered : readerOrdered = true) : Edge readerOrdered .readTail .readData

inductive Path (readerOrdered : Bool) : Event → Event → Prop where
  | one {a b} : Edge readerOrdered a b → Path readerOrdered a b
  | next {a b c} : Path readerOrdered a b → Edge readerOrdered b c → Path readerOrdered a c

def rank : Event → Nat
  | .readData => 0
  | .writeData => 1
  | .writeTail => 2
  | .readTail => 3

theorem plain_edge_increases {a b : Event} (edge : Edge false a b) : rank a < rank b := by
  cases edge <;> simp [rank] at *

theorem plain_path_increases {a b : Event} (path : Path false a b) : rank a < rank b := by
  induction path with
  | one edge => exact plain_edge_increases edge
  | next _ edge ih => exact Nat.lt_trans ih (plain_edge_increases edge)

theorem unordered_slice_acyclic (event : Event) : ¬ Path false event event := by
  intro path
  have impossible := plain_path_increases path
  omega

theorem ordered_stale_cycle : Path true .writeData .writeData :=
  .next (.next (.next (.one .publish) .observed) (.reader rfl)) .stale

/-- Acq/barrier ordering before the selected read forbids this stale candidate
under external ob acyclicity. No such ordering is inferred from a plain poll. -/
theorem ordered_slice_rejects_stale
    (externalAcyclic : ∀ event, ¬ Path true event event) : False :=
  externalAcyclic .writeData ordered_stale_cycle

/-- Source reader: tail read 0, link read 1, head write 2, barrier 3,
unlock 4, body scan 5. The full barrier orders tail-to-body, not tail-to-link.
The allocation RMW precedes the contended owner's publication. -/
def readAfterFence (index : Nat) : Bool := decide (3 < index)

theorem native_reader_boundaries :
    readAfterFence 1 = false ∧ readAfterFence 5 = true := by decide

theorem source_link_slice_open (event : Event) : ¬ Path (readAfterFence 1) event event :=
  unordered_slice_acyclic event

theorem source_body_slice_rejects_stale
    (externalAcyclic : ∀ event, ¬ Path (readAfterFence 5) event event) : False :=
  ordered_slice_rejects_stale externalAcyclic
end ChunkOrder

namespace TicketMachine
abbrev Word := BitVec 64
abbrev Registers := Nat → Word

def set (registers : Registers) (index : Nat) (value : Word) : Registers :=
  fun other => if other = index then value else registers other

inductive Event where
  | allocate (address old delta : Word)
  | read (address value : Word) (acquire : Bool)
  | write (address value : Word)
  | fence
  deriving DecidableEq, Repr

/-- Observation oracle, not a sequentially consistent memory axiom. Coherence,
read-from and barriers are checked separately by the full ARM cat model. -/
abbrev Oracle := Nat → Word → Word

structure State where
  registers : Registers
  pc : Nat
  zero : Bool
  reads : Nat
  events : List Event

/-- Instruction effects of the actual GCC lock/payload prefix. Numeric cases
are instruction positions, not schema versions. Unsupported PCs stutter only
outside the qualified prefix; no general decoder admission follows. -/
def step (_base : Word) (ticket : Word) (oracle : Oracle) (acquire : Bool) (state : State) : State :=
  let regs := state.registers
  match state.pc with
  | 0 => {state with registers := set regs 1 (regs 0 + 8), pc := 1}
  | 1 => {state with registers := set regs 2 8, pc := 2}
  | 2 =>
    {state with
      registers := set regs 2 ticket
      pc := 3
      events := state.events ++ [.allocate (regs 1) ticket (regs 2)]}
  | 3 =>
    let value := oracle state.reads (regs 0)
    {state with
      registers := set regs 1 value
      pc := 4
      reads := state.reads + 1
      events := state.events ++ [.read (regs 0) value acquire]}
  | 4 => {state with zero := regs 1 == regs 2, pc := 5}
  | 5 => {state with pc := if state.zero then 6 else 3}
  | 6 =>
    let address := regs 0 + 16
    let value := oracle state.reads address
    {state with
      registers := set regs 1 value
      pc := 7
      reads := state.reads + 1
      events := state.events ++ [.read address value false]}
  | _ => state

def run (base ticket : Word) (oracle : Oracle) (acquire : Bool) : Nat → State → State
  | 0, state => state
  | n + 1, state => run base ticket oracle acquire n (step base ticket oracle acquire state)

theorem run_add (base ticket : Word) (oracle : Oracle) (acquire : Bool)
    (first rest : Nat) (state : State) :
    run base ticket oracle acquire (first + rest) state =
      run base ticket oracle acquire rest (run base ticket oracle acquire first state) := by
  induction first generalizing state with
  | zero => simp only [Nat.zero_add, run]
  | succ n ih => simp only [Nat.succ_add, run, ih]

def initial (base : Word) : State := ⟨fun _ => base, 0, false, 0, []⟩

def waiting (base ticket : Word) (reads : Nat) (events : List Event) : State :=
  ⟨set (set (fun _ => base) 1 (base + 8)) 2 ticket, 3, false, reads, events⟩

theorem prefix_exact (base ticket : Word) (oracle : Oracle) (acquire : Bool) :
    run base ticket oracle acquire 3 (initial base) =
      waiting base ticket 0 [.allocate (base + 8) ticket 8] := by
  simp [run, step, initial, waiting, set]
  funext index
  simp only [set]
  split <;> simp_all

/-- Arbitrary incoming scratch registers and history: only x0 and the
entry PC are prerequisites of this compiler prefix. -/
theorem prefix_registers (base ticket : Word) (oracle : Oracle) (acquire : Bool)
    (state : State) (pc : state.pc = 0) (address : state.registers 0 = base) :
    let after := run base ticket oracle acquire 3 state
    after.pc = 3 ∧ after.registers 0 = base ∧ after.registers 2 = ticket ∧
      after.events = state.events ++ [.allocate (base + 8) ticket 8] := by
  simp [run, step, pc, address, set]

/-- Three actual instructions execute one source poll/C equality/backedge.
Register x1 is scratch; x0 and the allocated ticket x2 are invariant. -/
theorem failed_poll (base ticket : Word) (oracle : Oracle) (acquire : Bool)
    (reads : Nat) (events : List Event) (different : oracle reads base ≠ ticket) :
    let after := run base ticket oracle acquire 3 (waiting base ticket reads events)
    after.pc = 3 ∧ after.reads = reads + 1 ∧ after.registers 0 = base ∧
      after.registers 2 = ticket ∧
      after.events = events ++ [.read base (oracle reads base) acquire] := by
  simp [run, step, waiting, set, different]

/-- C volatile-loop projection. Every failed observation is recorded; a
successful observation is not erased from the history. No fairness is assumed. -/
def sourcePolls (base : Word) (oracle : Oracle) (acquire : Bool) (first count : Nat) : List Event :=
  match count with
  | 0 => []
  | n + 1 => .read base (oracle first base) acquire :: sourcePolls base oracle acquire (first + 1) n

def pollRun (base ticket : Word) (oracle : Oracle) (acquire : Bool) : Nat → State → State
  | 0, state => state
  | n + 1, state => pollRun base ticket oracle acquire n (run base ticket oracle acquire 3 state)

/-- Poll blocks are exactly the same small-step execution, not a second
interpreter assumed equivalent to the actual branch loop. -/
theorem pollRun_eq_run (base ticket : Word) (oracle : Oracle) (acquire : Bool)
    (count : Nat) (state : State) :
    pollRun base ticket oracle acquire count state =
      run base ticket oracle acquire (3 * count) state := by
  induction count generalizing state with
  | zero => rfl
  | succ n ih =>
    rw [pollRun, ih, ← run_add]
    congr 1
    omega

/-- Permit arbitrary scratch-register values at loop entry. A previous failed
load overwrites x1; subsequent loop proofs must not assume it was reset. -/
def LoopEntry (base ticket : Word) (state : State) : Prop :=
  state.pc = 3 ∧ state.registers 0 = base ∧ state.registers 2 = ticket

theorem poll_block (base ticket : Word) (oracle : Oracle) (acquire : Bool) (state : State)
    (entry : LoopEntry base ticket state) :
    let after := run base ticket oracle acquire 3 state
    after.pc = (if oracle state.reads base = ticket then 6 else 3) ∧
    after.reads = state.reads + 1 ∧ after.registers 0 = base ∧ after.registers 2 = ticket ∧
    after.events = state.events ++ [.read base (oracle state.reads base) acquire] := by
  rcases entry with ⟨pc, baseReg, ticketReg⟩
  simp [run, step, pc, baseReg, ticketReg, set]

/-- Arbitrary finite failed-poll prefixes, not only the single successful poll
used in the external litmus. The full source loop and actual instruction loop
produce the same current-word read history. -/
theorem failed_prefix (base ticket : Word) (oracle : Oracle) (acquire : Bool)
    (count : Nat) (state : State) (entry : LoopEntry base ticket state)
    (failed : ∀ index, index < count → oracle (state.reads + index) base ≠ ticket) :
    let after := pollRun base ticket oracle acquire count state
    LoopEntry base ticket after ∧ after.reads = state.reads + count ∧
    after.events = state.events ++ sourcePolls base oracle acquire state.reads count := by
  induction count generalizing state with
  | zero => simp [pollRun, sourcePolls, entry]
  | succ n ih =>
    have different : oracle state.reads base ≠ ticket := by simpa using failed 0 (by omega)
    obtain ⟨pc, reads, baseReg, ticketReg, events⟩ := poll_block base ticket oracle acquire state entry
    have nextEntry : LoopEntry base ticket (run base ticket oracle acquire 3 state) := by
      simp only [LoopEntry, pc, ite_eq_right different, baseReg, ticketReg, and_self]
    have tailFailed : ∀ index, index < n →
        oracle ((run base ticket oracle acquire 3 state).reads + index) base ≠ ticket := by
      intro index inside
      rw [reads]
      simpa [Nat.add_assoc, Nat.add_comm 1 index] using failed (index + 1) (by omega)
    obtain ⟨loop, totalReads, fullTrace⟩ := ih (run base ticket oracle acquire 3 state) nextEntry tailFailed
    refine ⟨loop, ?_, ?_⟩
    · simpa only [pollRun] using (show (pollRun base ticket oracle acquire n (run base ticket oracle acquire 3 state)).reads = state.reads + (n + 1) by omega)
    · simpa [pollRun, sourcePolls, events, reads, List.append_assoc] using fullTrace

/-- Successful source poll followed by the actual payload load. The observation
oracle may return an old payload: successful ticket equality does not justify
silently replacing that read with the publisher's latest value. -/
theorem successful_read (base ticket : Word) (oracle : Oracle) (acquire : Bool)
    (state : State) (entry : LoopEntry base ticket state)
    (matched : oracle state.reads base = ticket) :
    let after := run base ticket oracle acquire 4 state
    after.pc = 7 ∧ after.registers 1 = oracle (state.reads + 1) (base + 16) ∧
    after.events = state.events ++ [.read base ticket acquire,
      .read (base + 16) (oracle (state.reads + 1) (base + 16)) false] := by
  rcases entry with ⟨pc, baseReg, ticketReg⟩
  simp [run, step, pc, baseReg, ticketReg, matched, set, List.append_assoc]
/-- GCC's queue suffix starts at byte offset 24: LDP head/tail, CMP,
B.EQ, LDR link, SUB forwarding tag, STR head, DMB. The paired reads
are kept separate in the trace; the cat checker supplies their ISA relations. -/
structure QueueState where
  head : Word
  tail : Word
  raw : Word
  decoded : Word
  events : List Event
  deriving DecidableEq, Repr

def queueSuffix (base : Word) (oracle : Oracle) (first : Nat) : QueueState :=
  let head := oracle first (base + 24)
  let tail := oracle (first + 1) (base + 32)
  let paired := [.read (base + 24) head false, .read (base + 32) tail false]
  if head = tail then ⟨head, tail, 0, 0, paired⟩ else
    let raw := oracle (first + 2) head
    let decoded := raw - 3
    ⟨head, tail, raw, decoded,
      paired ++ [.read head raw false, .write (base + 24) decoded, .fence]⟩

/-- Source UNTAG_AS has the same unsigned modular SUB effect for the
selected FORW encoding. No well-formed pointer or imported visibility is
inferred from this arithmetic equality. -/
theorem forwarding_sub (pointer : Word) : (pointer + 3) - 3 = pointer := by
  exact BitVec.add_sub_cancel pointer 3

/-- The barrier follows the raw-link read and head update. It cannot be
silently inserted before that read in a C-to-ISA correspondence argument. -/
theorem queue_link_before_fence (base : Word) (oracle : Oracle) (first : Nat)
    (nonempty : oracle first (base + 24) ≠ oracle (first + 1) (base + 32)) :
    let head := oracle first (base + 24)
    let raw := oracle (first + 2) head
    let after := queueSuffix base oracle first
    after.raw = raw ∧ after.decoded = raw - 3 ∧
    after.events = [.read (base + 24) head false,
      .read (base + 32) (oracle (first + 1) (base + 32)) false,
      .read head raw false, .write (base + 24) (raw - 3), .fence] := by
  unfold queueSuffix
  dsimp only
  rw [ite_eq_right nonempty]
  exact ⟨rfl, rfl, rfl⟩

/-- Equality branches out without decoding an unobserved link. -/
theorem empty_queue_no_link (base : Word) (oracle : Oracle) (first : Nat)
    (empty : oracle first (base + 24) = oracle (first + 1) (base + 32)) :
    (queueSuffix base oracle first).events.length = 2 := by
  unfold queueSuffix
  dsimp only
  rw [ite_eq_left empty]
  rfl

end TicketMachine

namespace Contended
/-- Separate payload-read satisfaction from the later ticket observation. This
projection admits speculative stale payloads; it is not the full ARM model. -/
structure Observation where
  ticket : Nat
  published : Nat
  payload : Nat
  deriving DecidableEq, Repr

def PollSatisfied (expected : Nat) (observation : Observation) : Prop := observation.ticket = expected

def RefinesAcquire (observation : Observation) : Prop := observation.published ≤ observation.payload

/-- For every nonzero publication, a matching ticket alone permits a stale read.
A universally positive acquire theorem needs an additional ordering premise. -/
theorem no_plain_poll_refinement (ticket epoch : Nat) (changed : 0 < epoch) :
    ∃ observation : Observation, PollSatisfied ticket observation ∧ ¬ RefinesAcquire observation := by
  refine ⟨⟨ticket, epoch, 0⟩, rfl, ?_⟩
  simp [RefinesAcquire]; omega

/-- A program may satisfy an independent payload read speculatively while
waiting, publish the previous owner's writes, then observe the matching ticket
and consume that read. Logical poll-before-consume is preserved. -/
inductive Phase where | waiting | held | consumed
  deriving DecidableEq, Repr
structure State where
  phase : Phase
  currentTicket : Nat
  published : Nat
  sampled : Option Nat
  result : Option Nat
  deriving DecidableEq, Repr
inductive Step (expected : Nat) : State → State → Prop where
  | satisfy (s value) (waits : s.phase = .waiting) (past : value ≤ s.published) :
      Step expected s {s with sampled := some value}
  | unlock (s epoch) (waits : s.phase = .waiting) :
      Step expected s {s with currentTicket := expected, published := epoch}
  | poll (s) (waits : s.phase = .waiting) (equal : s.currentTicket = expected) :
      Step expected s {s with phase := .held}
  | consume (s value) (held : s.phase = .held) (sample : s.sampled = some value) :
      Step expected s {s with phase := .consumed, result := some value}
inductive Trace (expected : Nat) : State → State → Prop where
  | refl (s) : Trace expected s s
  | next {a b c} : Trace expected a b → Step expected b c → Trace expected a c

def initial (ticket : Nat) : State := ⟨.waiting, ticket - 1, 0, none, none⟩
def staleResult (ticket epoch : Nat) : State := ⟨.consumed, ticket, epoch, some 0, some 0⟩

theorem ordinary_poll_stale_history (ticket epoch : Nat) (_contended : 0 < ticket) :
    Trace ticket (initial ticket) (staleResult ticket epoch) := by
  have first := Trace.next (Trace.refl (expected := ticket) (initial ticket)) (Step.satisfy (initial ticket) 0 rfl (Nat.le_refl _))
  have second := Trace.next first (Step.unlock _ epoch rfl)
  have third := Trace.next second (Step.poll _ rfl rfl)
  exact Trace.next third (Step.consume _ 0 rfl rfl)

theorem ordinary_poll_not_universal (ticket epoch : Nat) (contended : 0 < ticket) (changed : 0 < epoch) :
    ∃ final : State, Trace ticket (initial ticket) final ∧ final.currentTicket = ticket ∧
      final.phase = .consumed ∧ final.result = some 0 ∧ ¬ final.published ≤ 0 := by
  exact ⟨staleResult ticket epoch, ordinary_poll_stale_history ticket epoch contended, rfl, rfl, rfl, by simp [staleResult]; omega⟩

/-- The allocation RMW can import the release visible *at allocation time*.
This is deliberately stronger than giving that RMW no synchronization effect;
it still cannot import a later owner's release through a plain polling load. -/
def allocationFence (state : Acquire.State) (worker : Nat) : Acquire.State :=
  {state with views := fun other => if other = worker then
    Acquire.join (state.views other) state.released else state.views other}

def beforePublication (epoch : Nat) : Acquire.State :=
  ⟨fun _ => epoch, fun _ => epoch, fun _ _ => epoch, some 0⟩

def afterOrdinaryHandoff (epoch location : Nat) : Acquire.State :=
  Acquire.plainEnter (Acquire.release
    (Acquire.write (allocationFence (beforePublication epoch) 1) 0 location) 0) 1

/-- Parametric counterexample with the source order allocation-fence, previous
owner write, release, matching poll, payload read. Ticket uniqueness and a
full-barrier allocation RMW are insufficient for the later handoff. -/
theorem early_fence_does_not_import_later_release (epoch location : Nat) :
    (afterOrdinaryHandoff epoch location).current location = epoch + 1 ∧
    (afterOrdinaryHandoff epoch location).released location = epoch + 1 ∧
    (afterOrdinaryHandoff epoch location).views 1 location = epoch ∧
    ¬ Acquire.Safe (afterOrdinaryHandoff epoch location) := by
  have current : (afterOrdinaryHandoff epoch location).current location = epoch + 1 := by
    simp [afterOrdinaryHandoff, Acquire.plainEnter, Acquire.release, Acquire.write,
      allocationFence, beforePublication, Acquire.put]
  have released : (afterOrdinaryHandoff epoch location).released location = epoch + 1 := by
    simp [afterOrdinaryHandoff, Acquire.plainEnter, Acquire.release, Acquire.write,
      allocationFence, beforePublication, Acquire.put, Acquire.join]
  have observed : (afterOrdinaryHandoff epoch location).views 1 location = epoch := by
    simp [afterOrdinaryHandoff, Acquire.plainEnter, Acquire.release, Acquire.write,
      allocationFence, beforePublication, Acquire.join]
  refine ⟨current, released, observed, ?_⟩
  intro safe
  have sees := safe.2 1 rfl location
  rw [current, observed] at sees
  omega

/-- Exact outstanding admission obligation for an ordinary poll: ownership
alone is safe precisely when every protected location is already visible to
this worker. Matching a ticket does not establish this quantified condition. -/
theorem ordinary_handoff_safe_iff (state : Acquire.State) (worker : Nat) :
    Acquire.Safe (Acquire.plainEnter state worker) ↔
      ∀ location, state.current location ≤ state.views worker location := by
  simp [Acquire.Safe, Acquire.plainEnter]
end Contended

end ParserEngine.Host
