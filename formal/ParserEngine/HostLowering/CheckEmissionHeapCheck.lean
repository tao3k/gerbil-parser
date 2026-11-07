import ParserEngine.HostLowering.CacheExecutionReservation
import ParserEngine.Host

/-! Concrete operand tags and heap-flonum writes at the reported Gambit boundary.
HostLowering composes the host producer models with C storage; it does not assert
that an arbitrary compiler or weak-memory backend refines these transitions. -/
namespace ParserEngine.Host
open ParserEngine.Representation

namespace CheckEmission
/-- Scheme #f is distinct from numeric zero and from #t. The target's FLO
suffix uses eqv?, not truthiness or equality after a numeric coercion. -/
inductive Datum where | absent | flag | count (value : Nat)
  deriving DecidableEq, Repr
structure Request where
  heap : Option Nat
  floating : Datum
  ssb : Option Nat
  poll : Bool
  combine : Bool
  stack : Nat
  deriving DecidableEq, Repr

def active (request : Request) : Bool := request.heap.isSome || request.ssb.isSome || request.poll

def floatingMatch (request : Request) : Bool :=
  match request.heap, request.floating with
  | some heap, .count floating => heap == floating
  | _, _ => false

def opcode (request : Request) : String :=
  (if request.combine then "CHECK" else if request.poll then "POLL" else "CHECK") ++
  (if request.heap.isSome then "_HEAP" else "") ++
  (if floatingMatch request then "_FLO" else "") ++
  (if request.ssb.isSome then "_SSB" else "") ++
  (if request.poll && request.combine then "_POLL" else "")

/-- ADJFP #f is still an entry in the target's code buffer. Return metadata
construction is represented by its own event, rather than assumed absent. -/
inductive Instruction where
  | box (location stamp : Nat)
  | label (identity : Nat)
  | taskPush (slots : Nat)
  | adjust (delta : Option Int)
  | check (name : String) (label : Nat) (heap ssb : Option Nat)
  | internalReturn (label stack : Nat)
  deriving DecidableEq, Repr
structure State where
  code : List Instruction
  nextLabel : Nat
  framePointer : Nat
  heapReserved : Nat
  floatingReserved : Nat
  ssbReserved : Nat
  readsHeap : Bool
  writesHeap : Bool
  readsFrame : Bool
  writesFrame : Bool
  deriving DecidableEq, Repr

def block (fudge : Nat) (request : Request) (state : State) : List Instruction :=
  [.adjust (if state.framePointer = request.stack then none else
      some ((request.stack : Int) - (state.framePointer : Int))),
   .check (opcode request) state.nextLabel (request.heap.map (fudge + ·)) request.ssb,
   .internalReturn state.nextLabel request.stack]

def emit (fudge : Nat) (request : Request) (state : State) : State :=
  if active request then
    { state with
      code := state.code ++ block fudge request state
      nextLabel := state.nextLabel + 1
      framePointer := request.stack
      heapReserved := if request.heap.isSome then 0 else state.heapReserved
      floatingReserved := if request.heap.isSome then 0 else state.floatingReserved
      ssbReserved := if request.ssb.isSome then 0 else state.ssbReserved
      readsHeap := state.readsHeap || request.heap.isSome || request.ssb.isSome
      writesHeap := state.writesHeap || request.heap.isSome || request.ssb.isSome
      readsFrame := state.readsFrame || request.poll || decide (state.framePointer ≠ request.stack)
      writesFrame := state.writesFrame || decide (state.framePointer ≠ request.stack) }
  else state

theorem disabled_identity (fudge : Nat) (request : Request) (state : State)
    (disabled : active request = false) : emit fudge request state = state := by
  simp [emit, disabled]

theorem complete_block (fudge : Nat) (request : Request) (state : State)
    (enabled : active request = true) :
    (emit fudge request state).code = state.code ++ block fudge request state ∧
    (emit fudge request state).nextLabel = state.nextLabel + 1 ∧
    (emit fudge request state).framePointer = request.stack := by simp [emit, enabled]

theorem zero_heap_is_active (floating : Datum) (ssb : Option Nat) (poll combine : Bool) (stack : Nat) :
    active ⟨some 0, floating, ssb, poll, combine, stack⟩ = true := by simp [active]

theorem boolean_floating_is_not_numeric (request : Request) :
    floatingMatch {request with floating := .flag} = false := by
  cases request.heap <;> simp [floatingMatch]

/-- Each enabled block preserves all unrelated budgets. Heap and float budgets
reset together; an SSB-only or poll-only block cannot reset them. -/
theorem reset_exact (fudge : Nat) (request : Request) (state : State)
    (enabled : active request = true) :
    (emit fudge request state).heapReserved = (if request.heap.isSome then 0 else state.heapReserved) ∧
    (emit fudge request state).floatingReserved = (if request.heap.isSome then 0 else state.floatingReserved) ∧
    (emit fudge request state).ssbReserved = (if request.ssb.isSome then 0 else state.ssbReserved) := by
  simp [emit, enabled]

/-- Arbitrarily many target checks preserve the exact concatenated instruction
sequence; every active request consumes exactly one label. -/
def emitMany (fudge : Nat) : List Request → State → State
  | [], state => state
  | request :: rest, state => emitMany fudge rest (emit fudge request state)

def activeCount (requests : List Request) : Nat :=
  requests.foldr (fun request total => (if active request then 1 else 0) + total) 0

theorem labels_exact (fudge : Nat) (requests : List Request) (state : State) :
    (emitMany fudge requests state).nextLabel = state.nextLabel + activeCount requests := by
  induction requests generalizing state with
  | nil => simp [emitMany, activeCount]
  | cons request rest ih =>
    rw [emitMany, ih]
    cases h : active request <;> simp [emit, h, activeCount, Nat.add_assoc, Nat.add_comm]

theorem code_length_exact (fudge : Nat) (requests : List Request) (state : State) :
    (emitMany fudge requests state).code.length = state.code.length + 3 * activeCount requests := by
  induction requests generalizing state with
  | nil => simp [emitMany, activeCount]
  | cons request rest ih =>
    rw [emitMany, ih]
    cases h : active request <;> simp [emit, h, block, activeCount, Nat.mul_add,
      Nat.add_assoc, Nat.add_comm]
/-- Extract the actual early-return cache events, in live-frame traversal order.
Repeated and absent locations cannot synthesize additional writeback events. -/
def materializeLive : List Nat → List (Option Cache.Entry) → List (Option Cache.Entry) × List Cache.Entry
  | [], rows => (rows, [])
  | location :: rest, rows =>
    let (updated, event) := Cache.flush location rows
    let (finalRows, events) := materializeLive rest updated
    (finalRows, event.toList ++ events)

theorem materialize_corresponds (locations : List Nat) (rows : List (Option Cache.Entry)) :
    (materializeLive locations rows).1 = (Reservation.flushMany locations rows).1 ∧
    (materializeLive locations rows).2.length = (Reservation.flushMany locations rows).2 := by
  induction locations generalizing rows with
  | nil => simp [materializeLive, Reservation.flushMany]
  | cons location rest ih =>
    have following := ih (Cache.flush location rows).1
    simp only [materializeLive, Reservation.flushMany]
    constructor
    · exact following.1
    · cases event : (Cache.flush location rows).2 <;>
        simp [Reservation.eventCount, following.2, Nat.add_comm]

/-- All writebacks precede the generated check. The source uses a boolean for
flo-space here, so it cannot equal a numeric heap argument under eqv?. -/
def flushAndCheck (space stack fudge charge : Nat) (locations : List Nat)
    (rows : List (Option Cache.Entry)) (state : State) : State :=
  let events := (materializeLive locations rows).2
  let flushed : State := {state with
    code := state.code ++ events.map (fun entry => .box entry.location entry.stamp)
    heapReserved := state.heapReserved + charge * events.length
    floatingReserved := state.floatingReserved + charge * events.length}
  let request : Request := ⟨some space,
    if 0 < flushed.floatingReserved ∧ flushed.floatingReserved = flushed.heapReserved
    then .flag else .absent, none, false, false, stack⟩
  emit fudge request flushed

theorem flush_and_check_complete (space stack fudge charge : Nat) (locations : List Nat)
    (rows : List (Option Cache.Entry)) (state : State) :
    let events := (materializeLive locations rows).2
    let after := flushAndCheck space stack fudge charge locations rows state
    after.code = state.code ++ events.map (fun entry => .box entry.location entry.stamp) ++
      [.adjust (if state.framePointer = stack then none else some ((stack : Int) - (state.framePointer : Int))),
       .check "CHECK_HEAP" state.nextLabel (some (fudge + space)) none,
       .internalReturn state.nextLabel stack] ∧
    after.heapReserved = 0 ∧ after.floatingReserved = 0 ∧
    after.ssbReserved = state.ssbReserved ∧ after.nextLabel = state.nextLabel + 1 := by
  dsimp [flushAndCheck]
  split <;> simp [emit, active, block, opcode, floatingMatch]

end CheckEmission

namespace HeapCheck
/-- Save/restore here denotes the C W_ALL/R_ALL pair, not a repeated GVM
PUSH_REGS frame. One kernel check restores even when collection reports overflow.
Only a returning exception handler admits the next attempt. These events model
control order, not the implementation of collection or exception handling. -/
inductive Event where
  | save | limit | collect | restore | raiseOverflow | retry | resume
  deriving DecidableEq, Repr
structure Attempt where
  hp : Nat
  threshold : Nat
  limitRequestsGC : Bool
  collectionOverflows : Bool
  deriving DecidableEq, Repr

def attemptEvents (attempt : Attempt) : List Event :=
  if attempt.hp > attempt.threshold then
    [.save, .limit] ++ (if attempt.limitRequestsGC then [.collect] else []) ++ [.restore] ++
      (if attempt.limitRequestsGC && attempt.collectionOverflows then [.raiseOverflow] else [.resume])
  else [.resume]

def overflows (attempt : Attempt) : Bool :=
  decide (attempt.hp > attempt.threshold) && attempt.limitRequestsGC && attempt.collectionOverflows

/-- Finite successful invocations may have arbitrarily many overflow attempts.
A raising or diverging handler does not manufacture a successful completion. -/
inductive Completes : List Attempt → Prop where
  | done (attempt) (ok : overflows attempt = false) : Completes [attempt]
  | retry (attempt) (overflow : overflows attempt = true) {rest} (returns : Completes rest) :
      Completes (attempt :: rest)

def invocationEvents : List Attempt → List Event
  | [] => []
  | attempt :: rest => attemptEvents attempt ++
      (if overflows attempt then [.retry] ++ invocationEvents rest else [])

def eventCount (event : Event) (events : List Event) : Nat := events.count event

theorem attempt_terminal (attempt : Attempt) :
    (overflows attempt = true → attemptEvents attempt =
      [.save, .limit, .collect, .restore, .raiseOverflow]) ∧
    (overflows attempt = false → eventCount .resume (attemptEvents attempt) = 1 ∧
      eventCount .raiseOverflow (attemptEvents attempt) = 0) := by
  rcases attempt with ⟨hp, threshold, requests, overflow⟩
  unfold overflows attemptEvents
  split <;> cases requests <;> cases overflow <;> simp_all [eventCount]

theorem completion_counts {attempts : List Attempt} (completion : Completes attempts) :
    eventCount .resume (invocationEvents attempts) = 1 ∧
    eventCount .raiseOverflow (invocationEvents attempts) + 1 = attempts.length ∧
    eventCount .retry (invocationEvents attempts) + 1 = attempts.length := by
  induction completion with
  | done attempt ok =>
    have terminal := (attempt_terminal attempt).2 ok
    simp [invocationEvents, ok, eventCount] at *
    have noRetry : eventCount .retry (attemptEvents attempt) = 0 := by
      rcases attempt with ⟨hp, threshold, requests, overflow⟩
      unfold attemptEvents
      split <;> cases requests <;> cases overflow <;> simp_all [eventCount]
    exact ⟨terminal.1, terminal.2, by simpa [eventCount] using noRetry⟩
  | retry attempt overflow returns ih =>
    have terminal := (attempt_terminal attempt).1 overflow
    simp [invocationEvents, overflow, terminal, eventCount] at *
    omega

/-- GC cycles compose with changed addresses and roots on every cycle. Atom
payloads keep all 64 bits. The native relocation implementation is an explicit
premise at each cycle, never inferred from a returning heap-limit call. -/
inductive RootedCycles (bits : BitVec 64) : Operational.Heap.Store (BitVec 64) → Nat →
    Operational.Heap.Store (BitVec 64) → Nat → Prop where
  | refl (heap address) : RootedCycles bits heap address heap address
  | next {before middle after original address} (earlier : RootedCycles bits before original middle address)
      (roots : List Nat) (held : address ∈ roots)
      (collection : Operational.Collection.Relocation middle after roots) :
      RootedCycles bits before original after (collection.address address)

theorem rooted_cycles_preserve {bits : BitVec 64} {before after : Operational.Heap.Store (BitVec 64)}
    {original address : Nat} (cycles : RootedCycles bits before original after address)
    (read : before[original]? = some (.atom bits)) : after[address]? = some (.atom bits) := by
  induction cycles with
  | refl => exact read
  | next earlier roots held collection ih =>
    simpa [Operational.Collection.renameCell] using
      collection.copied _ (.atom bits) (Operational.Collection.Reachable.root held) (ih read)
end HeapCheck

namespace ClosureEmission
/-- Typed closure-parms projection. Operands are evaluated during setup, after
all destinations have received their allocated closures. -/
structure Closure where
  destination : Nat
  label : Nat
  operands : List Operand.Value
  stackNeed : Nat
  deriving DecidableEq, Repr
inductive Event where
  | reserve (stack : Nat)
  | allocate (destination arity : Nat)
  | beginSetup (destination label arity : Nat)
  | element (index : Nat) (operand : Operand.Value)
  | endSetup (arity : Nat)
  | finalCheck
  deriving DecidableEq, Repr

def setup (closure : Closure) : List Event :=
  [.beginSetup closure.destination closure.label closure.operands.length] ++
  closure.operands.mapIdx (fun index operand => .element index operand) ++
  [.endSetup closure.operands.length]

def prelude (closures : List Closure) (stack : Nat) : List Event :=
  [.reserve stack] ++ closures.map (fun closure => .allocate closure.destination closure.operands.length)

/-- Direct control-flow transcription of targ-gen-close's recursive worker:
recurse first; allocate at the base; emit setup while unwinding. -/
def nativeWalk (original : List Closure) : List Closure → Nat → List Event
  | [], stack => prelude original stack
  | closure :: rest, stack => nativeWalk original rest (max stack closure.stackNeed) ++ setup closure

def highWater (closures : List Closure) (stack : Nat) : Nat :=
  closures.foldl (fun current closure => max current closure.stackNeed) stack

theorem unwind_exact (original closures : List Closure) (stack : Nat) :
    nativeWalk original closures stack =
      prelude original (highWater closures stack) ++ closures.reverse.flatMap setup := by
  induction closures generalizing stack with
  | nil => simp [nativeWalk, highWater]
  | cons closure rest ih =>
    simp [nativeWalk, ih, highWater, List.flatMap_append, List.append_assoc]

def generate (closures : List Closure) (stack : Nat) : List Event :=
  nativeWalk closures closures.reverse stack ++ [.finalCheck]

/-- The actual double reversal preserves declaration order, and every ALLOC_CLO
precedes every BEGIN_SETUP_CLO, including mutually recursive environments. -/
theorem complete_emission (closures : List Closure) (stack : Nat) :
    generate closures stack = prelude closures (highWater closures.reverse stack) ++
      closures.flatMap setup ++ [.finalCheck] := by
  simp [generate, unwind_exact, List.append_assoc]

theorem setup_length (closure : Closure) : (setup closure).length = closure.operands.length + 2 := by
  simp [setup]

def operandCount (closures : List Closure) : Nat :=
  closures.foldr (fun closure total => closure.operands.length + total) 0

theorem setup_total (closures : List Closure) :
    (closures.flatMap setup).length = operandCount closures + 2 * closures.length := by
  induction closures with
  | nil => simp [operandCount]
  | cons closure rest ih => simp [List.flatMap_cons, setup_length, ih, operandCount]; omega

theorem event_count (closures : List Closure) (stack : Nat) :
    (generate closures stack).length = operandCount closures + 3 * closures.length + 2 := by
  rw [complete_emission]
  simp only [List.length_append, List.length_singleton]
  rw [setup_total]
  simp [prelude]; omega

theorem high_water_bounds (closures : List Closure) (stack : Nat) :
    stack ≤ highWater closures stack ∧ ∀ closure ∈ closures, closure.stackNeed ≤ highWater closures stack := by
  induction closures generalizing stack with
  | nil => simp [highWater]
  | cons closure rest ih =>
    have tailBounds := ih (max stack closure.stackNeed)
    constructor
    · exact Nat.le_trans (Nat.le_max_left _ _) tailBounds.1
    · intro chosen member
      rcases List.mem_cons.mp member with same | inside
      · subst chosen; exact Nat.le_trans (Nat.le_max_right _ _) tailBounds.1
      · exact tailBounds.2 chosen inside
end ClosureEmission

namespace JumpEmission
/-- Noninlined local transfers after frame writeback and end-of-block checks.
An absent ret argument selects this path; procedure/global/inliner paths are
separate obligations, not represented by an arbitrary callback here. -/
inductive Target where | label (identity : Nat) | register (index : Nat)
  deriving DecidableEq, Repr
inductive Transfer where
  | fallthrough (arguments : Option Nat)
  | goTo (label : Nat) (arguments : Option Nat)
  | returnTo (register : Nat)
  | general (register arguments : Nat) (safe : Bool)
  deriving DecidableEq, Repr

def lower (target : Target) (arguments : Option Nat) (safe : Bool) (nextLabel : Option Nat) : Transfer :=
  match target with
  | .label label => if nextLabel = some label then .fallthrough arguments else .goTo label arguments
  | .register register => match arguments with
    | none => .returnTo register
    | some count => .general register count safe

theorem numeric_zero_is_call (register : Nat) (safe : Bool) (nextLabel : Option Nat) :
    lower (.register register) (some 0) safe nextLabel = .general register 0 safe := by rfl

theorem no_arguments_is_return (register : Nat) (safe : Bool) (nextLabel : Option Nat) :
    lower (.register register) none safe nextLabel = .returnTo register := by rfl

theorem local_fallthrough_exact (label : Nat) (arguments : Option Nat) (safe : Bool) (nextLabel : Option Nat) :
    lower (.label label) arguments safe nextLabel = .fallthrough arguments ↔ nextLabel = some label := by
  simp [lower]
end JumpEmission

namespace CollectorCopy
/-- Exact word-addressed projection of the qualified 64-bit, non-handle
mark_array loop. C pointer validity, section selection and source immutability
must be admitted separately. This is not a model of SMP publication. -/
abbrev Memory := Nat → Option (BitVec 64)
def write (memory : Memory) (address : Nat) (value : Option (BitVec 64)) : Memory :=
  fun other => if other = address then value else memory other

def copyWords : Nat → Memory → Nat → Nat → Memory
  | 0, memory, _, _ => memory
  | count + 1, memory, source, target =>
    copyWords count (write memory target (memory source)) (source + 1) (target + 1)

def Disjoint (source target count : Nat) : Prop :=
  source + count ≤ target ∨ target + count ≤ source

theorem outside (count : Nat) (memory : Memory) (source target address : Nat)
    (apart : address < target ∨ target + count ≤ address) :
    copyWords count memory source target address = memory address := by
  induction count generalizing memory source target with
  | zero => rfl
  | succ count ih =>
    simp only [copyWords]
    rw [ih _ _ _ (by omega)]
    simp [write, show address ≠ target by omega]

/-- Derives exact payload copying from individual reads/writes, rather than
assuming Collection.Relocation.copied. Zero length and all 64-bit patterns are
included. Disjointness protects unread source words from destination writes. -/
theorem exact_words (count : Nat) (memory : Memory) (source target : Nat)
    (separate : Disjoint source target count) (index : Nat) (inside : index < count) :
    copyWords count memory source target (target + index) = memory (source + index) := by
  induction count generalizing memory source target index with
  | zero => omega
  | succ count ih =>
    cases index with
    | zero =>
      simp only [copyWords, Nat.add_zero]
      rw [outside _ _ _ _ _ (Or.inl (by omega))]
      simp [write]
    | succ index =>
      have tailSeparate : Disjoint (source + 1) (target + 1) count := by unfold Disjoint at *; omega
      have tailRead := ih (write memory target (memory source)) (source + 1) (target + 1)
        tailSeparate index (by omega)
      have offsetTarget : target + (index + 1) = target + 1 + index := by omega
      have offsetSource : source + 1 + index = source + (index + 1) := by omega
      have sourceApart : source + (index + 1) ≠ target := by unfold Disjoint at separate; omega
      simpa [copyWords, offsetTarget, offsetSource, write, sourceApart] using tailRead

/-- Forwarding is installed in the old header before copying. Header addresses
are kept out of both body ranges. The target header is retained by the loop. -/
def move (memory : Memory) (oldHeader newHeader : Nat) (header forwarding : BitVec 64) (count : Nat) : Memory :=
  copyWords count (write (write memory newHeader (some header)) oldHeader (some forwarding))
    (oldHeader + 1) (newHeader + 1)

theorem moved_payload (memory : Memory) (oldHeader newHeader count : Nat)
    (header forwarding : BitVec 64)
    (separate : oldHeader + count + 1 ≤ newHeader ∨ newHeader + count + 1 ≤ oldHeader)
    (index : Nat) (inside : index < count) :
    move memory oldHeader newHeader header forwarding count (newHeader + 1 + index) =
      memory (oldHeader + 1 + index) := by
  unfold move
  rw [exact_words _ _ _ _ (by unfold Disjoint; omega) index inside]
  simp [write, show oldHeader + 1 + index ≠ oldHeader by omega,
    show oldHeader + 1 + index ≠ newHeader by omega]

theorem moved_headers (memory : Memory) (oldHeader newHeader count : Nat)
    (header forwarding : BitVec 64)
    (separate : oldHeader + count + 1 ≤ newHeader ∨ newHeader + count + 1 ≤ oldHeader) :
    move memory oldHeader newHeader header forwarding count newHeader = some header ∧
    move memory oldHeader newHeader header forwarding count oldHeader = some forwarding := by
  unfold move
  constructor
  · rw [outside _ _ _ _ _ (Or.inl (by omega))]
    simp [write, show newHeader ≠ oldHeader by omega]
  · rw [outside _ _ _ _ _ (by omega)]
    simp [write]
end CollectorCopy

namespace Forwarding
/-- Contiguous-region view of decoded movable-header forwarding entries.
Positive widths include the header and admitted object-specific padding.
HeaderTrace below handles nonmonotone physical section choices separately;
native pointer/tag decoding and section ownership remain explicit imports. -/
structure State where
  table : Nat → Option Nat
  next : Nat

def Bounded (state : State) : Prop := ∀ source target, state.table source = some target → target < state.next
def Unique (state : State) : Prop := ∀ left right target,
  state.table left = some target → state.table right = some target → left = right

def forward (width source : Nat) (state : State) : State × Nat :=
  match state.table source with
  | some target => (state, target)
  | none => (⟨fun address => if address = source then some state.next else state.table address,
      state.next + width⟩, state.next)

theorem already_forwarded (width source target : Nat) (state : State)
    (read : state.table source = some target) : forward width source state = (state, target) := by
  simp [forward, read]

theorem records_target (width source : Nat) (state : State) :
    (forward width source state).1.table source = some (forward width source state).2 := by
  unfold forward; split <;> simp_all

theorem retains (width source : Nat) (state : State) {older target : Nat}
    (read : state.table older = some target) : (forward width source state).1.table older = some target := by
  unfold forward; split
  · exact read
  · next absent =>
      have different : older ≠ source := by intro same; subst older; simp_all
      simp [different, read]

theorem bounded (width source : Nat) (positive : 0 < width) (state : State)
    (valid : Bounded state) : Bounded (forward width source state).1 := by
  unfold forward; split
  · exact valid
  · intro address target read
    dsimp at read ⊢
    split at read
    · cases read; omega
    · have old := valid address target read; omega

theorem unique (width source : Nat) (state : State) (valid : Bounded state) (unique : Unique state) :
    Unique (forward width source state).1 := by
  unfold forward; split
  · exact unique
  · intro left right target leftRead rightRead
    dsimp at leftRead rightRead
    split at leftRead <;> split at rightRead
    · omega
    · cases leftRead; have bound := valid right state.next rightRead; omega
    · cases rightRead; have bound := valid left state.next leftRead; omega
    · exact unique left right target leftRead rightRead

/-- Sequential MOVABLE0/FORW slice of mark_array: an already forwarded alias
reuses its destination; only a fresh header advances allocation. STILL/PERM and
weak-object classification are not supplied by this decoded interface. -/
def scan (width : Nat → Nat) : List Nat → State → State × List Nat
  | [], state => (state, [])
  | source :: rest, state =>
    let (updated, target) := forward (width source) source state
    let (finalState, targets) := scan width rest updated
    (finalState, target :: targets)

theorem scan_retains (width : Nat → Nat) (sources : List Nat) (state : State) {older target : Nat}
    (read : state.table older = some target) : (scan width sources state).1.table older = some target := by
  induction sources generalizing state with
  | nil => exact read
  | cons source rest ih => exact ih _ (retains (width source) source state read)

theorem scan_length (width : Nat → Nat) (sources : List Nat) (state : State) :
    (scan width sources state).2.length = sources.length := by
  induction sources generalizing state with
  | nil => rfl
  | cons source rest ih => simpa [scan] using ih (forward (width source) source state).1

theorem scan_bounded (width : Nat → Nat) (positive : ∀ source, 0 < width source)
    (sources : List Nat) (state : State) (valid : Bounded state) : Bounded (scan width sources state).1 := by
  induction sources generalizing state with
  | nil => exact valid
  | cons source rest ih => exact ih _ (bounded (width source) source (positive source) state valid)

theorem scan_unique (width : Nat → Nat) (positive : ∀ source, 0 < width source)
    (sources : List Nat) (state : State) (valid : Bounded state) (injective : Unique state) :
    Unique (scan width sources state).1 := by
  induction sources generalizing state with
  | nil => exact injective
  | cons source rest ih =>
    exact ih _ (bounded (width source) source (positive source) state valid)
      (unique (width source) source state valid injective)

/-- Every physical reference slot is rewritten to its final forwarding entry.
This covers arbitrary aliases and cycles presented by successive body scans. -/
theorem scanned_slot (width : Nat → Nat) (sources : List Nat) (state : State)
    {index source : Nat} (read : sources[index]? = some source) :
    ∃ target, (scan width sources state).2[index]? = some target ∧
      (scan width sources state).1.table source = some target := by
  induction sources generalizing state index with
  | nil => simp at read
  | cons first rest ih =>
    cases index with
    | zero =>
      have same : first = source := by simpa using read
      subst source
      refine ⟨(forward (width first) first state).2, ?_, ?_⟩
      · simp [scan]
      · exact scan_retains width rest _ (records_target (width first) first state)
    | succ index =>
      obtain ⟨target, slot, forwarded⟩ := ih (forward (width first) first state).1 (by simpa using read)
      exact ⟨target, by simpa [scan] using slot, forwarded⟩

theorem same_alias_destination (width : Nat → Nat) (sources : List Nat) (state : State)
    {left right source : Nat} (first : sources[left]? = some source) (second : sources[right]? = some source) :
    (scan width sources state).2[left]? = (scan width sources state).2[right]? := by
  obtain ⟨a, readA, mapA⟩ := scanned_slot width sources state first
  obtain ⟨b, readB, mapB⟩ := scanned_slot width sources state second
  have same : a = b := by rw [mapA] at mapB; exact Option.some.inj mapB
  simp [readA, readB, same]

/-- Sequential tail marking followed by later chunk scanning composes exactly.
The split is not itself a proof that all reachable objects are scheduled. -/
theorem scan_append (width : Nat → Nat) (first rest : List Nat) (state : State) :
    scan width (first ++ rest) state =
      let earlier := scan width first state
      let later := scan width rest earlier.1
      (later.1, earlier.2 ++ later.2) := by
  induction first generalizing state with
  | nil => simp [scan]
  | cons source first ih => simp [scan, ih]

def tailFields : List Nat → List Nat
  | [] => []
  | first :: _ => [first]
def laterFields : List Nat → List Nat
  | [] => []
  | _ :: rest => rest

theorem tail_and_later_complete (fields : List Nat) : tailFields fields ++ laterFields fields = fields := by
  cases fields <;> rfl

theorem native_split_scan (width : Nat → Nat) (fields : List Nat) (state : State) :
    scan width fields state =
      let earlier := scan width (tailFields fields) state
      let later := scan width (laterFields fields) earlier.1
      (later.1, earlier.2 ++ later.2) := by
  cases fields <;> simp [tailFields, laterFields, scan]

def nativeWidth (bodyWords : Nat) : Nat := bodyWords + 1

theorem native_width_positive (bodyWords : Nat) : 0 < nativeWidth bodyWords := by
  simp [nativeWidth]

/-- Strengthen the start-address bound to the whole reserved object extent.
This does not assert that the chosen native section has that much room. -/
def ExtentBounded (width : Nat → Nat) (state : State) : Prop :=
  ∀ source target, state.table source = some target → target + width source ≤ state.next

theorem extent_bounded (width : Nat → Nat) (source : Nat) (state : State)
    (valid : ExtentBounded width state) : ExtentBounded width (forward (width source) source state).1 := by
  unfold forward; split
  · exact valid
  · intro address target read
    dsimp at read ⊢
    split at read
    · next same => subst address; cases read; omega
    · have old := valid address target read; omega

def initial (cursor : Nat) : State := ⟨fun _ => none, cursor⟩

theorem scan_extent_bounded (width : Nat → Nat) (sources : List Nat) (state : State)
    (valid : ExtentBounded width state) : ExtentBounded width (scan width sources state).1 := by
  induction sources generalizing state with
  | nil => exact valid
  | cons source rest ih => exact ih _ (extent_bounded width source state valid)

/-- Starts from the actual empty-forwarding condition and derives the table's
invariants for arbitrary input histories and header-derived positive widths.
No caller-supplied alias uniqueness or target-copy certificate is consumed. -/
theorem native_scan_from_empty (bodyWords : Nat → Nat) (sources : List Nat) (cursor : Nat) :
    Bounded (scan (fun source => nativeWidth (bodyWords source)) sources (initial cursor)).1 ∧
    Unique (scan (fun source => nativeWidth (bodyWords source)) sources (initial cursor)).1 ∧
    ExtentBounded (fun source => nativeWidth (bodyWords source))
      (scan (fun source => nativeWidth (bodyWords source)) sources (initial cursor)).1 := by
  have boundedInitial : Bounded (initial cursor) := by simp [Bounded, initial]
  have uniqueInitial : Unique (initial cursor) := by simp [Unique, initial]
  have extentInitial : ExtentBounded (fun source => nativeWidth (bodyWords source)) (initial cursor) := by
    simp [ExtentBounded, initial]
  exact ⟨scan_bounded _ (fun source => native_width_positive (bodyWords source)) sources _ boundedInitial,
    scan_unique _ (fun source => native_width_positive (bodyWords source)) sources _ boundedInitial uniqueInitial,
    scan_extent_bounded _ sources _ extentInitial⟩
/-- Movable/immediate slice of mark_array. Every reference here must decode to
a MOVABLE0/FORW object; nonmoving references are not silently cast to immediates.
Immediate bits are preserved without interpreting them as addresses. Complete
native tag/head classification and nonmoving cases remain decoder obligations. -/
def scanWords (width : Nat → Nat) : List Roots.Word → State → State × List Roots.Word
  | [], state => (state, [])
  | .immediate bits :: rest, state =>
    let (after, words) := scanWords width rest state
    (after, .immediate bits :: words)
  | .reference source :: rest, state =>
    let (updated, target) := forward (width source) source state
    let (after, words) := scanWords width rest updated
    (after, .reference target :: words)

theorem mixed_scan_projection (width : Nat → Nat) (words : List Roots.Word) (state : State) :
    (scanWords width words state).1 = (scan width (Roots.references words) state).1 ∧
    Roots.references (scanWords width words state).2 = (scan width (Roots.references words) state).2 := by
  induction words generalizing state with
  | nil => simp [scanWords, scan, Roots.references]
  | cons word rest ih =>
    cases word with
    | immediate bits => simpa [scanWords, Roots.references] using ih state
    | reference source =>
      have following := ih (forward (width source) source state).1
      simpa [scanWords, scan, Roots.references] using following

theorem immediate_slot_retained (width : Nat → Nat) (words : List Roots.Word) (state : State)
    {index bits : Nat} (read : words[index]? = some (.immediate bits)) :
    (scanWords width words state).2[index]? = some (.immediate bits) := by
  induction words generalizing state index with
  | nil => simp at read
  | cons word rest ih =>
    cases index with
    | zero =>
      have same : word = .immediate bits := by simpa using read
      subst word; simp [scanWords]
    | succ index =>
      cases word with
      | immediate value => simpa [scanWords] using ih state (by simpa using read)
      | reference source =>
        simpa [scanWords] using ih (forward (width source) source state).1 (by simpa using read)

/-- A separate header projection admits physical allocation in any fresh
section, including one below previously allocated addresses. The contiguous
cursor bounds above are not imposed on this native placement interface. -/
abbrev Headers := Nat → Option Nat

def HeaderUnique (headers : Headers) : Prop :=
  ∀ left right target, headers left = some target → headers right = some target → left = right

def FreshTarget (headers : Headers) (candidate : Nat) : Prop :=
  ∀ source target, headers source = some target → candidate ≠ target

def forwardAt (headers : Headers) (source candidate : Nat) : Headers × Nat :=
  match headers source with
  | some target => (headers, target)
  | none => ((fun address => if address = source then some candidate else headers address), candidate)

theorem physical_records_target (headers : Headers) (source candidate : Nat) :
    (forwardAt headers source candidate).1 source = some (forwardAt headers source candidate).2 := by
  unfold forwardAt; split <;> simp_all

theorem physical_retains (headers : Headers) (source candidate : Nat) {old target : Nat}
    (read : headers old = some target) : (forwardAt headers source candidate).1 old = some target := by
  unfold forwardAt; split
  · exact read
  · next absent =>
      have different : old ≠ source := by intro same; subst old; simp_all
      simp [different, read]

theorem physical_unique (headers : Headers) (source candidate : Nat) (unique : HeaderUnique headers)
    (fresh : headers source = none → FreshTarget headers candidate) :
    HeaderUnique (forwardAt headers source candidate).1 := by
  unfold forwardAt; split
  · exact unique
  · next absent =>
      intro left right target leftRead rightRead
      dsimp at leftRead rightRead
      split at leftRead <;> split at rightRead
      · omega
      · cases leftRead; exact False.elim (fresh absent right candidate rightRead rfl)
      · cases rightRead; exact False.elim (fresh absent left candidate leftRead rfl)
      · exact unique left right target leftRead rightRead

inductive HeaderTrace : Headers → Headers → Prop where
  | refl (headers) : HeaderTrace headers headers
  | next {before middle} (earlier : HeaderTrace before middle) (source candidate : Nat)
      (fresh : middle source = none → FreshTarget middle candidate) :
      HeaderTrace before (forwardAt middle source candidate).1

theorem header_trace_unique {before after : Headers} (unique : HeaderUnique before)
    (history : HeaderTrace before after) : HeaderUnique after := by
  induction history with
  | refl => exact unique
  | next earlier source candidate fresh ih => exact physical_unique _ source candidate ih fresh

theorem header_trace_retains {before after : Headers} (history : HeaderTrace before after)
    {source target : Nat} (read : before source = some target) : after source = some target := by
  induction history with
  | refl => exact read
  | next earlier current candidate fresh ih => exact physical_retains _ current candidate ih

/-- Native placement cannot assume monotonically increasing physical addresses.
A lower fresh section remains admissible without coalescing existing objects. -/
theorem lower_section_admitted (high low : Nat) (lower : low < high) :
    HeaderUnique (forwardAt (fun source => if source = 0 then some high else none) 1 low).1 ∧
    (forwardAt (fun source => if source = 0 then some high else none) 1 low).1 0 = some high ∧
    (forwardAt (fun source => if source = 0 then some high else none) 1 low).1 1 = some low := by
  constructor
  · apply physical_unique
    · intro left right target a b
      dsimp at a b
      split at a <;> split at b <;> simp_all
    · intro absent source target read
      dsimp at read
      split at read
      · cases read; omega
      · cases read
  · simp [forwardAt]

end Forwarding


end ParserEngine.Host
