import ParserEngine.Host

/-! Concrete operand tags and heap-flonum writes at the reported Gambit boundary.
HostLowering composes the host producer models with C storage; it does not assert
that an arbitrary compiler or weak-memory backend refines these transitions. -/
namespace ParserEngine.Host
open ParserEngine.Representation

namespace Operand
inductive Row where
  | global (name : Nat)
  | closure (base index : Nat)
  | constant (identity : Nat)
  deriving DecidableEq, Repr

inductive Value where
  | register (index : Nat)
  | stack (slot : Nat)
  | label (index : Nat)
  | global (name : Nat)
  | closure (base index : Nat)
  | constant (identity : Nat)
  deriving DecidableEq, Repr

/-- enter-opnd's scan either reuses the first equal key or appends a row. Row
constructors retain the Scheme argument-pair distinction, e.g. #t versus #f. -/
def intern (row : Row) : List Row → List Row × Nat
  | [] => ([row], 0)
  | first :: rest => if first = row then (first :: rest, 0)
      else let (updated, index) := intern row rest; (first :: updated, index + 1)

theorem intern_read (row : Row) (table : List Row) :
    (intern row table).1[(intern row table).2]? = some row := by
  induction table with
  | nil => rfl
  | cons first rest ih => by_cases hit : first = row <;> simp [intern, hit, ih]

theorem intern_preserves {table : List Row} {index : Nat} {old : Row}
    (read : table[index]? = some old) (row : Row) : (intern row table).1[index]? = some old := by
  induction table generalizing index with
  | nil => simp at read
  | cons first rest ih =>
    by_cases hit : first = row
    · simpa [intern, hit] using read
    · cases index with
      | zero => simpa [intern, hit] using read
      | succ index => simpa [intern, hit] using ih read

def code (index tag : Nat) : Nat := index * 8 + tag

theorem tag_decode (index tag : Nat) (valid : tag < 8) : code index tag % 8 = tag := by
  simp [code, Nat.add_mod, Nat.mod_eq_of_lt valid]

theorem index_decode (index tag : Nat) (valid : tag < 8) : code index tag / 8 = index := by
  simp [code]; omega

theorem code_injective {left right a b : Nat} (la : a < 8) (rb : b < 8)
    (same : code left a = code right b) : left = right ∧ a = b := by
  have tags := congrArg (· % 8) same
  have indices := congrArg (· / 8) same
  simpa [tag_decode _ _ la, tag_decode _ _ rb, index_decode _ _ la, index_decode _ _ rb] using And.intro indices tags

/-- This decoder does not treat raw register zero as Scheme #f. Tags 6 and 7,
missing table rows and malformed table-kind combinations are rejected. -/
def decode (table : List Row) (raw : Nat) : Option Value :=
  match raw % 8 with
  | 0 => some (.register (raw / 8))
  | 1 => some (.stack (raw / 8))
  | 2 => some (.label (raw / 8))
  | 3 => match table[raw / 8]? with | some (.global name) => some (.global name) | _ => none
  | 4 => match table[raw / 8]? with | some (.closure base index) => some (.closure base index) | _ => none
  | 5 => match table[raw / 8]? with | some (.constant identity) => some (.constant identity) | _ => none
  | _ => none

def encode (table : List Row) : Value → List Row × Nat
  | .register index => (table, code index 0)
  | .stack slot => (table, code slot 1)
  | .label index => (table, code index 2)
  | .global name => let (updated, index) := intern (.global name) table; (updated, code index 3)
  | .closure base index => let (updated, slot) := intern (.closure base index) table; (updated, code slot 4)
  | .constant identity => let (updated, index) := intern (.constant identity) table; (updated, code index 5)

theorem encode_decode (table : List Row) (value : Value) :
    decode (encode table value).1 (encode table value).2 = some value := by
  cases value <;> simp [encode, decode, tag_decode, index_decode, intern_read]

/-- Resolved register/STK/global/CLO destinations. A closure base is a decoded
pointer address; recursive operand evaluation and typed base validation follow
below. A raw base code is never used directly as a physical closure address. -/
inductive Place where
  | register (index : Nat)
  | stack (wordAddress : Int)
  | global (name : Nat)
  | closure (base index : Nat)
  deriving DecidableEq, Repr

def location (base : Int) : Value → Option Place
  | .register index => some (.register index)
  | .stack slot => if 0 < slot then some (.stack (base - slot)) else none
  | .global name => some (.global name)
  | .closure _ _ => none
  | _ => none

def emittedLocation (base fp : Int) : Value → Option Place
  | .stack slot => if 0 < slot then some (.stack (Slots.emittedAddress base fp slot)) else none
  | other => location base other

theorem emitted_location_exact (base fp : Int) (value : Value) :
    emittedLocation base fp value = location base value := by
  cases value <;> simp [emittedLocation, location, Slots.emittedAddress]
  split <;> congr 2 <;> omega

abbrev Memory := Place → Roots.Word

def store (memory : Memory) (place : Place) (value : Roots.Word) : Memory :=
  fun query => if query = place then value else memory query

structure Constants where
  labels : Nat → Roots.Word
  objects : Nat → Roots.Word

/-- Resolve the closure base through the actual operand read. Two raw operands
that hold the same pointer alias the same closure fields. Fuel is an explicit
admission for recursive raw operands; arbitrary cyclic tables are not accepted. -/
def read (table : List Row) (base : Int) (memory : Memory) (constants : Constants) :
    Nat → Value → Option Roots.Word
  | _, .label index => some (constants.labels index)
  | _, .constant identity => some (constants.objects identity)
  | fuel, .closure raw index =>
      match fuel with
      | 0 => none
      | remaining + 1 => if 0 < index then do
          let operand ← decode table raw
          let pointer ← read table base memory constants remaining operand
          match pointer with
          | .reference address => some (memory (.closure address index))
          | .immediate _ => none
        else none
  | _, other => (location base other).map memory

def destination (table : List Row) (base : Int) (memory : Memory) (constants : Constants)
    (fuel : Nat) : Value → Option Place
  | .closure raw index =>
      match fuel with
      | 0 => none
      | remaining + 1 => if 0 < index then do
          let operand ← decode table raw
          let pointer ← read table base memory constants remaining operand
          match pointer with
          | .reference address => some (.closure address index)
          | .immediate _ => none
        else none
  | other => location base other

theorem closure_destination (table : List Row) (base : Int) (memory : Memory)
    (constants : Constants) (fuel raw index address : Nat) (positive : 0 < index)
    (loadedBase : (decode table raw).bind (read table base memory constants fuel) =
      some (.reference address)) :
    destination table base memory constants (fuel + 1) (.closure raw index) =
      some (.closure address index) := by
  cases decoded : decode table raw with
  | none => simp [decoded] at loadedBase
  | some operand =>
    cases loaded : read table base memory constants fuel operand with
    | none => simp [decoded, loaded] at loadedBase
    | some word =>
      simp [decoded, loaded] at loadedBase
      cases loadedBase
      simp [destination, positive, decoded, loaded]

theorem closure_aliases (table : List Row) (base : Int) (memory : Memory) (constants : Constants)
    (fuel rawLeft rawRight index address : Nat) (positive : 0 < index)
    (left : (decode table rawLeft).bind (read table base memory constants fuel) = some (.reference address))
    (right : (decode table rawRight).bind (read table base memory constants fuel) = some (.reference address)) :
    destination table base memory constants (fuel + 1) (.closure rawLeft index) =
      destination table base memory constants (fuel + 1) (.closure rawRight index) := by
  rw [closure_destination table base memory constants fuel rawLeft index address positive left,
    closure_destination table base memory constants fuel rawRight index address positive right]

theorem destination_read (table : List Row) (base : Int) (memory : Memory) (constants : Constants)
    (fuel : Nat) (operand : Value) {place : Place}
    (resolved : destination table base memory constants fuel operand = some place) :
    read table base memory constants fuel operand = some (memory place) := by
  cases operand with
  | label index => simp [destination, location] at resolved
  | constant identity => simp [destination, location] at resolved
  | register index => simp [destination, location] at resolved; cases resolved; simp [read, location]
  | stack slot =>
    simp only [destination, location] at resolved
    split at resolved
    · cases resolved; simp [read, location, *]
    · cases resolved
  | global name => simp [destination, location] at resolved; cases resolved; simp [read, location]
  | closure raw index =>
    cases fuel with
    | zero => simp [destination] at resolved
    | succ fuel =>
      simp only [destination] at resolved
      split at resolved
      · cases decoded : decode table raw with
        | none => simp [decoded] at resolved
        | some value =>
          cases loaded : read table base memory constants fuel value with
          | none => simp [decoded, loaded] at resolved
          | some pointer =>
            cases pointer with
            | immediate bits => simp [decoded, loaded] at resolved
            | reference address =>
              simp [decoded, loaded] at resolved
              cases resolved
              simp [read, *]
      · simp_all

structure CopyState where
  memory : Memory
  names : Place → Option Nat

/-- Frontend metadata is updated even when an equal source/destination suppresses
make-copy. #f source is distinct from operand code zero. -/
def copy (state : CopyState) (source : Option Place) (target : Place) (name : Option Nat) : CopyState :=
  let named := match name with
    | none => state.names
    | some variableId => fun query => if query = target then some variableId else state.names query
  { memory := match source with
      | none => state.memory
      | some origin => if origin = target then state.memory else store state.memory target (state.memory origin)
    names := named }

theorem copy_value (state : CopyState) (source target : Place) (name : Option Nat) :
    (copy state (some source) target name).memory target = state.memory source := by
  by_cases same : source = target <;> simp [copy, store, same]

theorem suppressed_copy_names (state : CopyState) (target : Place) (variableId : Nat) :
    (copy state (some target) target (some variableId)).memory = state.memory ∧
    (copy state (some target) target (some variableId)).names target = some variableId := by simp [copy]

theorem copy_other (state : CopyState) (source : Option Place) {target query : Place}
    (different : query ≠ target) (name : Option Nat) :
    (copy state source target name).memory query = state.memory query := by
  cases source with
  | none => rfl
  | some origin => by_cases same : origin = target <;> simp [copy, same, store, different]
/-- Denotation of put-copy/targ-gen-copy after operand-table decoding. A missing
source still updates variable metadata. Reading precedes writing, including
nested closure bases and immediate source values. This does not model target
cache invalidation or return-address metadata movement. -/
def executeCopy (table : List Row) (base : Int) (constants : Constants) (fuel : Nat)
    (state : CopyState) (source : Option Nat) (target : Nat) (name : Option Nat) : Option CopyState := do
  let operand ← decode table target
  let place ← destination table base state.memory constants fuel operand
  let named := copy state none place name
  match source with
  | none => some named
  | some raw => do
      let input ← decode table raw
      let word ← read table base state.memory constants fuel input
      some { named with memory := store state.memory place word }

theorem decoded_copy_readback (table : List Row) (base : Int) (constants : Constants)
    (fuel sourceRaw targetRaw : Nat) (state : CopyState) (name : Option Nat)
    {source target : Value} {place : Place} {word : Roots.Word}
    (sourceDecoded : decode table sourceRaw = some source)
    (targetDecoded : decode table targetRaw = some target)
    (resolved : destination table base state.memory constants fuel target = some place)
    (loaded : read table base state.memory constants fuel source = some word) :
    ∃ after, executeCopy table base constants fuel state (some sourceRaw) targetRaw name = some after ∧
      after.memory place = word ∧
      (∀ query, query ≠ place → after.memory query = state.memory query) ∧
      after.names = (copy state none place name).names := by
  refine ⟨{ (copy state none place name) with memory := store state.memory place word }, ?_, ?_, ?_, rfl⟩
  · simp [executeCopy, sourceDecoded, targetDecoded, resolved, loaded]
  · simp [store]
  · intro query different; simp [store, different]

end Operand

namespace Boxing
/-- The qualified branch is a 64-bit, one-payload-word, non-handle heap flonum.
Immediate/self-tagging/NaN boxing branches require different allocation laws. -/
inductive Cell where
  | header (value : Nat)
  | bits (value : BitVec 64)
  deriving DecidableEq, Repr

structure ABI where
  header : Nat
  tag : Nat

structure Heap where
  hp : Nat
  limit : Nat
  cells : Nat → Option Cell

def store (cells : Nat → Option Cell) (address : Nat) (value : Cell) : Nat → Option Cell :=
  fun query => if query = address then some value else cells query

/-- ALLOC_ALIGN64(2) advances hp, then the macro writes hp[-2] and hp[-1]. -/
def allocate (abi : ABI) (heap : Heap) (bits : BitVec 64) : Heap :=
  let advanced := heap.hp + 2
  ⟨advanced, heap.limit, store (store heap.cells (advanced - 2) (.header abi.header)) (advanced - 1) (.bits bits)⟩

def tagged (abi : ABI) (heap : Heap) : Nat := heap.hp * 8 + abi.tag

def body (abi : ABI) (pointer : Nat) : Nat := (pointer - abi.tag) / 8 + 1

def Ready (heap : Heap) : Prop := heap.hp + 2 ≤ heap.limit

def Fresh (heap : Heap) : Prop := ∀ address, heap.hp ≤ address → heap.cells address = none

theorem allocated_header (abi : ABI) (heap : Heap) (bits : BitVec 64) :
    (allocate abi heap bits).cells heap.hp = some (.header abi.header) := by simp [allocate, store]

theorem allocated_payload (abi : ABI) (heap : Heap) (bits : BitVec 64) :
    (allocate abi heap bits).cells (body abi (tagged abi heap)) = some (.bits bits) := by
  simp [allocate, store, body, tagged]

theorem pointer_header (abi : ABI) (heap : Heap) : (tagged abi heap - abi.tag) / 8 = heap.hp := by
  simp [tagged]

theorem old_cell_retained (abi : ABI) (heap : Heap) (bits : BitVec 64) {address : Nat}
    (old : address < heap.hp) : (allocate abi heap bits).cells address = heap.cells address := by
  have headerDifferent : address ≠ heap.hp := by omega
  have payloadDifferent : address ≠ heap.hp + 1 := by omega
  simp [allocate, store, headerDifferent, payloadDifferent]

theorem allocated_fresh (abi : ABI) {heap : Heap} (fresh : Fresh heap) (bits : BitVec 64) :
    Fresh (allocate abi heap bits) := by
  intro address later
  have headerDifferent : address ≠ heap.hp := by simp only [allocate] at later; omega
  have payloadDifferent : address ≠ heap.hp + 1 := by simp only [allocate] at later; omega
  simpa [allocate, store, headerDifferent, payloadDifferent] using fresh address (by simp only [allocate] at later; omega)

theorem allocation_within_segment (abi : ABI) {heap : Heap} (ready : Ready heap) (bits : BitVec 64) :
    (allocate abi heap bits).hp ≤ heap.limit := ready

theorem allocation_identity (abi : ABI) {heap : Heap} (fresh : Fresh heap) (bits : BitVec 64) :
    heap.cells heap.hp = none ∧ (allocate abi heap bits).cells heap.hp = some (.header abi.header) ∧
      (allocate abi heap bits).cells (heap.hp + 1) = some (.bits bits) := by
  refine ⟨fresh _ (Nat.le_refl _), allocated_header abi heap bits, ?_⟩
  simpa [body, tagged] using allocated_payload abi heap bits

def initial (capacity : Nat) : Heap := ⟨0, capacity, fun _ => none⟩

theorem initial_fresh (capacity : Nat) : Fresh (initial capacity) := by intro address later; rfl

inductive Trace (abi : ABI) : Heap → Heap → Prop where
  | refl (heap) : Trace abi heap heap
  | box {before after} : Trace abi before after → Ready after → (bits : BitVec 64) → Trace abi before (allocate abi after bits)

theorem trace_fresh {abi : ABI} {before after : Heap} (fresh : Fresh before)
    (history : Trace abi before after) : Fresh after := by
  induction history with
  | refl => exact fresh
  | box _ ready bits ih => exact allocated_fresh abi ih bits

theorem tagged_fits {abi : ABI} {heap : Heap} (ready : Ready heap)
    (capacity : heap.limit * 8 + abi.tag < 2 ^ 64) : tagged abi heap < 2 ^ 64 := by
  unfold Ready at ready
  unfold tagged
  omega

/-- Decode the physical tagged header address as a root, then compose with
cache materialization and actual frame stores. No arbitrary box function or
boxed-pointer readback certificate is supplied by the caller. -/
theorem cache_allocation_root {rows : List (Option Cache.Entry)} {counter location index size width : Nat}
    (history : Cache.Trace counter rows) {entry : Cache.Entry} (selected : Cache.lookup location rows = some entry)
    (dirty : entry.dirty = true) (abi : ABI) (heap : Heap) (ready : Ready heap) (fresh : Fresh heap) (memory : Nat → Roots.Word)
    (stack : Int → Roots.Word) (base fp : Int) {flags : List Bool}
    (positive : 0 < width) (live : flags[index]? = some true) (inside : index < size) :
    let bits := BitVec.ofNat 64 entry.payload
    let pointer := (tagged abi heap - abi.tag) / 8
    let box := fun _ : Nat => Roots.Word.reference pointer
    let payload := Cache.writeback box memory (Cache.flush location rows).2 entry.location
    let written := Slots.store stack (Slots.emittedAddress base fp (index + 1 : Nat)) payload
    heap.cells heap.hp = none ∧ (allocate abi heap bits).hp ≤ heap.limit ∧
      (allocate abi heap bits).cells (heap.hp + 1) = some (.bits bits) ∧
      heap.hp ∈ Roots.frame (FrameMaps.generate 1 0 flags) 0 (Slots.scanWords written base size) := by
  dsimp
  refine ⟨fresh _ (Nat.le_refl _), allocation_within_segment abi ready _, ?_, ?_⟩
  · simpa [body, tagged] using allocated_payload abi heap (BitVec.ofNat 64 entry.payload)
  · simpa [pointer_header] using
      (Cache.dirty_spill_signed history selected dirty (fun _ => Roots.Word.reference heap.hp) rfl
        memory stack base fp positive live inside).2
end Boxing

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
end Contended
end ParserEngine.Host
