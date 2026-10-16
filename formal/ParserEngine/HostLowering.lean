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

namespace Table
/-- Observed Scheme scalar classes. Keys name canonical eqv? classes of other
objects; their extraction from the host heap is an explicit boundary. -/
inductive Atom where
  | boolean (value : Bool)
  | number (value : Nat)
  | key (identity : Nat)
  deriving DecidableEq, Repr

structure Pair where
  first : Atom
  second : Atom
  deriving DecidableEq, Repr

def identity : Atom → Nat
  | .boolean false => 0
  | .boolean true => 1
  | .number value => value * 4 + 2
  | .key value => value * 4 + 3

/-- car/cdr extraction, retaining #t/#f versus numeric field indices. -/
def observe (pair : Pair) : Option Row :=
  match pair.second with
  | .boolean true => some (.global (identity pair.first))
  | .boolean false => some (.constant (identity pair.first))
  | .number index => match pair.first with
      | .number base => some (.closure base index)
      | _ => none
  | .key _ => none

/-- Only the allocated vector prefix is observed. Capacity cells are not rows. -/
def extract : Nat → List (Option Pair) → Option (List Row)
  | 0, _ => some []
  | _ + 1, [] => none
  | _ + 1, none :: _ => none
  | count + 1, some pair :: rest => do
      let row ← observe pair
      let rows ← extract count rest
      some (row :: rows)

theorem extract_size {count : Nat} {slots : List (Option Pair)} {rows : List Row}
    (snapshot : extract count slots = some rows) : rows.length = count ∧ count ≤ slots.length := by
  induction count generalizing slots rows with
  | zero => simp [extract] at snapshot; subst rows; simp
  | succ count ih =>
    cases slots with
    | nil => simp [extract] at snapshot
    | cons slot rest =>
      cases slot with
      | none => simp [extract] at snapshot
      | some pair =>
        cases decoded : observe pair with
        | none => simp [extract, decoded] at snapshot
        | some row =>
          cases extracted : extract count rest with
          | none => simp [extract, decoded, extracted] at snapshot
          | some tailRows =>
            simp [extract, decoded, extracted] at snapshot
            subst rows
            obtain ⟨length, bound⟩ := ih extracted
            simp only [List.length_cons]; omega

theorem extract_read {count : Nat} {slots : List (Option Pair)} {rows : List Row}
    (snapshot : extract count slots = some rows) {index : Nat} {row : Row}
    (selected : rows[index]? = some row) :
    ∃ pair, slots[index]? = some (some pair) ∧ observe pair = some row := by
  induction count generalizing slots rows index with
  | zero => simp [extract] at snapshot; subst rows; simp at selected
  | succ count ih =>
    cases slots with
    | nil => simp [extract] at snapshot
    | cons slot rest =>
      cases slot with
      | none => simp [extract] at snapshot
      | some pair =>
        cases decoded : observe pair with
        | none => simp [extract, decoded] at snapshot
        | some first =>
          cases extracted : extract count rest with
          | none => simp [extract, decoded, extracted] at snapshot
          | some tailRows =>
            simp [extract, decoded, extracted] at snapshot
            subst rows
            cases index with
            | zero => simp at selected; subst row; exact ⟨pair, rfl, decoded⟩
            | succ index =>
              obtain ⟨pair, read, observation⟩ := ih extracted selected
              exact ⟨pair, read, observation⟩

theorem extract_suffix (count : Nat) (slots suffix : List (Option Pair))
    (bound : count ≤ slots.length) : extract count (slots ++ suffix) = extract count slots := by
  induction count generalizing slots with
  | zero => rfl
  | succ count ih =>
    cases slots with
    | nil => simp at bound
    | cons slot rest =>
      cases slot with
      | none => rfl
      | some pair => simp [extract, ih rest (by simpa using bound)]

/-- extend-opnd-table! uses floor(3*n/2)+1 and copies every existing cell. -/
def grow (slots : List (Option Pair)) : List (Option Pair) :=
  slots ++ List.replicate (slots.length / 2 + 1) none

theorem grow_size (slots : List (Option Pair)) :
    (grow slots).length = 3 * slots.length / 2 + 1 := by simp [grow]; omega

theorem grow_snapshot {count : Nat} {slots : List (Option Pair)} {rows : List Row}
    (snapshot : extract count slots = some rows) : extract count (grow slots) = some rows := by
  rw [grow, extract_suffix count slots _ (extract_size snapshot).2, snapshot]

theorem write_next {count : Nat} {slots : List (Option Pair)} {rows : List Row}
    (snapshot : extract count slots = some rows) (capacity : count < slots.length)
    (pair : Pair) {row : Row} (observed : observe pair = some row) :
    extract (count + 1) (slots.set count (some pair)) = some (rows ++ [row]) := by
  induction count generalizing slots rows with
  | zero =>
    simp [extract] at snapshot; subst rows
    cases slots with
    | nil => simp at capacity
    | cons slot rest => simp [extract, observed]
  | succ count ih =>
    cases slots with
    | nil => simp at capacity
    | cons slot rest =>
      cases slot with
      | none => simp [extract] at snapshot
      | some first =>
        cases decoded : observe first with
        | none => simp [extract, decoded] at snapshot
        | some head =>
          cases extracted : extract count rest with
          | none => simp [extract, decoded, extracted] at snapshot
          | some tailRows =>
            simp [extract, decoded, extracted] at snapshot
            subst rows
            simp [extract, decoded, ih extracted (by simpa using capacity)]

/-- Native alloc increment, optional growth and vector-set! at the old count. -/
def append (count : Nat) (slots : List (Option Pair)) (pair : Pair) : List (Option Pair) :=
  (if count < slots.length then slots else grow slots).set count (some pair)

theorem append_snapshot {count : Nat} {slots : List (Option Pair)} {rows : List Row}
    (snapshot : extract count slots = some rows) (pair : Pair) {row : Row}
    (observed : observe pair = some row) :
    extract (count + 1) (append count slots pair) = some (rows ++ [row]) := by
  by_cases spare : count < slots.length
  · simpa [append, spare] using write_next snapshot spare pair observed
  · have bound := (extract_size snapshot).2
    have grown : count < (grow slots).length := by simp [grow]; omega
    simpa [append, spare] using write_next (grow_snapshot snapshot) grown pair observed
end Table

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
  extent : Nat
  cells : Nat → Option Cell

def store (cells : Nat → Option Cell) (address : Nat) (value : Cell) : Nat → Option Cell :=
  fun query => if query = address then some value else cells query

/-- ALLOC_ALIGN64(2) advances hp, then the macro writes hp[-2] and hp[-1]. -/
def allocate (abi : ABI) (heap : Heap) (bits : BitVec 64) : Heap :=
  let advanced := heap.hp + 2
  ⟨advanced, heap.limit, max heap.extent advanced, store (store heap.cells (advanced - 2) (.header abi.header)) (advanced - 1) (.bits bits)⟩

def tagged (abi : ABI) (heap : Heap) : Nat := heap.hp * 8 + abi.tag

def body (abi : ABI) (pointer : Nat) : Nat := (pointer - abi.tag) / 8 + 1

def Ready (heap : Heap) : Prop := heap.hp + 2 ≤ heap.limit

/-- Ownership of the current section tail only. Other sections may contain live
objects at either lower or higher addresses. -/
def Fresh (heap : Heap) : Prop := ∀ address, heap.hp ≤ address → address < heap.limit → heap.cells address = none

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
  intro address later inside
  have headerDifferent : address ≠ heap.hp := by simp only [allocate] at later; omega
  have payloadDifferent : address ≠ heap.hp + 1 := by simp only [allocate] at later; omega
  simpa [allocate, store, headerDifferent, payloadDifferent] using fresh address (by simp only [allocate] at later; omega) inside

theorem allocation_within_segment (abi : ABI) {heap : Heap} (ready : Ready heap) (bits : BitVec 64) :
    (allocate abi heap bits).hp ≤ heap.limit := ready

/-- Local section ownership preserves every existing cell, including live
objects in a different section above the current hp. -/
theorem allocated_cell_retained (abi : ABI) {heap : Heap} (ready : Ready heap) (fresh : Fresh heap)
    (bits : BitVec 64) {address : Nat} {cell : Cell} (read : heap.cells address = some cell) :
    (allocate abi heap bits).cells address = some cell := by
  have headerDifferent : address ≠ heap.hp := by
    intro equal
    have vacant := fresh address (by omega) (by unfold Ready at ready; omega)
    rw [vacant] at read; cases read
  have payloadDifferent : address ≠ heap.hp + 1 := by
    intro equal
    have vacant := fresh address (by omega) (by unfold Ready at ready; omega)
    rw [vacant] at read; cases read
  simp [allocate, store, headerDifferent, payloadDifferent, read]

theorem allocation_identity (abi : ABI) {heap : Heap} (ready : Ready heap) (fresh : Fresh heap) (bits : BitVec 64) :
    heap.cells heap.hp = none ∧ (allocate abi heap bits).cells heap.hp = some (.header abi.header) ∧
      (allocate abi heap bits).cells (heap.hp + 1) = some (.bits bits) := by
  refine ⟨fresh _ (Nat.le_refl _) (by unfold Ready at ready; omega), allocated_header abi heap bits, ?_⟩
  simpa [body, tagged] using allocated_payload abi heap bits

def initial (capacity : Nat) : Heap := ⟨0, capacity, capacity, fun _ => none⟩

theorem initial_fresh (capacity : Nat) : Fresh (initial capacity) := by intro address later inside; rfl

/-- Native msection selection changes the active allocation interval, not the
addresses or contents of existing objects. Free-list ownership supplies the
vacant new interval; it need not lie above the previous hp. -/
def selectSection (heap : Heap) (start stop : Nat) : Heap := ⟨start, stop, max heap.extent stop, heap.cells⟩

def SectionFree (heap : Heap) (start stop : Nat) : Prop :=
  ∀ address, start ≤ address → address < stop → heap.cells address = none

theorem selected_fresh (heap : Heap) (start stop : Nat) (free : SectionFree heap start stop) :
    Fresh (selectSection heap start stop) := free

theorem selected_retains (heap : Heap) (start stop address : Nat) :
    (selectSection heap start stop).cells address = heap.cells address := rfl

inductive Trace (abi : ABI) : Heap → Heap → Prop where
  | refl (heap) : Trace abi heap heap
  | box {before after} : Trace abi before after → Ready after → (bits : BitVec 64) → Trace abi before (allocate abi after bits)
  | switchSection {before after} : Trace abi before after → (start stop : Nat) → SectionFree after start stop →
      Trace abi before (selectSection after start stop)

theorem trace_fresh {abi : ABI} {before after : Heap} (fresh : Fresh before)
    (history : Trace abi before after) : Fresh after := by
  induction history with
  | refl => exact fresh
  | box _ ready bits ih => exact allocated_fresh abi ih bits
  | switchSection _ start stop free _ => exact selected_fresh _ start stop free

/-- The active cursor can move backwards; the whole-heap snapshot extent cannot. -/
theorem trace_extent {abi : ABI} {before after : Heap} (history : Trace abi before after) :
    before.extent ≤ after.extent := by
  induction history with
  | refl => exact Nat.le_refl _
  | box earlier ready bits ih => simp only [allocate]; omega
  | switchSection earlier start stop free ih => simp only [selectSection]; omega

theorem trace_retains {abi : ABI} {before after : Heap} (fresh : Fresh before)
    (history : Trace abi before after) {address : Nat} {cell : Cell}
    (read : before.cells address = some cell) : after.cells address = some cell := by
  induction history with
  | refl => exact read
  | box earlier ready bits ih => exact allocated_cell_retained abi ready (trace_fresh fresh earlier) bits ih
  | switchSection _ start stop free ih => exact ih

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
  refine ⟨fresh _ (Nat.le_refl _) (by unfold Ready at ready; omega), allocation_within_segment abi ready _, ?_, ?_⟩
  · simpa [body, tagged] using allocated_payload abi heap (BitVec.ofNat 64 entry.payload)
  · simpa [pointer_header] using
      (Cache.dirty_spill_signed history selected dirty (fun _ => Roots.Word.reference heap.hp) rfl
        memory stack base fp positive live inside).2
end Boxing

namespace HeapCheck
/-- C returns 0 after reuse/growth; nonzero requests collection. Overflow is
reported only if collection also returns nonzero. The collector itself remains
an explicit relation, not an assumed success flag. -/
inductive Result where | resume | overflow
  deriving DecidableEq, Repr

def dispatch (hp threshold : Nat) (limitRequestsGC collectionOverflows : Bool) : Result :=
  if hp > threshold then
    if limitRequestsGC && collectionOverflows then .overflow else .resume
  else .resume

def invokesCollection (hp threshold : Nat) (limitRequestsGC : Bool) : Bool :=
  decide (hp > threshold) && limitRequestsGC

theorem overflow_exact (hp threshold : Nat) (limitRequestsGC collectionOverflows : Bool) :
    dispatch hp threshold limitRequestsGC collectionOverflows = .overflow ↔
      hp > threshold ∧ limitRequestsGC = true ∧ collectionOverflows = true := by
  unfold dispatch
  split <;> cases limitRequestsGC <;> cases collectionOverflows <;> simp_all

theorem no_gc_on_reuse (hp threshold : Nat) : invokesCollection hp threshold false = false := by
  simp [invokesCollection]

theorem equality_does_not_trip (threshold : Nat) (limitRequestsGC collectionOverflows : Bool) :
    dispatch threshold threshold limitRequestsGC collectionOverflows = .resume ∧
      invokesCollection threshold threshold limitRequestsGC = false := by simp [dispatch, invokesCollection]

/-- The native heap handler saves every GVM register, not just the entries
selected by the caller's ordinary liveness map. Return metadata is separate. -/
def savedRoots (registers stack : List Roots.Word) : List Nat := Roots.references (registers ++ stack)

theorem saved_register {registers stack : List Roots.Word} {index address : Nat}
    (read : registers[index]? = some (.reference address)) : address ∈ savedRoots registers stack := by
  apply (Roots.reference_member _ _).mpr
  exact List.mem_append_left _ (List.mem_iff_getElem?.mpr ⟨index, read⟩)

/-- Unlike Roots.decode's pointer-only projection, this snapshot retains all
64 payload bits in atom cells. Non-object/interior locations have no fields. -/
def objectAt (heap : Boxing.Heap) (address : Nat) : Operational.Heap.Cell (BitVec 64) :=
  match heap.cells address, heap.cells (address + 1) with
  | some (.header _), some (.bits bits) => .atom bits
  | _, _ => .vector []

def snapshot (heap : Boxing.Heap) : Operational.Heap.Store (BitVec 64) :=
  (List.range heap.extent).map (objectAt heap)

/-- Switching to a lower section must not truncate higher-address live objects
out of the collector's snapshot. -/
theorem selected_snapshot_retains (heap : Boxing.Heap) (start stop address : Nat)
    (inside : address < heap.extent) :
    (snapshot (Boxing.selectSection heap start stop))[address]? = (snapshot heap)[address]? := by
  have afterInside : address < (Boxing.selectSection heap start stop).extent := by
    simp only [Boxing.selectSection]; omega
  unfold snapshot
  rw [List.getElem?_map, List.getElem?_map, List.getElem?_range afterInside, List.getElem?_range inside]
  rfl

theorem allocated_object (abi : Boxing.ABI) (heap : Boxing.Heap) (bits : BitVec 64) :
    (snapshot (Boxing.allocate abi heap bits))[heap.hp]? = some (.atom bits) := by
  have inside : heap.hp < (Boxing.allocate abi heap bits).extent := by simp [Boxing.allocate]; omega
  have payload : (Boxing.allocate abi heap bits).cells (heap.hp + 1) = some (.bits bits) := by
    simpa [Boxing.body, Boxing.tagged] using Boxing.allocated_payload abi heap bits
  simp [snapshot, List.getElem?_map, List.getElem?_range inside, objectAt,
    Boxing.allocated_header, payload]

/-- Conditional native slow-path composition: allocation derives the object
contents; root saving derives admission; the existing relocation contract
preserves its exact bits. No native GC byte-copy correctness is asserted. -/
theorem boxed_register_survives (abi : Boxing.ABI) (heap : Boxing.Heap) (bits : BitVec 64)
    (registers stack : List Roots.Word) {index : Nat}
    (held : registers[index]? = some (.reference heap.hp))
    {after : Operational.Heap.Store (BitVec 64)}
    (collection : Operational.Collection.Relocation (snapshot (Boxing.allocate abi heap bits)) after
      (savedRoots registers stack)) :
    after[collection.address heap.hp]? = some (.atom bits) := by
  simpa [Operational.Collection.renameCell] using collection.copied heap.hp (.atom bits)
    (Operational.Collection.Reachable.root (saved_register held)) (allocated_object abi heap bits)
end HeapCheck

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

namespace SwitchEmission
inductive Kind where | fixnum | character | symbol | keyword | other
  deriving DecidableEq, Repr
structure Case where
  kind : Kind
  key : Nat
  label : Nat
  hash : Nat
  deriving DecidableEq, Repr

/-- Five native mutable reverse accumulators, projected by kind. -/
def accumulate : List Case → (Kind → List Case) → Kind → List Case
  | [], buckets => buckets
  | entry :: rest, buckets =>
    accumulate rest (fun kind => if entry.kind = kind then entry :: buckets kind else buckets kind)

theorem accumulated (cases : List Case) (buckets : Kind → List Case) (kind : Kind) :
    accumulate cases buckets kind = (cases.filter (fun entry => entry.kind == kind)).reverse ++ buckets kind := by
  induction cases generalizing buckets with
  | nil => simp [accumulate]
  | cons entry rest ih =>
    rw [accumulate, ih]
    by_cases same : entry.kind = kind <;> simp [same, List.append_assoc]

def group (cases : List Case) (kind : Kind) : List Case := (accumulate cases (fun _ => []) kind).reverse

theorem stable_group (cases : List Case) (kind : Kind) :
    group cases kind = cases.filter (fun entry => entry.kind == kind) := by simp [group, accumulated]

structure Groups where
  general : List Case
  fixnums : List Case
  characters : List Case
  symkeys : List Case

def groups (cases : List Case) : Groups :=
  let fixnums := group cases .fixnum
  let characters := group cases .character
  ⟨(if characters.length ≤ 2 then characters else []) ++
    (if fixnums.length ≤ 2 then fixnums else []) ++ group cases .other,
   if fixnums.length ≤ 2 then [] else fixnums,
   if characters.length ≤ 2 then [] else characters,
   group cases .symbol ++ group cases .keyword⟩

/-- Hash-table buckets are consed by the producer then reversed at emission.
Use key identity separately from its hash; collisions cannot identify cases. -/
def collectBucket (capacity bucket : Nat) : List Case → List Case → List Case
  | [], accumulated => accumulated
  | entry :: rest, accumulated =>
    collectBucket capacity bucket rest (if entry.hash % capacity = bucket then entry :: accumulated else accumulated)

theorem bucket_accumulated (capacity bucket : Nat) (cases accumulated : List Case) :
    collectBucket capacity bucket cases accumulated =
      (cases.filter (fun entry => entry.hash % capacity == bucket)).reverse ++ accumulated := by
  induction cases generalizing accumulated with
  | nil => simp [collectBucket]
  | cons entry rest ih =>
    rw [collectBucket, ih]
    by_cases same : entry.hash % capacity = bucket <;> simp [same, List.append_assoc]

def bucket (capacity index : Nat) (cases : List Case) : List Case :=
  (collectBucket capacity index cases []).reverse

theorem stable_bucket (capacity index : Nat) (cases : List Case) :
    bucket capacity index cases = cases.filter (fun entry => entry.hash % capacity == index) := by
  simp [bucket, bucket_accumulated]

theorem hashed_case_covered (capacity : Nat) (positive : 0 < capacity) (cases : List Case)
    (entry : Case) (member : entry ∈ cases) :
    ∃ index, index < capacity ∧ entry ∈ bucket capacity index cases := by
  refine ⟨entry.hash % capacity, Nat.mod_lt _ positive, ?_⟩
  simp [stable_bucket, member]
/-- The native doubling loop starts at one and stops strictly above the number
of symkey cases. Fuel is a proof bound, not a runtime timeout or API parameter. -/
def capacityLoop (count : Nat) : Nat → Nat → Nat
  | 0, current => current
  | fuel + 1, current => if count < current then current else capacityLoop count fuel (2 * current)

def capacity (count : Nat) : Nat := capacityLoop count (count + 1) 1

inductive CapacityTrace (count : Nat) : Nat → Nat → Prop where
  | done (current) (exit : count < current) : CapacityTrace count current current
  | step (current) (continues : current ≤ count) {result}
      (later : CapacityTrace count (2 * current) result) : CapacityTrace count current result

theorem capacity_loop_terminates (count fuel current : Nat) (positive : 0 < current)
    (enough : count + 1 ≤ current + fuel) :
    CapacityTrace count current (capacityLoop count fuel current) := by
  induction fuel generalizing current with
  | zero => exact CapacityTrace.done current (by omega)
  | succ fuel ih =>
    simp only [capacityLoop]
    split
    · next exit => exact CapacityTrace.done current exit
    · next notExit =>
      exact CapacityTrace.step current (by omega) (ih _ (by omega) (by omega))

theorem capacity_trace_exit {count initial result : Nat} (trace : CapacityTrace count initial result) :
    count < result := by
  induction trace with
  | done current exit => exact exit
  | step current continues later ih => exact ih

theorem capacity_above (count : Nat) : count < capacity count := by
  apply capacity_trace_exit (capacity_loop_terminates count (count + 1) 1 (by omega) (by omega))

theorem capacity_loop_power (count fuel exponent : Nat) :
    ∃ power, capacityLoop count fuel (2 ^ exponent) = 2 ^ power := by
  induction fuel generalizing exponent with
  | zero => exact ⟨exponent, rfl⟩
  | succ fuel ih =>
    simp only [capacityLoop]
    split
    · exact ⟨exponent, rfl⟩
    · rw [show 2 * 2 ^ exponent = 2 ^ (exponent + 1) by simp [Nat.pow_succ, Nat.mul_comm]]
      exact ih (exponent + 1)

theorem capacity_power (count : Nat) : ∃ exponent, capacity count = 2 ^ exponent := by
  simpa [capacity] using capacity_loop_power count (count + 1) 0

/-- C BEGIN_SWITCH_SYMKEY masks by capacity-1, whereas the generator uses
modulo. The source's power-of-two producer makes those operations identical. -/
theorem mask_matches_generator (count hash : Nat) :
    hash &&& (capacity count - 1) = hash % capacity count := by
  obtain ⟨exponent, power⟩ := capacity_power count
  rw [power]
  exact Nat.and_two_pow_sub_one_eq_mod hash exponent

theorem native_bucket_retains (cases : List Case) (entry : Case) (member : entry ∈ cases) :
    entry ∈ bucket (capacity cases.length) (entry.hash &&& (capacity cases.length - 1)) cases := by
  rw [mask_matches_generator]
  simp [stable_bucket, member]

end SwitchEmission

namespace IfJumpEmission
structure Plan where
  inverted : Bool
  branch : Nat
  fallthrough : Nat
  deriving DecidableEq, Repr

def select (trueLabel falseLabel : Nat) (nextLabel : Option Nat) : Plan :=
  if nextLabel = some trueLabel then ⟨true, falseLabel, trueLabel⟩ else ⟨false, trueLabel, falseLabel⟩

def destination (plan : Plan) (predicate : Bool) : Nat :=
  if (if plan.inverted then !predicate else predicate) then plan.branch else plan.fallthrough

/-- Branch inversion preserves routing even when labels coincide or neither
successor is the next block. Predicate-specific inliner semantics is separate. -/
theorem routing_exact (trueLabel falseLabel : Nat) (nextLabel : Option Nat) (predicate : Bool) :
    destination (select trueLabel falseLabel nextLabel) predicate =
      if predicate then trueLabel else falseLabel := by
  cases predicate <;> unfold select <;> split <;> simp [destination]

inductive Event where
  | adjust | test (inverted : Bool) | clearResult (location : Nat)
  | goTo (label : Nat) | endIf
  deriving DecidableEq, Repr

def emit (trueLabel falseLabel : Nat) (nextLabel : Option Nat) (result : Option Nat) : List Event :=
  let plan := select trueLabel falseLabel nextLabel
  [.adjust, .test plan.inverted] ++
    (if plan.inverted then result.toList.map .clearResult else []) ++
    [.goTo plan.branch, .endIf] ++
    (if plan.inverted then [] else result.toList.map .clearResult) ++
    (if nextLabel = some plan.fallthrough then [] else [.goTo plan.fallthrough])

theorem clear_result_once (trueLabel falseLabel : Nat) (nextLabel : Option Nat) (location : Nat) :
    (emit trueLabel falseLabel nextLabel (some location)).count (.clearResult location) = 1 := by
  unfold emit select
  split <;> simp_all <;> split <;> simp

end IfJumpEmission

namespace SwitchEmission
inductive Guard where | symbols | keywords | symkeys
  deriving DecidableEq, Repr

def guard (cases : List Case) : Guard :=
  if (cases.filter (fun entry => entry.kind == .keyword)).isEmpty then .symbols
  else if (cases.filter (fun entry => entry.kind == .symbol)).isEmpty then .keywords else .symkeys

inductive Event where
  | beginGroup (kind : Kind)
  | caseGoTo (entry : Case)
  | endGroup (kind : Kind)
  | equalGoTo (entry : Case)
  | beginHash (capacity : Nat) (guard : Guard)
  | hashBucket (index : Nat)
  | hashCaseGoTo (entry : Case)
  | endHash
  | goTo (label : Nat)
  deriving DecidableEq, Repr

def emitGroup (kind : Kind) (cases : List Case) : List Event :=
  if cases.isEmpty then [] else [.beginGroup kind] ++ cases.map .caseGoTo ++ [.endGroup kind]

def emitHash (capacity defaultLabel : Nat) (cases : List Case) : List Event :=
  [.beginHash capacity (guard cases)] ++ (List.range capacity).flatMap (fun index =>
    [.hashBucket index] ++ (bucket capacity index cases).map .hashCaseGoTo ++ [.goTo defaultLabel]) ++ [.endHash]

def emit (cases : List Case) (capacity defaultLabel : Nat) (nextLabel : Option Nat) : List Event :=
  let split := groups cases
  emitGroup .other split.general ++ emitGroup .fixnum split.fixnums ++ emitGroup .character split.characters ++
    (if split.symkeys.length ≤ 20 then split.symkeys.map .equalGoTo else emitHash capacity defaultLabel split.symkeys) ++
    (if nextLabel = some defaultLabel then [] else [.goTo defaultLabel])

theorem every_hashed_case_emitted (capacity defaultLabel : Nat) (positive : 0 < capacity)
    (cases : List Case) (entry : Case) (member : entry ∈ cases) :
    Event.hashCaseGoTo entry ∈ emitHash capacity defaultLabel cases := by
  obtain ⟨index, bound, retained⟩ := hashed_case_covered capacity positive cases entry member
  simp only [emitHash, List.mem_append, List.mem_singleton, List.mem_flatMap]
  left; right
  refine ⟨index, List.mem_range.mpr bound, ?_⟩
  simp [retained]

/-- Each bucket, including an empty one, has its own default exit. This cannot
be replaced by only the final default, which may be elided for fallthrough. -/
theorem bucket_default_emitted (capacity defaultLabel index : Nat) (cases : List Case)
    (bound : index < capacity) : Event.goTo defaultLabel ∈ emitHash capacity defaultLabel cases := by
  simp only [emitHash, List.mem_append, List.mem_singleton, List.mem_flatMap]
  left; right
  exact ⟨index, List.mem_range.mpr bound, by simp⟩
end SwitchEmission

namespace LabelEmission
/-- The GVM dispatcher starts a basic block before simple/task-entry emission.
Cache-region clearing is a producer effect; predecessor value preservation is
still an inter-block obligation, not implied by discarding this local cache. -/
def start (task : Bool) (label frameSize : Nat) (state : CheckEmission.State)
    (_rows : List (Option Cache.Entry)) : CheckEmission.State × List (Option Cache.Entry) :=
  ({state with
    code := state.code ++ [.label label] ++ (if task then [.taskPush 0] else [])
    framePointer := frameSize
    heapReserved := 0
    floatingReserved := 0
    ssbReserved := 0}, [])

theorem complete_simple_task_entry (task : Bool) (label frameSize : Nat) (state : CheckEmission.State)
    (rows : List (Option Cache.Entry)) :
    (start task label frameSize state rows).1.code =
      state.code ++ [.label label] ++ (if task then [.taskPush 0] else []) ∧
    (start task label frameSize state rows).1.framePointer = frameSize ∧
    (start task label frameSize state rows).1.heapReserved = 0 ∧
    (start task label frameSize state rows).1.floatingReserved = 0 ∧
    (start task label frameSize state rows).1.ssbReserved = 0 ∧
    (start task label frameSize state rows).1.nextLabel = state.nextLabel ∧
    (start task label frameSize state rows).2 = [] := by simp [start]
end LabelEmission

namespace ApplyFusion
/-- Decoded GVM protocol: primitive test metadata is optional; an empty operand
list differs from the single eqv?-identical result operand required by fusion. -/
inductive Test where
  | identity | negate | other
  deriving DecidableEq, Repr

structure Input where
  firstApply : Bool
  secondIfJump : Bool
  primitiveTest : Option Bool
  test : Test
  result : Nat
  operands : List Nat
  deriving Repr

def admitted (input : Input) : Bool :=
  input.firstApply && input.secondIfJump && input.primitiveTest.getD false &&
    (input.test == .identity || input.test == .negate) &&
    (input.operands == [input.result])

theorem guard_exact (input : Input) : admitted input = true ↔
    input.firstApply = true ∧ input.secondIfJump = true ∧
    input.primitiveTest = some true ∧ (input.test = .identity ∨ input.test = .negate) ∧
    input.operands = [input.result] := by
  cases input with
  | mk first second metadata test result operands =>
    cases metadata <;> cases test <;> simp [admitted] <;> cases first <;> cases second <;> simp

/-- The fused generator tests the primitive directly and swaps destinations for
NOT. Primitive evaluation, result storage and resource effects remain imports. -/
def original (test : Test) (value : Bool) (yes no : Nat) : Nat :=
  if (if test == .negate then !value else value) then yes else no

def fused (test : Test) (value : Bool) (yes no : Nat) : Nat :=
  if value then (if test == .negate then no else yes)
  else (if test == .negate then yes else no)

theorem destination_exact (test : Test) (value : Bool) (yes no : Nat) :
    fused test value yes no = original test value yes no := by
  cases test <;> cases value <;> rfl

inductive Event where
  | apply | branch (destination : Nat)
  deriving DecidableEq, Repr

def pair (input : Input) (value : Bool) (yes no : Nat) : List Event :=
  (if admitted input then [] else [.apply]) ++
    [.branch (if admitted input then fused input.test value yes no
              else original input.test value yes no)]

theorem pair_complete (input : Input) (value : Bool) (yes no : Nat)
    (selected : admitted input = true) :
    pair input value yes no = [.branch (original input.test value yes no)] := by
  simp [pair, selected, destination_exact]
end ApplyFusion

namespace EntryDispatch
inductive Rest where
  | absent | ordinary | dsssl
  deriving DecidableEq, Repr
inductive Terminal where
  | wrongArity | rest | key | keyRest | restKey
  deriving DecidableEq, Repr

def terminal (keys : Bool) (rest : Rest) : Terminal :=
  if keys then match rest with
    | .absent => .key
    | .ordinary => .keyRest
    | .dsssl => .restKey
  else if rest == .absent then .wrongArity else .rest

/-- Keyword slots and the optional rest slot are excluded from positional arity.
The recursive native dispatch emits one test per accepted positional arity,
then exactly one overflow handler. Empty-but-present keys still select GET_KEY. -/
def arities (required optional : Nat) : List Nat :=
  (List.range (optional + 1)).map (required + ·)

theorem accepts_exactly (required optional args : Nat) :
    args ∈ arities required optional ↔
    required ≤ args ∧ args ≤ required + optional := by
  simp only [arities, List.mem_map, List.mem_range]
  constructor
  · rintro ⟨index, bound, rfl⟩; omega
  · rintro ⟨lower, upper⟩
    exact ⟨args - required, by omega, by omega⟩

theorem dispatch_count (required optional : Nat) :
    (arities required optional).length = optional + 1 := by
  simp [arities]

/-- Native recursion advances nb-args by one until the single terminal case. -/
theorem dispatch_decreases (args maximum : Nat) (active : args ≤ maximum) :
    maximum + 1 - (args + 1) < maximum + 1 - args := by omega

/-- Native recursive setup visits parameters in source order, despite recursively
constructing the remaining suffix before consing each operation. -/
def setup (start count : Nat) : List Nat :=
  match count with
  | 0 => []
  | n + 1 => start :: setup (start + 1) n

theorem setup_order (start count : Nat) :
    setup start count = (List.range count).map (start + ·) := by
  induction count generalizing start with
  | zero => rfl
  | succ n ih =>
    simp only [setup, ih, List.range_succ_eq_map, List.map_cons, List.map_map]
    congr 1
    apply List.map_congr_left
    intro a _
    dsimp
    omega

/-- Counts computed by the native entry generator, with saturating subtraction
admitted only after the decoded grammar/procedure supplies its shape equation. -/
def sourceCounts (parameters optional keywords restSlots : Nat) : Nat × Nat :=
  let positional := parameters - restSlots - keywords
  (positional - optional, positional)

theorem source_counts_exact (required optional keywords restSlots : Nat) :
    sourceCounts (required + optional + keywords + restSlots) optional keywords restSlots =
      (required, required + optional) := by
  dsimp [sourceCounts]
  apply Prod.ext <;> dsimp <;> omega

/-- Terminal selection preserves the presence bit independently of key count. -/
theorem empty_present_keys (rest : Rest) :
    terminal true rest ≠ terminal false rest := by cases rest <;> decide


/-- Logical register transfers for the already-present arguments. PUSH writes
and defaults are separate effects. Assigning a register to itself denotes the
native equal-stack elision without changing its value. -/
def transfer (stacked finalStack : Nat) : List Nat → (Nat → Nat) → (Nat → Nat)
  | [], registers => registers
  | parameter :: rest, registers =>
    let updated := if finalStack < parameter then
      fun register => if register = parameter - finalStack then registers (parameter - stacked)
                      else registers register
      else registers
    transfer stacked finalStack rest updated

theorem argument_stack_monotone (arguments parameters argumentRegisters : Nat)
    (valid : arguments ≤ parameters) :
    arguments - argumentRegisters ≤ parameters - argumentRegisters := by omega

/-- Increasing source-parameter order prevents earlier register moves from
clobbering any later source, including a register-to-stack boundary shift. -/
theorem later_source_retained (stacked finalStack future : Nat) (parameters : List Nat)
    (registers : Nat → Nat) (growth : stacked ≤ finalStack)
    (later : ∀ parameter ∈ parameters, parameter < future) :
    transfer stacked finalStack parameters registers (future - stacked) = registers (future - stacked) := by
  induction parameters generalizing registers with
  | nil => rfl
  | cons parameter rest ih =>
    have earlier := later parameter (by simp)
    have tailEarlier : ∀ p ∈ rest, p < future := fun p member => later p (by simp [member])
    simp only [transfer]
    rw [ih _ tailEarlier]
    split
    · next present =>
      have different : future - stacked ≠ parameter - finalStack := by omega
      simp [different]
    · rfl

end EntryDispatch

namespace ReturnEmission
inductive Kind where
  | ordinary | task | internal
  deriving DecidableEq, Repr
structure Metadata where
  kind : Kind
  frameSize : Nat
  link : Nat
  gcMap : Nat
  deriving DecidableEq, Repr
inductive Event where
  | label (identity : Nat) (metadata : Metadata)
  | pop (destination : Nat)
  deriving DecidableEq, Repr

/-- extend-vars removes or prepends at the head before reversal. The resulting
return metadata lists retain the low physical slots and pad high ones. -/
def slots (empty : Nat) (frame : List Nat) (size : Nat) : List Nat :=
  (frame.reverse ++ List.replicate size empty).take size

theorem slots_length (empty size : Nat) (frame : List Nat) :
    (slots empty frame size).length = size := by
  simp [slots, List.length_take, List.length_append]

theorem retained_slot (empty size index value : Nat) (frame : List Nat)
    (bound : index < size) (read : frame.reverse[index]? = some value) :
    (slots empty frame size)[index]? = some value := by
  have inside : index < frame.reverse.length := (List.getElem?_eq_some_iff.mp read).1
  rw [slots, List.getElem?_take_of_lt bound, List.getElem?_append_left inside]
  exact read

/-- Closed form of the two native extend-vars loops before reversal: repeated
cons for positive delta, repeated cdr for negative delta. Frame size is the
actual slot-list length admitted by the frame decoder. -/
def nativeExtend (empty : Nat) (frame : List Nat) (size : Nat) : List Nat :=
  if frame.length ≤ size then List.replicate (size - frame.length) empty ++ frame
  else frame.drop (frame.length - size)

theorem source_slots (empty size : Nat) (frame : List Nat) :
    (nativeExtend empty frame size).reverse = slots empty frame size := by
  unfold nativeExtend slots
  split
  · rename_i growth
    rw [List.reverse_append, List.reverse_replicate, List.take_append]
    rw [List.take_of_length_le (by simpa using growth)]
    simp only [List.length_reverse, List.take_replicate]
    rw [Nat.min_eq_left (by omega)]
  · rename_i shrink
    rw [List.reverse_drop, List.take_append_of_le_length (by simp; omega)]
    congr 1
    omega

/-- If the return address remains in the retained physical frame, the actual
source extension/reversal cannot silently discard it. -/
theorem source_return_link (empty size index returnVar : Nat) (frame : List Nat)
    (bound : index < size) (read : frame.reverse[index]? = some returnVar) :
    (nativeExtend empty frame size).reverse[index]? = some returnVar := by
  rw [source_slots]
  exact retained_slot empty size index returnVar frame bound read

/-- The label allocator and code buffer are mutable native producer state. -/
structure State where
  code : List Event
  nextLabel : Nat

def emitReturn (kind : Kind) (label frameSize link gcMap : Nat) (state : State) : State :=
  {state with code := state.code ++ [.label label ⟨kind, frameSize, link, gcMap⟩]}

/-- Source sequence: reserve a fresh continuation, generate task-return metadata,
append TASK_POP, then generate ordinary-return metadata for that continuation.
Counter freshness relative to existing labels remains an allocator premise. -/
def taskReturn (label frameSize link gcMap : Nat) (state : State) : State :=
  let fresh := state.nextLabel
  let allocated := {state with nextLabel := fresh + 1}
  let task := emitReturn .task label frameSize link gcMap allocated
  let popped := {task with code := task.code ++ [.pop fresh]}
  emitReturn .ordinary fresh frameSize link gcMap popped

theorem task_continuation (label frameSize link gcMap : Nat) (state : State) :
    (taskReturn label frameSize link gcMap state).code =
      state.code ++ [.label label ⟨.task, frameSize, link, gcMap⟩, .pop state.nextLabel,
       .label state.nextLabel ⟨.ordinary, frameSize, link, gcMap⟩] ∧
    (taskReturn label frameSize link gcMap state).nextLabel = state.nextLabel + 1 := by
  simp [taskReturn, emitReturn, List.append_assoc]
end ReturnEmission

namespace ForwardingCodec
/-- Actual unsigned-word TAG/UNTAG_AS arithmetic, including wrap. Pointer
provenance/alignment and valid dereference are separate physical premises. -/
def tag (address kind : BitVec 64) : BitVec 64 := address + kind
def untag (word kind : BitVec 64) : BitVec 64 := word - kind

theorem roundtrip (address kind : BitVec 64) : untag (tag address kind) kind = address := by
  simp [tag, untag, BitVec.add_sub_cancel]

def forwarding (body referenceBytes : BitVec 64) : BitVec 64 := tag (body - referenceBytes) 3

def redirected (header objectTag : BitVec 64) : BitVec 64 := tag (untag header 3) objectTag

theorem forwarding_body (body referenceBytes objectTag : BitVec 64) :
    untag (redirected (forwarding body referenceBytes) objectTag) objectTag + referenceBytes = body := by
  simp [redirected, forwarding, tag, untag, BitVec.add_sub_cancel, BitVec.sub_add_cancel]

/-- Allocation before CAS consists of only the saved head. On a losing CAS,
that tentative word remains in unowned free storage; cursor rollback does not
clear it. No copied body is claimed and the winner's header is not overwritten. -/
structure Tentative where
  cursor : Nat
  storage : Nat → BitVec 64

def reserveHead (state : Tentative) (head : BitVec 64) : Tentative :=
  ⟨state.cursor + 1, fun index => if index = state.cursor then head else state.storage index⟩
def rollback (state : Tentative) : Tentative := {state with cursor := state.cursor - 1}

theorem loser_rollback (state : Tentative) (head : BitVec 64) :
    (rollback (reserveHead state head)).cursor = state.cursor ∧
    ∀ index, index < state.cursor →
      (rollback (reserveHead state head)).storage index = state.storage index := by
  constructor
  · simp [rollback, reserveHead]
  · intro index bound; simp [rollback, reserveHead, show index ≠ state.cursor by omega]

/-- A non-equal CAS return is not automatically a forwarding header. The
collector must establish that all competing header transitions publish FORW. -/
inductive Header where
  | movable (bits : BitVec 64)
  | forwarded (body : BitVec 64)
  deriving DecidableEq, Repr
inductive Race (expected : BitVec 64) : Header → Prop where
  | unchanged : Race expected (.movable expected)
  | won (destination : BitVec 64) : Race expected (.forwarded destination)

theorem failed_cas_is_forwarded (expected : BitVec 64) (observed : Header)
    (history : Race expected observed) (failed : observed ≠ .movable expected) :
    ∃ destination, observed = .forwarded destination := by
  cases history with
  | unchanged => exact False.elim (failed rfl)
  | won destination => exact ⟨destination, rfl⟩
end ForwardingCodec

namespace SpecialScan
/-- Offsets are body-word indices. Frame and continuation traversal is a
separate event, never an ordinary pointer scan of the encoded frame field. -/
inductive Shape where
  | opaque
  | name
  | will (traverseWeak : Bool)
  | closure (freeStart : Nat)
  | continuation
  | frame (liveSlots : List Nat)
  | hashTable (allocatedKeys weakKeys weakValues : Bool) (keyStart valueStart : Nat)
  | ordinary (tailMarked : Bool)
  deriving DecidableEq, Repr

def offsets (shape : Shape) (words : Nat) : List Nat :=
  match shape with
  | .opaque => []
  | .name => [0]
  | .will weak => if weak then [1, 2] else [2]
  | .closure first => (List.range words).filter (first ≤ ·)
  | .continuation => (List.range words).filter (1 ≤ ·)
  | .frame live => live ++ [0]
  | .hashTable allocated weakKeys weakValues keyStart valueStart =>
      ((List.range words).reverse.filter fun i =>
        allocated && !weakKeys && decide (keyStart ≤ i) && decide ((words - 2 - i) % 2 = 0) && decide (i + 2 ≤ words)) ++
      ((List.range words).reverse.filter fun i =>
        !weakValues && decide (valueStart ≤ i) && decide ((words - 1 - i) % 2 = 0))
  | .ordinary marked => (List.range words).filter (if marked then (1 ≤ ·) else fun _ => True)

theorem closure_fields_exact (first words index : Nat) :
    index ∈ offsets (.closure first) words ↔ first ≤ index ∧ index < words := by
  simp [offsets]; omega

theorem continuation_skips_frame (words : Nat) : 0 ∉ offsets .continuation words := by
  simp [offsets]

theorem weak_testator_excluded : 1 ∉ offsets (.will false) 3 := by decide

theorem will_link_excluded (weak : Bool) : 0 ∉ offsets (.will weak) 3 := by
  cases weak <;> decide

theorem no_weak_hash_keys (words keyStart valueStart : Nat) :
    offsets (.hashTable true true true keyStart valueStart) words = [] := by simp [offsets]

theorem ordinary_tail_partition (words index : Nat) (nonempty : 0 < words) :
    index ∈ (0 :: offsets (.ordinary true) words) ↔ index < words := by
  simp [offsets]; omega

theorem strong_hash_keys_exact (words keyStart valueStart index : Nat) :
    index ∈ offsets (.hashTable true false true keyStart valueStart) words ↔
      index < words ∧ keyStart ≤ index ∧ (words - 2 - index) % 2 = 0 ∧ index + 2 ≤ words := by
  simp [offsets]
  omega

theorem strong_hash_values_exact (words keyStart valueStart index : Nat) :
    index ∈ offsets (.hashTable false true false keyStart valueStart) words ↔
      index < words ∧ valueStart ≤ index ∧ (words - 1 - index) % 2 = 0 := by
  simp [offsets]


/-- Body references in this adapter must already classify as movable/forwarded.
The selector excludes code/link/weak fields before forwarding. Mixed tags and
nonmoving objects require their own native decoder, not an identity shortcut. -/
def forwardedFields (shape : Shape) (words : Nat) (body : Nat → Nat)
    (width : Nat → Nat) (state : Forwarding.State) : Forwarding.State :=
  (Forwarding.scan width ((offsets shape words).map body) state).1

def rewrittenBody (shape : Shape) (words : Nat) (body : Nat → Nat)
    (width : Nat → Nat) (state : Forwarding.State) : Nat → Nat := fun index =>
  if index ∈ offsets shape words then
    ((forwardedFields shape words body width state).table (body index)).getD (body index)
  else body index

theorem selected_field_forwarded (shape : Shape) (words : Nat) (body : Nat → Nat)
    (width : Nat → Nat) (state : Forwarding.State) (index : Nat)
    (selected : index ∈ offsets shape words) :
    ∃ target, (forwardedFields shape words body width state).table (body index) = some target ∧
      rewrittenBody shape words body width state index = target := by
  obtain ⟨position, read⟩ := List.getElem?_of_mem selected
  have mapped : ((offsets shape words).map body)[position]? = some (body index) := by
    simp [List.getElem?_map, read]
  obtain ⟨target, _, recorded⟩ := Forwarding.scanned_slot width ((offsets shape words).map body) state mapped
  refine ⟨target, recorded, ?_⟩
  simp [rewrittenBody, selected, forwardedFields, recorded]

theorem excluded_field_unchanged (shape : Shape) (words : Nat) (body : Nat → Nat)
    (width : Nat → Nat) (state : Forwarding.State) (index : Nat)
    (excluded : index ∉ offsets shape words) :
    rewrittenBody shape words body width state index = body index := by
  simp [rewrittenBody, excluded]

theorem selected_aliases_agree (shape : Shape) (words : Nat) (body : Nat → Nat)
    (width : Nat → Nat) (state : Forwarding.State) (left right : Nat)
    (leftSelected : left ∈ offsets shape words) (rightSelected : right ∈ offsets shape words)
    (aliasing : body left = body right) :
    rewrittenBody shape words body width state left = rewrittenBody shape words body width state right := by
  simp [rewrittenBody, leftSelected, rightSelected, aliasing]


/-- Header/layout admission is explicit. Without it, a will/name/frame selector
could ask for a field outside a malformed body; C unsigned subtraction is not
justified by saturating Nat subtraction. -/
def Valid (shape : Shape) (words : Nat) : Prop :=
  match shape with
  | .opaque => True
  | .name => 1 ≤ words
  | .will _ => words = 3
  | .closure first => first ≤ words
  | .continuation => 1 ≤ words
  | .frame live => 0 < words ∧ ∀ index ∈ live, index < words
  | .hashTable _ _ _ keyStart valueStart => keyStart = 5 ∧ valueStart = 6 ∧ 7 ≤ words
  | .ordinary marked => marked = true → 0 < words

theorem selected_fields_in_bounds (shape : Shape) (words index : Nat)
    (valid : Valid shape words) (selected : index ∈ offsets shape words) : index < words := by
  cases shape with
  | «opaque» => simp [offsets] at selected
  | name => simp only [offsets, List.mem_singleton] at selected; subst index; exact valid
  | will weak => cases weak <;> simp [offsets] at selected <;> simp [Valid] at valid <;> omega
  | closure first => exact (closure_fields_exact first words index).mp selected |>.2
  | continuation => simp [offsets] at selected; omega
  | frame live =>
    simp only [offsets, List.mem_append, List.mem_singleton] at selected
    rcases selected with inside | zero
    · exact valid.2 index inside
    · subst index; exact valid.1
  | hashTable allocated weakKeys weakValues keyStart valueStart =>
    simp [offsets] at selected
    rcases selected with keys | values <;> exact (by omega)
  | ordinary marked => simp [offsets] at selected; omega

end SpecialScan

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
