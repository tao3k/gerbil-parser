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
