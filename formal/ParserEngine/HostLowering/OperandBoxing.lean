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


end ParserEngine.Host
