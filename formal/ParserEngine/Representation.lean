import ParserEngine.Operational
import ParserEngine.Encoding

/-! Representation adapters shared by root discovery and compiler scheduling.
These algorithms expose descriptor, equality and dependency assumptions; they
are not a verification of the host collector or compiler's GC-map generator. -/
namespace ParserEngine.Representation

namespace Roots
inductive Word where
  | immediate (bits : Nat)
  | reference (address : Nat)
  deriving DecidableEq, Repr

def references : List Word → List Nat
  | [] => []
  | .immediate _ :: rest => references rest
  | .reference address :: rest => address :: references rest

@[simp] theorem reference_member (words : List Word) (address : Nat) :
    address ∈ references words ↔ Word.reference address ∈ words := by
  induction words with
  | nil => simp [references]
  | cons word rest ih => cases word <;> simp [references, ih]

/-- Frame maps are indexed from the first payload slot, excluding the return
address and link metadata. Multiword native maps denote this unbounded mask. -/
def frame (map offset : Nat) : List Word → List Nat
  | [] => []
  | word :: rest =>
      (if map.testBit offset then references [word] else []) ++ frame map (offset + 1) rest

theorem frame_slot {words : List Word} {map offset index address : Nat}
    (read : words[index]? = some (.reference address))
    (marked : map.testBit (offset + index) = true) : address ∈ frame map offset words := by
  induction words generalizing offset index with
  | nil => simp at read
  | cons word rest ih =>
    cases index with
    | zero =>
      simp only [List.getElem?_cons_zero, Option.some.injEq] at read
      subst word
      simp [frame, show map.testBit offset = true by simpa using marked]
    | succ index =>
      apply List.mem_append_right
      apply ih read
      simpa [Nat.add_assoc, Nat.add_comm, Nat.add_left_comm] using marked

theorem frame_sound {words : List Word} {map offset address : Nat}
    (found : address ∈ frame map offset words) :
    ∃ index, words[index]? = some (.reference address) ∧ map.testBit (offset + index) = true := by
  induction words generalizing offset with
  | nil => simp [frame] at found
  | cons word rest ih =>
    simp only [frame, List.mem_append] at found
    rcases found with selected | later
    · split at selected
      next marked =>
        have same : word = .reference address := by simpa using (List.mem_singleton.mp ((reference_member [word] address).mp selected)).symm
        exact ⟨0, by simp [same], by simpa using marked⟩
      next unmarked => simp at selected
    · obtain ⟨index, read, bit⟩ := ih later
      exact ⟨index + 1, by simpa using read, by simpa [Nat.add_assoc, Nat.add_comm, Nat.add_left_comm] using bit⟩

inductive Object where
  | pair (first rest : Word)
  | vector (words : List Word)
  | closure (freeVariables : List Word)
  | continuation (savedFrame : Nat) (dynamicEnvironment : List Word)
  | stackFrame (map : Nat) (slots : List Word)
  | strongTable (keys values : List Word)
  | opaque
  deriving DecidableEq, Repr

def fields : Object → List Nat
  | .pair first rest => references [first, rest]
  | .vector words | .closure words => references words
  | .continuation saved environment => saved :: references environment
  | .stackFrame map slots => frame map 0 slots
  | .strongTable keys values => references keys ++ references values
  | .opaque => []

structure Banks where
  registers : List Word
  saved : List Word
  globals : List Word
  vm : List Word
  symbolKeywordTables : List Word
  referenceCounted : List Word
  typeCache : List Word
  processor : List Word
  activeFrames : List (Nat × List Word)
  continuationHandles : List Word

def discover (banks : Banks) : List Nat :=
  references (banks.registers ++ banks.saved ++ banks.globals ++ banks.vm ++
    banks.symbolKeywordTables ++ banks.referenceCounted ++ banks.typeCache ++
    banks.processor ++ banks.continuationHandles) ++
    banks.activeFrames.flatMap (fun row => frame row.1 0 row.2)

/-- Decode only strong pointer payloads into the earlier relocation heap. -/
def decode (heap : List Object) : Operational.Heap.Store Unit :=
  heap.map (fun object => .vector (fields object))

def Reachable (heap : List Object) (banks : Banks) (address : Nat) : Prop :=
  Operational.Collection.Reachable (decode heap) (discover banks) address

theorem captured_handle {banks : Banks} {heap : List Object} {address : Nat}
    (present : Word.reference address ∈ banks.continuationHandles) : Reachable heap banks address := by
  apply Operational.Collection.Reachable.root
  simp [discover, present]

theorem active_slot {banks : Banks} {heap : List Object} {map slots index address}
    (owner : (map, slots) ∈ banks.activeFrames)
    (read : slots[index]? = some (.reference address)) (marked : map.testBit index = true) :
    Reachable heap banks address := by
  apply Operational.Collection.Reachable.root
  have member := frame_slot (offset := 0) read (by simpa using marked)
  simp only [discover, List.mem_append, List.mem_flatMap]
  exact Or.inr ⟨(map, slots), owner, member⟩

theorem field_reached {heap : List Object} {banks : Banks} {owner child object}
    (live : Reachable heap banks owner) (read : heap[owner]? = some object)
    (pointer : child ∈ fields object) : Reachable heap banks child := by
  apply Operational.Collection.Reachable.slot live (references := fields object) _ pointer
  simp [decode, List.getElem?_map, read]

theorem captured_frame_reached {heap : List Object} {banks : Banks} {owner saved environment}
    (present : Word.reference owner ∈ banks.continuationHandles)
    (read : heap[owner]? = some (.continuation saved environment)) : Reachable heap banks saved :=
  field_reached (captured_handle present) read (by simp [fields])

/-- Arbitrary-depth closure/frame/table paths compose, including cycles: no
fixed traversal depth or finite fixture substitutes for the reachability proof. -/
inductive Path (heap : List Object) : Nat → Nat → Prop where
  | refl (address) : Path heap address address
  | step {root owner child object} (earlier : Path heap root owner)
      (read : heap[owner]? = some object) (pointer : child ∈ fields object) : Path heap root child

theorem path_reached {heap : List Object} {banks : Banks} {root child}
    (live : Reachable heap banks root) (path : Path heap root child) : Reachable heap banks child := by
  induction path with
  | refl => exact live
  | step earlier read pointer ih => exact field_reached ih read pointer

theorem discovered_path_relocated {heap : List Object} {banks : Banks} {root child object after}
    (handle : Word.reference root ∈ banks.continuationHandles)
    (path : Path heap root child) (read : heap[child]? = some object)
    (gc : Operational.Collection.Relocation (decode heap) after (discover banks)) :
    after[gc.address child]? = some (.vector ((fields object).map gc.address)) := by
  exact gc.copied child (.vector (fields object)) (path_reached (captured_handle handle) path)
    (by simp [decode, List.getElem?_map, read])

/-- The collector's strong marking problem compiled from descriptor scans,
using the same notification semantics as nullable/FIRST. -/
def program (heap : List Object) (banks : Banks) : List (Worklist.Clause Nat) :=
  (discover banks).map (fun root => ⟨[], root⟩) ++
    heap.zipIdx.flatMap (fun (object, owner) =>
      (fields object).map (fun child => ⟨[owner], child⟩))

theorem program_member {heap : List Object} {banks : Banks} {clause : Worklist.Clause Nat} :
    clause ∈ program heap banks ↔
      (clause.dependencies = [] ∧ clause.head ∈ discover banks) ∨
      ∃ owner object, heap[owner]? = some object ∧
        clause.dependencies = [owner] ∧ clause.head ∈ fields object := by
  simp only [program, List.mem_append, List.mem_map, List.mem_flatMap]
  constructor
  · rintro (⟨root, member, equal⟩ | ⟨⟨object, owner⟩, member, child, pointer, equal⟩)
    · cases equal; exact Or.inl ⟨rfl, member⟩
    · cases equal
      exact Or.inr ⟨owner, object, List.mk_mem_zipIdx_iff_getElem?.mp member, rfl, pointer⟩
  · rintro (⟨empty, root⟩ | ⟨owner, object, read, only, pointer⟩)
    · exact Or.inl ⟨clause.head, root, by cases clause; simp_all⟩
    · exact Or.inr ⟨(object, owner), List.mk_mem_zipIdx_iff_getElem?.mpr read,
        clause.head, pointer, by cases clause; simp_all⟩

inductive Marked (heap : List Object) (banks : Banks) : Nat → Prop where
  | root {address} (present : address ∈ discover banks) : Marked heap banks address
  | field {owner child object} (live : Marked heap banks owner)
      (read : heap[owner]? = some object) (pointer : child ∈ fields object) : Marked heap banks child

theorem marking_program_exact (heap : List Object) (banks : Banks) (address : Nat) :
    Worklist.Entails (program heap banks) address ↔ Marked heap banks address := by
  constructor
  · intro proof
    induction proof with
    | rule clause member body ih =>
      rcases program_member.mp member with ⟨empty, root⟩ | ⟨owner, object, read, only, pointer⟩
      · exact Marked.root root
      · exact Marked.field (ih owner (by simp [only])) read pointer
  · intro marked
    induction marked with
    | root present =>
      apply Worklist.Entails.rule ⟨[], _⟩
      · exact program_member.mpr (Or.inl ⟨rfl, present⟩)
      · simp
    | @field owner child object earlier read pointer ih =>
      apply Worklist.Entails.rule ⟨[owner], child⟩
      · exact program_member.mpr (Or.inr ⟨owner, object, read, rfl, pointer⟩)
      · intro dependency present
        have same := List.mem_singleton.mp present
        simpa [same] using ih

theorem marking_control_flow_exact (heap : List Object) (banks : Banks) (last : Worklist.State Nat)
    (history : Worklist.Trace (program heap banks) (Worklist.initial (program heap banks)) last)
    (quiescent : last.pending = []) : ∀ address, address ∈ last.known ↔ Marked heap banks address := by
  intro address
  exact (Worklist.quiescent_exact _ _ history quiescent address).trans (marking_program_exact _ _ _)

theorem marked_reached {heap : List Object} {banks : Banks} {address : Nat}
    (marked : Marked heap banks address) : Reachable heap banks address := by
  induction marked with
  | root present => exact Operational.Collection.Reachable.root present
  | field earlier read pointer ih => exact field_reached ih read pointer

end Roots

namespace Masks
/-- The scalar and field paths share one unbounded bit denotation. -/
def callbacks (mask limit : Nat) : List Nat :=
  (List.range limit).filter (fun bit => mask.testBit bit)

@[simp] theorem callback_member (mask limit bit : Nat) :
    bit ∈ callbacks mask limit ↔ bit < limit ∧ mask.testBit bit = true := by
  simp [callbacks]

theorem callback_unique (mask limit : Nat) : (callbacks mask limit).Nodup :=
  List.nodup_range.filter _

def address (width bit : Nat) : Nat × Nat := (bit / width, bit % width)

theorem field_address_injective (width left right : Nat) (positive : 0 < width)
    (same : address width left = address width right) : left = right := by
  have quotient := congrArg Prod.fst same
  have remainder := congrArg Prod.snd same
  have l := Encoding.field_address width left positive
  have r := Encoding.field_address width right positive
  simp only [address] at quotient remainder
  rw [quotient, remainder] at l
  omega

/-- Every scalar callback has exactly one owning field and in-field position;
empty intervening fields cannot erase it or generate a duplicate callback. -/
theorem field_callback_exact (mask width bit : Nat) (positive : 0 < width) :
    bit % width < width ∧
    ((Encoding.field mask width (width * (bit / width))).testBit (bit % width) = true ↔
      mask.testBit bit = true) :=
  ⟨(Encoding.field_address width bit positive).1, Encoding.field_complete mask width bit positive⟩

/-- Slot maps use exactly the same decoder as compiler masks. -/
theorem frame_field_exact {words : List Roots.Word} {width index address map : Nat}
    (positive : 0 < width) (read : words[index]? = some (.reference address))
    (marked : (Encoding.field map width (width * (index / width))).testBit (index % width) = true) :
    address ∈ Roots.frame map 0 words := by
  apply Roots.frame_slot read
  simpa using (Encoding.field_complete map width index positive).mp marked
end Masks

namespace Hash
/-- Buckets may collide arbitrarily. Equality, not a hash match, selects an
entry. Option separates absence from the valid compiler index zero. -/
def search [DecidableEq Key] (key : Key) : List (Key × Value) → Option Value
  | [] => none
  | (candidate, value) :: rest => if key = candidate then some value else search key rest

abbrev Table (Key Value : Type) := Nat → List (Key × Value)

def get [DecidableEq Key] (hash : Key → Nat) (table : Table Key Value) (key : Key) : Option Value :=
  search key (table (hash key))

def put (hash : Key → Nat) (table : Table Key Value) (key : Key) (value : Value) : Table Key Value :=
  fun bucket => if bucket = hash key then (key, value) :: table bucket else table bucket

theorem get_put [DecidableEq Key] (hash : Key → Nat) (table : Table Key Value)
    (key : Key) (value : Value) : get hash (put hash table key value) key = some value := by
  simp [get, put, search]

theorem get_put_other [DecidableEq Key] (hash : Key → Nat) (table : Table Key Value)
    (key other : Key) (value : Value) (different : other ≠ key) :
    get hash (put hash table key value) other = get hash table other := by
  by_cases bucket : hash other = hash key <;> simp [get, put, search, bucket, different]

/-- An interner lookup is correct even for a constant hash function. -/
def intern [DecidableEq Key] (hash : Key → Nat) (table : Table Key Nat) (count : Nat) (key : Key) :
    Table Key Nat × Nat × Bool :=
  match get hash table key with
  | some index => (table, index, false)
  | none => (put hash table key count, count, true)

theorem reuse_zero [DecidableEq Key] (hash : Key → Nat) (table : Table Key Nat) (key : Key) (count : Nat)
    (present : get hash table key = some 0) : intern hash table count key = (table, 0, false) := by
  simp [intern, present]

theorem intern_lookup [DecidableEq Key] (hash : Key → Nat) (table : Table Key Nat) (count : Nat) (key : Key) :
    get hash (intern hash table count key).1 key = some (intern hash table count key).2.1 := by
  unfold intern
  split
  · assumption
  · exact get_put _ _ _ _


/-- Physical probe observations: UNUSED stops, DELETED continues, an occupied
slot compares keys. Zero values and hash collisions are ordinary payloads. -/
inductive Slot (Key Value : Type) where
  | unused
  | deleted
  | entry (key : Key) (value : Value)

def probe [DecidableEq Key] (key : Key) : List (Slot Key Value) → Option Value
  | [] => none
  | .unused :: _ => none
  | .deleted :: rest => probe key rest
  | .entry candidate value :: rest => if key = candidate then some value else probe key rest

def probeEntries : List (Slot Key Value) → List (Key × Value)
  | [] => []
  | .unused :: _ => []
  | .deleted :: rest => probeEntries rest
  | .entry key value :: rest => (key, value) :: probeEntries rest

theorem probe_refines_search [DecidableEq Key] (key : Key) (observations : List (Slot Key Value)) :
    probe key observations = search key (probeEntries observations) := by
  induction observations with
  | nil => rfl
  | cons slot rest ih =>
    cases slot with
    | unused => rfl
    | deleted => exact ih
    | entry candidate value => simpa only [probe, probeEntries, search] using congrArg (fun next => if key = candidate then some value else next) ih

theorem search_read [DecidableEq Key] {key : Key} {entries : List (Key × Value)} {value : Value}
    (found : search key entries = some value) : (key, value) ∈ entries := by
  induction entries with
  | nil => simp [search] at found
  | cons entry rest ih =>
    rcases entry with ⟨candidate, stored⟩
    by_cases same : key = candidate
    · simp [search, same] at found
      simp [same, found]
    · exact List.mem_cons_of_mem _ (ih (by simpa [search, same] using found))

def Functional (entries : List (Key × Value)) : Prop :=
  ∀ key first second, (key, first) ∈ entries → (key, second) ∈ entries → first = second

theorem search_complete [DecidableEq Key] {key : Key} {entries : List (Key × Value)} {value : Value}
    (functional : Functional entries) (member : (key, value) ∈ entries) : search key entries = some value := by
  induction entries with
  | nil => simp at member
  | cons entry rest ih =>
    rcases entry with ⟨candidate, stored⟩
    by_cases same : key = candidate
    · have equal : stored = value := functional key stored value (by simp [same]) member
      simp [search, same, equal]
    · have later : (key, value) ∈ rest := by simpa [same] using member
      have valid : Functional rest := fun k a b l r => functional k a b
        (List.mem_cons_of_mem _ l) (List.mem_cons_of_mem _ r)
      simpa [search, same] using ih valid later

/-- Rehash order and deleted slots can change. Membership preservation, not
an assumed equality of lookup results, supplies the composition premise. -/
theorem rehash_lookup [DecidableEq Key] (key : Key) (before after : List (Slot Key Value))
    (functional : Functional (probeEntries before))
    (retained : ∀ entry, entry ∈ probeEntries before ↔ entry ∈ probeEntries after) :
    probe key before = probe key after := by
  rw [probe_refines_search, probe_refines_search]
  have afterFunctional : Functional (probeEntries after) := fun k a b l r =>
    functional k a b ((retained _).mpr l) ((retained _).mpr r)
  cases result : search key (probeEntries before) with
  | some value =>
    exact (search_complete afterFunctional ((retained _).mp (search_read result))).symm
  | none =>
    cases other : search key (probeEntries after) with
    | none => rfl
    | some value =>
      have impossible := search_complete functional ((retained _).mpr (search_read other))
      rw [result] at impossible
      cases impossible

/-- Representation relation to the logical canonical key catalogue. -/
def Indexed [BEq Key] [LawfulBEq Key] [DecidableEq Key]
    (hash : Key → Nat) (table : Table Key Nat) (keys : List Key) : Prop :=
  ∀ key, get hash table key = if key ∈ keys then some (keys.idxOf key) else none

theorem intern_correspondence [BEq Key] [LawfulBEq Key] [DecidableEq Key]
    (hash : Key → Nat) (table : Table Key Nat) (keys : List Key) (key : Key)
    (indexed : Indexed hash table keys) :
    (intern hash table keys.length key).2.1 = (Construction.intern keys key).2 ∧
    ((intern hash table keys.length key).2.2 = true ↔ key ∉ keys) := by
  by_cases member : key ∈ keys
  · simp [intern, indexed key, Construction.intern, member]
  · simp [intern, indexed key, Construction.intern, member]

theorem intern_preserves_index [BEq Key] [LawfulBEq Key] [DecidableEq Key]
    (hash : Key → Nat) (table : Table Key Nat) (keys : List Key) (key : Key)
    (indexed : Indexed hash table keys) :
    Indexed hash (intern hash table keys.length key).1 (Construction.intern keys key).1 := by
  intro other
  by_cases member : key ∈ keys
  · simpa [intern, indexed key, Construction.intern, member] using indexed other
  · simp only [intern, indexed key, member, ↓reduceIte, Construction.intern]
    by_cases same : other = key
    · subst other
      simp [get_put, List.idxOf_append, member]
    · rw [get_put_other _ _ _ _ _ same, indexed other]
      by_cases previous : other ∈ keys
      · simp [previous, same, List.idxOf_append]
      · simp [previous, same]


/-- Native interning publishes a queue owner exactly on a successful insertion.
The bucket lookup supplies the ID; zero never takes the absent branch. -/
def queueStep [DecidableEq Key] (hash : Key → Nat) (table : Table Key Nat)
    (state : Operational.Canonical.Queue Key) (key : Key) :
    Operational.Canonical.Queue Key × Table Key Nat :=
  let result := intern hash table state.keys.length key
  (if result.2.2 then
    ⟨state.keys ++ [key], state.pending ++ [result.2.1], state.processed⟩ else state, result.1)

theorem queue_step_refines [BEq Key] [LawfulBEq Key] [DecidableEq Key]
    (hash : Key → Nat) (table : Table Key Nat) (state : Operational.Canonical.Queue Key) (key : Key)
    (indexed : Indexed hash table state.keys) :
    (queueStep hash table state key).1 = Operational.Canonical.intern state key := by
  by_cases member : key ∈ state.keys <;>
    simp [queueStep, intern, indexed key, Operational.Canonical.intern, member]

theorem queue_step_indexed [BEq Key] [LawfulBEq Key] [DecidableEq Key]
    (hash : Key → Nat) (table : Table Key Nat) (state : Operational.Canonical.Queue Key) (key : Key)
    (indexed : Indexed hash table state.keys) :
    Indexed hash (queueStep hash table state key).2 (queueStep hash table state key).1.keys := by
  rw [queue_step_refines _ _ _ _ indexed, Operational.Canonical.intern_keys]
  exact intern_preserves_index hash table state.keys key indexed

end Hash

namespace Dependencies
/-- Edges point from the signature owner to the vertex it reads. A concrete
core graph is obtained by mapping endpoints to dotted-core identities. -/
def reverse (edges : List (Nat × Nat)) (target : Nat) : List Nat :=
  (edges.filter (fun edge => edge.2 == target)).map Prod.fst

@[simp] theorem reverse_member (edges : List (Nat × Nat)) (owner target : Nat) :
    owner ∈ reverse edges target ↔ (owner, target) ∈ edges := by
  simp only [reverse, List.mem_map, List.mem_filter]
  constructor
  · rintro ⟨⟨source, destination⟩, ⟨member, same⟩, equal⟩
    simp only [beq_iff_eq] at same
    simpa [← equal, same] using member
  · intro member; exact ⟨(owner, target), ⟨member, by simp⟩, rfl⟩

def dirty (edges : List (Nat × Nat)) (changed : List Nat) : List Nat :=
  changed.flatMap (reverse edges)

@[simp] theorem dirty_member (edges : List (Nat × Nat)) (changed : List Nat) (owner : Nat) :
    owner ∈ dirty edges changed ↔ ∃ target ∈ changed, (owner, target) ∈ edges := by
  simp [dirty]

def project (owner : Nat → Nat) (edges : List (Nat × Nat)) : List (Nat × Nat) :=
  edges.map (fun edge => (owner edge.1, owner edge.2))

theorem projected_notification {edges : List (Nat × Nat)} {core : Nat → Nat} {changed : List Nat} {owner target}
    (read : (owner, target) ∈ edges) (split : core target ∈ changed) :
    core owner ∈ dirty (project core edges) changed := by
  apply (dirty_member _ _ _).mpr
  exact ⟨core target, split, List.mem_map.mpr ⟨(owner, target), read, rfl⟩⟩


/-- Functional semantics of vector-set!: cons each reader onto its target's
reverse row. No collision-free, duplicate-free or sorted edge premise. -/
def push (rows : Nat → List Nat) (edge : Nat × Nat) : Nat → List Nat :=
  fun target => if target = edge.2 then edge.1 :: rows target else rows target

def buildReverse : List (Nat × Nat) → (Nat → List Nat) → Nat → List Nat
  | [], rows => rows
  | edge :: rest, rows => buildReverse rest (push rows edge)

theorem built_reverse_member (edges : List (Nat × Nat)) (rows : Nat → List Nat) (owner target : Nat) :
    owner ∈ buildReverse edges rows target ↔ owner ∈ rows target ∨ (owner, target) ∈ edges := by
  induction edges generalizing rows with
  | nil => simp [buildReverse]
  | cons edge rest ih =>
    rw [buildReverse, ih]
    by_cases hit : target = edge.2
    · simp [push, hit, List.mem_cons, Prod.ext_iff, eq_comm, or_assoc, or_left_comm]
    · simp [push, hit, List.mem_cons, Prod.ext_iff]

theorem native_notification_exact (edges : List (Nat × Nat)) (changed : List Nat) (owner : Nat) :
    owner ∈ changed.flatMap (buildReverse edges (fun _ => [])) ↔ owner ∈ dirty edges changed := by
  simp [built_reverse_member, dirty]

structure Core where
  shift : Option Nat
  children : List Nat

def metadataEdges (metadata : List Core) : List (Nat × Nat) :=
  metadata.zipIdx.flatMap (fun (item, source) =>
    (item.shift.toList ++ item.children).map (fun target => (source, target)))

theorem metadata_edge_member (metadata : List Core) (source target : Nat) :
    (source, target) ∈ metadataEdges metadata ↔
      ∃ item, metadata[source]? = some item ∧ target ∈ item.shift.toList ++ item.children := by
  simp only [metadataEdges, List.mem_flatMap, List.mem_map]
  constructor
  · rintro ⟨⟨item, owner⟩, present, destination, member, equal⟩
    have owners := congrArg Prod.fst equal
    have targets := congrArg Prod.snd equal
    simp only at owners targets
    subst owner
    subst destination
    exact ⟨item, List.mk_mem_zipIdx_iff_getElem?.mp present, member⟩
  · rintro ⟨item, read, member⟩
    exact ⟨(item, source), List.mk_mem_zipIdx_iff_getElem?.mpr read, target, member, rfl⟩

theorem metadata_reader_notified {metadata : List Core} {source target : Nat} {item : Core} {changed : List Nat}
    (read : metadata[source]? = some item)
    (dependency : target ∈ item.shift.toList ++ item.children) (split : target ∈ changed) :
    source ∈ changed.flatMap (buildReverse (metadataEdges metadata) (fun _ => [])) := by
  apply (native_notification_exact _ _ _).mpr
  exact (dirty_member _ _ _).mpr ⟨target, split, (metadata_edge_member _ _ _).mpr ⟨item, read, dependency⟩⟩


/-- Native metadata stores symbol presence, not a freely authored shift ID.
A present next symbol advances one physical dotted-core slot. -/
def decodeCore (source : Nat) (symbolPresent : Bool) (children : List Nat) : Core :=
  ⟨if symbolPresent then some (source + 1) else none, children⟩

theorem decoded_shift_notified {metadata : List Core} {source : Nat} {children changed : List Nat}
    (read : metadata[source]? = some (decodeCore source true children)) (split : source + 1 ∈ changed) :
    source ∈ changed.flatMap (buildReverse (metadataEdges metadata) (fun _ => [])) := by
  apply metadata_reader_notified read _ split
  simp [decodeCore]

theorem decoded_child_notified {metadata : List Core} {source target : Nat} {symbolPresent : Bool}
    {children changed : List Nat} (read : metadata[source]? = some (decodeCore source symbolPresent children))
    (child : target ∈ children) (split : target ∈ changed) :
    source ∈ changed.flatMap (buildReverse (metadataEdges metadata) (fun _ => [])) := by
  apply metadata_reader_notified read _ split
  exact List.mem_append_right _ child

/-- Complete reverse notification preserves equality of successor signatures
on inactive owners, even if every block ID is renumbered. -/
theorem clean_outgoing_transport {old next : Nat → Nat} {edges : List (Nat × Nat)}
    {core : Nat → Nat} {changed : List Nat} {left right : Nat}
    (edge : Nat → Nat → Nat → Prop)
    (covered : ∀ source label target, edge source label target → (source, target) ∈ edges)
    (changes : ∀ vertex, Construction.SplitGroup old next vertex → core vertex ∈ changed)
    (ownership : ∀ target, next target = next left → core target = core left)
    (clean : core left ∉ dirty (project core edges) changed)
    (earlier : Construction.SameIncoming old (fun source label target => edge target label source) left right) :
    Construction.SameIncoming next (fun source label target => edge target label source) left right := by
  apply Construction.clean_predecessor_transport _ earlier
  rintro ⟨source, target, label, split, read, same⟩
  have delivered := projected_notification (covered target label source read) (changes source split)
  rw [ownership target same] at delivered
  exact clean delivered

end Dependencies
end ParserEngine.Representation
