import ParserEngine.VectorStorage

/-! Logical references model physical template identity. Fresh allocation never
writes an address reachable through an older directory entry or plan. Host
pointer encoding, allocation and vector primitives remain external premises. -/
namespace ParserEngine.PlanStorage
open ProducerAdmission
variable {Template : Type}

structure Plan where
  kernel : Nat
  reference : Nat

structure Memory (Template : Type) where
  directory : Nat → Option Nat
  objects : Nat → Option Template
  next : Nat
  plans : Nat → Option Plan
  capacity : Nat

structure Layout where
  initial : Nat
  positive : 0 < initial

def empty (layout : Layout) : Memory Template :=
  ⟨fun _ => none, fun _ => none, 0, fun _ => none, layout.initial⟩

structure Valid (make : Nat → Template) (key : Nat → Nat) (memory : Memory Template) : Prop where
  directory : ∀ mask reference, memory.directory mask = some reference →
    reference < memory.next ∧ memory.objects reference = some (make mask)
  plans : ∀ node plan, memory.plans node = some plan → memory.directory (key plan.kernel) = some plan.reference

def fetch (make : Nat → Template) (before : Memory Template) (mask : Nat) : Nat × Memory Template :=
  match before.directory mask with
  | some reference => (reference, before)
  | none => (before.next, {before with
      directory := Storage.update before.directory mask before.next
      objects := Storage.update before.objects before.next (make mask)
      next := before.next + 1})

theorem directory_retained (make : Nat → Template) (before : Memory Template) (mask old reference : Nat)
    (found : before.directory old = some reference) :
    (fetch make before mask).2.directory old = some reference := by
  cases read : before.directory mask with
  | some _ => simpa [fetch, read] using found
  | none =>
    have different : old ≠ mask := by intro same; subst old; rw [read] at found; cases found
    simpa [fetch, read, Storage.update, different] using found

theorem objects_retained (make : Nat → Template) (before : Memory Template) (mask reference : Nat)
    (old : reference < before.next) :
    (fetch make before mask).2.objects reference = before.objects reference := by
  cases read : before.directory mask <;> simp [fetch, read, Storage.update, Nat.ne_of_lt old]

/-- A cache miss cannot alias any object already reachable from the directory. -/
theorem fresh_separates (make : Nat → Template) (key : Nat → Nat) (before : Memory Template)
    (valid : Valid make key before) (mask old reference : Nat)
    (missing : before.directory mask = none) (found : before.directory old = some reference) :
    (fetch make before mask).1 ≠ reference := by
  have bound := (valid.directory old reference found).1
  simpa [fetch, missing] using Nat.ne_of_gt bound

theorem fetch_installed (make : Nat → Template) (before : Memory Template) (mask : Nat) :
    (fetch make before mask).2.directory mask = some (fetch make before mask).1 := by
  cases read : before.directory mask <;> simp [fetch, read, Storage.update]

theorem fetch_valid (make : Nat → Template) (key : Nat → Nat) (before : Memory Template)
    (valid : Valid make key before) (mask : Nat) : Valid make key (fetch make before mask).2 := by
  cases read : before.directory mask with
  | some reference => simpa [fetch, read] using valid
  | none =>
    refine ⟨?_, ?_⟩
    · intro query reference found
      by_cases same : query = mask
      · subst query
        have equal : before.next = reference := by simpa [fetch, read, Storage.update] using found
        subst reference
        simp [fetch, read, Storage.update]
      · have old : before.directory query = some reference := by simpa [fetch, read, Storage.update, same] using found
        obtain ⟨bound, object⟩ := valid.directory query reference old
        refine ⟨by simpa [fetch, read] using Nat.lt_succ_of_lt bound, ?_⟩
        rw [objects_retained make before mask reference bound]
        exact object
    · intro node plan found
      have old : before.plans node = some plan := by simpa [fetch, read] using found
      exact directory_retained make before mask _ _ (valid.plans node plan old)

def install (make : Nat → Template) (key : Nat → Nat) (before : Memory Template) (node kernel : Nat) : Memory Template :=
  let loaded := fetch make before (key kernel)
  {loaded.2 with
    plans := Storage.update loaded.2.plans node ⟨kernel, loaded.1⟩
    capacity := VectorStorage.capacity node before.capacity}

theorem install_valid (make : Nat → Template) (key : Nat → Nat) (before : Memory Template)
    (valid : Valid make key before) (node kernel : Nat) : Valid make key (install make key before node kernel) := by
  have loaded := fetch_valid make key before valid (key kernel)
  refine ⟨loaded.directory, ?_⟩
  intro query plan found
  by_cases same : query = node
  · subst query
    have equal : (⟨kernel, (fetch make before (key kernel)).1⟩ : Plan) = plan := by
      simpa [install, Storage.update] using found
    subst plan
    exact fetch_installed make before (key kernel)
  · exact loaded.plans query plan (by simpa [install, Storage.update, same] using found)

theorem install_plans (make : Nat → Template) (key : Nat → Nat) (before : Memory Template) (node kernel : Nat) :
    (install make key before node kernel).plans = Storage.update before.plans node ⟨kernel, (fetch make before (key kernel)).1⟩ := by
  cases read : before.directory (key kernel) <;> simp [install, fetch, read]

def view (memory : Memory Template) (mask : Nat) : Option Template :=
  (memory.directory mask).bind memory.objects

theorem view_valid (make : Nat → Template) (key : Nat → Nat) (memory : Memory Template)
    (valid : Valid make key memory) (mask : Nat) (value : Template) (found : view memory mask = some value) : value = make mask := by
  obtain ⟨reference, indexed, object⟩ := Option.bind_eq_some_iff.mp found
  exact Option.some.inj (object.symm.trans (valid.directory mask reference indexed).2)

theorem install_stored (make : Nat → Template) (key : Nat → Nat) (before : Memory Template)
    (valid : Valid make key before) (node kernel : Nat) :
    view (install make key before node kernel) (key kernel) = some (make (key kernel)) := by
  have indexed := fetch_installed make before (key kernel)
  have object := (fetch_valid make key before valid (key kernel)).directory _ _ indexed
  simp only [install, view, indexed, Option.bind_some]
  exact object.2

theorem install_retained (make : Nat → Template) (key : Nat → Nat) (before : Memory Template)
    (valid : Valid make key before) (node kernel mask : Nat) (value : Template)
    (found : view before mask = some value) : view (install make key before node kernel) mask = some value := by
  obtain ⟨reference, indexed, object⟩ := Option.bind_eq_some_iff.mp found
  have kept := directory_retained make before (key kernel) mask reference indexed
  simp only [install, view, kept, Option.bind_some]
  rw [objects_retained make before _ reference (valid.directory mask reference indexed).1]
  exact object

theorem install_plan (make : Nat → Template) (key : Nat → Nat) (before : Memory Template) (node kernel : Nat) :
    (install make key before node kernel).plans node = some ⟨kernel, (fetch make before (key kernel)).1⟩ := by
  simp [install, Storage.update]

theorem install_other_plan (make : Nat → Template) (key : Nat → Nat) (before : Memory Template)
    (node kernel old : Nat) (different : old ≠ node) :
    (install make key before node kernel).plans old = before.plans old := by
  cases read : before.directory (key kernel) <;> simp [install, fetch, read, Storage.update, different]

def decode (memory : Memory Template) (node : Nat) : Option (Nat × Template) :=
  (memory.plans node).bind fun plan => (memory.objects plan.reference).map fun value => (plan.kernel, value)

theorem decode_install (make : Nat → Template) (key : Nat → Nat) (before : Memory Template)
    (valid : Valid make key before) (node kernel : Nat) :
    decode (install make key before node kernel) node = some (kernel, make (key kernel)) := by
  have installed := install_valid make key before valid node kernel
  have plan := install_plan make key before node kernel
  have indexed := installed.plans _ _ plan
  have object := (installed.directory _ _ indexed).2
  simp [decode, plan, object]

theorem decode_other (make : Nat → Template) (key : Nat → Nat) (before : Memory Template)
    (valid : Valid make key before) (node kernel old : Nat) (different : old ≠ node) :
    decode (install make key before node kernel) old = decode before old := by
  rw [decode, install_other_plan _ _ _ _ _ _ different]
  cases found : before.plans old with
  | none => simp [decode, found]
  | some plan =>
    have bounded := (valid.directory _ _ (valid.plans old plan found)).1
    simp only [Option.bind_some, install]
    rw [objects_retained make before (key kernel) plan.reference bounded]
    simp [decode, found]

/-- Equal prediction masks share the same immutable object reference even
when installed in different physical plan slots for distinct kernels. -/
theorem shared_reference (make : Nat → Template) (key : Nat → Nat) (memory : Memory Template)
    (valid : Valid make key memory) (left right : Nat) (a b : Plan)
    (one : memory.plans left = some a) (two : memory.plans right = some b)
    (shared : key a.kernel = key b.kernel) : a.reference = b.reference := by
  have first := valid.plans left a one
  have second := valid.plans right b two
  rw [shared] at first
  exact Option.some.inj (first.symm.trans second)

/-- Embed the physical plan function in a bounded vector. This is an encoding
law, not a premise asserting equality of some arbitrary final vector. -/
def vectorize (memory : Memory Template) (capacity : Nat) : List (Option Plan) :=
  (List.range capacity).map memory.plans

def decodeVector (objects : Nat → Option Template) (slots : List (Option Plan)) (node : Nat) : Option (Nat × Template) :=
  (slots[node]?.join).bind fun plan => (objects plan.reference).map fun value => (plan.kernel, value)

theorem vector_read (memory : Memory Template) (capacity node : Nat) (inside : node < capacity) :
    (vectorize memory capacity)[node]? = some (memory.plans node) := by
  have bound : node < (List.range capacity).length := by simpa using inside
  have read : (List.range capacity)[node]? = some node := by
    rw [List.getElem?_eq_getElem bound]
    simp
  simp only [vectorize, List.getElem?_map, read, Option.map_some]

theorem vector_decode (memory : Memory Template) (capacity node : Nat) (inside : node < capacity) :
    decodeVector memory.objects (vectorize memory capacity) node = decode memory node := by
  simp [decodeVector, vector_read memory capacity node inside, decode]

theorem grow_decode (objects : Nat → Option Template) (slots : List (Option Plan)) (node : Nat)
    (old : node < slots.length) :
    decodeVector objects (StateInterning.grow slots) node = decodeVector objects slots node := by
  simp only [decodeVector, StateInterning.grow_retains slots node old]

/-- The copy/write vector is produced alongside logical plan installation.
Captured IDs advance once per append, starting before initial installation. -/
inductive Trace (make : Nat → Template) (key : Nat → Nat) : Memory Template → Nat → List (Option Plan) → Prop where
  | start (layout : Layout) : Trace make key (empty layout) 0 (List.replicate layout.initial none)
  | next {memory count slots} (earlier : Trace make key memory count slots) (kernel : Nat) :
      Trace make key (install make key memory count kernel) (count + 1)
        (VectorStorage.append slots count ⟨kernel, (fetch make memory (key kernel)).1⟩)

theorem trace_shape {make : Nat → Template} {key : Nat → Nat} {memory : Memory Template} {count : Nat} {slots : List (Option Plan)}
    (trace : Trace make key memory count slots) : memory.capacity = slots.length ∧ count ≤ slots.length ∧
      0 < slots.length ∧ ∀ node, node < count → slots[node]?.join = memory.plans node := by
  induction trace with
  | start layout => simp [empty, layout.positive]
  | @next memory count slots earlier kernel ih =>
    have room := VectorStorage.room count slots.length ih.2.1 ih.2.2.1
    refine ⟨?_, ?_, ?_, ?_⟩
    · simpa only [install, VectorStorage.append_length] using congrArg (VectorStorage.capacity count) ih.1
    · simpa only [VectorStorage.append_length] using Nat.succ_le_of_lt room
    · simp only [VectorStorage.append_length]; omega
    · rw [install_plans]
      exact VectorStorage.append_refines slots memory.plans count _ ih.2.1 ih.2.2.1 ih.2.2.2

theorem trace_decode {make : Nat → Template} {key : Nat → Nat} {memory : Memory Template} {count : Nat} {slots : List (Option Plan)}
    (trace : Trace make key memory count slots) (node : Nat) (inside : node < count) :
    decodeVector memory.objects slots node = decode memory node := by
  simp only [decodeVector, (trace_shape trace).2.2.2 node inside, decode]

end ParserEngine.PlanStorage
