import ParserEngine.HostLowering.CheckEmissionHeapCheck
import ParserEngine.Host

/-! Concrete operand tags and heap-flonum writes at the reported Gambit boundary.
HostLowering composes the host producer models with C storage; it does not assert
that an arbitrary compiler or weak-memory backend refines these transitions. -/
namespace ParserEngine.Host
open ParserEngine.Representation

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


end ParserEngine.Host
