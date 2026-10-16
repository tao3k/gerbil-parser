import ParserEngine.Representation
import Init.Data.List.Nat.TakeDrop

/-! Source adapters for Gambit frontend liveness and GCC chunk publication.
Varsets are lists modulo membership. Physical placement and compiler refinement
remain separate obligations. No C sequential-consistency axiom is introduced. -/
namespace ParserEngine.Host
open ParserEngine.Representation
namespace Liveness
inductive Expr where
  | placeholder
  | returnValue
  | ordinary (free : List Nat)
  deriving Repr, DecidableEq

def uses (tail : Bool) (ret : Nat) : Expr → List Nat
  | .placeholder => []
  | .returnValue => if tail then [ret] else []
  | .ordinary free => free

/-- compute-live-vars-at-each-expr; force-live is absent at the final boundary. -/
def before (base force : List Nat) (tail : Bool) (ret : Nat) : List Expr → List Nat
  | [] => base
  | expr :: rest => force ++ uses tail ret expr ++ before base force tail ret rest

def boundaries (base force : List Nat) (tail : Bool) (ret : Nat) : List Expr → List (List Nat)
  | [] => [base]
  | expr :: rest => before base force tail ret (expr :: rest) :: boundaries base force tail ret rest

theorem boundary_count (base force : List Nat) (tail : Bool) (ret : Nat) (exprs : List Expr) :
    (boundaries base force tail ret exprs).length = exprs.length + 1 := by
  induction exprs <;> simp_all [boundaries]

/-- Applies to the actual arg-eval-order sequence, never stale original indices. -/
theorem boundary_suffix (base force : List Nat) (tail : Bool) (ret : Nat)
    (leading suffix : List Expr) :
    (boundaries base force tail ret (leading ++ suffix))[leading.length]? =
      some (before base force tail ret suffix) := by
  induction leading with
  | nil => cases suffix <;> simp [boundaries, before]
  | cons expr rest ih => simpa [boundaries] using ih

theorem before_exact (base force : List Nat) (tail : Bool) (ret var : Nat) (exprs : List Expr) :
    var ∈ before base force tail ret exprs ↔
      var ∈ base ∨ (exprs ≠ [] ∧ var ∈ force) ∨
        ∃ expr ∈ exprs, var ∈ uses tail ret expr := by
  induction exprs with
  | nil => simp [before]
  | cons expr rest ih =>
    simp only [before, List.mem_append, ih, List.mem_cons]
    grind

/-- frame-live? returns the closed list, Scheme-truthy even when empty.
This Bool models truth only, not the value of that list. -/
def frameLive (environment : Nat) (closed live : List Nat) (var : Nat) : Bool :=
  decide (if var = environment then var ∈ live ∨ ∃ captured ∈ closed, captured ∈ live
          else var ∈ live)

theorem captured_environment_live {environment captured : Nat} {closed live : List Nat}
    (stored : captured ∈ closed) (needed : captured ∈ live) :
    frameLive environment closed live environment = true := by
  simp [frameLive, show ∃ c ∈ closed, c ∈ live from ⟨captured, stored, needed⟩]

/-- Arbitrary suffix use selects the physically placed closure environment. -/
theorem future_capture_selected {base force closed : List Nat} {tail : Bool}
    {ret environment captured index : Nat} {suffix : List Expr}
    {expr : Expr} {slots : List Nat}
    (future : expr ∈ suffix) (used : captured ∈ uses tail ret expr)
    (stored : captured ∈ closed) (placed : slots[index]? = some environment) :
    (slots.map (frameLive environment closed
      (before base force tail ret suffix)))[index]? = some true := by
  have needed := (before_exact base force tail ret captured suffix).mpr
    (Or.inr (Or.inr ⟨expr, future, used⟩))
  simp [List.getElem?_map, placed, captured_environment_live stored needed]

/-- Future use -> environment selection -> generated map root. Physical
slot-to-word identity remains explicit rather than inferred from a finite test. -/
theorem future_capture_root {base force closed : List Nat} {tail : Bool}
    {ret environment captured index root : Nat} {suffix : List Expr}
    {expr : Expr} {slots : List Nat} {words : List Roots.Word}
    (future : expr ∈ suffix) (used : captured ∈ uses tail ret expr)
    (stored : captured ∈ closed) (placed : slots[index]? = some environment)
    (read : words[index]? = some (.reference root)) :
    root ∈ Roots.frame (FrameMaps.generate 1 0
      (slots.map (frameLive environment closed (before base force tail ret suffix)))) 0 words :=
  FrameMaps.emitted_frame_root (future_capture_selected future used stored placed) read

/-- The same producer chain crosses serialization and the signed C field cursor.
Neither final bit correctness nor root retention is an input certificate. -/
theorem future_capture_signed {base force closed : List Nat} {tail : Bool}
    {ret environment captured index root width : Nat} {suffix : List Expr}
    {expr : Expr} {slots : List Nat} {words : List Roots.Word}
    (positive : 0 < width) (future : expr ∈ suffix) (used : captured ∈ uses tail ret expr)
    (stored : captured ∈ closed) (placed : slots[index]? = some environment)
    (read : words[index]? = some (.reference root)) :
    let flags := slots.map (frameLive environment closed (before base force tail ret suffix))
    let mask := FrameMaps.generate 1 0 flags
    ((BitVec.ofNat width (Encoding.field mask width (width * (index / width)))).sshiftRight
      (index % width)).getLsbD 0 = true ∧ root ∈ Roots.frame mask 0 words :=
  (FrameMaps.emitted_signed_root positive (future_capture_selected future used stored placed) read).2
end Liveness


namespace Slots
/-- extend-vars pads the front when the requested size grows, or drops the
front when it shrinks. Reversal then gives physical slot order. -/
def extend (empty : α) (slots : List α) (size : Nat) : List α :=
  List.replicate (size - slots.length) empty ++ slots.drop (slots.length - size)

def physical (empty : α) (slots : List α) (size : Nat) : List α :=
  (extend empty slots size).reverse

theorem physical_exact (empty : α) (slots : List α) (size : Nat) :
    physical empty slots size = slots.reverse.take size ++ List.replicate (size - slots.length) empty := by
  by_cases grows : slots.length ≤ size
  · simp [physical, extend, Nat.sub_eq_zero_of_le grows, List.reverse_append,
      List.take_of_length_le (show slots.reverse.length ≤ size by simpa using grows)]
  · have short : size ≤ slots.length := by omega
    have diff : slots.length - (slots.length - size) = size := by omega
    simp [physical, extend, Nat.sub_eq_zero_of_le short, List.reverse_drop, diff]

theorem physical_length (empty : α) (slots : List α) (size : Nat) :
    (physical empty slots size).length = size := by
  rw [physical_exact]
  simp only [List.length_append, List.length_take, List.length_reverse, List.length_replicate]
  omega

theorem physical_retained {empty value : α} {slots : List α} {size index : Nat}
    (inside : index < size) (source : slots.reverse[index]? = some value) :
    (physical empty slots size)[index]? = some value := by
  have old : index < slots.length := by
    by_cases bound : index < slots.length
    · exact bound
    · have absent : slots.reverse[index]? = none := List.getElem?_eq_none (by simp; omega)
      rw [absent] at source
      cases source
  rw [physical_exact, List.getElem?_append_left (by simp; omega)]
  rw [List.getElem?_take_of_lt inside]
  exact source

theorem physical_padding {empty : α} {slots : List α} {size index : Nat}
    (after : slots.length ≤ index) (inside : index < size) :
    (physical empty slots size)[index]? = some empty := by
  rw [physical_exact, List.getElem?_append_right (by simp; omega)]
  simp only [List.length_take, List.length_reverse]
  rw [List.getElem?_replicate_of_lt (by omega)]

/-- Native contexts carry size separately. The target adapter requires their
agreement; actual push/shrink branches preserve it for arbitrary contexts. -/
structure Context (α : Type) where
  declared : Nat
  slots : List α

def Valid (context : Context α) : Prop := context.declared = context.slots.length

def push (context : Context α) (value : α) : Context α :=
  ⟨context.declared + 1, value :: context.slots⟩
def shrink (context : Context α) (size : Nat) : Context α :=
  ⟨size, context.slots.drop (context.declared - size)⟩

theorem push_valid {context : Context α} (valid : Valid context) (value : α) :
    Valid (push context value) := by simpa [Valid, push] using valid

theorem shrink_valid {context : Context α} (valid : Valid context) {size : Nat}
    (inside : size ≤ context.declared) : Valid (shrink context size) := by
  simp only [Valid, shrink, List.length_drop]
  simp only [Valid] at valid
  omega

def extendDeclared (empty : α) (context : Context α) (size : Nat) : List α :=
  List.replicate (size - context.declared) empty ++ context.slots.drop (context.declared - size)

theorem declared_adapter {context : Context α} (valid : Valid context) (empty : α) (size : Nat) :
    (extendDeclared empty context size).reverse = physical empty context.slots size := by
  simp only [Valid] at valid
  simp [extendDeclared, physical, extend, valid]

/-- STK(slot-fp) uses current fp=base-fp; FP_STK subtracts its argument.
Use mathematical word addresses here; pointer validity/alignment are separate. -/
def emittedAddress (base fp slot : Int) : Int := (base - fp) - (slot - fp)
def scannedAddress (base : Int) (index : Nat) : Int := base - (index + 1 : Nat)

theorem emitted_scanned (base fp : Int) (index : Nat) :
    emittedAddress base fp (index + 1 : Nat) = scannedAddress base index := by
  simp [emittedAddress, scannedAddress]; omega

theorem collector_base (base frameSpace : Int) :
    (base - frameSpace) - (-frameSpace) = base := by omega

/-- Pointer subtraction scales by the physical word width. This identity is
independent of that width; validity of the allocated address interval is external. -/
theorem emitted_byte_address (base fp slot bytes : Int) :
    (base - bytes * fp) - bytes * (slot - fp) = base - bytes * slot := by
  rw [Int.mul_sub]
  omega

def align (size boundary : Nat) : Nat := ((size + boundary - 1) / boundary) * boundary

theorem alignment_covers (size boundary : Nat) (positive : 0 < boundary) :
    size ≤ align size boundary ∧ align size boundary < size + boundary := by
  have division := Nat.mod_add_div (size + boundary - 1) boundary
  have residue := Nat.mod_lt (size + boundary - 1) positive
  simp only [align, Nat.mul_comm] at *
  omega

def store (heap : Int → Roots.Word) (address : Int) (value : Roots.Word) : Int → Roots.Word :=
  fun where_ => if where_ = address then value else heap where_

def scanWords (heap : Int → Roots.Word) (base : Int) (size : Nat) : List Roots.Word :=
  (List.range size).map (fun index => heap (scannedAddress base index))

theorem stored_word_read (heap : Int → Roots.Word) (base fp : Int) (size index : Nat)
    (inside : index < size) (value : Roots.Word) :
    (scanWords (store heap (emittedAddress base fp (index + 1 : Nat)) value) base size)[index]? = some value := by
  rw [emitted_scanned]
  simp [scanWords, List.getElem?_map, List.getElem?_range inside, store]

/-- Discharge the previous physical read/placement premises using the source
resize/reverse operation and the generated store/C reader address equations. -/
theorem future_capture_stored {baseLive force closed slots : List Nat} {tail : Bool}
    {ret environment captured index root empty size width : Nat} {suffix : List Liveness.Expr}
    {expr : Liveness.Expr} (heap : Int → Roots.Word) (base fp : Int)
    (positive : 0 < width) (future : expr ∈ suffix) (used : captured ∈ Liveness.uses tail ret expr)
    (closedVar : captured ∈ closed) (inside : index < size)
    (source : slots.reverse[index]? = some environment) :
    let vars := physical empty slots size
    let memory := store heap (emittedAddress base fp (index + 1 : Nat)) (.reference root)
    let mask := FrameMaps.generate 1 0
      (vars.map (Liveness.frameLive environment closed (Liveness.before baseLive force tail ret suffix)))
    ((BitVec.ofNat width (Encoding.field mask width (width * (index / width)))).sshiftRight
      (index % width)).getLsbD 0 = true ∧ root ∈ Roots.frame mask 0 (scanWords memory base size) :=
  Liveness.future_capture_signed positive future used closedVar (physical_retained inside source)
    (stored_word_read heap base fp size index inside (.reference root))

/-- Internal-return order: aligned ordinary frame, saved GVM registers in
register order, return variable, then padding. -/
def internal (empty ret : α) (slots regs : List α) (aligned registers trailing : Nat) : List α :=
  physical empty slots aligned ++ physical empty regs.reverse registers ++ [ret] ++
    List.replicate trailing empty

theorem saved_register_at {empty ret value : α} {slots regs : List α}
    {aligned registers trailing index : Nat} (inside : index < registers)
    (source : regs[index]? = some value) :
    (internal empty ret slots regs aligned registers trailing)[aligned + index]? = some value := by
  simp only [internal, List.append_assoc]
  rw [List.getElem?_append_right (by rw [physical_length]; omega)]
  rw [physical_length]
  simp only [Nat.add_sub_cancel_left]
  rw [List.getElem?_append_left (by rw [physical_length]; exact inside)]
  exact physical_retained inside (by simpa using source)

theorem return_variable_at (empty ret : α) (slots regs : List α) (aligned registers trailing : Nat) :
    (internal empty ret slots regs aligned registers trailing)[aligned + registers]? = some ret := by
  simp only [internal, List.append_assoc]
  rw [List.getElem?_append_right (by rw [physical_length]; omega), physical_length]
  simp only [Nat.add_sub_cancel_left]
  rw [List.getElem?_append_right (by rw [physical_length]; exact Nat.le_refl _), physical_length]
  simp

theorem internal_length (empty ret : α) (slots regs : List α) (aligned registers trailing : Nat) :
    (internal empty ret slots regs aligned registers trailing).length = aligned + registers + 1 + trailing := by
  simp only [internal, List.length_append, physical_length, List.length_cons,
    List.length_nil, List.length_replicate]
/-- Match RETI_CFS_TO_FS using arbitrary alignment and reserve constants, not
only the installed ABI's small example. -/
def frameSpace (size reserve boundary : Nat) : Nat := align (size + reserve) boundary

theorem internal_size_matches (empty ret : α) (slots regs : List α)
    (cfs registers reserve boundary : Nat) (positive : 0 < boundary) :
    (internal empty ret slots regs (align cfs boundary) registers
      (frameSpace (registers + 1) reserve boundary - reserve - (registers + 1))).length =
      align cfs boundary + frameSpace (registers + 1) reserve boundary - reserve := by
  have room := (alignment_covers (registers + 1 + reserve) boundary positive).1
  rw [internal_length]
  unfold frameSpace at *
  omega

end Slots

namespace Acquire
/-- A location's version records all writes observed there. Mutex release
publishes the owner view; a successful acquire imports that released view. -/
abbrev View := Nat → Nat

def join (left right : View) : View := fun location => max (left location) (right location)
def put (view : View) (location value : Nat) : View :=
  fun target => if target = location then value else view target

structure State where
  current : View
  released : View
  views : Nat → View
  owner : Option Nat

def Safe (s : State) : Prop :=
  (s.owner = none → ∀ location, s.current location ≤ s.released location) ∧
  (∀ worker, s.owner = some worker → ∀ location, s.current location ≤ s.views worker location)

def initial : State := ⟨fun _ => 0, fun _ => 0, fun _ _ => 0, none⟩

theorem initial_safe : Safe initial := by simp [Safe, initial]

def write (s : State) (worker location : Nat) : State :=
  let next := s.current location + 1
  { s with
    current := put s.current location next
    views := fun w => if w = worker then put (s.views w) location next else s.views w }

def release (s : State) (worker : Nat) : State :=
  {s with owner := none, released := join s.released (s.views worker)}

def mutexAcquire (s : State) (worker : Nat) : State :=
  { s with
    owner := some worker
    views := fun w => if w = worker then join (s.views w) s.released else s.views w }

theorem write_safe {s : State} (safe : Safe s) {worker location : Nat}
    (owns : s.owner = some worker) : Safe (write s worker location) := by
  constructor
  · simp [write, owns]
  · intro w owner target
    have same : w = worker := by simpa [write, owns] using owner.symm
    subst w
    by_cases hit : target = location
    · simp [write, put, hit]
    · simpa [write, put, hit] using safe.2 worker owns target

theorem release_safe {s : State} (safe : Safe s) {worker : Nat}
    (owns : s.owner = some worker) : Safe (release s worker) := by
  constructor
  · intro _ location
    exact Nat.le_trans (safe.2 worker owns location) (Nat.le_max_right _ _)
  · simp [release]

theorem acquire_safe {s : State} (safe : Safe s) {worker : Nat}
    (free : s.owner = none) : Safe (mutexAcquire s worker) := by
  constructor
  · simp [mutexAcquire]
  · intro w owner location
    have same : worker = w := by simpa [mutexAcquire] using owner
    subst w
    apply Nat.le_trans (safe.1 free location)
    simpa [mutexAcquire, join] using (Nat.le_max_right (s.views worker location) (s.released location))

inductive Step : State → State → Prop where
  | write (s worker location) (owns : s.owner = some worker) : Step s (write s worker location)
  | release (s worker) (owns : s.owner = some worker) : Step s (release s worker)
  | acquire (s worker) (free : s.owner = none) : Step s (mutexAcquire s worker)
inductive Trace : State → State → Prop where
  | refl (s) : Trace s s
  | next {a b c} : Trace a b → Step b c → Trace a c

theorem step_safe {a b : State} (safe : Safe a) (step : Step a b) : Safe b := by
  cases step with
  | write _ _ owns => exact write_safe safe owns
  | release _ owns => exact release_safe safe owns
  | acquire _ free => exact acquire_safe safe free

theorem trace_safe {a b : State} (safe : Safe a) (trace : Trace a b) : Safe b := by
  induction trace with
  | refl => exact safe
  | next _ step ih => exact step_safe ih step

/-- Any protected location, arbitrary workers and admitted finite histories. -/
theorem owner_observes {a b : State} (safe : Safe a) (trace : Trace a b)
    {worker : Nat} (owns : b.owner = some worker) (location : Nat) :
    b.current location ≤ b.views worker location :=
  (trace_safe safe trace).2 worker owns location

/-- Complete admitted histories start from a derived valid empty state. No
final visibility invariant is supplied by the consumer. -/
theorem complete_handoff {s : State} (history : Trace initial s)
    {worker location epoch : Nat} (owns : s.owner = some worker)
    (published : epoch ≤ s.current location) : epoch ≤ s.views worker location :=
  Nat.le_trans published (owner_observes initial_safe history owns location)

/-- Plain polling only changes ownership in this weak observation projection.
It has no rule importing the released view. -/
def plainEnter (s : State) (worker : Nat) : State := {s with owner := some worker}

theorem plain_poll_counterexample :
    ∃ s : State, Safe s ∧ s.owner = none ∧ ¬ Safe (plainEnter s 1) := by
  refine ⟨⟨fun _ => 1, fun _ => 1, fun _ _ => 0, none⟩, ?_, rfl, ?_⟩
  · simp [Safe]
  · simp [Safe, plainEnter]

/-- Metadata classification is not a proof of architecture-specific lowering.
Single-thread execution has no cross-worker acquire obligation; it is never
reported as a successful SMP handoff. -/
inductive Backend where
  | singleThread
  | posixMutex
  | ticketPoll
  deriving DecidableEq, Repr

def acquireQualified : Backend → Bool
  | .posixMutex => true
  | _ => false

theorem no_smp_inference : acquireQualified .singleThread = false ∧
    acquireQualified .ticketPoll = false := by decide
end Acquire

namespace Publication
/-- Epochs count complete immutable chunks, including end marker and link.
The weak observer may see a pointer early; only admitted fences forbid that. -/
structure State where
  initialized : Nat
  visible : Nat
  tail : Nat
  deriving Repr, DecidableEq

def Safe (s : State) : Prop := s.tail ≤ s.visible ∧ s.visible ≤ s.initialized
def writeChunk (s : State) : State := {s with initialized := s.initialized + 1}
def fence (s : State) : State := {s with visible := s.initialized}
def publishTail (s : State) : State := {s with tail := s.initialized}

theorem write_safe {s : State} (h : Safe s) : Safe (writeChunk s) := by
  simp only [Safe, writeChunk] at *; omega

theorem fence_safe {s : State} (h : Safe s) : Safe (fence s) := by
  simp only [Safe, fence] at *; omega

theorem fenced_publication_safe (s : State) : Safe (publishTail (fence (writeChunk s))) := by
  simp [Safe, publishTail, fence, writeChunk]

/-- end_heap_chunk has a second barrier after updating the tail. In this
payload-safety projection that barrier stutters; wakeup progress is not claimed. -/
def endChunk (s : State) : State := fence (publishTail (fence (writeChunk s)))

theorem end_chunk_safe (s : State) : Safe (endChunk s) :=
  fence_safe (fenced_publication_safe s)

inductive Step : State → State → Prop where
  | write (s) : Step s (writeChunk s)
  | flush (s) : Step s (fence s)
  | publish (s) (ready : s.visible = s.initialized) : Step s (publishTail s)
inductive Trace : State → State → Prop where
  | refl (s) : Trace s s
  | next {a b c} : Trace a b → Step b c → Trace a c

theorem step_safe {a b : State} (h : Safe a) (step : Step a b) : Safe b := by
  cases step with
  | write => exact write_safe h
  | flush => exact fence_safe h
  | publish ready => simp [Safe, publishTail, ready]

theorem trace_safe {a b : State} (h : Safe a) (trace : Trace a b) : Safe b := by
  induction trace with
  | refl => exact h
  | next _ step ih => exact step_safe ih step

theorem observed_payload {a b : State} (h : Safe a) (trace : Trace a b)
    {chunk : Nat} (observed : chunk ≤ b.tail) : chunk ≤ b.visible :=
  Nat.le_trans observed (trace_safe h trace).1

theorem unfenced_counterexample : ¬ Safe (publishTail (writeChunk ⟨0, 0, 0⟩)) := by simp [Safe, publishTail, writeChunk]

/-- Locked recheck reads current queue, not an optimistic pre-lock head. -/
def take : List Nat → Option Nat × List Nat
  | [] => (none, [])
  | head :: rest => (some head, rest)

theorem distinct_reservations {queue : List Nat} (unique : queue.Nodup)
    {left right : Nat} (first : (take queue).1 = some left)
    (second : (take (take queue).2).1 = some right) : left ≠ right := by
  cases queue with
  | nil => simp [take] at first
  | cons head rest =>
    simp only [take, Option.some.injEq] at first
    subst left
    cases rest with
    | nil => simp [take] at second
    | cons next more =>
      simp only [take, Option.some.injEq] at second
      subst right
      have h := (List.nodup_cons.mp unique).1
      simp only [List.mem_cons, not_or] at h
      exact h.1
/-- Bridge visibility to the existing logical queued/held ownership transfer. -/
theorem reservation_visible {a b : State} (safe : Safe a) (history : Trace a b)
    {worker : CollectorSchedule.Worker} {object : Nat} {rest : List Nat}
    (head : worker.queued = object :: rest)
    (published : ∀ chunk ∈ worker.queued, chunk ≤ b.tail) :
    object ≤ b.visible ∧
      List.Perm (CollectorSchedule.work (CollectorSchedule.reserve worker object rest))
        (CollectorSchedule.work worker) := by
  refine ⟨observed_payload safe history (published object ?_),
    CollectorSchedule.reservation_retains_work worker object rest head⟩
  simp [head]
end Publication

namespace Atomic
/-- Logical decoded ticket counts. Physical fixnum wrap requires a bound. -/
def fetchAdd (current increment : Nat) : Nat × Nat := (current, current + increment)
def compareSwap (current expected replacement : Nat) : Nat × Nat :=
  (current, if current = expected then replacement else current)

theorem tickets_unique (initial first second : Nat) (different : first ≠ second) :
    (fetchAdd (initial + first) 1).1 ≠ (fetchAdd (initial + second) 1).1 := by
  simp only [fetchAdd]; omega

theorem compare_swap_success (current expected replacement : Nat) :
    (compareSwap current expected replacement).1 = expected ↔ current = expected := by rfl

theorem compare_swap_failed {current expected replacement : Nat} (h : current ≠ expected) :
    (compareSwap current expected replacement).2 = current := by simp [compareSwap, h]

/-- Physical modulo tickets are unique only within an admitted no-wrap window. -/
theorem bounded_tickets_unique {initial first second modulus : Nat}
    (left : initial + first < modulus) (right : initial + second < modulus)
    (different : first ≠ second) :
    (initial + first) % modulus ≠ (initial + second) % modulus := by
  rw [Nat.mod_eq_of_lt left, Nat.mod_eq_of_lt right]
  omega
end Atomic
end ParserEngine.Host
