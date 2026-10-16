import ParserEngine.Representation

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
