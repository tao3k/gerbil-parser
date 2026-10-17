import ParserEngine.LookaheadExecution

/-! The native prediction queue has multiple initial own masks, reverse
parent edges and no FIRST-activation events. FIFO publication is shared with
the lookahead machine; no single synthetic seed replaces native initialization. -/
namespace ParserEngine.PredictionQueue
open Encoding
abbrev MState := LookaheadExecution.Machine.State
abbrev Parents := Nat → List Nat
abbrev Seeds := List (Nat × Nat)

inductive Reach (parents : Parents) (seeds : Seeds) : Nat → Nat → Prop where
  | own {owner mask bit} (member : (owner, mask) ∈ seeds) (present : mask.testBit bit = true) :
      Reach parents seeds owner bit
  | parent {child owner bit} (known : Reach parents seeds child bit) (edge : owner ∈ parents child) :
      Reach parents seeds owner bit

def flow (parents : Parents) : LookaheadExecution.Machine.Graph :=
  ⟨parents, fun _ => none, fun _ => 0, fun _ => true⟩
def blank : MState := ⟨⟨fun _ => ⟨0, 0⟩, []⟩, fun _ => false⟩
def initial (seeds : Seeds) : MState := LookaheadExecution.Machine.publications blank seeds

def callbacks (parents : Parents) (state : MState) (node : Nat) : List (Nat × Nat) :=
  (parents node).map fun target => (target, (state.queue.cells node).pending)
def step (parents : Parents) (state : MState) (node : Nat) (tail : List Nat) : MState :=
  LookaheadExecution.Machine.publications
    (LookaheadExecution.Machine.drained (flow parents) state node tail) (callbacks parents state node)

structure Basic (parents : Parents) (seeds : Seeds) (state : MState) : Prop where
  queue : QueueValid state.queue
  pending : LookaheadExecution.Machine.PendingKnown state.queue
  sound : ∀ node bit, (state.queue.cells node).known.testBit bit = true → Reach parents seeds node bit

structure Valid (parents : Parents) (seeds : Seeds) (state : MState) : Prop where
  basic : Basic parents seeds state
  own : ∀ event ∈ seeds, ∀ bit, event.2.testBit bit = true →
    (state.queue.cells event.1).known.testBit bit = true
  closed : ∀ node bit, (LookaheadExecution.Machine.sent state.queue node).testBit bit = true →
    ∀ target ∈ parents node, (state.queue.cells target).known.testBit bit = true

open LookaheadExecution.Machine in
theorem publish_basic (parents : Parents) (seeds : Seeds) (state : MState)
    (valid : Basic parents seeds state) (owner evidence : Nat)
    (origin : ∀ bit, evidence.testBit bit = true → Reach parents seeds owner bit) :
    Basic parents seeds (publish state owner evidence) := by
  refine ⟨enqueue_queue _ valid.queue _ _, enqueue_pending _ valid.pending _ _, ?_⟩
  intro node bit present
  rw [LookaheadExecution.Machine.publish, enqueue_known] at present
  split at present
  next same =>
    simp only [Nat.testBit_or, Bool.or_eq_true] at present
    rcases present with prior | fresh
    · exact valid.sound node bit prior
    · subst node; exact origin bit fresh
  next other => exact valid.sound node bit present

open LookaheadExecution.Machine in
theorem publications_basic (parents : Parents) (seeds : Seeds) (state : MState)
    (valid : Basic parents seeds state) (events : List (Nat × Nat))
    (origin : ∀ event ∈ events, ∀ bit, event.2.testBit bit = true → Reach parents seeds event.1 bit) :
    Basic parents seeds (publications state events) := by
  induction events generalizing state with
  | nil => exact valid
  | cons event rest ih =>
    apply ih (publish state event.1 event.2)
      (publish_basic parents seeds state valid _ _ (origin event (by simp)))
    intro next member; exact origin next (by simp [member])

open LookaheadExecution.Machine in
theorem initial_valid (parents : Parents) (seeds : Seeds) : Valid parents seeds (initial seeds) := by
  have base : Basic parents seeds blank := by
    refine ⟨⟨by simp [blank], ?_⟩, ?_, ?_⟩
    · intro node; simp [blank]
    · intro node bit present; simp [blank] at present
    · intro node bit present; simp [blank] at present
  refine ⟨publications_basic parents seeds blank base seeds (fun event member bit present =>
    Reach.own member present), ?_, ?_⟩
  · exact fun event member bit present => publications_cover blank seeds event member bit present
  · intro node bit present
    rw [initial, publications_sent] at present
    simp [sent, blank, difference_bit] at present

open LookaheadExecution.Machine in
theorem drained_basic (parents : Parents) (seeds : Seeds) (state : MState) (node : Nat) (tail : List Nat)
    (valid : Basic parents seeds state) (front : state.queue.work = node :: tail) :
    Basic parents seeds (drained (flow parents) state node tail) := by
  refine ⟨drain_queue_valid _ valid.queue node tail front, ?_, ?_⟩
  · intro index bit pending
    by_cases same : index = node
    · simp [drained, drain, same, clear] at pending
    · have original : (state.queue.cells index).pending.testBit bit = true := by
        simpa [drained, drain, same] using pending
      simpa [drained, drain, same] using valid.pending index bit original
  · intro index bit present
    apply valid.sound index bit
    by_cases same : index = node <;> simpa [drained, drain, clear, same] using present

open LookaheadExecution.Machine in
theorem step_monotone (parents : Parents) (state : MState) (node : Nat) (tail : List Nat)
    (target bit : Nat) (present : (state.queue.cells target).known.testBit bit = true) :
    ((step parents state node tail).queue.cells target).known.testBit bit = true := by
  apply publications_monotone
  by_cases same : target = node <;> simpa [drained, drain, clear, same] using present

open LookaheadExecution.Machine in
theorem step_valid (parents : Parents) (seeds : Seeds) (state : MState) (node : Nat) (tail : List Nat)
    (valid : Valid parents seeds state) (front : state.queue.work = node :: tail) :
    Valid parents seeds (step parents state node tail) := by
  refine ⟨?_, ?_, ?_⟩
  · apply publications_basic parents seeds _ (drained_basic parents seeds state node tail valid.basic front)
    intro event member bit present
    obtain ⟨target, edge, equal⟩ := List.mem_map.mp member
    cases equal
    exact Reach.parent (valid.basic.sound node bit (valid.basic.pending node bit present)) edge
  · intro event member bit present
    exact step_monotone parents state node tail _ _ (valid.own event member bit present)
  · intro source bit delivered target edge
    rw [step, publications_sent, drained_sent] at delivered
    by_cases same : source = node
    · subst source
      simp only [ite_true] at delivered
      cases pending : (state.queue.cells node).pending.testBit bit with
      | true =>
        exact publications_cover _ _ (target, (state.queue.cells node).pending)
          (List.mem_map.mpr ⟨target, edge, rfl⟩) bit pending
      | false =>
        have old : (sent state.queue node).testBit bit = true := by
          simp [sent, difference_bit, delivered, pending]
        exact step_monotone parents state node tail target bit (valid.closed node bit old target edge)
    · simp only [same, ite_false] at delivered
      exact step_monotone parents state node tail target bit (valid.closed source bit delivered target edge)

inductive Trace (parents : Parents) (seeds : Seeds) : MState → Prop where
  | start : Trace parents seeds (initial seeds)
  | next {state node tail} (earlier : Trace parents seeds state)
      (front : state.queue.work = node :: tail) : Trace parents seeds (step parents state node tail)

theorem trace_valid {parents seeds state} (history : Trace parents seeds state) : Valid parents seeds state := by
  induction history with
  | start => exact initial_valid parents seeds
  | next earlier front ih => exact step_valid parents seeds _ _ _ ih front

/- Queue exhaustion derives the least prediction relation; no final closure
or per-production soundness certificate is supplied by a test or caller. -/
open LookaheadExecution.Machine in
theorem quiescent_exact (parents : Parents) (seeds : Seeds) (state : MState)
    (history : Trace parents seeds state) (finished : state.queue.work = []) :
    ∀ node bit, (state.queue.cells node).known.testBit bit = true ↔ Reach parents seeds node bit := by
  have valid := trace_valid history
  intro node bit
  refine ⟨valid.basic.sound node bit, ?_⟩
  intro reached
  induction reached with
  | own member present => exact valid.own _ member _ present
  | @parent child owner bit earlier edge ih =>
    have empty : (state.queue.cells child).pending = 0 := by
      by_cases zero : (state.queue.cells child).pending = 0
      · exact zero
      · have member := (valid.basic.queue.support child).mpr zero
        simp [finished] at member
    exact valid.closed child bit (by simp [sent, difference_bit, empty, ih]) owner edge

def replay (parents : Parents) : MState → List Nat → Option MState
  | state, [] => some state
  | state, node :: rest => match state.queue.work with
    | [] => none
    | expected :: tail => if node = expected then replay parents (step parents state node tail) rest else none

theorem replay_trace (parents : Parents) (seeds : Seeds) (before after : MState) (witness : List Nat)
    (history : Trace parents seeds before) (checked : replay parents before witness = some after) :
    Trace parents seeds after := by
  induction witness generalizing before with
  | nil => cases checked; exact history
  | cons node rest ih =>
    cases front : before.queue.work with
    | nil => simp [replay, front] at checked
    | cons expected tail =>
      by_cases same : node = expected
      · subst expected
        simp only [replay, front, ↓reduceIte] at checked
        exact ih (step parents before node tail) (Trace.next history front) checked
      · simp [replay, front, same] at checked

theorem checked_exact (parents : Parents) (seeds : Seeds) (witness : List Nat) (after : MState)
    (checked : replay parents (initial seeds) witness = some after) (finished : after.queue.work = []) :
    ∀ node bit, (after.queue.cells node).known.testBit bit = true ↔ Reach parents seeds node bit :=
  quiescent_exact parents seeds after (replay_trace parents seeds _ after witness Trace.start checked) finished

/- Each drained owner removes one FIFO position. Novel bits can replenish the
queue only while decreasing the finite set of unseen owner/production pairs. -/
open LookaheadExecution.Machine LookaheadExecution.Termination in
theorem step_rank (parents : Parents) (state : MState) (node : Nat) (tail : List Nat)
    (front : state.queue.work = node :: tail) (domain : List (Nat × Nat))
    (bounded : ∀ target ∈ parents node, ∀ bit,
      (state.queue.cells node).pending.testBit bit = true → (target, bit) ∈ domain) :
    rank (step parents state node tail) domain < rank state domain := by
  apply Nat.lt_of_le_of_lt (publications_rank _ domain _ ?_) (drain_rank (flow parents) state node tail front domain)
  intro event member bit present
  obtain ⟨target, edge, equal⟩ := List.mem_map.mp member
  cases equal
  exact bounded target edge bit present

end ParserEngine.PredictionQueue
