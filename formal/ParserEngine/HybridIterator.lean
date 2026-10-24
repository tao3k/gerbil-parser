import ParserEngine.IteratorExecutor

/-! Defunctionalization of event-program-walk/inline's native walk procedure.
Call depth, sequential loop activations and saved footer returns are explicit
proof state, not a claim that the Scheme host reifies its stack as vectors.
The fallback executes the previously verified packet interpreter. -/
namespace ParserEngine.HybridIterator
open IteratorEncoding IteratorMachine

inductive Activation where
  | call (address : Ref) (delta : Int) (moved : Bool) (depth : Nat)
  | sequence (items : List Ref) (delta : Int) (moved : Bool) (depth : Nat)
  | finish (address : Ref) (delta : Int) (moved : Bool) (context : Option Int)

structure Snapshot where
  pending : List Activation
  emitted : List Observation
  nextNode : Nat

def projectActivation : Activation → List Frame
  | .call address delta moved _ => [.next address delta moved]
  | .sequence items delta moved _ => items.map (fun item => .next item delta moved)
  | .finish address delta moved context => [.finish address delta moved context]

def project (state : Snapshot) : State :=
  ⟨state.pending.flatMap projectActivation, state.emitted, state.nextNode⟩

def initial (address : Ref) (depth : Nat := 0) : Snapshot := ⟨[.call address 0 false depth], [], 0⟩

def plan (address : Ref) (record : Record) (delta : Int) (moved : Bool) (depth : Nat)
    (rest : List Activation) (out : List Observation) (counter : Nat) : Snapshot :=
  match record with
  | .empty => ⟨rest, out, counter⟩
  | .token value => ⟨rest, out ++ [observe (.token value) delta moved none], counter⟩
  | .branch left right _ _ _ => ⟨.call left delta moved (depth + 1) :: .call right delta moved (depth + 1) :: rest, out, counter⟩
  | .view body shift => ⟨.call body (delta + shift) true (depth + 1) :: rest, out, counter⟩
  | .append leading blocks tail _ _ _ =>
    ⟨.call leading delta moved (depth + 1) :: .sequence blocks.reverse delta moved (depth + 1) ::
      .call tail delta moved (depth + 1) :: rest, out, counter⟩
  | .chunk items _ _ => ⟨.sequence items delta moved (depth + 1) :: rest, out, counter⟩
  | .value (some kind) start _ body =>
    ⟨.call body delta moved (depth + 1) :: .finish address delta moved (some (counter : Int)) :: rest,
      out ++ [observe (.openNode kind start) delta moved none], counter + 1⟩
  | .value none _ _ body =>
    ⟨.call body delta moved (depth + 1) :: .finish address delta moved none :: rest, out, counter⟩
  | .field name start _ body =>
    ⟨.call body delta moved (depth + 1) :: .finish address delta moved none :: rest,
      out ++ [observe (.openField name start) delta moved none], counter⟩

def footer (heap : Nat → Option (List Cell)) (address : Ref) (delta : Int) (moved : Bool)
    (context : Option Int) : Option Observation := do
  let record ← readRecord heap address
  match record, context with
  | .value (some kind) _ finish _, some id => pure (observe (.closeNode kind finish) delta moved (some id))
  | .value none _ finish _, none => pure (observe (.boundary finish) delta moved none)
  | .field name _ finish _, none => pure (observe (.closeField name finish) delta moved none)
  | _, _ => none

def slice (address : Ref) (delta : Int) (moved : Bool) (counter : Nat) : State :=
  ⟨[.next address delta moved], [], counter⟩

/-- Fuel bounds the verification interpreter, not the production API. The
source's depth cutoff is a separate input; its production value is 64. -/
def advance (heap : Nat → Option (List Cell)) (cutoff budget : Nat) (state : Snapshot) : Option Snapshot := do
  match state.pending with
  | [] => none
  | .call none _ _ _ :: rest => pure ⟨rest, state.emitted, state.nextNode⟩
  | .call (some address) delta moved depth :: rest =>
    if cutoff ≤ depth then
      let packet ← IteratorExecutor.run heap budget (IteratorExecutor.encode (slice (some address) delta moved state.nextNode))
      let result ← IteratorExecutor.decode packet
      if result.pending.isEmpty then pure ⟨rest, state.emitted ++ result.emitted, result.nextNode⟩ else none
    else
      let record ← readRecord heap (some address)
      pure (plan (some address) record delta moved depth rest state.emitted state.nextNode)
  | .sequence [] _ _ _ :: rest => pure ⟨rest, state.emitted, state.nextNode⟩
  | .sequence (head :: tail) delta moved depth :: rest =>
    pure ⟨.call head delta moved depth :: .sequence tail delta moved depth :: rest, state.emitted, state.nextNode⟩
  | .finish address delta moved context :: rest =>
    let result ← footer heap address delta moved context
    pure ⟨rest, state.emitted ++ [result], state.nextNode⟩

inductive Transition (heap : Nat → Option (List Cell)) (cutoff budget : Nat) : Snapshot → Snapshot → Prop where
  | empty (rest out counter delta moved depth) :
    Transition heap cutoff budget ⟨.call none delta moved depth :: rest, out, counter⟩ ⟨rest, out, counter⟩
  | direct (address delta moved depth rest out counter record)
      (below : ¬cutoff ≤ depth) (read : readRecord heap (some address) = some record) :
    Transition heap cutoff budget ⟨.call (some address) delta moved depth :: rest, out, counter⟩
      (plan (some address) record delta moved depth rest out counter)
  | fallback (address delta moved depth rest out counter packet result)
      (deep : cutoff ≤ depth)
      (executed : IteratorExecutor.run heap budget (IteratorExecutor.encode (slice (some address) delta moved counter)) = some packet)
      (decoded : IteratorExecutor.decode packet = some result) (finished : result.pending = []) :
    Transition heap cutoff budget ⟨.call (some address) delta moved depth :: rest, out, counter⟩
      ⟨rest, out ++ result.emitted, result.nextNode⟩
  | sequenceDone (rest out counter delta moved depth) :
    Transition heap cutoff budget ⟨.sequence [] delta moved depth :: rest, out, counter⟩ ⟨rest, out, counter⟩
  | sequenceNext (head tail delta moved depth rest out counter) :
    Transition heap cutoff budget ⟨.sequence (head :: tail) delta moved depth :: rest, out, counter⟩
      ⟨.call head delta moved depth :: .sequence tail delta moved depth :: rest, out, counter⟩
  | finish (address delta moved context rest out counter result)
      (returned : footer heap address delta moved context = some result) :
    Transition heap cutoff budget ⟨.finish address delta moved context :: rest, out, counter⟩
      ⟨rest, out ++ [result], counter⟩

theorem advance_transition (executed : advance heap cutoff budget before = some after) :
    Transition heap cutoff budget before after := by
  obtain ⟨pending, out, counter⟩ := before
  cases pending with
  | nil => simp [advance] at executed
  | cons task rest =>
    cases task with
    | call address delta moved depth =>
      cases address with
      | none =>
        simp [advance] at executed
        subst after
        exact .empty rest out counter delta moved depth
      | some address =>
        by_cases deep : cutoff ≤ depth
        · cases ran : IteratorExecutor.run heap budget (IteratorExecutor.encode (slice (some address) delta moved counter)) with
          | none => simp [advance, deep, ran] at executed
          | some packet =>
            cases decoded : IteratorExecutor.decode packet with
            | none => simp [advance, deep, ran, decoded] at executed
            | some result =>
              by_cases done : result.pending = []
              · simp [advance, deep, ran, decoded, done] at executed
                subst after
                exact .fallback address delta moved depth rest out counter packet result deep ran decoded done
              · simp [advance, deep, ran, decoded, done] at executed
        · cases read : readRecord heap (some address) with
          | none => simp [advance, deep, read] at executed
          | some record =>
            simp [advance, deep, read] at executed
            subst after
            exact .direct address delta moved depth rest out counter record deep read
    | sequence items delta moved depth =>
      cases items with
      | nil =>
        simp [advance] at executed
        subst after
        exact .sequenceDone rest out counter delta moved depth
      | cons head tail =>
        simp [advance] at executed
        subst after
        exact .sequenceNext head tail delta moved depth rest out counter
    | finish address delta moved context =>
      cases returned : footer heap address delta moved context with
      | none => simp [advance, returned] at executed
      | some result =>
        simp [advance, returned] at executed
        subst after
        exact .finish address delta moved context rest out counter result returned

/-- A stable vector view at index i is precisely the remaining sequence.
The native loop advances i once and passes the same child-call depth. -/
theorem sequence_index_refines (items : List Ref) (index : Nat) (head : Ref) (tail : List Ref)
    (slot : items.drop index = head :: tail) :
    advance heap cutoff budget ⟨.sequence (items.drop index) delta moved depth :: rest, out, counter⟩ =
      some ⟨.call head delta moved depth :: .sequence (items.drop (index + 1)) delta moved depth :: rest, out, counter⟩ := by
  have next : items.drop (index + 1) = tail := by
    simp [List.drop_add_one_eq_tail_drop, slot]
  simp [slot, next, advance]

theorem sequence_end_refines (items : List Ref) (index : Nat) (finished : items.length ≤ index) :
    advance heap cutoff budget ⟨.sequence (items.drop index) delta moved depth :: rest, out, counter⟩ =
      some ⟨rest, out, counter⟩ := by
  simp [List.drop_eq_nil_iff.mpr finished, advance]

def ActivationValid (certificate : Extraction.Certificate heap) : Activation → Prop
  | .call address _ _ _ => address ∈ certificate.domain
  | .sequence items _ _ _ => ∀ address ∈ items, address ∈ certificate.domain
  | .finish address delta moved context => FrameValid certificate (.finish address delta moved context)

def Valid (certificate : Extraction.Certificate heap) (state : Snapshot) : Prop :=
  ∀ task ∈ state.pending, ActivationValid certificate task

theorem project_valid (valid : Valid certificate state) : StateValid certificate (project state) := by
  intro frame member
  obtain ⟨task, inState, inTask⟩ := List.mem_flatMap.mp member
  have taskValid := valid task inState
  cases task <;> simp_all [projectActivation, ActivationValid, FrameValid]
  grind

theorem plan_valid (certificate : Extraction.Certificate heap) (address : Ref)
    (inside : address ∈ certificate.domain) (read : readRecord heap address = some record)
    (restValid : ∀ task ∈ rest, ActivationValid certificate task) :
    Valid certificate (plan address record delta moved depth rest out counter) := by
  cases record <;> simp_all [Valid, plan, ActivationValid, FrameValid]
  all_goals grind [certified_children, recordChildren]

theorem plan_simulates (certificate : Extraction.Certificate heap) (address : Ref)
    (inside : address ∈ certificate.domain) (read : readRecord heap address = some record) :
    out.map Observation.event ++ pendingMeaning certificate (project ⟨.call address delta moved depth :: rest, out, counter⟩).pending =
      (plan address record delta moved depth rest out counter).emitted.map Observation.event ++
        pendingMeaning certificate (project (plan address record delta moved depth rest out counter)).pending := by
  cases record <;>
    simp_all [plan, project, projectActivation, pendingMeaning, frameMeaning, Extraction.environment_equation,
      lowerRecord, EventIterator.meaning, observe, List.flatMap_map, List.map_append, List.append_assoc,
      EventIterator.append_order]
  case append => simp only [← List.map_reverse, List.flatMap_map, frameMeaning]
  case value kind first finish body =>
    cases kind <;> simp_all [projectActivation, frameMeaning, EventIterator.meaning, List.append_assoc]
  case chunk items start finish =>
    exact chunk_callbacks (Extraction.environment certificate) items start finish delta moved

theorem footer_executes (returned : footer heap address delta moved context = some result) :
    IteratorExecutor.advance heap ⟨.finish address delta moved context :: pending, out, counter⟩ =
      some ⟨pending, out ++ [result], counter⟩ := by
  cases read : readRecord heap address with
  | none => simp [footer, read] at returned
  | some record =>
    cases record <;> cases context <;> simp_all [footer, IteratorExecutor.advance]
    all_goals split at returned <;> simp_all

theorem fallback_refines (certificate : Extraction.Certificate heap) (address : Ref)
    (inside : address ∈ certificate.domain)
    (executed : IteratorExecutor.run heap budget (IteratorExecutor.encode (slice address delta moved counter)) = some packet)
    (decoded : IteratorExecutor.decode packet = some result) (finished : result.pending = [])
    (outer : List Frame) (prefixEvents : List Observation) :
    IteratorMachine.Trace certificate ⟨.next address delta moved :: outer, prefixEvents, counter⟩
      ⟨outer, prefixEvents ++ result.emitted, result.nextNode⟩ := by
  obtain ⟨history, _, _, _⟩ := IteratorExecutor.run_trace executed
  have valid : StateValid certificate (slice address delta moved counter) := by
    simpa [StateValid, FrameValid, slice] using inside
  obtain ⟨actual, actualDecoded, steps, _⟩ := IteratorExecutor.trace_refines certificate history
    (IteratorExecutor.decode_encode _) valid
  have same : actual = result := Option.some.inj (actualDecoded.symm.trans decoded)
  subst actual
  exact fallback_returns_to_caller steps finished outer prefixEvents

theorem transition_valid (step : Transition heap cutoff budget before after) (certificate : Extraction.Certificate heap)
    (valid : Valid certificate before) : Valid certificate after := by
  cases step <;> simp_all [Valid, ActivationValid]
  case direct address delta moved depth rest out counter record below read =>
    exact plan_valid certificate (some address) valid.1 read valid.2

theorem transition_simulates (step : Transition heap cutoff budget before after) (certificate : Extraction.Certificate heap)
    (valid : Valid certificate before) :
    before.emitted.map Observation.event ++ pendingMeaning certificate (project before).pending =
      after.emitted.map Observation.event ++ pendingMeaning certificate (project after).pending := by
  cases step with
  | empty rest out counter delta moved depth =>
    have inside : none ∈ certificate.domain := valid (.call none delta moved depth) (by simp)
    have equation := Extraction.environment_equation certificate none inside .empty rfl
    simp [project, projectActivation, pendingMeaning, frameMeaning, equation, lowerRecord, EventIterator.meaning]
  | direct address delta moved depth rest out counter record below read =>
    have inside : some address ∈ certificate.domain := valid (.call (some address) delta moved depth) (by simp)
    exact plan_simulates certificate (some address) inside read
  | fallback address delta moved depth rest out counter packet result deep executed decoded finished =>
    have inside : some address ∈ certificate.domain := valid (.call (some address) delta moved depth) (by simp)
    simpa [project, projectActivation] using trace_simulates
      (fallback_refines certificate (some address) inside executed decoded finished (rest.flatMap projectActivation) out)
  | sequenceDone | sequenceNext => simp [project, projectActivation, pendingMeaning]
  | finish address delta moved context rest out counter result returned =>
    have executed := footer_executes (pending := rest.flatMap projectActivation) (out := out) (counter := counter) returned
    have step := IteratorExecutor.advance_refines certificate (project_valid valid) executed
    exact step_simulates step

theorem plan_controls (read : readRecord heap address = some record)
    (recorded : runControl out start = some (IteratorMachine.control heap (project ⟨.call address delta moved depth :: rest, out, counter⟩))) :
    runControl (plan address record delta moved depth rest out counter).emitted start =
      some (IteratorMachine.control heap (project (plan address record delta moved depth rest out counter))) := by
  cases record <;>
    simp_all [plan, project, projectActivation, IteratorMachine.control, footerBracket, run_control_append,
      runControl, controlStep, observe, List.flatMap_map]
  case value kind first finish body =>
    cases kind <;> simp_all [projectActivation, footerBracket, run_control_append, runControl, controlStep]

theorem trace_controls_from {heap : Nat → Option (List Cell)} {certificate : Extraction.Certificate heap}
    (trace : IteratorMachine.Trace certificate before after)
    (recorded : runControl before.emitted start = some (IteratorMachine.control heap before)) :
    runControl after.emitted start = some (IteratorMachine.control heap after) := by
  induction trace with
  | refl => exact recorded
  | next history step ih => exact step_controls step ih

theorem transition_controls (step : Transition heap cutoff budget before after) (certificate : Extraction.Certificate heap)
    (valid : Valid certificate before)
    (recorded : runControl before.emitted start = some (IteratorMachine.control heap (project before))) :
    runControl after.emitted start = some (IteratorMachine.control heap (project after)) := by
  cases step with
  | empty | sequenceDone | sequenceNext =>
    simpa [project, projectActivation, IteratorMachine.control, footerBracket, List.flatMap_map, Function.comp_def] using recorded
  | direct address delta moved depth rest out counter record below read => exact plan_controls read recorded
  | fallback address delta moved depth rest out counter packet result deep executed decoded finished =>
    have inside : some address ∈ certificate.domain := valid (.call (some address) delta moved depth) (by simp)
    exact trace_controls_from
      (fallback_refines certificate (some address) inside executed decoded finished (rest.flatMap projectActivation) out) recorded
  | finish address delta moved context rest out counter result returned =>
    have executed := footer_executes (pending := rest.flatMap projectActivation) (out := out) (counter := counter) returned
    exact step_controls (IteratorExecutor.advance_refines certificate (project_valid valid) executed) recorded

def callCost (certificate : Extraction.Certificate heap) (address : Ref) : Nat :=
  4 * work (Extraction.environment certificate address) + 1

def activationWeight (certificate : Extraction.Certificate heap) : Activation → Nat
  | .call address _ _ _ => callCost certificate address
  | .sequence items _ _ _ => 1 + (items.map (callCost certificate)).sum + items.length
  | .finish _ _ _ _ => 1

def weight (certificate : Extraction.Certificate heap) (state : Snapshot) : Nat :=
  (state.pending.map (activationWeight certificate)).sum

theorem call_cost_sum (certificate : Extraction.Certificate heap) (items : List Ref) :
    (items.map (callCost certificate)).sum = 4 * refsWork certificate items + items.length := by
  induction items with
  | nil => rfl
  | cons head tail ih => simp_all [callCost, refsWork]; omega

theorem plan_decreases (certificate : Extraction.Certificate heap) (address : Ref)
    (inside : address ∈ certificate.domain) (read : readRecord heap address = some record) :
    weight certificate (plan address record delta moved depth rest out counter) <
      weight certificate ⟨.call address delta moved depth :: rest, out, counter⟩ := by
  cases record <;>
    simp_all [weight, activationWeight, callCost, plan, Extraction.environment_equation,
      lowerRecord, work, work_blocks, work_chunk, refsWork, EventIterator.appendEnvelope,
      call_cost_sum, List.map_reverse, List.sum_reverse]
  case value kind first finish body =>
    cases kind <;> simp_all [activationWeight, callCost, work] <;> omega
  all_goals try simp only [Function.comp_def] at *
  all_goals omega

theorem transition_decreases (step : Transition heap cutoff budget before after) (certificate : Extraction.Certificate heap)
    (valid : Valid certificate before) : weight certificate after < weight certificate before := by
  cases step <;> simp_all [weight, activationWeight, callCost]
  case direct address delta moved depth rest out counter record below read =>
    have inside : some address ∈ certificate.domain := valid (.call (some address) delta moved depth) (by simp)
    exact plan_decreases certificate (some address) inside read
  all_goals omega

theorem footer_available (certificate : Extraction.Certificate heap)
    (valid : FrameValid certificate (.finish address delta moved context)) :
    ∃ result, footer heap address delta moved context = some result := by
  have single : StateValid certificate ⟨[.finish address delta moved context], [], 0⟩ := by
    simpa [StateValid] using valid
  obtain ⟨after, step⟩ := IteratorMachine.progress certificate _ single (by simp)
  cases step <;> simp_all [footer]

def BudgetValid (certificate : Extraction.Certificate heap) (budget : Nat) : Prop :=
  ∀ address ∈ certificate.domain, work (Extraction.environment certificate address) < budget

theorem progress (certificate : Extraction.Certificate heap) (state : Snapshot)
    (valid : Valid certificate state) (pending : state.pending ≠ []) (bounded : BudgetValid certificate budget) :
    ∃ after, advance heap cutoff budget state = some after := by
  obtain ⟨tasks, out, counter⟩ := state
  cases tasks with
  | nil => exact False.elim (pending rfl)
  | cons task rest =>
    have first := valid task (by simp)
    cases task with
    | call address delta moved depth =>
      cases address with
      | none => exact ⟨⟨rest, out, counter⟩, rfl⟩
      | some address =>
        have inside : some address ∈ certificate.domain := first
        by_cases deep : cutoff ≤ depth
        · have sliceValid : StateValid certificate (slice (some address) delta moved counter) := by
            simpa [slice, StateValid, FrameValid] using inside
          obtain ⟨last, executed, finished⟩ := IteratorExecutor.run_complete certificate _ sliceValid budget
            (by simpa [IteratorMachine.weight, frameWeight, slice] using bounded (some address) inside)
          exact ⟨⟨rest, out ++ last.emitted, last.nextNode⟩, by simp [advance, deep, executed, IteratorExecutor.decode_encode, finished]⟩
        · obtain ⟨record, read, _, _⟩ := certificate.valid (some address) inside
          exact ⟨plan (some address) record delta moved depth rest out counter, by simp [advance, deep, read]⟩
    | sequence items delta moved depth =>
      cases items with
      | nil => exact ⟨⟨rest, out, counter⟩, rfl⟩
      | cons head tail => exact ⟨⟨.call head delta moved depth :: .sequence tail delta moved depth :: rest, out, counter⟩, rfl⟩
    | finish address delta moved context =>
      obtain ⟨result, returned⟩ := footer_available (address := address) (delta := delta) (moved := moved) (context := context) certificate first
      exact ⟨⟨rest, out ++ [result], counter⟩, by simp [advance, returned]⟩

inductive Trace (heap : Nat → Option (List Cell)) (cutoff budget : Nat) : Snapshot → Snapshot → Prop where
  | refl (state) : Trace heap cutoff budget state state
  | next : Trace heap cutoff budget first before → advance heap cutoff budget before = some after →
      Trace heap cutoff budget first after

theorem trace_valid (certificate : Extraction.Certificate heap) (trace : Trace heap cutoff budget before after)
    (valid : Valid certificate before) : Valid certificate after := by
  induction trace with
  | refl => exact valid
  | next history executed ih => exact transition_valid (advance_transition executed) certificate ih

theorem trace_simulates (certificate : Extraction.Certificate heap) (trace : Trace heap cutoff budget before after)
    (valid : Valid certificate before) :
    before.emitted.map Observation.event ++ pendingMeaning certificate (project before).pending =
      after.emitted.map Observation.event ++ pendingMeaning certificate (project after).pending := by
  induction trace with
  | refl => rfl
  | next history executed ih =>
    exact ih.trans (transition_simulates (advance_transition executed) certificate (trace_valid certificate history valid))

theorem trace_controls (certificate : Extraction.Certificate heap) (trace : Trace heap cutoff budget before after)
    (valid : Valid certificate before)
    (recorded : runControl before.emitted start = some (IteratorMachine.control heap (project before))) :
    runControl after.emitted start = some (IteratorMachine.control heap (project after)) := by
  induction trace with
  | refl => exact recorded
  | next history executed ih =>
    exact transition_controls (advance_transition executed) certificate (trace_valid certificate history valid) ih

def run (heap : Nat → Option (List Cell)) (cutoff budget : Nat) : Nat → Snapshot → Option Snapshot
  | 0, _ => none
  | fuel + 1, state =>
    if state.pending.isEmpty then some state else do
      let next ← advance heap cutoff budget state
      run heap cutoff budget fuel next

theorem run_complete (certificate : Extraction.Certificate heap) (state : Snapshot) (valid : Valid certificate state)
    (bounded : BudgetValid certificate budget) (fuel : Nat) (enough : weight certificate state < fuel) :
    ∃ last, run heap cutoff budget fuel state = some last ∧ last.pending = [] := by
  induction fuel generalizing state with
  | zero => omega
  | succ fuel ih =>
    by_cases done : state.pending = []
    · exact ⟨state, by simp [run, done], done⟩
    · obtain ⟨after, executed⟩ := progress (cutoff := cutoff) certificate state valid done bounded
      have step := advance_transition executed
      have decreased := transition_decreases step certificate valid
      obtain ⟨last, remaining, finished⟩ := ih after (transition_valid step certificate valid) (by omega)
      exact ⟨last, by simpa [run, done, executed] using remaining, finished⟩

theorem run_trace (executed : run heap cutoff budget fuel before = some after) :
    Trace heap cutoff budget before after ∧ after.pending = [] := by
  induction fuel generalizing before with
  | zero => simp [run] at executed
  | succ fuel ih =>
    by_cases done : before.pending = []
    · simp [run, done] at executed
      subst after
      exact ⟨.refl before, done⟩
    · cases step : advance heap cutoff budget before with
      | none => simp [run, done, step] at executed
      | some next =>
        obtain ⟨history, finished⟩ := ih (by simpa [run, done, step] using executed)
        have prepend : ∀ {last}, Trace heap cutoff budget next last → Trace heap cutoff budget before last := by
          intro last trace
          induction trace with
          | refl => exact .next (.refl before) step
          | next prefixTrace current ih => exact .next ih current
        exact ⟨prepend history, finished⟩

theorem complete_events (certificate : Extraction.Certificate heap) (address : Ref) (inside : address ∈ certificate.domain)
    (executed : run heap cutoff budget fuel (initial address depth) = some last) :
    last.emitted.map Observation.event = EventIterator.meaning (Extraction.environment certificate address) 0 false ∧
      runControl last.emitted ⟨0, []⟩ = some ⟨last.nextNode, []⟩ := by
  obtain ⟨history, finished⟩ := run_trace executed
  have valid : Valid certificate (initial address depth) := by simpa [Valid, initial, ActivationValid] using inside
  have events := trace_simulates certificate history valid
  have contexts := trace_controls certificate history valid rfl
  constructor
  · simpa [initial, project, projectActivation, pendingMeaning, frameMeaning, finished] using events.symm
  · simpa [initial, project, projectActivation, footerBracket, IteratorMachine.control, finished] using contexts

def fallbackBudget (certificate : Extraction.Certificate heap) : Nat :=
  1 + (certificate.domain.map fun address => work (Extraction.environment certificate address)).sum

theorem member_work_le (certificate : Extraction.Certificate heap) (items : List Ref) (address : Ref)
    (inside : address ∈ items) :
    work (Extraction.environment certificate address) ≤ (items.map fun ref => work (Extraction.environment certificate ref)).sum := by
  induction items with
  | nil => simp at inside
  | cons head tail ih =>
    simp only [List.mem_cons] at inside
    rcases inside with same | member
    · subst address; simp
    · have smaller := ih member
      simp only [List.map_cons, List.sum_cons]
      omega

theorem fallback_budget_valid (certificate : Extraction.Certificate heap) : BudgetValid certificate (fallbackBudget certificate) := by
  intro address inside
  have bounded := member_work_le certificate certificate.domain address inside
  unfold fallbackBudget
  omega

theorem initial_completes (certificate : Extraction.Certificate heap) (address : Ref) (inside : address ∈ certificate.domain)
    (cutoff depth : Nat) :
    ∃ fuel last, run heap cutoff (fallbackBudget certificate) fuel (initial address depth) = some last ∧
      last.pending = [] ∧ last.emitted.map Observation.event = EventIterator.meaning (Extraction.environment certificate address) 0 false ∧
      runControl last.emitted ⟨0, []⟩ = some ⟨last.nextNode, []⟩ := by
  have valid : Valid certificate (initial address depth) := by simpa [Valid, initial, ActivationValid] using inside
  let fuel := weight certificate (initial address depth) + 1
  obtain ⟨last, executed, finished⟩ := run_complete (cutoff := cutoff) certificate _ valid
    (fallback_budget_valid certificate) fuel (by omega)
  have output := complete_events certificate address inside executed
  exact ⟨fuel, last, executed, finished, output⟩

/-- For the artifact publisher protocol, an event and admitted control state
uniquely determine its retained footer context and next control state. -/
theorem observation_unique (left right : Observation) (start first second : Control)
    (events : left.event = right.event) (one : controlStep start left = some first)
    (two : controlStep start right = some second) : left = right ∧ first = second := by
  obtain ⟨event, leftContext⟩ := left
  obtain ⟨other, rightContext⟩ := right
  change event = other at events
  subst other
  obtain ⟨command, delta, moved⟩ := event
  obtain ⟨counter, frames⟩ := start
  cases frames with
  | nil => cases command <;> cases leftContext <;> cases rightContext <;> simp_all [controlStep]
  | cons head rest =>
    cases head <;> cases command <;> cases leftContext <;> cases rightContext <;> simp_all [controlStep]
    all_goals grind

theorem controlled_trace_unique (left right : List Observation) (start first second : Control)
    (events : left.map Observation.event = right.map Observation.event)
    (one : runControl left start = some first) (two : runControl right start = some second) :
    left = right ∧ first = second := by
  induction left generalizing right start with
  | nil =>
    have empty : right = [] := by simpa using events.symm
    subst right
    simp_all [runControl]
  | cons head tail ih =>
    cases right with
    | nil => simp at events
    | cons other more =>
      have sameEvents : head.event = other.event ∧ tail.map Observation.event = more.map Observation.event := by
        simpa using events
      cases leftStep : controlStep start head with
      | none => simp [runControl, leftStep] at one
      | some next =>
        cases rightStep : controlStep start other with
        | none => simp [runControl, rightStep] at two
        | some actual =>
          obtain ⟨sameHead, sameNext⟩ := observation_unique head other start next actual sameEvents.1 leftStep rightStep
          subst actual
          have remaining := ih more next sameEvents.2 (by simpa [runControl, leftStep] using one)
            (by simpa [runControl, rightStep] using two)
          exact ⟨by simp [sameHead, remaining.1], remaining.2⟩

theorem cutoff_independent (certificate : Extraction.Certificate heap) (address : Ref) (inside : address ∈ certificate.domain)
    (one : run heap firstCutoff firstBudget firstFuel (initial address depth) = some first)
    (two : run heap secondCutoff secondBudget secondFuel (initial address depth) = some second) :
    first.emitted = second.emitted ∧ first.nextNode = second.nextNode := by
  have firstOutput := complete_events certificate address inside one
  have secondOutput := complete_events certificate address inside two
  have unique := controlled_trace_unique first.emitted second.emitted ⟨0, []⟩ _ _
    (firstOutput.1.trans secondOutput.1.symm) firstOutput.2 secondOutput.2
  exact ⟨unique.1, congrArg Control.nextNode unique.2⟩

theorem packet_equivalent (certificate : Extraction.Certificate heap) (address : Ref) (inside : address ∈ certificate.domain)
    (hybrid : run heap cutoff budget fuel (initial address depth) = some last)
    (packet : IteratorExecutor.run heap packetFuel (IteratorExecutor.encode (IteratorMachine.initial address)) = some physical) :
    last.emitted = physical.emitted ∧ last.nextNode = physical.nextNode := by
  have hybridOutput := complete_events certificate address inside hybrid
  obtain ⟨state, decoded, steps, finished⟩ := IteratorExecutor.run_refines certificate address inside packet
  have output := IteratorExecutor.decode_observations decoded
  have physicalEvents : physical.emitted.map Observation.event = EventIterator.meaning (Extraction.environment certificate address) 0 false :=
    IteratorExecutor.complete_events certificate address inside physical packet
  have physicalContexts : runControl physical.emitted ⟨0, []⟩ = some ⟨physical.nextNode, []⟩ := by
    simpa [output.1, output.2] using complete_callbacks_bracketed steps finished
  have unique := controlled_trace_unique last.emitted physical.emitted ⟨0, []⟩ _ _
    (hybridOutput.1.trans physicalEvents.symm) hybridOutput.2 physicalContexts
  exact ⟨unique.1, congrArg Control.nextNode unique.2⟩

theorem checked_execution (heap : Nat → Option (List Cell)) (domain : List Ref) (rank : Ref → Nat)
    (checked : Extraction.verify heap domain rank = true) (address : Ref) (inside : address ∈ domain)
    (cutoff depth : Nat) :
    ∃ program budget fuel last,
      Extraction.generate heap (rank address + 1) address = some program ∧ Extract heap address program ∧
      run heap cutoff budget fuel (initial address depth) = some last ∧ last.pending = [] ∧
      last.emitted.map Observation.event = EventIterator.meaning program 0 false ∧
      runControl last.emitted ⟨0, []⟩ = some ⟨last.nextNode, []⟩ := by
  obtain ⟨certificate, sameDomain, sameRank⟩ := Extraction.verify_certificate heap domain rank checked
  have member : address ∈ certificate.domain := by simpa [sameDomain] using inside
  obtain ⟨program, generated, extracted⟩ := Extraction.generate_witness certificate address member
  obtain ⟨fuel, last, executed, finished, events, contexts⟩ := initial_completes certificate address member cutoff depth
  have meaning : Extraction.environment certificate address = program := by simp [Extraction.environment, generated]
  exact ⟨program, fallbackBudget certificate, fuel, last, by simpa [sameRank] using generated, extracted,
    executed, finished, by simpa [meaning] using events, contexts⟩

/-- The source hybrid result feeds the same complete LR request product as the
packet executor. Its physical witness is constructed here, not assumed. -/
theorem complete_request_product {heap : Nat → Option (List Cell)} (driver : RequestAST.Driver Raw) (input : List Raw)
    (last : RequestAST.State Raw) (instructions : List (RequestAST.Instruction Raw))
    (root : Publication.Value) (source : List Publication.Token) (sourceEnd : Nat)
    (certificate : Extraction.Certificate heap) (address : Ref) (result : Snapshot)
    (resolve : EventIterator.Located Command → List Publication.Op)
    (trivia : Publication.Token → Prop) (published : Publication.State) (events : List Publication.Event)
    (request : RequestAST.Trace driver (RequestAST.initial driver input) instructions last)
    (complete : last.raw.rest = [])
    (accepted : driver.engine.select (last.raw.states.headD 0) none = some (none, .accept))
    (rootShape : last.values = [[(none, Publication.decodeValue root)]])
    (inside : address ∈ certificate.domain)
    (executed : run heap cutoff budget fuel (initial address depth) = some result)
    (code : Publication.rootOps root sourceEnd = some ((EventIterator.meaning (Extraction.environment certificate address) 0 false).flatMap resolve))
    (callbacks : Callback.Trace trivia (Publication.initial source) calls
      ((result.emitted.map Observation.event).flatMap resolve) published events)
    (consumed : published.remaining = []) (closed : published.frames = []) :
    (∃ terminals, Grammar.Derives driver.engine.grammar [.nonterminal driver.engine.start] terminals ∧
      RawRequest.Relates driver.engine.realizesToken terminals input) ∧
    RequestAST.run driver.rules driver.leaf instructions [] = some [[(none, Publication.decodeValue root)]] ∧
    Publication.rootOps root sourceEnd = some ((result.emitted.map Observation.event).flatMap resolve) ∧
    Publication.tokenEntries events = Publication.numbered 0 source ∧
    published.nextNode = Publication.nodeOpens events ∧ published.frames = [] := by
  have valid : StateValid certificate (IteratorMachine.initial address) := by
    simpa [StateValid, IteratorMachine.initial, FrameValid] using inside
  obtain ⟨physical, physicalRun, _⟩ := IteratorExecutor.run_complete certificate _ valid
    (IteratorMachine.weight certificate (IteratorMachine.initial address) + 1) (by omega)
  have same := packet_equivalent certificate address inside executed physicalRun
  have physicalCallbacks : Callback.Trace trivia (Publication.initial source) calls
      (((IteratorExecutor.encode physical).emitted.map Observation.event).flatMap resolve) published events := by
    simpa [same.1] using callbacks
  have product := IteratorExecutor.complete_request_product driver input last instructions root source sourceEnd
    certificate address (IteratorExecutor.encode physical) resolve trivia published events request complete accepted rootShape
    inside physicalRun code physicalCallbacks consumed closed
  simpa [same.1] using product

end ParserEngine.HybridIterator
