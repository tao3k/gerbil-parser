import ParserEngine.Extraction
import ParserEngine.Callback

/-! Grouped small steps of the parser's physical event loop. Current registers
are projected as the first next/finish frame; pending vectors decode to the
remaining frames. Primitive record reads and normal-return callback effects are
external. All record dispatch and resume branches below are explicit. -/
namespace ParserEngine.IteratorMachine
open IteratorEncoding

structure Observation where
  event : EventIterator.Located Command
  context : Option Int

structure State where
  pending : List Frame
  emitted : List Observation
  nextNode : Nat

def initial (address : Ref) : State := ⟨[.next address 0 false], [], 0⟩

def observe (command : Command) (delta : Int) (moved : Bool) (context : Option Int) : Observation :=
  ⟨⟨command, delta, moved⟩, context⟩

def frameMeaning (certificate : Extraction.Certificate heap) : Frame → List (EventIterator.Located Command)
  | .next address delta moved => EventIterator.meaning (Extraction.environment certificate address) delta moved
  | .chunk address index delta moved =>
      match readRecord heap address with
      | some (.chunk items _ _) => (items.drop index.toNat).flatMap fun item =>
          EventIterator.meaning (Extraction.environment certificate item) delta moved
      | _ => []
  | .finish address delta moved _ =>
      match readRecord heap address with
      | some (.value (some kind) _ finish _) => [⟨.closeNode kind finish, delta, moved⟩]
      | some (.value none _ finish _) => [⟨.boundary finish, delta, moved⟩]
      | some (.field name _ finish _) => [⟨.closeField name finish, delta, moved⟩]
      | _ => []

def pendingMeaning (certificate : Extraction.Certificate heap) (frames : List Frame) :=
  frames.flatMap (frameMeaning certificate)

/-- Registers and the vector stack form one sequence of work. These grouped
steps include record dispatch and restore the current register after each pop;
there is no assumption that a physical transition already is a Rope.Step. -/
inductive Step (certificate : Extraction.Certificate heap) : State → State → Prop where
  | empty (address rest out counter) (inside : address ∈ certificate.domain)
      (read : readRecord heap address = some .empty) (delta moved) :
      Step certificate ⟨.next address delta moved :: rest, out, counter⟩ ⟨rest, out, counter⟩
  | token (address rest out counter) (inside : address ∈ certificate.domain)
      (read : readRecord heap address = some (.token value)) (delta moved) :
      Step certificate ⟨.next address delta moved :: rest, out, counter⟩
        ⟨rest, out ++ [observe (.token value) delta moved none], counter⟩
  | branch (address rest out counter) (inside : address ∈ certificate.domain)
      (read : readRecord heap address = some (.branch left right start finish height)) (delta moved) :
      Step certificate ⟨.next address delta moved :: rest, out, counter⟩
        ⟨.next left delta moved :: .next right delta moved :: rest, out, counter⟩
  | view (address rest out counter) (inside : address ∈ certificate.domain)
      (read : readRecord heap address = some (.view body shift)) (delta moved) :
      Step certificate ⟨.next address delta moved :: rest, out, counter⟩
        ⟨.next body (delta + shift) true :: rest, out, counter⟩
  | append (address rest out counter) (inside : address ∈ certificate.domain)
      (read : readRecord heap address = some (.append leading blocks tail start finish height)) (delta moved) :
      Step certificate ⟨.next address delta moved :: rest, out, counter⟩
        ⟨.next leading delta moved :: pushBlocks blocks delta moved (.next tail delta moved :: rest), out, counter⟩
  | chunk (address rest out counter) (inside : address ∈ certificate.domain)
      (read : readRecord heap address = some (.chunk (head :: tail) start finish)) (delta moved) :
      Step certificate ⟨.next address delta moved :: rest, out, counter⟩
        ⟨.next head delta moved :: .chunk address 1 delta moved :: rest, out, counter⟩
  | chunkLast (address rest out counter) (read : readRecord heap address = some (.chunk items start finish))
      (index : Nat) (slot : items.drop index = [head]) (delta moved) :
      Step certificate ⟨.chunk address (index : Int) delta moved :: rest, out, counter⟩
        ⟨.next head delta moved :: rest, out, counter⟩
  | chunkMore (address rest out counter) (read : readRecord heap address = some (.chunk items start finish))
      (index : Nat) (slot : items.drop index = head :: next :: tail) (delta moved) :
      Step certificate ⟨.chunk address (index : Int) delta moved :: rest, out, counter⟩
        ⟨.next head delta moved :: .chunk address ((index + 1 : Nat) : Int) delta moved :: rest, out, counter⟩
  | node (address rest out counter) (inside : address ∈ certificate.domain)
      (read : readRecord heap address = some (.value (some kind) start finish body)) (delta moved) :
      Step certificate ⟨.next address delta moved :: rest, out, counter⟩
        ⟨.next body delta moved :: .finish address delta moved (some (counter : Int)) :: rest,
          out ++ [observe (.openNode kind start) delta moved none], counter + 1⟩
  | fragment (address rest out counter) (inside : address ∈ certificate.domain)
      (read : readRecord heap address = some (.value none start finish body)) (delta moved) :
      Step certificate ⟨.next address delta moved :: rest, out, counter⟩
        ⟨.next body delta moved :: .finish address delta moved none :: rest, out, counter⟩
  | field (address rest out counter) (inside : address ∈ certificate.domain)
      (read : readRecord heap address = some (.field name start finish body)) (delta moved) :
      Step certificate ⟨.next address delta moved :: rest, out, counter⟩
        ⟨.next body delta moved :: .finish address delta moved none :: rest,
          out ++ [observe (.openField name start) delta moved none], counter⟩
  | closeNode (address rest out counter id) (read : readRecord heap address = some (.value (some kind) start finish body))
      (delta moved) :
      Step certificate ⟨.finish address delta moved (some id) :: rest, out, counter⟩
        ⟨rest, out ++ [observe (.closeNode kind finish) delta moved (some id)], counter⟩
  | boundary (address rest out counter) (read : readRecord heap address = some (.value none start finish body))
      (delta moved) :
      Step certificate ⟨.finish address delta moved none :: rest, out, counter⟩
        ⟨rest, out ++ [observe (.boundary finish) delta moved none], counter⟩
  | closeField (address rest out counter) (read : readRecord heap address = some (.field name start finish body))
      (delta moved) :
      Step certificate ⟨.finish address delta moved none :: rest, out, counter⟩
        ⟨rest, out ++ [observe (.closeField name finish) delta moved none], counter⟩

theorem step_simulates (step : Step certificate before after) :
    before.emitted.map Observation.event ++ pendingMeaning certificate before.pending =
      after.emitted.map Observation.event ++ pendingMeaning certificate after.pending := by
  cases step <;>
    simp_all [pendingMeaning, frameMeaning, Extraction.environment_equation, lowerRecord,
      EventIterator.meaning, observe, pushed_blocks_order, EventIterator.append_order,
      List.append_assoc, List.map_append]
  case append => simp only [← List.map_reverse, List.flatMap_map, frameMeaning]
  case chunk => exact chunk_callbacks (Extraction.environment certificate) _ 0 0 _ _
  case chunkMore => simp_all only [List.drop_add_one_eq_tail_drop, List.tail_cons, List.flatMap_cons, List.append_assoc]

inductive Trace (certificate : Extraction.Certificate heap) : State → State → Prop where
  | refl (state) : Trace certificate state state
  | next : Trace certificate first before → Step certificate before after → Trace certificate first after

theorem trace_simulates (trace : Trace certificate before after) :
    before.emitted.map Observation.event ++ pendingMeaning certificate before.pending =
      after.emitted.map Observation.event ++ pendingMeaning certificate after.pending := by
  induction trace with
  | refl => rfl
  | next history step ih => exact ih.trans (step_simulates step)

theorem complete_machine (trace : Trace certificate (initial address) last) (finished : last.pending = []) :
    last.emitted.map Observation.event = EventIterator.meaning (Extraction.environment certificate address) 0 false := by
  simpa [initial, pendingMeaning, frameMeaning, finished] using (trace_simulates trace).symm

/-- Structural normalization measure, not a CPU/allocation performance model. -/
def work : Program → Nat
  | .empty | .atom _ => 1
  | .branch left right => 2 + work left + work right
  | .view body _ => 1 + work body

def refsWork (certificate : Extraction.Certificate heap) (items : List Ref) : Nat :=
  (items.map (fun ref => work (Extraction.environment certificate ref))).sum

theorem work_chunk (child : Ref → Program) (items : List Ref) :
    work (items.foldr (fun item rest => .branch (child item) rest) .empty) =
      (items.map (fun ref => work (child ref))).sum + 2 * items.length + 1 := by
  induction items with
  | nil => rfl
  | cons head rest ih => simp [work, ih]; omega

theorem work_blocks (blocks : List Program) (tail : Program) :
    work (blocks.foldl (fun rest block => .branch block rest) tail) =
      work tail + (blocks.map work).sum + 2 * blocks.length := by
  induction blocks generalizing tail with
  | nil => simp
  | cons block rest ih => simp [work, ih]; omega

def frameWeight (certificate : Extraction.Certificate heap) : Frame → Nat
  | .next address _ _ => work (Extraction.environment certificate address)
  | .finish _ _ _ _ => 1
  | .chunk address index _ _ =>
      match readRecord heap address with
      | some (.chunk items _ _) => refsWork certificate (items.drop index.toNat) + 2 * (items.drop index.toNat).length + 1
      | _ => 1

def weight (certificate : Extraction.Certificate heap) (state : State) : Nat :=
  (state.pending.map (frameWeight certificate)).sum

theorem step_decreases (step : Step certificate before after) : weight certificate after < weight certificate before := by
  cases step <;>
    simp_all [weight, frameWeight, Extraction.environment_equation, lowerRecord, work,
      work_chunk, work_blocks, EventIterator.appendEnvelope, pushed_blocks_order,
      refsWork, List.map_append, List.map_map, List.drop_add_one_eq_tail_drop, Function.comp_def]
  all_goals omega

theorem physical_steps_wellFounded (certificate : Extraction.Certificate heap) :
    WellFounded (fun after before => Step certificate before after) := by
  exact Subrelation.wf (fun step => step_decreases step) (measure (weight certificate)).wf

inductive Bracket where
  | node (id : Int) (kind : String)
  | field (name : String)
  deriving DecidableEq

structure Control where
  nextNode : Nat
  frames : List Bracket
  deriving DecidableEq

def footerBracket (heap : Nat → Option (List Cell)) : Frame → List Bracket
  | .finish address _ _ context =>
      match readRecord heap address with
      | some (.value (some kind) _ _ _) => context.toList.map (fun id => .node id kind)
      | some (.field name _ _ _) => if context.isNone then [.field name] else []
      | _ => []
  | _ => []

def control (heap : Nat → Option (List Cell)) (state : State) : Control :=
  ⟨state.nextNode, state.pending.flatMap (footerBracket heap)⟩

/-- Reference callback bracketing, recovered from saved native footers. This
is proof state only; no second stack is added to the native implementation. -/
def controlStep (state : Control) (observation : Observation) : Option Control :=
  match observation.event.value with
  | .openNode kind _ =>
      if observation.context.isNone then
        some ⟨state.nextNode + 1, .node (state.nextNode : Int) kind :: state.frames⟩ else none
  | .closeNode kind _ =>
      match state.frames, observation.context with
      | .node id expected :: rest, some actual =>
          if id = actual ∧ expected = kind then some ⟨state.nextNode, rest⟩ else none
      | _, _ => none
  | .openField name _ =>
      if observation.context.isNone then some ⟨state.nextNode, .field name :: state.frames⟩ else none
  | .closeField name _ =>
      match state.frames with
      | .field expected :: rest =>
          if expected = name ∧ observation.context.isNone then some ⟨state.nextNode, rest⟩ else none
      | _ => none
  | _ => if observation.context.isNone then some state else none

def runControl : List Observation → Control → Option Control
  | [], state => some state
  | observation :: rest, state => (controlStep state observation).bind (runControl rest)

theorem run_control_append (left right : List Observation) (state : Control) :
    runControl (left ++ right) state = (runControl left state).bind (runControl right) := by
  induction left generalizing state with
  | nil => rfl
  | cons head rest ih =>
    cases result : controlStep state head with
    | none => simp [runControl, result]
    | some next => simpa only [List.cons_append, runControl, result, Option.bind_some] using ih next

theorem step_controls {heap : Nat → Option (List Cell)}
    {certificate : Extraction.Certificate heap} (step : Step certificate before after)
    (recorded : runControl before.emitted start = some (control heap before)) :
    runControl after.emitted start = some (control heap after) := by
  cases step <;>
    simp_all [run_control_append, control, footerBracket, runControl, controlStep, observe,
      pushed_blocks_order, List.flatMap_append]

theorem trace_controls {heap : Nat → Option (List Cell)}
    {certificate : Extraction.Certificate heap} (trace : Trace certificate (initial address) last) :
    runControl last.emitted ⟨0, []⟩ = some (control heap last) := by
  induction trace with
  | refl => rfl
  | next history step ih => exact step_controls step ih

theorem complete_callbacks_bracketed {heap : Nat → Option (List Cell)}
    {certificate : Extraction.Certificate heap} (trace : Trace certificate (initial address) last)
    (finished : last.pending = []) : runControl last.emitted ⟨0, []⟩ = some ⟨last.nextNode, []⟩ := by
  simpa [control, finished] using trace_controls trace

structure Registers where
  code : Ref
  finish : Bool
  delta : Int
  moved : Bool
  context : Option Int

def registerFrame (registers : Registers) : Frame :=
  if registers.finish then .finish registers.code registers.delta registers.moved registers.context
  else .next registers.code registers.delta registers.moved

def projectRegisters (registers : Registers) (packets : List (List Cell)) : Option (List Frame) := do
  let pending ← decodeStack packets
  pure (registerFrame registers :: pending)

/-- Reifying suspended caller activations at the depth fallback preserves all
control fields, including zero callback results and cancelled-view provenance. -/
theorem fallback_projection (registers : Registers) (activations : List Frame) :
    projectRegisters registers (activations.map encodeFrame) =
      some (registerFrame registers :: activations) := by
  simp [projectRegisters, stack_roundtrip]

def extend (state : State) (outer : List Frame) (prefixEvents : List Observation) : State :=
  {state with pending := state.pending ++ outer, emitted := prefixEvents ++ state.emitted}

theorem push_blocks_suffix (blocks : List Ref) (delta : Int) (moved : Bool) (pending outer : List Frame) :
    pushBlocks blocks delta moved (pending ++ outer) = pushBlocks blocks delta moved pending ++ outer := by
  simp [pushed_blocks_order, List.append_assoc]

theorem step_under_suffix (step : Step certificate before after) (outer : List Frame)
    (prefixEvents : List Observation) : Step certificate (extend before outer prefixEvents) (extend after outer prefixEvents) := by
  cases step <;> simp only [extend, List.cons_append, ← List.append_assoc, ← push_blocks_suffix]
  all_goals first
    | apply Step.empty
    | apply Step.token
    | apply Step.branch
    | apply Step.view
    | apply Step.append
    | apply Step.chunk
    | apply Step.chunkLast
    | apply Step.chunkMore
    | apply Step.node
    | apply Step.fragment
    | apply Step.field
    | apply Step.closeNode
    | apply Step.boundary
    | apply Step.closeField
  all_goals assumption

theorem trace_under_suffix (trace : Trace certificate before after) (outer : List Frame)
    (prefixEvents : List Observation) : Trace certificate (extend before outer prefixEvents) (extend after outer prefixEvents) := by
  induction trace with
  | refl => exact .refl _
  | next history step ih => exact .next ih (step_under_suffix step outer prefixEvents)

theorem fallback_returns_to_caller
    (trace : Trace certificate ⟨[.next address delta moved], [], counter⟩ last)
    (finished : last.pending = []) (outer : List Frame) (prefixEvents : List Observation) :
    Trace certificate ⟨.next address delta moved :: outer, prefixEvents, counter⟩
      ⟨outer, prefixEvents ++ last.emitted, last.nextNode⟩ := by
  simpa [extend, finished] using trace_under_suffix trace outer prefixEvents

/-- The generated heap environment and physical machine replace the earlier
assumed abstract iterator trace at the complete-request/publication seam. -/
theorem complete_request_product {heap : Nat → Option (List Cell)} (driver : RequestAST.Driver Raw) (input : List Raw)
    (last : RequestAST.State Raw) (instructions : List (RequestAST.Instruction Raw))
    (root : Publication.Value) (source : List Publication.Token) (sourceEnd : Nat)
    (certificate : Extraction.Certificate heap) (address : Ref) (machine : State) (resolve : EventIterator.Located Command → List Publication.Op)
    (trivia : Publication.Token → Prop) (published : Publication.State) (events : List Publication.Event)
    (request : RequestAST.Trace driver (RequestAST.initial driver input) instructions last)
    (complete : last.raw.rest = [])
    (accepted : driver.engine.select (last.raw.states.headD 0) none = some (none, .accept))
    (rootShape : last.values = [[(none, Publication.decodeValue root)]])
    (walk : Trace certificate (initial address) machine) (finished : machine.pending = [])
    (code : Publication.rootOps root sourceEnd = some ((EventIterator.meaning (Extraction.environment certificate address) 0 false).flatMap resolve))
    (callbacks : Callback.Trace trivia (Publication.initial source) calls
      ((machine.emitted.map Observation.event).flatMap resolve) published events)
    (consumed : published.remaining = []) (closed : published.frames = []) :
    (∃ terminals, Grammar.Derives driver.engine.grammar [.nonterminal driver.engine.start] terminals ∧
      RawRequest.Relates driver.engine.realizesToken terminals input) ∧
    RequestAST.run driver.rules driver.leaf instructions [] = some [[(none, Publication.decodeValue root)]] ∧
    Publication.rootOps root sourceEnd = some ((machine.emitted.map Observation.event).flatMap resolve) ∧
    Publication.tokenEntries events = Publication.numbered 0 source ∧
    published.nextNode = Publication.nodeOpens events ∧ published.frames = [] := by
  let rope := Extraction.environment certificate address
  let iterator : EventIterator.State Command := ⟨[], EventIterator.meaning rope 0 false⟩
  have callbackMeaning : Callback.Trace trivia (Publication.initial source) calls
      ((EventIterator.meaning rope 0 false).flatMap resolve) published events := by
    simpa [complete_machine walk finished, rope] using callbacks
  have combined := Callback.complete_request_product driver input last instructions root source sourceEnd
    rope iterator resolve trivia published events request complete accepted rootShape
    (EventIterator.iterator_total rope) rfl code callbackMeaning consumed closed
  simpa [iterator, rope, complete_machine walk finished] using combined

theorem certified_children (certificate : Extraction.Certificate heap) (address : Ref)
    (inside : address ∈ certificate.domain) (read : readRecord heap address = some record)
    (child : Ref) (member : child ∈ recordChildren record) : child ∈ certificate.domain := by
  obtain ⟨actual, actualRead, admitted, children⟩ := certificate.valid address inside
  have same : actual = record := Option.some.inj (actualRead.symm.trans read)
  subst actual
  exact (children child member).1

theorem certified_admission (certificate : Extraction.Certificate heap) (address : Ref)
    (inside : address ∈ certificate.domain) (read : readRecord heap address = some record) :
    recordAdmitted address record := by
  obtain ⟨actual, actualRead, admitted, _⟩ := certificate.valid address inside
  have same : actual = record := Option.some.inj (actualRead.symm.trans read)
  subst actual
  exact admitted

theorem drop_head_member (items : List Ref) (index : Nat) (head : Ref) (tail : List Ref)
    (slot : items.drop index = head :: tail) : head ∈ items := by
  exact List.mem_of_mem_drop (i := index) (by simp [slot])

theorem drop_two_bound (items : List Ref) (index : Nat) (head next : Ref) (tail : List Ref)
    (slot : items.drop index = head :: next :: tail) : index + 1 < items.length := by
  have sizes := congrArg List.length slot
  simp only [List.length_drop, List.length_cons] at sizes
  omega

def FrameValid (certificate : Extraction.Certificate heap) : Frame → Prop
  | .next address _ _ => address ∈ certificate.domain
  | .chunk address index _ _ => address ∈ certificate.domain ∧ ∃ (items : List Ref) (start finish : Int) (position : Nat),
      readRecord heap address = some (.chunk items start finish) ∧ index = (position : Int) ∧ position < items.length
  | .finish address _ _ context => address ∈ certificate.domain ∧
      match readRecord heap address with
      | some (.value (some _) _ _ _) => context.isSome = true
      | some (.value none _ _ _) | some (.field _ _ _ _) => context.isNone = true
      | _ => False

def StateValid (certificate : Extraction.Certificate heap) (state : State) : Prop :=
  ∀ frame ∈ state.pending, FrameValid certificate frame

theorem step_preserves_validity (step : Step certificate before after) (valid : StateValid certificate before) :
    StateValid certificate after := by
  cases step <;>
    simp_all [StateValid, FrameValid, pushed_blocks_order, List.mem_append, List.mem_map]
  case chunk head tail start finish address rest out counter inside read delta moved =>
    have admitted := certified_admission certificate address inside read
    have child := certified_children certificate address inside read head (by simp [recordChildren])
    refine ⟨child, 1, rfl, ?_⟩
    have width := admitted.2
    simp only [List.length_cons] at width
    omega
  case chunkMore items start finish head next tail address rest out counter read index slot delta moved =>
    have child := certified_children certificate address valid.1.1 read head
      (by simpa [recordChildren] using drop_head_member items index head (next :: tail) slot)
    exact ⟨child, index + 1, by omega, drop_two_bound items index head next tail slot⟩
  all_goals grind [certified_children, recordChildren, drop_head_member]

theorem progress (certificate : Extraction.Certificate heap) (state : State)
    (valid : StateValid certificate state) (pending : state.pending ≠ []) :
    ∃ after, Step certificate state after := by
  obtain ⟨tasks, out, counter⟩ := state
  cases tasks with
  | nil => exact False.elim (pending rfl)
  | cons frame rest =>
    have first := valid frame (by simp)
    cases frame with
    | next address delta moved =>
      obtain ⟨record, read, admitted, _⟩ := certificate.valid address first
      cases record with
      | empty => exact ⟨_, .empty address rest out counter first read delta moved⟩
      | token value => exact ⟨_, .token address rest out counter first read delta moved⟩
      | branch left right start finish height => exact ⟨_, .branch address rest out counter first read delta moved⟩
      | view body shift => exact ⟨_, .view address rest out counter first read delta moved⟩
      | append leading blocks tail start finish height => exact ⟨_, .append address rest out counter first read delta moved⟩
      | chunk items start finish =>
        cases items with
        | nil => simp [recordAdmitted] at admitted
        | cons head tail => exact ⟨_, .chunk address rest out counter first read delta moved⟩
      | value kind start finish body =>
        cases kind with
        | none => exact ⟨_, .fragment address rest out counter first read delta moved⟩
        | some kind => exact ⟨_, .node address rest out counter first read delta moved⟩
      | field name start finish body => exact ⟨_, .field address rest out counter first read delta moved⟩
    | chunk address index delta moved =>
      obtain ⟨_, items, start, finish, position, read, same, bound⟩ := first
      subst index
      cases slot : items.drop position with
      | nil =>
        have sizes := congrArg List.length slot
        simp only [List.length_drop, List.length_nil] at sizes
        omega
      | cons head tail =>
        cases tail with
        | nil => exact ⟨_, .chunkLast address rest out counter read position slot delta moved⟩
        | cons next tail => exact ⟨_, .chunkMore address rest out counter read position slot delta moved⟩
    | finish address delta moved context =>
      obtain ⟨record, read, _, _⟩ := certificate.valid address first.1
      have footer := first.2
      cases record with
      | value kind start finish body =>
        cases kind with
        | none =>
          have same : context = none := by simpa [read] using footer
          subst context
          exact ⟨_, .boundary address rest out counter read delta moved⟩
        | some kind =>
          cases context with
          | none => simp [read] at footer
          | some id => exact ⟨_, .closeNode address rest out counter id read delta moved⟩
      | field name start finish body =>
        have same : context = none := by simpa [read] using footer
        subst context
        exact ⟨_, .closeField address rest out counter read delta moved⟩
      | empty | token _ | branch _ _ _ _ _ | view _ _ | append _ _ _ _ _ _ | chunk _ _ _ =>
        simp [read] at footer

theorem trace_trans (first : Trace certificate start middle) (second : Trace certificate middle last) :
    Trace certificate start last := by
  induction second with
  | refl => exact first
  | next history step ih => exact .next ih step

/-- A checked graph has a finite normal-return run: the rank certificate closes
references, state validity rules out stuck frames, and physical work decreases. -/
theorem machine_completes (certificate : Extraction.Certificate heap) (state : State)
    (valid : StateValid certificate state) :
    ∃ last, Trace certificate state last ∧ last.pending = [] := by
  induction state using (physical_steps_wellFounded certificate).induction with
  | h state ih =>
    by_cases done : state.pending = []
    · exact ⟨state, .refl state, done⟩
    · obtain ⟨after, step⟩ := progress certificate state valid done
      obtain ⟨last, trace, finished⟩ := ih after step (step_preserves_validity step valid)
      exact ⟨last, trace_trans (.next (.refl state) step) trace, finished⟩

theorem initial_completes (certificate : Extraction.Certificate heap) (address : Ref)
    (inside : address ∈ certificate.domain) :
    ∃ last, Trace certificate (initial address) last ∧ last.pending = [] ∧
      last.emitted.map Observation.event = EventIterator.meaning (Extraction.environment certificate address) 0 false := by
  have valid : StateValid certificate (initial address) := by
    simpa [StateValid, initial, FrameValid] using inside
  obtain ⟨last, trace, finished⟩ := machine_completes certificate (initial address) valid
  exact ⟨last, trace, finished, complete_machine trace finished⟩

end ParserEngine.IteratorMachine
