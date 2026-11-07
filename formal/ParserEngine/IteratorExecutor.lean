import ParserEngine.IteratorMachine

/-! Executable dispatch over decoded native continuation packets. Decoding,
record reads, callback IDs, chunk updates and rejection are functions; execution
traces no longer assume an IteratorMachine.Step supplied by a caller. Current
register dispatch and restoration are grouped as in IteratorMachine. -/
namespace ParserEngine.IteratorExecutor
open IteratorEncoding IteratorMachine

def advance (heap : Nat → Option (List Cell)) (state : State) : Option State := do
  match state.pending with
  | [] => none
  | .next address delta moved :: rest =>
    let record ← readRecord heap address
    match record with
    | .empty => pure ⟨rest, state.emitted, state.nextNode⟩
    | .token value => pure ⟨rest, state.emitted ++ [observe (.token value) delta moved none], state.nextNode⟩
    | .branch left right _ _ _ => pure ⟨.next left delta moved :: .next right delta moved :: rest, state.emitted, state.nextNode⟩
    | .view body shift => pure ⟨.next body (delta + shift) true :: rest, state.emitted, state.nextNode⟩
    | .append leading blocks tail _ _ _ =>
      pure ⟨.next leading delta moved :: pushBlocks blocks delta moved (.next tail delta moved :: rest), state.emitted, state.nextNode⟩
    | .chunk [] _ _ => none
    | .chunk (head :: _) _ _ => pure ⟨.next head delta moved :: .chunk address 1 delta moved :: rest, state.emitted, state.nextNode⟩
    | .value (some kind) start _ body =>
      pure ⟨.next body delta moved :: .finish address delta moved (some (state.nextNode : Int)) :: rest,
        state.emitted ++ [observe (.openNode kind start) delta moved none], state.nextNode + 1⟩
    | .value none _ _ body =>
      pure ⟨.next body delta moved :: .finish address delta moved none :: rest, state.emitted, state.nextNode⟩
    | .field name start _ body =>
      pure ⟨.next body delta moved :: .finish address delta moved none :: rest,
        state.emitted ++ [observe (.openField name start) delta moved none], state.nextNode⟩
  | .chunk address index delta moved :: rest =>
    if index < 0 then none else
      let record ← readRecord heap address
      match record with
      | .chunk items _ _ =>
        match items.drop index.toNat with
        | [] => none
        | [head] => pure ⟨.next head delta moved :: rest, state.emitted, state.nextNode⟩
        | head :: _ :: _ =>
          pure ⟨.next head delta moved :: .chunk address (index + 1) delta moved :: rest, state.emitted, state.nextNode⟩
      | _ => none
  | .finish address delta moved context :: rest =>
    let record ← readRecord heap address
    match record, context with
    | .value (some kind) _ finish _, some id =>
      pure ⟨rest, state.emitted ++ [observe (.closeNode kind finish) delta moved (some id)], state.nextNode⟩
    | .value none _ finish _, none =>
      pure ⟨rest, state.emitted ++ [observe (.boundary finish) delta moved none], state.nextNode⟩
    | .field name _ finish _, none =>
      pure ⟨rest, state.emitted ++ [observe (.closeField name finish) delta moved none], state.nextNode⟩
    | _, _ => none

theorem step_executes {heap : Nat → Option (List Cell)}
    {certificate : Extraction.Certificate heap} (step : Step certificate before after) :
    advance heap before = some after := by
  cases step <;> simp_all [advance, Int.natCast_add]

theorem advance_refines (certificate : Extraction.Certificate heap) (valid : StateValid certificate before)
    (executed : advance heap before = some after) : Step certificate before after := by
  have pending : before.pending ≠ [] := by
    intro empty
    simp [advance, empty] at executed
  obtain ⟨actual, step⟩ := progress certificate before valid pending
  have same : actual = after := Option.some.inj ((step_executes step).symm.trans executed)
  subst actual
  exact step

theorem step_deterministic (left : Step certificate before first) (right : Step certificate before second) :
    first = second := Option.some.inj ((step_executes left).symm.trans (step_executes right))

structure PacketState where
  pending : List (List Cell)
  emitted : List Observation
  nextNode : Nat

def encode (state : State) : PacketState := ⟨state.pending.map encodeFrame, state.emitted, state.nextNode⟩

def decode (state : PacketState) : Option State := do
  let pending ← decodeStack state.pending
  pure ⟨pending, state.emitted, state.nextNode⟩

theorem decode_encode (state : State) : decode (encode state) = some state := by
  simp [decode, encode, stack_roundtrip]

theorem stack_exact (decoded : decodeStack packets = some frames) : frames.map encodeFrame = packets := by
  induction packets generalizing frames with
  | nil => simpa [decodeStack] using decoded.symm
  | cons packet rest ih =>
    cases first : decodeFrame packet with
    | none => simp [decodeStack, first] at decoded
    | some frame =>
      cases tail : decodeStack rest with
      | none => simp [decodeStack, first, tail] at decoded
      | some more =>
        simp [decodeStack, first, tail] at decoded
        subst frames
        simp [frame_exact packet frame first, ih tail]

theorem encode_decoded (decoded : decode packet = some state) : encode state = packet := by
  cases packet with
  | mk packets emitted counter =>
    cases found : decodeStack packets with
    | none => simp [decode, found] at decoded
    | some frames =>
      have same : (⟨frames, emitted, counter⟩ : State) = state := by simpa [decode, found] using decoded
      subst state
      simp [encode, stack_exact found]

/-- Physical vector slot one changes only the chunk cursor. Code, provenance,
translation and the complete pending suffix retain their exact encodings. -/
theorem chunk_slot_update (address : Ref) (index delta : Int) (moved : Bool) :
    (encodeFrame (.chunk address index delta moved)).set 1 (.number (index + 1)) =
      encodeFrame (.chunk address (index + 1) delta moved) := by rfl

def tick (heap : Nat → Option (List Cell)) (state : PacketState) : Option PacketState := do
  let current ← decode state
  let next ← advance heap current
  pure (encode next)

theorem tick_encoded (state : State) : tick heap (encode state) = (advance heap state).map encode := by
  unfold tick
  rw [decode_encode]
  change ((advance heap state).bind fun next => some (encode next)) = _
  cases advance heap state <;> rfl

theorem decoded_tick (packet : PacketState) (before : State) (after : PacketState)
    (decoded : decode packet = some before) (executed : tick heap packet = some after) :
    ∃ next, advance heap before = some next ∧ after = encode next := by
  simp only [tick, decoded] at executed
  cases advanced : advance heap before with
  | none => simp [advanced] at executed
  | some next => exact ⟨next, rfl, by simpa [advanced] using executed.symm⟩

inductive Trace (heap : Nat → Option (List Cell)) : PacketState → PacketState → Prop where
  | refl (state) : Trace heap state state
  | next : Trace heap first before → tick heap before = some after → Trace heap first after

/-- Every executable packet trace over an admitted initial state produces its
own semantic simulation and validity witnesses. No Step/Extract premise is
accepted at this boundary. -/
theorem trace_refines (certificate : Extraction.Certificate heap) (trace : Trace heap packet last)
    (decoded : decode packet = some state) (valid : StateValid certificate state) :
    ∃ final, decode last = some final ∧ IteratorMachine.Trace certificate state final ∧ StateValid certificate final := by
  induction trace with
  | refl => exact ⟨state, decoded, .refl state, valid⟩
  | next history executed ih =>
    obtain ⟨before, beforeDecoded, steps, beforeValid⟩ := ih
    obtain ⟨after, advanced, same⟩ := decoded_tick _ _ _ beforeDecoded executed
    subst same
    have step := advance_refines certificate beforeValid advanced
    exact ⟨after, decode_encode after, .next steps step, step_preserves_validity step beforeValid⟩

def run (heap : Nat → Option (List Cell)) : Nat → PacketState → Option PacketState
  | 0, _ => none
  | fuel + 1, state => do
    let current ← decode state
    if current.pending.isEmpty then pure state else do
      let next ← tick heap state
      run heap fuel next

theorem run_complete (certificate : Extraction.Certificate heap) (state : State)
    (valid : StateValid certificate state) (fuel : Nat) (enough : weight certificate state < fuel) :
    ∃ last, run heap fuel (encode state) = some (encode last) ∧ last.pending = [] := by
  induction fuel generalizing state with
  | zero => omega
  | succ fuel ih =>
    by_cases done : state.pending = []
    · exact ⟨state, by simp [run, decode_encode, done], done⟩
    · obtain ⟨after, step⟩ := progress certificate state valid done
      have decreased := step_decreases step
      obtain ⟨last, executed, finished⟩ := ih after (step_preserves_validity step valid) (by omega)
      exact ⟨last, by simpa [run, decode_encode, tick_encoded, step_executes step, done] using executed, finished⟩

theorem run_trace (executed : run heap fuel packet = some last) :
    Trace heap packet last ∧ ∃ final, decode last = some final ∧ final.pending = [] := by
  induction fuel generalizing packet with
  | zero => simp [run] at executed
  | succ fuel ih =>
    cases decoded : decode packet with
    | none => simp [run, decoded] at executed
    | some state =>
      by_cases done : state.pending = []
      · simp [run, decoded, done] at executed
        subst last
        exact ⟨.refl packet, state, decoded, done⟩
      · cases step : tick heap packet with
        | none => simp [run, decoded, done, step] at executed
        | some after =>
          have remaining : run heap fuel after = some last := by simpa [run, decoded, done, step] using executed
          obtain ⟨history, final, finalDecoded, finished⟩ := ih remaining
          have prepend : ∀ {endState}, Trace heap after endState → Trace heap packet endState := by
            intro endState trace
            induction trace with
            | refl => exact .next (.refl packet) step
            | next history current ih => exact .next ih current
          exact ⟨prepend history, final, finalDecoded, finished⟩

theorem complete_events (certificate : Extraction.Certificate heap) (address : Ref)
    (inside : address ∈ certificate.domain) (last : PacketState)
    (executed : run heap fuel (encode (initial address)) = some last) :
    last.emitted.map Observation.event = EventIterator.meaning (Extraction.environment certificate address) 0 false := by
  obtain ⟨trace, final, decoded, finished⟩ := run_trace executed
  have valid : StateValid certificate (initial address) := by simpa [StateValid, initial, FrameValid] using inside
  obtain ⟨actual, same, steps, _⟩ := trace_refines certificate trace (decode_encode _) valid
  have equal : actual = final := Option.some.inj (same.symm.trans decoded)
  subst actual
  have result := complete_machine steps finished
  have out : final.emitted = last.emitted := by
    cases found : decodeStack last.pending with
    | none => simp [decode, found] at decoded
    | some frames =>
      have equal : (⟨frames, last.emitted, last.nextNode⟩ : State) = final := by
        simpa [decode, found] using decoded
      exact congrArg State.emitted equal.symm
  simpa [out] using result

theorem decode_observations (decoded : decode packet = some state) :
    state.emitted = packet.emitted ∧ state.nextNode = packet.nextNode := by
  cases found : decodeStack packet.pending with
  | none => simp [decode, found] at decoded
  | some frames =>
    have equal : (⟨frames, packet.emitted, packet.nextNode⟩ : State) = state := by
      simpa [decode, found] using decoded
    cases equal
    exact ⟨rfl, rfl⟩

theorem run_refines (certificate : Extraction.Certificate heap) (address : Ref)
    (inside : address ∈ certificate.domain)
    (executed : run heap fuel (encode (initial address)) = some last) :
    ∃ final, decode last = some final ∧ IteratorMachine.Trace certificate (initial address) final ∧ final.pending = [] := by
  obtain ⟨history, final, finalDecoded, finished⟩ := run_trace executed
  have valid : StateValid certificate (initial address) := by simpa [StateValid, initial, FrameValid] using inside
  obtain ⟨actual, actualDecoded, steps, _⟩ := trace_refines certificate history (decode_encode _) valid
  have same : actual = final := Option.some.inj (actualDecoded.symm.trans finalDecoded)
  subst actual
  exact ⟨final, finalDecoded, steps, finished⟩

theorem undecodable_rejected (invalid : decode packet = none) : tick heap packet = none := by
  simp [tick, invalid]

theorem negative_chunk_rejected (index : Int) (negative : index < 0) :
    advance heap ⟨.chunk address index delta moved :: rest, out, counter⟩ = none := by
  simp [advance, negative]

/-- A successful finite certificate check constructs both an extraction witness
and an executable, completed packet run, with exact output and retained IDs. -/
theorem checked_execution (heap : Nat → Option (List Cell)) (domain : List Ref) (rank : Ref → Nat)
    (checked : Extraction.verify heap domain rank = true) (address : Ref) (inside : address ∈ domain) :
    ∃ program fuel last,
      Extraction.generate heap (rank address + 1) address = some program ∧
      Extract heap address program ∧ run heap fuel (encode (initial address)) = some last ∧
      last.pending = [] ∧ last.emitted.map Observation.event = EventIterator.meaning program 0 false ∧
      runControl last.emitted ⟨0, []⟩ = some ⟨last.nextNode, []⟩ := by
  obtain ⟨certificate, sameDomain, sameRank⟩ := Extraction.verify_certificate heap domain rank checked
  have member : address ∈ certificate.domain := by simpa [sameDomain] using inside
  obtain ⟨program, generated, extracted⟩ := Extraction.generate_witness certificate address member
  have valid : StateValid certificate (initial address) := by simpa [StateValid, initial, FrameValid] using member
  let fuel := weight certificate (initial address) + 1
  obtain ⟨final, executed, finished⟩ := run_complete certificate (initial address) valid fuel (by omega)
  obtain ⟨machine, decoded, history, complete⟩ := run_refines certificate address member executed
  have same : machine = final := Option.some.inj (decoded.symm.trans (decode_encode final))
  subst machine
  have meaning : Extraction.environment certificate address = program := by
    simp [Extraction.environment, generated]
  refine ⟨program, fuel, encode final, ?_, extracted, executed, ?_, ?_, ?_⟩
  · simpa [sameRank] using generated
  · simp [encode, finished]
  · simpa [encode, meaning] using complete_machine history complete
  · simpa [encode] using complete_callbacks_bracketed history complete

/-- The full request theorem now takes actual executable packet-run success;
no physical Step or iterator trace is an input premise. Source/AST resolution
and normal-return publication remain their explicit independent adapters. -/
theorem complete_request_product {heap : Nat → Option (List Cell)} (driver : RequestAST.Driver Raw) (input : List Raw)
    (last : RequestAST.State Raw) (instructions : List (RequestAST.Instruction Raw))
    (root : Publication.Value) (source : List Publication.Token) (sourceEnd : Nat)
    (certificate : Extraction.Certificate heap) (address : Ref) (packet : PacketState)
    (resolve : EventIterator.Located Command → List Publication.Op)
    (trivia : Publication.Token → Prop) (published : Publication.State) (events : List Publication.Event)
    (request : RequestAST.Trace driver (RequestAST.initial driver input) instructions last)
    (complete : last.raw.rest = [])
    (accepted : driver.engine.select (last.raw.states.headD 0) none = some (none, .accept))
    (rootShape : last.values = [[(none, Publication.decodeValue root)]])
    (inside : address ∈ certificate.domain)
    (executed : run heap fuel (encode (initial address)) = some packet)
    (code : Publication.rootOps root sourceEnd = some ((EventIterator.meaning (Extraction.environment certificate address) 0 false).flatMap resolve))
    (callbacks : Callback.Trace trivia (Publication.initial source) calls
      ((packet.emitted.map Observation.event).flatMap resolve) published events)
    (consumed : published.remaining = []) (closed : published.frames = []) :
    (∃ terminals, Grammar.Derives driver.engine.grammar [.nonterminal driver.engine.start] terminals ∧
      RawRequest.Relates driver.engine.realizesToken terminals input) ∧
    RequestAST.run driver.rules driver.leaf instructions [] = some [[(none, Publication.decodeValue root)]] ∧
    Publication.rootOps root sourceEnd = some ((packet.emitted.map Observation.event).flatMap resolve) ∧
    Publication.tokenEntries events = Publication.numbered 0 source ∧
    published.nextNode = Publication.nodeOpens events ∧ published.frames = [] := by
  obtain ⟨machine, decoded, walk, finished⟩ := run_refines certificate address inside executed
  have output := (decode_observations decoded).1
  have nativeCallbacks : Callback.Trace trivia (Publication.initial source) calls
      ((machine.emitted.map Observation.event).flatMap resolve) published events := by
    simpa [output] using callbacks
  have product := IteratorMachine.complete_request_product driver input last instructions root source sourceEnd
    certificate address machine resolve trivia published events request complete accepted rootShape
    walk finished code nativeCallbacks consumed closed
  simpa [output] using product

end ParserEngine.IteratorExecutor
