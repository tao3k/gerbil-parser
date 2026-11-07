import ParserEngine.FrameOwnership

/-! Complete restricted syntax admission for event-program-walk/inline's
iterative procedure. Each ordered clause is consumed; the normalized interpreter
uses the admitted publisher protocol. Reader, standard forms, record primitives,
fresh allocation and compiler correctness are external premises. -/
namespace ParserEngine.IteratorSource
open WalkSource SourceEffects IteratorEncoding IteratorMachine

structure Program where
  initialCursor : Nat
  relocateMoved : Bool
  deriving DecidableEq, Repr

def transfer (code finish delta moved context pending : Datum) :=
  app "loop" [code, finish, delta, moved, context, pending]
def finishClause := l [s "finish?", app "cond" [
  l [get "event-program-value?", app "if" [get "event-program-value-kind",
    callback "close-node" (get "event-program-value-kind") (get "event-program-value-end") (s "state"),
    callback "boundary" (b false) (get "event-program-value-end") (b false)]],
  l [get "event-field-frame?", callback "close-field" (get "event-field-frame-name") (get "event-field-frame-end") (b false)],
  l [s "else", app "error" [.text "invalid event continuation"]]],
  transfer (b false) (b false) (s "delta") (s "moved?") (b false) (s "pending")]
def branchClause := l [get "event-code-branch?",
  transfer (get "event-code-branch-left") (b false) (s "delta") (s "moved?") (b false)
    (app "cons" [nextPacket (get "event-code-branch-right"), s "pending"])]
def valueClause := l [get "event-program-value?", app "let" [
  l [s "node-state", app "and" [get "event-program-value-kind",
    callback "open-node" (get "event-program-value-kind") (get "event-program-value-start") (b false)]],
  transfer (get "event-program-value-body") (b false) (s "delta") (s "moved?") (b false)
    (app "cons" [app "vector" [s "code", b true, s "delta", s "moved?", s "node-state"], s "pending"])]]
def fieldClause := l [get "event-field-frame?",
  callback "open-field" (get "event-field-frame-name") (get "event-field-frame-start") (b false),
  transfer (get "event-field-frame-body") (b false) (s "delta") (s "moved?") (b false)
    (app "cons" [app "vector" [s "code", b true, s "delta", s "moved?", b false], s "pending"])]
def tokenClause := l [get "token?", callback "token" (s "code") (get "token-start") (b false),
  transfer (b false) (b false) (s "delta") (s "moved?") (b false) (s "pending")]

def decode (source : Datum) : Option Program := do
  let clauses ← iterativeClauses source
  match clauses with
  | [_, footer, view, _, branch, chunk, value, field, token, invalid] =>
    if (decodeResume source).map resumeAdmitted ≠ some true ∨ appendChecked source ≠ true ∨
      footer ≠ finishClause ∨ branch ≠ branchClause ∨ value ≠ valueClause ∨ field ≠ fieldClause ∨
      token ≠ tokenClause ∨ invalid ≠ l [s "else", app "error" [.text "invalid event instruction"]] then none
    else do
      let moved ← match view with
        | .list [test, .list [.symbol "loop", body, .boolean false, delta, .boolean moved, .boolean false, .symbol "pending"]] =>
          if test = get "event-code-view?" ∧ body = get "event-code-view-code" ∧
            delta = app "+" [s "delta", get "event-code-view-delta"] then some moved else none
        | _ => none
      let cursor ← match chunk with
        | .list [test, .list [.symbol "loop", child, .boolean false, .symbol "delta", .symbol "moved?", .boolean false,
            .list [.symbol "cons", .list [.symbol "vector", .symbol "code", .integer index, .symbol "delta", .symbol "moved?"], .symbol "pending"]]] =>
          if test = get "event-code-chunk?" ∧ child = app "vector-ref" [get "event-code-chunk-items", n 0]
          then some index else none
        | _ => none
      pure ⟨cursor, moved⟩
  | _ => none

def admitted (program : Program) : Bool := program == ⟨1, true⟩

/-- Normalized execution of source transfers, saved footer packets and callback
operations. Negative cursors/malformed frames have no admitted normal return. -/
def advance (program : Program) (heap : Nat → Option (List Cell)) (state : State) : Option State := do
  match state.pending with
  | [] => none
  | .next address delta moved :: rest =>
    let record ← readRecord heap address
    match record with
    | .empty => pure ⟨rest, state.emitted, state.nextNode⟩
    | .token value => pure ⟨rest, state.emitted ++ [observe (.token value) delta moved none], state.nextNode⟩
    | .branch left right _ _ _ => pure ⟨.next left delta moved :: .next right delta moved :: rest, state.emitted, state.nextNode⟩
    | .view body shift => pure ⟨.next body (delta + shift) program.relocateMoved :: rest, state.emitted, state.nextNode⟩
    | .append prefixCode blocks tail _ _ _ =>
      pure ⟨.next prefixCode delta moved :: appendAssignments blocks delta moved (.next tail delta moved :: rest), state.emitted, state.nextNode⟩
    | .chunk [] _ _ => none
    | .chunk (head :: _) _ _ => pure ⟨.next head delta moved :: .chunk address (program.initialCursor : Int) delta moved :: rest, state.emitted, state.nextNode⟩
    | .value kind start _ body =>
      match kind with
      | some name => pure ⟨.next body delta moved :: .finish address delta moved (some (state.nextNode : Int)) :: rest,
          state.emitted ++ [observe (.openNode name start) delta moved none], state.nextNode + 1⟩
      | none => pure ⟨.next body delta moved :: .finish address delta moved none :: rest, state.emitted, state.nextNode⟩
    | .field name start _ body => pure ⟨.next body delta moved :: .finish address delta moved none :: rest,
        state.emitted ++ [observe (.openField name start) delta moved none], state.nextNode⟩
  | .finish address delta moved context :: rest =>
    let record ← readRecord heap address
    match record, context with
    | .value (some kind) _ finish _, some id => pure ⟨rest, state.emitted ++ [observe (.closeNode kind finish) delta moved (some id)], state.nextNode⟩
    | .value none _ finish _, none => pure ⟨rest, state.emitted ++ [observe (.boundary finish) delta moved none], state.nextNode⟩
    | .field name _ finish _, none => pure ⟨rest, state.emitted ++ [observe (.closeField name finish) delta moved none], state.nextNode⟩
    | _, _ => none
  | .chunk address index delta moved :: rest =>
    if index < 0 then none else do
      let record ← readRecord heap address
      match record with
      | .chunk items _ _ =>
        match items.drop index.toNat with
        | [] => none
        | [head] => pure ⟨.next head delta moved :: rest, state.emitted, state.nextNode⟩
        | head :: _ :: _ => pure ⟨.next head delta moved :: .chunk address (index + 1) delta moved :: rest, state.emitted, state.nextNode⟩
      | _ => none

theorem advance_refines (program : Program) (checked : admitted program = true) (state : State) :
    advance program heap state = IteratorExecutor.advance heap state := by
  have equal : program = ⟨1, true⟩ := by simpa [admitted] using checked
  subst program
  obtain ⟨pending, out, counter⟩ := state
  cases pending with
  | nil => rfl
  | cons frame rest =>
    cases frame with
    | finish address delta moved context => rfl
    | chunk address index delta moved => rfl
    | next address delta moved =>
      cases found : readRecord heap address with
      | none => simp [advance, IteratorExecutor.advance, found]
      | some record =>
        cases record <;> simp [advance, IteratorExecutor.advance, found, appendAssignments, pushBlocks]
        case chunk items start finish => cases items <;> rfl
        case value kind start finish body => cases kind <;> rfl

def tick (program : Program) (heap : Nat → Option (List Cell)) (packet : IteratorExecutor.PacketState) := do
  let state ← IteratorExecutor.decode packet
  let next ← advance program heap state
  pure (IteratorExecutor.encode next)

def run (program : Program) (heap : Nat → Option (List Cell)) : Nat → IteratorExecutor.PacketState → Option IteratorExecutor.PacketState
  | 0, _ => none
  | fuel + 1, packet => do
    let current ← IteratorExecutor.decode packet
    if current.pending.isEmpty then pure packet else do
      let next ← tick program heap packet
      run program heap fuel next

theorem run_refines (program : Program) (checked : admitted program = true) (fuel : Nat) (packet : IteratorExecutor.PacketState) :
    run program heap fuel packet = IteratorExecutor.run heap fuel packet := by
  have step : tick program heap = IteratorExecutor.tick heap := by
    funext packet
    simp [tick, IteratorExecutor.tick, advance_refines program checked]
  induction fuel generalizing packet with
  | zero => rfl
  | succ fuel ih =>
    simp only [run, IteratorExecutor.run, step]
    cases decoded : IteratorExecutor.decode packet with
    | none => rfl
    | some state =>
      change (if state.pending.isEmpty then some packet else
        (IteratorExecutor.tick heap packet).bind (run program heap fuel)) =
        (if state.pending.isEmpty then some packet else
          (IteratorExecutor.tick heap packet).bind (IteratorExecutor.run heap fuel))
      split
      · rfl
      · cases IteratorExecutor.tick heap packet <;> simp [ih]

/-- Source admission constructs the program, complete run and exact callback
meaning from checked records. No manually supplied dispatch trace. -/
theorem checked_execution (source : Datum) (checked : (decode source).map admitted = some true)
    (heap : Nat → Option (List Cell)) (domain : List Ref) (rank : Ref → Nat)
    (graph : Extraction.verify heap domain rank = true) (address : Ref) (inside : address ∈ domain) :
    ∃ program meaning fuel last, decode source = some program ∧
      Extraction.generate heap (rank address + 1) address = some meaning ∧ Extract heap address meaning ∧
      run program heap fuel (IteratorExecutor.encode (initial address)) = some last ∧ last.pending = [] ∧
      last.emitted.map Observation.event = EventIterator.meaning meaning 0 false ∧
      runControl last.emitted ⟨0, []⟩ = some ⟨last.nextNode, []⟩ := by
  cases decoded : decode source with
  | none => simp [decoded] at checked
  | some program =>
    have accepted : admitted program = true := by simpa [decoded] using checked
    obtain ⟨meaning, fuel, last, generated, extracted, executed, finished, events, contexts⟩ :=
      IteratorExecutor.checked_execution heap domain rank graph address inside
    exact ⟨program, meaning, fuel, last, rfl, generated, extracted,
      by simpa [run_refines program accepted] using executed, finished, events, contexts⟩

/-- Allocation order is the source order: append tail first, then newest-first
blocks. The mutable pending list consequently receives their reversed order. -/
def allocations (program : Program) (record : Record) (address : Ref) (delta : Int) (moved : Bool) (counter : Nat) : List Frame :=
  match record with
  | .branch _ right _ _ _ => [.next right delta moved]
  | .append _ blocks tail _ _ _ => .next tail delta moved :: blocks.map (fun block => .next block delta moved)
  | .chunk (_ :: _) _ _ => [.chunk address (program.initialCursor : Int) delta moved]
  | .value kind _ _ _ => [.finish address delta moved (kind.map (fun _ => (counter : Int)))]
  | .field .. => [.finish address delta moved none]
  | _ => []

def registers (program : Program) (record : Record) (delta : Int) (moved : Bool) : List Frame :=
  match record with
  | .branch left _ _ _ _ => [.next left delta moved]
  | .append leading _ _ _ _ _ => [.next leading delta moved]
  | .chunk (head :: _) _ _ => [.next head delta moved]
  | .value _ _ _ body | .field _ _ _ body => [.next body delta moved]
  | .view body shift => [.next body (delta + shift) program.relocateMoved]
  | _ => []

theorem dispatch_storage (program : Program) (read : readRecord heap address = some record)
    (executed : advance program heap ⟨.next address delta moved :: frames, out, counter⟩ = some result) :
    result.pending = registers program record delta moved ++ (allocations program record address delta moved counter).reverse ++ frames := by
  cases record <;> simp [advance, read] at executed
  all_goals try cases executed
  all_goals try rfl
  case chunk items start finish =>
    cases items with
    | nil => simp at executed
    | cons head tail => cases executed; rfl
  case value kind start finish body =>
    cases kind <;> simp at executed <;> cases executed <;> rfl
  case append leading blocks tail start finish height =>
    simp only [registers, allocations, List.reverse_cons]
    rw [append_assignment_refines, pushed_blocks_order]
    simp [List.append_assoc]

/-- Construct the physical allocation witness for any admitted direct dispatch.
The caller supplies no fresh identity choices or storage-effect trace. -/
theorem checked_dispatch_owned (source : Datum) (checked : (decode source).map admitted = some true)
    (store : FrameOwnership.Store) (owned : FrameOwnership.Owned store)
    (decodedStack : FrameOwnership.project store.heap store.pending = some frames)
    (read : readRecord heap address = some record)
    (decoded : decode source = some program)
    (executed : advance program heap ⟨.next address delta moved :: frames, out, counter⟩ = some result) :
    ∃ next stored,
      FrameOwnership.run ((allocations program record address delta moved counter).map FrameOwnership.Action.allocate) store = some next ∧
      FrameOwnership.Owned next ∧ FrameOwnership.project next.heap next.pending = some stored ∧
      result.pending = registers program record delta moved ++ stored := by
  have _sourceAccepted : admitted program = true := by simpa [decoded] using checked
  let created := allocations program record address delta moved counter
  exact ⟨FrameOwnership.allocateMany created store, created.reverse ++ frames,
    FrameOwnership.allocate_many_executes created store, FrameOwnership.allocate_many_owned created owned,
    FrameOwnership.allocate_many_project created owned decodedStack, by simpa [created] using dispatch_storage program read executed⟩

#print axioms advance_refines
#print axioms run_refines
#print axioms checked_execution
#print axioms checked_dispatch_owned
end ParserEngine.IteratorSource
