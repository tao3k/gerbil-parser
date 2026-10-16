import ParserEngine.IteratorEncoding

/-! Normal-return callback semantics of artifact.ss. The frame stack is a ghost
invariant supplied by well-bracketed traversal, not an extra native stack or a
claim that the native close callback checks nesting itself. Emitter effects and
exceptional/reentrant callbacks are outside this normal-return relation. -/
namespace ParserEngine.Callback

inductive Call where
  | token (old : Publication.Token) (delta : Int) (moved : Bool)
  | openNode (kind : String) (position : Nat)
  | closeNode (id : Nat) (kind : String) (position : Nat)
  | openField (name : String) (position : Nat)
  | closeField (name : String) (position : Nat)
  | boundary (position : Nat)

/-- Every callback includes the actual trivia flush and canonical emission.
The close ID is the retained callback result, not the current node counter. -/
inductive Step (trivia : Publication.Token → Prop) :
    Publication.State → Call → Publication.State → Option Nat → List Publication.Op → List Publication.Event → Prop where
  | token (before ready : Publication.State) (old actual : Publication.Token) (rest : List Publication.Token)
      (delta : Int) (moved : Bool) (sameObject : Prop) (position : Nat) (flushed : List Publication.Event)
      (coordinate : (position : Int) = (old.span.start : Int) + delta)
      (flush : Publication.Flush trivia before position ready flushed)
      (pending : ready.remaining = actual :: rest)
      (claim : EventIterator.tokenClaim old actual sameObject delta moved) :
      Step trivia before (.token old delta moved)
        {ready with remaining := rest, nextToken := ready.nextToken + 1} none
        [.flush position, .claim actual] (flushed ++ [.token ready.nextToken actual])
  | openNode (before ready : Publication.State) (kind : String) (position : Nat) (flushed : List Publication.Event)
      (flush : Publication.Flush trivia before position ready flushed) :
      Step trivia before (.openNode kind position)
        {ready with nextNode := ready.nextNode + 1, frames := .node ready.nextNode kind :: ready.frames}
        (some ready.nextNode) [.flush position, .openNode kind position]
        (flushed ++ [.openNode ready.nextNode kind position])
  | closeNode (before ready : Publication.State) (id : Nat) (kind : String) (position : Nat)
      (rest : List Publication.Frame) (flushed : List Publication.Event)
      (flush : Publication.Flush trivia before position ready flushed)
      (bracket : ready.frames = .node id kind :: rest) :
      Step trivia before (.closeNode id kind position) {ready with frames := rest} none
        [.flush position, .closeNode kind position] (flushed ++ [.closeNode id kind position])
  | openField (before ready : Publication.State) (name : String) (position : Nat) (flushed : List Publication.Event)
      (flush : Publication.Flush trivia before position ready flushed) :
      Step trivia before (.openField name position) {ready with frames := .field name :: ready.frames} none
        [.flush position, .openField name position] (flushed ++ [.openField name position])
  | closeField (state : Publication.State) (name : String) (position : Nat) (rest : List Publication.Frame)
      (bracket : state.frames = .field name :: rest) :
      Step trivia state (.closeField name position) {state with frames := rest} none
        [.closeField name position] [.closeField name position]
  | boundary (before after : Publication.State) (position : Nat) (flushed : List Publication.Event)
      (flush : Publication.Flush trivia before position after flushed) :
      Step trivia before (.boundary position) after none [.flush position] flushed

theorem step_erases (step : Step trivia before call after returned ops events) :
    Publication.Trace trivia before ops after events := by
  cases step with
  | token ready old actual rest delta moved identity position flushed coordinate flush pending claim =>
    exact .next (.next (.refl _) (.flush flush)) (.claim ready actual rest pending)
  | openNode ready kind position flushed flush =>
    exact .next (.next (.refl _) (.flush flush)) (.openNode ready kind position)
  | closeNode ready id kind position rest flushed flush bracket =>
    exact .next (.next (.refl _) (.flush flush)) (.closeNode ready id kind position rest bracket)
  | openField ready name position flushed flush =>
    exact .next (.next (.refl _) (.flush flush)) (.openField ready name position)
  | closeField name position rest bracket =>
    simpa using Publication.Trace.next (.refl _) (.closeField _ name position rest bracket)
  | boundary after position flushed flush =>
    simpa using Publication.Trace.next (.refl _) (.flush flush)

/-- The opening result equals the pre-callback counter even when trivia was
emitted first. This value is what the physical five-slot frame must save. -/
theorem open_returns_original_counter
    (step : Step trivia before (.openNode kind position) after returned ops events) :
    returned = some before.nextNode := by
  cases step with
  | openNode ready kind position flushed flush =>
    exact congrArg some (Publication.flush_node_stable flush).1

/-- The actual five-slot frame and the opening callback agree on the retained
ID. This remains the opening counter regardless of later nested callbacks. -/
theorem retained_opening_return (openingId : Nat)
    (opened : Step trivia before (.openNode kind position) after (some openingId) ops events)
    (cells : List IteratorEncoding.Cell) (code : IteratorEncoding.Ref) (delta returned : Int) (moved : Bool)
    (saved : cells = IteratorEncoding.encodeFrame (.finish code delta moved (some (openingId : Int))))
    (decoded : IteratorEncoding.decodeFrame cells = some (.finish code delta moved (some returned))) :
    returned = (before.nextNode : Int) := by
  have original : openingId = before.nextNode := Option.some.inj (open_returns_original_counter opened)
  have value : (openingId : Int) = returned := by
    simpa [saved, IteratorEncoding.frame_roundtrip] using decoded
  rw [← value, original]

theorem publication_append (first : Publication.Trace trivia a xs b left)
    (second : Publication.Trace trivia b ys c right) :
    Publication.Trace trivia a (xs ++ ys) c (left ++ right) := by
  induction second with
  | refl => simpa using first
  | next history step ih => simpa [List.append_assoc] using Publication.Trace.next ih step

inductive Trace (trivia : Publication.Token → Prop) :
    Publication.State → List (Call × Option Nat) → List Publication.Op → Publication.State → List Publication.Event → Prop where
  | refl (state) : Trace trivia state [] [] state []
  | next (history : Trace trivia a calls first b left)
      (step : Step trivia b call c returned ops right) :
      Trace trivia a (calls ++ [(call, returned)]) (first ++ ops) c (left ++ right)

theorem trace_erases (trace : Trace trivia before calls ops after events) :
    Publication.Trace trivia before ops after events := by
  induction trace with
  | refl => exact .refl _
  | next history step ih => exact publication_append ih (step_erases step)

/-- Full callback history and exact physical iterator output feed the same
publication trace consumed by the complete-request product theorem. -/
theorem iterator_callback_publication (code : EventIterator.Rope α)
    (walk : EventIterator.Trace (EventIterator.initial code) iterator)
    (finished : iterator.pending = []) (resolve : EventIterator.Located α → List Publication.Op)
    (callbacks : Trace trivia before calls (iterator.emitted.flatMap resolve) after events) :
    Publication.Trace trivia before ((EventIterator.meaning code 0 false).flatMap resolve) after events := by
  exact EventIterator.publication_refinement code walk finished resolve (trace_erases callbacks)

/-- Complete request and product, supplied by the proved callback erasure. -/
theorem complete_request_product (driver : RequestAST.Driver Raw) (input : List Raw)
    (last : RequestAST.State Raw) (instructions : List (RequestAST.Instruction Raw))
    (root : Publication.Value) (source : List Publication.Token) (sourceEnd : Nat)
    (rope : EventIterator.Rope α) (iterator : EventIterator.State α) (resolve : EventIterator.Located α → List Publication.Op)
    (trivia : Publication.Token → Prop) (published : Publication.State) (events : List Publication.Event)
    (request : RequestAST.Trace driver (RequestAST.initial driver input) instructions last)
    (complete : last.raw.rest = [])
    (accepted : driver.engine.select (last.raw.states.headD 0) none = some (none, .accept))
    (rootShape : last.values = [[(none, Publication.decodeValue root)]])
    (walk : EventIterator.Trace (EventIterator.initial rope) iterator) (finished : iterator.pending = [])
    (code : Publication.rootOps root sourceEnd = some ((EventIterator.meaning rope 0 false).flatMap resolve))
    (callbacks : Trace trivia (Publication.initial source) calls
      (iterator.emitted.flatMap resolve) published events)
    (consumed : published.remaining = []) (closed : published.frames = []) :
    (∃ terminals, Grammar.Derives driver.engine.grammar [.nonterminal driver.engine.start] terminals ∧
      RawRequest.Relates driver.engine.realizesToken terminals input) ∧
    RequestAST.run driver.rules driver.leaf instructions [] = some [[(none, Publication.decodeValue root)]] ∧
    Publication.rootOps root sourceEnd = some (iterator.emitted.flatMap resolve) ∧
    Publication.tokenEntries events = Publication.numbered 0 source ∧
    published.nextNode = Publication.nodeOpens events ∧ published.frames = [] := by
  exact EventIterator.complete_request_product driver input last instructions root source sourceEnd
    rope iterator resolve trivia published events request complete accepted rootShape walk finished code
    (trace_erases callbacks) consumed closed

end ParserEngine.Callback
