import ParserEngine.RequestAST

/-! Canonical publication after typed native extraction. Token payloads retain
kind, lexeme and coordinates. Native object identity/slot reads, signed-position
admission, GC and effectful callback execution remain external premises. -/
namespace ParserEngine.Publication

structure Token where
  kind : String
  lexeme : String
  span : Span
  deriving DecidableEq, Repr

inductive Event where
  | token (id : Nat) (value : Token)
  | openNode (id : Nat) (kind : String) (position : Nat)
  | closeNode (id : Nat) (kind : String) (position : Nat)
  | openField (name : String) (position : Nat)
  | closeField (name : String) (position : Nat)
  deriving DecidableEq, Repr

inductive Op where
  | claim (value : Token)
  | flush (boundary : Nat)
  | openNode (kind : String) (position : Nat)
  | closeNode (kind : String) (position : Nat)
  | openField (name : String) (position : Nat)
  | closeField (name : String) (position : Nat)

mutual
  /-- Finite extraction of native token/node/fragment records. Child lists and
  branch sequences have the same decoder; relocation is outside this adapter. -/
  inductive Value where
    | token (value : Token)
    | node (kind : String) (span : Span) (children : ChildSequence)
    | fragment (span : Span) (children : ChildSequence)
  inductive ChildSequence where
    | empty
    | child (field : Option String) (value : Value)
    | append (left right : ChildSequence)
end

mutual
  def decodeValue : Value → Tree
    | .token token => .token token.kind token.span
    | .node kind span children => .node kind span (decodeChildren children)
    | .fragment span children => .fragment span (decodeChildren children)
  def decodeChildren : ChildSequence → Children
    | .empty => []
    | .child field value => [(field, decodeValue value)]
    | .append left right => decodeChildren left ++ decodeChildren right
end

def valueSpan : Value → Span
  | .token token => token.span
  | .node _ span _ | .fragment span _ => span

theorem extraction_bounds (value : Value) : (decodeValue value).span = valueSpan value := by
  cases value <;> rfl

def fieldOpen (field : Option String) (position : Nat) : List Op :=
  field.toList.map fun name => .openField name position

def fieldClose (field : Option String) (position : Nat) : List Op :=
  field.toList.map fun name => .closeField name position

mutual
  def valueOps : Value → Option String → Bool → Nat → List Op
    | .token token, field, _, _ =>
      [.flush token.span.start] ++ fieldOpen field token.span.start ++ [.claim token] ++
        fieldClose field token.span.finish
    | .node kind span children, field, root, sourceEnd =>
      let start := if root then 0 else span.start
      let finish := if root then sourceEnd else span.finish
      [.flush start] ++ fieldOpen field start ++ [.openNode kind start] ++
        childrenOps children sourceEnd ++ [.flush finish, .closeNode kind finish] ++ fieldClose field finish
    | .fragment span children, field, _, sourceEnd =>
      [.flush span.start] ++ fieldOpen field span.start ++ childrenOps children sourceEnd ++
        [.flush span.finish] ++ fieldClose field span.finish
  def childrenOps : ChildSequence → Nat → List Op
    | .empty, _ => []
    | .child field value, sourceEnd => valueOps value field false sourceEnd
    | .append left right, sourceEnd => childrenOps left sourceEnd ++ childrenOps right sourceEnd
end

/-- Native root admission rejects token and fragment roots. Root node bounds
are widened to the whole request, independently of semantic child bounds. -/
def rootOps (value : Value) (sourceEnd : Nat) : Option (List Op) :=
  match value with
  | .node _ _ _ => some (valueOps value none true sourceEnd)
  | _ => none

inductive Frame where
  | node (id : Nat) (kind : String)
  | field (name : String)

structure State where
  remaining : List Token
  nextToken : Nat
  nextNode : Nat
  frames : List Frame

def initial (tokens : List Token) : State := ⟨tokens, 0, 0, []⟩

def tokenEntries (events : List Event) : List (Nat × Token) :=
  events.filterMap fun event => match event with | .token id token => some (id, token) | _ => none

theorem tokenEntries_append (first rest : List Event) :
    tokenEntries (first ++ rest) = tokenEntries first ++ tokenEntries rest := by
  exact List.filterMap_append

def numbered : Nat → List Token → List (Nat × Token)
  | _, [] => []
  | id, token :: rest => (id, token) :: numbered (id + 1) rest

theorem numbered_values (id : Nat) (tokens : List Token) :
    (numbered id tokens).map Prod.snd = tokens := by
  induction tokens generalizing id with
  | nil => rfl
  | cons token rest ih => simp [numbered, ih]

def sourceText (tokens : List Token) : String :=
  (tokens.map Token.lexeme).foldr String.append ""

inductive Flush (trivia : Token → Prop) : State → Nat → State → List Event → Prop where
  | empty (state : State) (boundary : Nat) (done : state.remaining = []) : Flush trivia state boundary state []
  | stop (state : State) (boundary : Nat) (token : Token) (rest : List Token)
      (pending : state.remaining = token :: rest) (later : boundary < token.span.finish) :
      Flush trivia state boundary state []
  | emit (state next last : State) (boundary : Nat) (token : Token) (rest : List Token) (events : List Event)
      (pending : state.remaining = token :: rest) (before : token.span.finish ≤ boundary)
      (allowed : trivia token) (advance : next = {state with remaining := rest, nextToken := state.nextToken + 1})
      (tail : Flush trivia next boundary last events) :
      Flush trivia state boundary last (.token state.nextToken token :: events)

theorem flush_conserves {trivia : Token → Prop} {before after : State} {boundary events}
    (flush : Flush trivia before boundary after events) :
    tokenEntries events ++ numbered after.nextToken after.remaining = numbered before.nextToken before.remaining := by
  induction flush with
  | empty => simp [tokenEntries]
  | stop => simp [tokenEntries]
  | emit state next last boundary token rest events pending before allowed advance tail ih =>
    subst next
    simp only [tokenEntries, List.filterMap_cons, List.cons_append] at *
    simpa [pending, numbered] using congrArg (List.cons (state.nextToken, token)) ih

def nodeOpens : List Event → Nat
  | [] => 0
  | .openNode _ _ _ :: rest => nodeOpens rest + 1
  | _ :: rest => nodeOpens rest

theorem nodeOpens_append (first rest : List Event) :
    nodeOpens (first ++ rest) = nodeOpens first + nodeOpens rest := by
  induction first with
  | nil => simp [nodeOpens]
  | cons event first ih => cases event <;> simp [nodeOpens, ih] <;> omega

theorem flush_node_stable {trivia : Token → Prop} {before after : State} {boundary events}
    (flush : Flush trivia before boundary after events) :
    after.nextNode = before.nextNode ∧ nodeOpens events = 0 := by
  induction flush with
  | empty => simp [nodeOpens]
  | stop => simp [nodeOpens]
  | emit state next last boundary token rest events pending before allowed advance tail ih =>
    subst next
    simpa [nodeOpens] using ih

inductive Step (trivia : Token → Prop) : State → Op → State → List Event → Prop where
  | claim (state : State) (token : Token) (rest : List Token) (pending : state.remaining = token :: rest) :
      Step trivia state (.claim token) {state with remaining := rest, nextToken := state.nextToken + 1}
        [.token state.nextToken token]
  | flush {before after boundary events} (flushed : Flush trivia before boundary after events) :
      Step trivia before (.flush boundary) after events
  | openNode (state : State) (kind : String) (position : Nat) :
      Step trivia state (.openNode kind position)
        {state with nextNode := state.nextNode + 1, frames := .node state.nextNode kind :: state.frames}
        [.openNode state.nextNode kind position]
  | closeNode (state : State) (id : Nat) (kind : String) (position : Nat) (rest : List Frame)
      (matching : state.frames = .node id kind :: rest) :
      Step trivia state (.closeNode kind position) {state with frames := rest} [.closeNode id kind position]
  | openField (state : State) (name : String) (position : Nat) :
      Step trivia state (.openField name position) {state with frames := .field name :: state.frames}
        [.openField name position]
  | closeField (state : State) (name : String) (position : Nat) (rest : List Frame)
      (matching : state.frames = .field name :: rest) :
      Step trivia state (.closeField name position) {state with frames := rest} [.closeField name position]

theorem step_conserves {trivia : Token → Prop} {before after : State} {op events}
    (step : Step trivia before op after events) :
    tokenEntries events ++ numbered after.nextToken after.remaining = numbered before.nextToken before.remaining := by
  cases step with
  | claim token rest pending => simp [tokenEntries, pending, numbered]
  | flush flushed => exact flush_conserves flushed
  | openNode | closeNode | openField | closeField => simp [tokenEntries]

theorem step_node_counter {trivia : Token → Prop} {before after : State} {op events}
    (step : Step trivia before op after events) :
    after.nextNode = before.nextNode + nodeOpens events := by
  cases step with
  | flush flushed =>
    obtain ⟨stable, empty⟩ := flush_node_stable flushed
    simp [stable, empty]
  | claim | openNode | closeNode | openField | closeField => simp [nodeOpens]

inductive Trace (trivia : Token → Prop) : State → List Op → State → List Event → Prop where
  | refl (state : State) : Trace trivia state [] state []
  | next {a b c first rest op events} (history : Trace trivia a first b rest)
      (step : Step trivia b op c events) : Trace trivia a (first ++ [op]) c (rest ++ events)

theorem trace_conserves {trivia : Token → Prop} {before after : State} {ops events}
    (trace : Trace trivia before ops after events) :
    tokenEntries events ++ numbered after.nextToken after.remaining = numbered before.nextToken before.remaining := by
  induction trace with
  | refl => simp [tokenEntries]
  | next history step ih =>
    simp only [tokenEntries_append, List.append_assoc]
    rw [show tokenEntries _ ++ numbered _ _ = _ from step_conserves step]
    exact ih

theorem trace_node_counter {trivia : Token → Prop} {before after : State} {ops events}
    (trace : Trace trivia before ops after events) :
    after.nextNode = before.nextNode + nodeOpens events := by
  induction trace with
  | refl => simp [nodeOpens]
  | next history step ih =>
    rw [nodeOpens_append]
    have count := step_node_counter step
    omega

/-- Actual vector shape after primitive symbol/string/natural extraction. -/
inductive Cell where
  | tag (value : String)
  | text (value : String)
  | number (value : Nat)

def encodeEvent : Event → List Cell
  | .token id token => [.tag "token", .number id, .text token.kind, .text token.lexeme,
      .number token.span.start, .number token.span.finish]
  | .openNode id kind pos => [.tag "start-node", .number id, .text kind, .number pos]
  | .closeNode id kind pos => [.tag "finish-node", .number id, .text kind, .number pos]
  | .openField name pos => [.tag "start-field", .text name, .number pos]
  | .closeField name pos => [.tag "finish-field", .text name, .number pos]

def decodeEvent : List Cell → Option Event
  | [.tag "token", .number id, .text kind, .text lexeme, .number start, .number finish] =>
      some (.token id ⟨kind, lexeme, ⟨start, finish⟩⟩)
  | [.tag "start-node", .number id, .text kind, .number pos] => some (.openNode id kind pos)
  | [.tag "finish-node", .number id, .text kind, .number pos] => some (.closeNode id kind pos)
  | [.tag "start-field", .text name, .number pos] => some (.openField name pos)
  | [.tag "finish-field", .text name, .number pos] => some (.closeField name pos)
  | _ => none

theorem event_extraction_roundtrip (event : Event) : decodeEvent (encodeEvent event) = some event := by
  cases event with
  | token id token => cases token; rfl
  | openNode | closeNode | openField | closeField => rfl

/-- Successful extraction retains every encoded slot; a malformed vector
cannot become an accepted event by dropping extra fields or changing payloads. -/
theorem event_extraction_exact (cells : List Cell) (event : Event)
    (decoded : decodeEvent cells = some event) : encodeEvent event = cells := by
  unfold decodeEvent at decoded
  split at decoded <;> simp_all [encodeEvent] <;> subst event <;> rfl

/-- The same extracted root links recognition, complete semantic replay and
canonical publication. Native extraction and both trace simulations must be
established; this theorem does not infer them from an artifact's valid flag. -/
theorem complete_product (driver : RequestAST.Driver Raw) (input : List Raw)
    (last : RequestAST.State Raw) (instructions : List (RequestAST.Instruction Raw))
    (root : Value) (source : List Token) (sourceEnd : Nat) (ops : List Op)
    (trivia : Token → Bool) (published : State) (events : List Event)
    (tokenView : Raw → Token) (original : String)
    (sourceAdmission : input.map tokenView = source.filter (fun token => !trivia token))
    (scannerCoverage : sourceText source = original)
    (byteLength : sourceEnd = original.utf8ByteSize)
    (leafView : ∀ raw, driver.leaf raw = .token (tokenView raw).kind (tokenView raw).span)
    (endView : driver.inputEnd = RawRequest.Entry.scanEnd (fun raw => (tokenView raw).span.finish) input 0)
    (request : RequestAST.Trace driver (RequestAST.initial driver input) instructions last)
    (complete : last.raw.rest = [])
    (accepted : driver.engine.select (last.raw.states.headD 0) none = some (none, .accept))
    (rootShape : last.values = [[(none, decodeValue root)]])
    (code : rootOps root sourceEnd = some ops)
    (publication : Trace (fun token => trivia token = true) (initial source) ops published events)
    (consumed : published.remaining = []) (closed : published.frames = []) :
    (∃ terminals, Grammar.Derives driver.engine.grammar [.nonterminal driver.engine.start] terminals ∧
      RawRequest.Relates driver.engine.realizesToken terminals input) ∧
    RequestAST.run driver.rules (fun raw => .token (tokenView raw).kind (tokenView raw).span)
      instructions [] = some [[(none, decodeValue root)]] ∧
    rootOps root original.utf8ByteSize = some ops ∧ published.frames = [] ∧
    tokenEntries events = numbered 0 source ∧
    ((tokenEntries events).map Prod.snd).filter (fun token => !trivia token) = input.map tokenView ∧
    sourceText ((tokenEntries events).map Prod.snd) = original ∧
    published.nextNode = nodeOpens events ∧
    driver.inputEnd = RawRequest.Entry.scanEnd (fun raw => (tokenView raw).span.finish) input 0 := by
  have requestResult := RequestAST.complete_request driver input last instructions (decodeValue root)
    request complete accepted rootShape
  have leafEquality : driver.leaf = fun raw => .token (tokenView raw).kind (tokenView raw).span :=
    funext leafView
  rw [leafEquality] at requestResult
  have conserved : tokenEntries events = numbered 0 source := by
    simpa [initial, consumed, numbered] using trace_conserves publication
  have payloads : (tokenEntries events).map Prod.snd = source := by
    rw [conserved, numbered_values]
  have rootCode : rootOps root original.utf8ByteSize = some ops := by simpa [byteLength] using code
  refine ⟨requestResult.1, requestResult.2, rootCode, closed, conserved, ?_, ?_, ?_, endView⟩
  · rw [payloads]
    exact sourceAdmission.symm
  · rw [payloads]
    exact scannerCoverage
  · simpa [initial] using trace_node_counter publication

end ParserEngine.Publication
