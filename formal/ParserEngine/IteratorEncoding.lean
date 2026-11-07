import ParserEngine.EventIterator

/-! Parser-owned record extraction and physical continuation encoding. Primitive
record reads, identity, symbol conversion and callback effects are host premises;
this module does not model or modify the host runtime. -/
namespace ParserEngine.IteratorEncoding

abbrev Ref := Option Nat

structure TokenData where
  kind : String
  lexeme : String
  start : Int
  finish : Int
  deriving DecidableEq

/-- Slots of event-program-inspect, including cached fields unused by walking. -/
inductive Record where
  | empty
  | token (value : TokenData)
  | branch (left right : Ref) (start finish height : Int)
  | view (body : Ref) (delta : Int)
  | append (leading : Ref) (newestFirst : List Ref) (tail : Ref) (start finish height : Int)
  | chunk (items : List Ref) (start finish : Int)
  | value (kind : Option String) (start finish : Int) (body : Ref)
  | field (name : String) (start finish : Int) (body : Ref)
  deriving DecidableEq

inductive Cell where
  | tag (value : String)
  | text (value : String)
  | name (value : Option String)
  | number (value : Int)
  | ref (value : Ref)
  | refs (value : List Ref)
  | boolean (value : Bool)
  deriving DecidableEq

def encodeRecord : Record → List Cell
  | .empty => [.tag "empty"]
  | .token t => [.tag "token", .text t.kind, .text t.lexeme, .number t.start, .number t.finish]
  | .branch l r s e h => [.tag "branch", .ref l, .ref r, .number s, .number e, .number h]
  | .view body delta => [.tag "view", .ref body, .number delta]
  | .append leading blocks tail s e h =>
      [.tag "append", .ref leading, .refs blocks, .ref tail, .number s, .number e, .number h]
  | .chunk items s e => [.tag "chunk", .refs items, .number s, .number e]
  | .value kind s e body => [.tag "value", .name kind, .number s, .number e, .ref body]
  | .field name s e body => [.tag "field", .text name, .number s, .number e, .ref body]

def decodeRecord : List Cell → Option Record
  | [.tag "empty"] => some .empty
  | [.tag "token", .text k, .text l, .number s, .number e] => some (.token ⟨k,l,s,e⟩)
  | [.tag "branch", .ref l, .ref r, .number s, .number e, .number h] => some (.branch l r s e h)
  | [.tag "view", .ref body, .number delta] => some (.view body delta)
  | [.tag "append", .ref leading, .refs blocks, .ref tail, .number s, .number e, .number h] =>
      some (.append leading blocks tail s e h)
  | [.tag "chunk", .refs items, .number s, .number e] => some (.chunk items s e)
  | [.tag "value", .name kind, .number s, .number e, .ref body] => some (.value kind s e body)
  | [.tag "field", .text name, .number s, .number e, .ref body] => some (.field name s e body)
  | _ => none

theorem record_roundtrip (record : Record) : decodeRecord (encodeRecord record) = some record := by
  cases record <;> rfl

theorem record_exact (cells : List Cell) (record : Record)
    (decoded : decodeRecord cells = some record) : encodeRecord record = cells := by
  unfold decodeRecord at decoded
  split at decoded <;> simp_all [encodeRecord] <;> subst record <;> rfl

/-- Four slots for siblings/chunk cursors, five for saved callback returns.
Index is an integer here; cursor admission separately checks vector bounds. -/
inductive Frame where
  | next (code : Ref) (delta : Int) (moved : Bool)
  | chunk (code : Ref) (index delta : Int) (moved : Bool)
  | finish (code : Ref) (delta : Int) (moved : Bool) (context : Option Int)
  deriving DecidableEq

def encodeContext : Option Int → Cell
  | none => .boolean false
  | some id => .number id

def encodeFrame : Frame → List Cell
  | .next code delta moved => [.ref code, .boolean false, .number delta, .boolean moved]
  | .chunk code index delta moved => [.ref code, .number index, .number delta, .boolean moved]
  | .finish code delta moved context =>
      [.ref code, .boolean true, .number delta, .boolean moved, encodeContext context]

def decodeFrame : List Cell → Option Frame
  | [.ref code, .boolean false, .number delta, .boolean moved] => some (.next code delta moved)
  | [.ref code, .number index, .number delta, .boolean moved] => some (.chunk code index delta moved)
  | [.ref code, .boolean true, .number delta, .boolean moved, .boolean false] =>
      some (.finish code delta moved none)
  | [.ref code, .boolean true, .number delta, .boolean moved, .number id] =>
      some (.finish code delta moved (some id))
  | _ => none

theorem frame_roundtrip (frame : Frame) : decodeFrame (encodeFrame frame) = some frame := by
  cases frame with
  | next | chunk => rfl
  | finish code delta moved context => cases context <;> rfl

theorem frame_exact (cells : List Cell) (frame : Frame)
    (decoded : decodeFrame cells = some frame) : encodeFrame frame = cells := by
  unfold decodeFrame at decoded
  split at decoded <;> simp_all [encodeFrame, encodeContext] <;> subst frame <;> rfl

theorem opening_return_retained (code : Ref) (delta id : Int) (moved : Bool) :
    decodeFrame (encodeFrame (.finish code delta moved (some id))) =
      some (.finish code delta moved (some id)) := frame_roundtrip _

/-- Native append pushes tail first and then conses newest-first blocks. -/
def pushBlocks (blocks : List Ref) (delta : Int) (moved : Bool) (pending : List Frame) : List Frame :=
  blocks.foldl (fun stack block => Frame.next block delta moved :: stack) pending

theorem pushed_blocks_order (blocks : List Ref) (delta : Int) (moved : Bool) (pending : List Frame) :
    pushBlocks blocks delta moved pending =
      blocks.reverse.map (fun block => Frame.next block delta moved) ++ pending := by
  induction blocks generalizing pending with
  | nil => rfl
  | cons block rest ih =>
    simp_all [pushBlocks, List.foldl, List.reverse_cons, List.append_assoc]

/-- The physical chunk frame advances exactly one slot; it cannot consume an
out-of-range element. The pending suffix is independent of any chunk prefix. -/
def chunkResume (items : List Ref) (index : Nat) : Option (Ref × List Ref) :=
  match items.drop index with
  | [] => none
  | head :: rest => some (head, rest)

theorem chunk_resume_exact (items : List Ref) (index : Nat) (head : Ref) (rest : List Ref) :
    chunkResume items index = some (head, rest) ↔ items.drop index = head :: rest := by
  simp only [chunkResume]
  generalize items.drop index = suffix
  cases suffix <;> simp

theorem exhausted_chunk_rejected (items : List Ref) (index : Nat) (endOfChunk : items.length ≤ index) :
    chunkResume items index = none := by
  simp [chunkResume, List.drop_eq_nil_iff.mpr endOfChunk]

inductive Command where
  | token (value : TokenData)
  | openNode (kind : String) (offset : Int)
  | closeNode (kind : String) (offset : Int)
  | openField (name : String) (offset : Int)
  | closeField (name : String) (offset : Int)
  | boundary (offset : Int)
  deriving DecidableEq

abbrev Program := EventIterator.Rope Command

def recordChildren : Record → List Ref
  | .empty | .token _ => []
  | .branch left right _ _ _ => [left, right]
  | .view body _ | .value _ _ _ body | .field _ _ _ body => [body]
  | .append leading blocks tail _ _ _ => leading :: blocks ++ [tail]
  | .chunk items _ _ => items

/-- All private record variants lower compositionally, without invoking the
native walker. Cached bounds/heights remain extracted but do not emit events. -/
def lowerRecord (child : Ref → Program) : Record → Program
  | .empty => .empty
  | .token value => .atom (.token value)
  | .branch left right _ _ _ => .branch (child left) (child right)
  | .view body delta => .view (child body) delta
  | .append leading blocks tail _ _ _ =>
      EventIterator.appendEnvelope (child leading) (blocks.map child) (child tail)
  | .chunk items _ _ => items.foldr (fun item rest => .branch (child item) rest) .empty
  | .value none _ finish body => .branch (child body) (.atom (.boundary finish))
  | .value (some kind) start finish body =>
      .branch (.atom (.openNode kind start)) (.branch (child body) (.atom (.closeNode kind finish)))
  | .field name start finish body =>
      .branch (.atom (.openField name start)) (.branch (child body) (.atom (.closeField name finish)))

def readRecord (heap : Nat → Option (List Cell)) : Ref → Option Record
  | none => some .empty
  | some address => (heap address).bind decodeRecord

/-- Iterative chunks always have a first item and a resumable successor. The
native singleton case is a bare leaf, never a one-element chunk. -/
def recordAdmitted (address : Ref) : Record → Prop
  | .empty => address = none
  | .chunk items _ _ => address ≠ none ∧ 2 ≤ items.length
  | _ => address ≠ none

/-- Finite extraction witnesses recurse over every referenced child, including
append blocks and chunk elements. Source-slot reads must establish readRecord;
there is no axiom that turns arbitrary native data into such a witness. -/
inductive Extract (heap : Nat → Option (List Cell)) : Ref → Program → Prop where
  | record (address : Ref) (record : Record) (child : Ref → Program)
      (read : readRecord heap address = some record)
      (admitted : recordAdmitted address record)
      (children : ∀ ref, ref ∈ recordChildren record → Extract heap ref (child ref)) :
      Extract heap address (lowerRecord child record)

theorem extracted_record_slots (heap : Nat → Option (List Cell)) (address : Nat)
    (cells : List Cell) (record : Record) (found : heap address = some cells)
    (read : readRecord heap (some address) = some record) : encodeRecord record = cells := by
  exact record_exact cells record (by simpa [readRecord, found] using read)

/-- Physically advancing a chunk's index leaves exactly the unvisited suffix. -/
theorem advanced_chunk_suffix (items : List Ref) (index : Nat) (head : Ref) (rest : List Ref)
    (slot : chunkResume items index = some (head, rest)) : items.drop (index + 1) = rest := by
  rw [List.drop_add_one_eq_tail_drop, (chunk_resume_exact items index head rest).mp slot]
  rfl

/-- The actual vector-set! at slot one changes only the next chunk index. -/
theorem physical_chunk_write (code : Ref) (index delta : Int) (moved : Bool) :
    decodeFrame ((encodeFrame (.chunk code index delta moved)).set 1 (.number (index + 1))) =
      some (.chunk code (index + 1) delta moved) := by rfl

def decodeStack : List (List Cell) → Option (List Frame)
  | [] => some []
  | packet :: rest => do
      let frame ← decodeFrame packet
      let tail ← decodeStack rest
      pure (frame :: tail)

theorem stack_roundtrip (frames : List Frame) :
    decodeStack (frames.map encodeFrame) = some frames := by
  induction frames with
  | nil => rfl
  | cons frame rest ih => simp [decodeStack, frame_roundtrip, ih]

theorem physical_append_stack (blocks : List Ref) (tail : Ref) (delta : Int)
    (moved : Bool) (pending : List Frame) :
    decodeStack ((pushBlocks blocks delta moved (.next tail delta moved :: pending)).map encodeFrame) =
      some (blocks.reverse.map (fun block => Frame.next block delta moved) ++
        .next tail delta moved :: pending) := by
  rw [stack_roundtrip, pushed_blocks_order]

theorem saved_zero_is_not_missing (code : Ref) (delta : Int) (moved : Bool) :
    encodeFrame (.finish code delta moved (some 0)) ≠ encodeFrame (.finish code delta moved none) := by
  simp [encodeFrame, encodeContext]

theorem extracted_chunk_has_successor (extracted : Extract heap address program)
    (readChunk : readRecord heap address = some (.chunk items start finish)) : 2 ≤ items.length := by
  cases extracted with
  | record address record child read admitted children =>
    have same : record = .chunk items start finish := Option.some.inj (read.symm.trans readChunk)
    subst record
    exact admitted.2

/-- The producer's join of two nonempty bounded leaf blocks cannot create the
single-item chunk that would make its initial index-one frame invalid. -/
theorem block_join_has_successor (left right : List Ref) (nonemptyLeft : left ≠ [])
    (nonemptyRight : right ≠ []) : 2 ≤ (left ++ right).length := by
  have leftPositive : 0 < left.length := List.length_pos_iff.mpr nonemptyLeft
  have rightPositive : 0 < right.length := List.length_pos_iff.mpr nonemptyRight
  simp only [List.length_append]
  omega

theorem node_callbacks (child : Ref → Program) (kind : String) (start finish delta : Int)
    (body : Ref) (moved : Bool) :
    EventIterator.meaning (lowerRecord child (.value (some kind) start finish body)) delta moved =
      [⟨.openNode kind start, delta, moved⟩] ++ EventIterator.meaning (child body) delta moved ++
        [⟨.closeNode kind finish, delta, moved⟩] := by
  simp [lowerRecord, EventIterator.meaning]

theorem field_callbacks (child : Ref → Program) (name : String) (start finish delta : Int)
    (body : Ref) (moved : Bool) :
    EventIterator.meaning (lowerRecord child (.field name start finish body)) delta moved =
      [⟨.openField name start, delta, moved⟩] ++ EventIterator.meaning (child body) delta moved ++
        [⟨.closeField name finish, delta, moved⟩] := by
  simp [lowerRecord, EventIterator.meaning]

theorem fragment_callbacks (child : Ref → Program) (start finish delta : Int) (body : Ref) (moved : Bool) :
    EventIterator.meaning (lowerRecord child (.value none start finish body)) delta moved =
      EventIterator.meaning (child body) delta moved ++ [⟨.boundary finish, delta, moved⟩] := by rfl

theorem chunk_callbacks (child : Ref → Program) (items : List Ref) (start finish delta : Int) (moved : Bool) :
    EventIterator.meaning (lowerRecord child (.chunk items start finish)) delta moved =
      items.flatMap (fun item => EventIterator.meaning (child item) delta moved) := by
  induction items with
  | nil => rfl
  | cons item rest ih =>
    simpa [lowerRecord, EventIterator.meaning] using
      congrArg (List.append (EventIterator.meaning (child item) delta moved)) ih

theorem append_callbacks (child : Ref → Program) (leading tail : Ref) (blocks : List Ref)
    (start finish height delta : Int) (moved : Bool) :
    EventIterator.meaning (lowerRecord child (.append leading blocks tail start finish height)) delta moved =
      EventIterator.meaning (child leading) delta moved ++
        blocks.reverse.flatMap (fun block => EventIterator.meaning (child block) delta moved) ++
          EventIterator.meaning (child tail) delta moved := by
  rw [lowerRecord, EventIterator.append_order, ← List.map_reverse, List.flatMap_map]

end ParserEngine.IteratorEncoding
