import ParserEngine.Publication

/-! Position views and explicit rope continuations. Native extraction, callback
execution and source-token identity remain external premises. -/
namespace ParserEngine.EventIterator

inductive Rope (α : Type) where
  | empty
  | atom (value : α)
  | branch (left right : Rope α)
  | view (body : Rope α) (delta : Int)

structure Located (α : Type) where
  value : α
  delta : Int
  moved : Bool
  deriving DecidableEq

/-- Chunks preserve vector-index order; append blocks are newest first. -/
def chunk (items : List α) : Rope α :=
  items.foldr (fun item rest => .branch (.atom item) rest) .empty

def appendEnvelope (leading : Rope α) (newestFirst : List (Rope α)) (tail : Rope α) : Rope α :=
  .branch leading (newestFirst.foldl (fun rest block => .branch block rest) tail)

def meaning : Rope α → Int → Bool → List (Located α)
  | .empty, _, _ => []
  | .atom value, delta, moved => [⟨value, delta, moved⟩]
  | .branch left right, delta, moved => meaning left delta moved ++ meaning right delta moved
  | .view body shift, delta, _ => meaning body (delta + shift) true

theorem chunk_order (items : List α) (delta : Int) (moved : Bool) :
    meaning (chunk items) delta moved = items.map (fun value => ⟨value, delta, moved⟩) := by
  induction items with
  | nil => rfl
  | cons item rest ih => simpa [chunk, meaning] using congrArg (List.cons ⟨item, delta, moved⟩) ih

theorem blocks_order (blocks : List (Rope α)) (tail : Rope α) (delta : Int) (moved : Bool) :
    meaning (blocks.foldl (fun rest block => .branch block rest) tail) delta moved =
      (blocks.reverse.flatMap (fun block => meaning block delta moved)) ++ meaning tail delta moved := by
  induction blocks generalizing tail with
  | nil => rfl
  | cons block rest ih =>
    simp [List.foldl, ih, List.reverse_cons, List.flatMap_append, meaning, List.append_assoc]

theorem append_order (leading : Rope α) (blocks : List (Rope α)) (tail : Rope α)
    (delta : Int) (moved : Bool) :
    meaning (appendEnvelope leading blocks tail) delta moved =
      meaning leading delta moved ++ blocks.reverse.flatMap (fun block => meaning block delta moved) ++
        meaning tail delta moved := by
  simp [appendEnvelope, meaning, blocks_order, List.append_assoc]

/-- Cancellation retains translated provenance, including for zero shift. -/
theorem view_composition (body : Rope α) (outer inner delta : Int) (moved : Bool) :
    meaning (.view (.view body inner) outer) delta moved =
      meaning (.view body (outer + inner)) delta moved := by
  simp [meaning, Int.add_assoc]

theorem cancelled_view (body : Rope α) (shift delta : Int) (moved : Bool) :
    meaning (.view (.view body (-shift)) shift) delta moved = meaning body delta true := by
  simp only [meaning]
  have cancel : delta + shift + -shift = delta := by omega
  rw [cancel]

/-- Exact translated metadata checks from artifact.ss. The unchanged path also
requires native object identity; its truth is supplied by extraction. -/
def tokenClaim (old current : Publication.Token) (sameObject : Prop) (delta : Int) (moved : Bool) : Prop :=
  if moved then
    old.kind = current.kind ∧ old.lexeme = current.lexeme ∧
    (current.span.start : Int) = (old.span.start : Int) + delta ∧
    (current.span.finish : Int) = (old.span.finish : Int) + delta
  else sameObject ∧ old = current

theorem cancelled_token_claim (old current : Publication.Token) (sameObject : Prop) :
    tokenClaim old current sameObject 0 true ↔ old = current := by
  rcases old with ⟨ok, ol, ⟨os, oe⟩⟩
  rcases current with ⟨ck, cl, ⟨cs, ce⟩⟩
  simp only [tokenClaim, ↓reduceIte, Int.add_zero]
  constructor
  · rintro ⟨rfl, rfl, hs, he⟩
    have starts : cs = os := by omega
    have ends : ce = oe := by omega
    subst cs; subst ce; rfl
  · intro equal
    cases equal
    exact ⟨rfl, rfl, rfl, rfl⟩

theorem translated_claim_composition (old middle current : Publication.Token)
    (first second : Int) (sameObject : Prop)
    (left : tokenClaim old middle sameObject first true)
    (right : tokenClaim middle current sameObject second true) :
    tokenClaim old current sameObject (first + second) true := by
  simp only [tokenClaim, ↓reduceIte] at left right ⊢
  exact ⟨left.1.trans right.1, left.2.1.trans right.2.1, by omega, by omega⟩

theorem negative_start_rejected (old current : Publication.Token) (delta : Int)
    (sameObject : Prop) (negative : (old.span.start : Int) + delta < 0) :
    ¬tokenClaim old current sameObject delta true := by
  simp only [tokenClaim, ↓reduceIte]
  rintro ⟨_, _, start, _⟩
  omega

theorem unchanged_token_requires_identity (old current : Publication.Token) (sameObject : Prop) :
    tokenClaim old current sameObject 0 false → sameObject := by
  simp [tokenClaim]
  exact fun identity _ => identity

structure Frame (α : Type) where
  code : Rope α
  delta : Int
  moved : Bool

def pendingMeaning (pending : List (Frame α)) : List (Located α) :=
  pending.flatMap fun frame => meaning frame.code frame.delta frame.moved

structure State (α : Type) where
  pending : List (Frame α)
  emitted : List (Located α)

def initial (code : Rope α) : State α := ⟨[⟨code, 0, false⟩], []⟩

/-- Deferred siblings retain their own shift and moved bit. -/
inductive Step : State α → State α → Prop where
  | empty (rest out delta moved) :
      Step ⟨⟨.empty, delta, moved⟩ :: rest, out⟩ ⟨rest, out⟩
  | atom (value rest out delta moved) :
      Step ⟨⟨.atom value, delta, moved⟩ :: rest, out⟩
        ⟨rest, out ++ [⟨value, delta, moved⟩]⟩
  | branch (left right rest out delta moved) :
      Step ⟨⟨.branch left right, delta, moved⟩ :: rest, out⟩
        ⟨⟨left, delta, moved⟩ :: ⟨right, delta, moved⟩ :: rest, out⟩
  | view (body shift rest out delta moved) :
      Step ⟨⟨.view body shift, delta, moved⟩ :: rest, out⟩
        ⟨⟨body, delta + shift, true⟩ :: rest, out⟩

theorem step_conserves (step : Step before after) :
    before.emitted ++ pendingMeaning before.pending =
      after.emitted ++ pendingMeaning after.pending := by
  cases step <;> simp [pendingMeaning, meaning, List.append_assoc]

inductive Trace : State α → State α → Prop where
  | refl (state) : Trace state state
  | next : Trace first before → Step before after → Trace first after

theorem trace_conserves (trace : Trace before after) :
    before.emitted ++ pendingMeaning before.pending =
      after.emitted ++ pendingMeaning after.pending := by
  induction trace with
  | refl => rfl
  | next _ step ih => exact ih.trans (step_conserves step)

theorem trace_trans (first : Trace a b) (second : Trace b c) : Trace a c := by
  induction second with
  | refl => exact first
  | next _ step ih => exact .next ih step

/-- Every finite extracted rope has a terminating explicit-stack traversal.
This is not a premise about cyclic or concurrently mutated native objects. -/
theorem frame_completion (code : Rope α) (delta : Int) (moved : Bool)
    (rest : List (Frame α)) (out : List (Located α)) :
    Trace ⟨⟨code, delta, moved⟩ :: rest, out⟩
      ⟨rest, out ++ meaning code delta moved⟩ := by
  induction code generalizing delta moved rest out with
  | empty => simpa [meaning] using Trace.next (Trace.refl _) (Step.empty rest out delta moved)
  | atom value => exact .next (.refl _) (.atom value rest out delta moved)
  | branch left right il ir =>
    have start := Trace.next (Trace.refl _) (Step.branch left right rest out delta moved)
    have leftDone := trace_trans start (il delta moved (⟨right, delta, moved⟩ :: rest) out)
    simpa [meaning, List.append_assoc] using
      trace_trans leftDone (ir delta moved rest (out ++ meaning left delta moved))
  | view body shift ih =>
    exact trace_trans (.next (.refl _) (.view body shift rest out delta moved))
      (ih (delta + shift) true rest out)

theorem iterator_total (code : Rope α) :
    Trace (initial code) ⟨[], meaning code 0 false⟩ := by
  simpa [initial] using frame_completion code 0 false [] []

theorem complete_iterator (code : Rope α) (trace : Trace (initial code) last)
    (finished : last.pending = []) : last.emitted = meaning code 0 false := by
  have h := trace_conserves trace
  simpa [initial, pendingMeaning, finished] using h.symm

/-- Consumer resolution admits signed positions and claims. There is no hidden
Int.toNat conversion. This transports the exact publication steps used by
Publication.complete_product, including source payloads and retained node IDs. -/
theorem publication_refinement (code : Rope α) (trace : Trace (initial code) last)
    (finished : last.pending = []) (resolve : Located α → List Publication.Op)
    (published : Publication.Trace trivia before (last.emitted.flatMap resolve) after events) :
    Publication.Trace trivia before ((meaning code 0 false).flatMap resolve) after events := by
  simpa [complete_iterator code trace finished] using published

/-- Joint request/iterator/product theorem. The extracted rope must denote the
canonical root operations; this correspondence is never inferred from validity
of the final artifact or from a finite test. -/
theorem complete_request_product (driver : RequestAST.Driver Raw) (input : List Raw)
    (last : RequestAST.State Raw) (instructions : List (RequestAST.Instruction Raw))
    (root : Publication.Value) (source : List Publication.Token) (sourceEnd : Nat)
    (rope : Rope α) (iterator : State α) (resolve : Located α → List Publication.Op)
    (trivia : Publication.Token → Prop) (published : Publication.State) (events : List Publication.Event)
    (request : RequestAST.Trace driver (RequestAST.initial driver input) instructions last)
    (complete : last.raw.rest = [])
    (accepted : driver.engine.select (last.raw.states.headD 0) none = some (none, .accept))
    (rootShape : last.values = [[(none, Publication.decodeValue root)]])
    (walk : Trace (initial rope) iterator) (finished : iterator.pending = [])
    (code : Publication.rootOps root sourceEnd = some ((meaning rope 0 false).flatMap resolve))
    (publication : Publication.Trace trivia (Publication.initial source)
      (iterator.emitted.flatMap resolve) published events)
    (consumed : published.remaining = []) (closed : published.frames = []) :
    (∃ terminals, Grammar.Derives driver.engine.grammar [.nonterminal driver.engine.start] terminals ∧
      RawRequest.Relates driver.engine.realizesToken terminals input) ∧
    RequestAST.run driver.rules driver.leaf instructions [] = some [[(none, Publication.decodeValue root)]] ∧
    Publication.rootOps root sourceEnd = some (iterator.emitted.flatMap resolve) ∧
    Publication.tokenEntries events = Publication.numbered 0 source ∧
    published.nextNode = Publication.nodeOpens events ∧ published.frames = [] := by
  have parsed := RequestAST.complete_request driver input last instructions
    (Publication.decodeValue root) request complete accepted rootShape
  have canonical := publication_refinement rope walk finished resolve publication
  have conserved := Publication.trace_conserves canonical
  have rootCode : Publication.rootOps root sourceEnd = some (iterator.emitted.flatMap resolve) := by
    simpa [complete_iterator rope walk finished] using code
  refine ⟨parsed.1, parsed.2, rootCode, ?_, ?_, closed⟩
  · simpa [Publication.initial, Publication.numbered, consumed] using conserved
  · simpa [Publication.initial] using Publication.trace_node_counter canonical

end ParserEngine.EventIterator
