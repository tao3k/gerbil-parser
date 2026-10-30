import ParserEngine.Viable
import ParserEngine.Lookahead

/-! Raw-token requests commit a grammar terminal only on shift. Reduction
queries can select a different terminal for the same pending raw token.
The compiler/selector certificates are explicit implementation boundaries. -/
namespace ParserEngine.RawRequest
open Grammar

/-- Ordered relation between chosen grammar terminals and original raw values. -/
inductive Relates (relation : α → β → Prop) : List α → List β → Prop where
  | nil : Relates relation [] []
  | cons {a b as bs} (head : relation a b) (tail : Relates relation as bs) :
      Relates relation (a :: as) (b :: bs)

structure Frame (Raw : Type) where
  grammar : Grammar.Frame
  raw : List Raw

structure Config (Raw : Type) where
  states : List Nat
  frames : List (Frame Raw)
  rest : List Raw

def word (frames : List (Frame Raw)) : List Nat :=
  Request.terminalYield (frames.map fun frame => frame.grammar.terminals)
def rawYield (frames : List (Frame Raw)) : List Raw :=
  Request.terminalYield (frames.map Frame.raw)
def frameSymbols (frames : List (Frame Raw)) : List Symbol :=
  frames.map fun frame => frame.grammar.symbol

/-- The built-in row producer must bind every exact/folded/kind result to
both the published table action and the terminal predicate for that raw value. -/
def RowMeaning (table : Table) (realizesToken : Nat → Lookahead.RawToken → Prop)
    (state : Nat) (input : Option Lookahead.RawToken) (entry : Option Nat × Grammar.Action) : Prop :=
  table.action state entry.1 = some entry.2 ∧
    ∀ raw terminal, input = some raw → entry.1 = some terminal → realizesToken terminal raw

theorem builtin_action_and_terminal (table : Table)
    (realizesToken : Nat → Lookahead.RawToken → Prop)
    (rows : Nat → Lookahead.SelectorRow (Option Nat × Grammar.Action))
    (certificate : ∀ state, Lookahead.RowSupported (rows state) (RowMeaning table realizesToken state))
    (state : Nat) (input : Option Lookahead.RawToken) (key : Option Nat) (action : Grammar.Action)
    (chosen : Lookahead.selectRaw (rows state) input = some (key, action)) :
    table.action state key = some action ∧
      ∀ raw terminal, input = some raw → key = some terminal → realizesToken terminal raw :=
  Lookahead.selected_raw_supported (rows state) (RowMeaning table realizesToken state)
    (certificate state) input (key, action) chosen

structure Engine (Raw : Type) where
  grammar : CFG
  start : Nat
  augmented : Production
  graph : Viable.Graph
  table : Table
  realizesToken : Nat → Raw → Prop
  select : Nat → Option Raw → Option (Option Nat × Grammar.Action)
  constructed : Viable.Constructed grammar augmented graph
  foreign : augmented ∉ grammar
  shape : augmented.rhs = [.nonterminal start]
  supported : TableSupported grammar start augmented table
  covered : ∀ state item, item ∈ table.items state →
    (⟨item.production, item.dot⟩ : Viable.Core) ∈ graph.items state
  sameGoto : table.goto = graph.goto
  selected : ∀ state input key action, select state input = some (key, action) →
    table.action state key = some action
  matched : ∀ state input terminal action, select state (some input) = some (some terminal, action) →
    realizesToken terminal input

structure Valid (engine : Engine Raw) (input : List Raw) (c : Config Raw) : Prop where
  path : Viable.StackPath engine.graph c.states (frameSymbols c.frames)
  grammar : Grammar.FramesValid engine.grammar (c.frames.map Frame.grammar)
  realizes : Relates engine.realizesToken (word c.frames) (rawYield c.frames)
  conserved : rawYield c.frames ++ c.rest = input

def initial (engine : Engine Raw) (input : List Raw) : Config Raw :=
  ⟨[engine.graph.entry], [], input⟩

theorem initial_valid (engine : Engine Raw) (input : List Raw) :
    Valid engine input (initial engine input) := by
  constructor
  · exact Viable.StackPath.start
  · simp [initial, Grammar.FramesValid]
  · exact Relates.nil
  · rfl

def shift (c : Config Raw) (target terminal : Nat) (raw : Raw) (tail : List Raw) : Config Raw :=
  ⟨target :: c.states, ⟨⟨.terminal terminal, [terminal]⟩, [raw]⟩ :: c.frames, tail⟩

def reduce (c : Config Raw) (production : Production) (target : Nat) : Config Raw :=
  let width := production.rhs.length
  ⟨target :: c.states.drop width,
    ⟨⟨.nonterminal production.lhs, word (c.frames.take width)⟩,
      rawYield (c.frames.take width)⟩ :: c.frames.drop width, c.rest⟩

theorem reduce_word (c : Config Raw) (production : Production) (target : Nat) :
    word (reduce c production target).frames = word c.frames := by
  simpa [word, reduce, List.map_take, List.map_drop, List.map_map,
    Capture.split_prefix_exact, Request.terminalYield] using
    Request.capture_conserves (c.frames.map fun frame => frame.grammar.terminals) production.rhs.length

theorem reduce_raw (c : Config Raw) (production : Production) (target : Nat) :
    rawYield (reduce c production target).frames = rawYield c.frames := by
  simpa [rawYield, reduce, List.map_take, List.map_drop, List.map_map,
    Capture.split_prefix_exact, Request.terminalYield] using
    Request.capture_conserves (c.frames.map Frame.raw) production.rhs.length

theorem push_path {graph : Viable.Graph} {states symbols}
    (path : Viable.StackPath graph states symbols) (symbol : Symbol) (target : Nat)
    (edge : graph.goto (states.headD 0) symbol = some target) :
    Viable.StackPath graph (target :: states) (symbol :: symbols) := by
  cases states with
  | nil => have impossible := Viable.stack_depth path; simp at impossible
  | cons state rest => exact Viable.StackPath.push target path edge

private theorem realizes_append {r : α → β → Prop} {a b c d}
    (first : Relates r a b) (second : Relates r c d) :
    Relates r (a ++ c) (b ++ d) := by
  induction first with
  | nil => exact second
  | cons head tail ih => exact Relates.cons head ih

theorem shift_valid (engine : Engine Raw) (input : List Raw) (c : Config Raw)
    (target terminal : Nat) (raw : Raw) (tail : List Raw) (valid : Valid engine input c)
    (pending : c.rest = raw :: tail)
    (selected : engine.select (c.states.headD 0) (some raw) = some (some terminal, .shift target)) :
    Valid engine input (shift c target terminal raw tail) := by
  have action := engine.selected _ _ _ _ selected
  obtain ⟨actual, equal, edge, _⟩ := engine.supported _ _ _ action
  cases equal
  rw [engine.sameGoto] at edge
  constructor
  · exact push_path valid.path (.terminal terminal) target edge
  · intro frame member
    rcases List.mem_cons.mp member with rfl | member
    · exact Derives.terminal terminal Derives.empty
    · exact valid.grammar frame member
  · have result := realizes_append valid.realizes
      (Relates.cons (engine.matched _ _ _ _ selected) Relates.nil)
    simpa [word, rawYield, shift, Request.terminalYield, List.reverse_cons,
      List.flatten_append] using result
  · simpa [rawYield, shift, Request.terminalYield, List.reverse_cons,
      List.flatten_append, List.append_assoc, pending] using valid.conserved

/-- Width/RHS obligations are derived from the graph, not supplied by a caller. -/
theorem reduce_valid (engine : Engine Raw) (input : List Raw) (c : Config Raw)
    (key : Option Nat) (production : Production) (target : Nat) (valid : Valid engine input c)
    (selected : engine.select (c.states.headD 0) c.rest.head? = some (key, .reduce production))
    (edge : engine.table.goto ((c.states.drop production.rhs.length).headD 0)
      (.nonterminal production.lhs) = some target) :
    Valid engine input (reduce c production target) := by
  have action := engine.selected _ _ _ _ selected
  have supported := engine.supported _ _ _ action
  have core := engine.covered _ _ supported.2
  have top : c.states.head? = some (c.states.headD 0) := by
    cases h : c.states with
    | nil => have impossible := Viable.stack_depth valid.path; simp [h] at impossible
    | cons state rest => rfl
  obtain ⟨fits, rhsEq⟩ := Viable.completed_rhs engine.constructed valid.path top production core
  have fitsFrames : production.rhs.length ≤ c.frames.length := by simpa [frameSymbols] using fits
  have rhsGrammar : Grammar.symbols (((c.frames.map Frame.grammar).take production.rhs.length).reverse) =
      production.rhs := by
    simpa [Grammar.symbols, frameSymbols, List.map_reverse, List.map_take, List.map_map, Function.comp_def] using rhsEq
  have prefixValid : Grammar.FramesValid engine.grammar
      (((c.frames.map Frame.grammar).take production.rhs.length).reverse) := by
    intro frame member
    exact valid.grammar frame (List.mem_of_mem_take (List.mem_reverse.mp member))
  have derived := Grammar.reduction_derives production supported.1 _ prefixValid rhsGrammar
  constructor
  · have popped := Viable.drop_path valid.path production.rhs.length fits
    rw [engine.sameGoto] at edge
    have path := push_path popped (.nonterminal production.lhs) target edge
    simpa [reduce, frameSymbols, List.map_drop] using path
  · intro frame member
    rcases List.mem_cons.mp member with rfl | member
    · simpa [reduce, word, Grammar.terminals, Request.terminalYield,
        List.map_reverse, List.map_take, List.map_map, Function.comp_def] using derived
    · exact valid.grammar frame (List.mem_of_mem_drop (by simpa [List.map_drop] using member))
  · rw [reduce_word, reduce_raw]
    exact valid.realizes
  · change rawYield (reduce c production target).frames ++ c.rest = input
    rw [reduce_raw]
    exact valid.conserved

inductive Step (engine : Engine Raw) : Config Raw → Config Raw → Prop where
  | shift (c : Config Raw) (target terminal : Nat) (raw : Raw) (tail : List Raw)
      (pending : c.rest = raw :: tail)
      (selected : engine.select (c.states.headD 0) (some raw) = some (some terminal, .shift target)) :
      Step engine c (shift c target terminal raw tail)
  | reduce (c : Config Raw) (key : Option Nat) (production : Production) (target : Nat)
      (selected : engine.select (c.states.headD 0) c.rest.head? = some (key, .reduce production))
      (edge : engine.table.goto ((c.states.drop production.rhs.length).headD 0)
        (.nonterminal production.lhs) = some target) : Step engine c (reduce c production target)

inductive Trace (engine : Engine Raw) : Config Raw → Config Raw → Prop where
  | refl (c : Config Raw) : Trace engine c c
  | next {a b c} (history : Trace engine a b) (step : Step engine b c) : Trace engine a c

theorem trace_valid {engine : Engine Raw} {input : List Raw} {before after : Config Raw}
    (valid : Valid engine input before) (trace : Trace engine before after) : Valid engine input after := by
  induction trace with
  | refl => exact valid
  | next history step ih =>
    cases step with
    | shift target terminal raw tail pending selected =>
      exact shift_valid engine input _ target terminal raw tail ih pending selected
    | reduce key production target selected edge =>
      exact reduce_valid engine input _ key production target ih selected edge

theorem accepted_derivation (engine : Engine Raw) (input : List Raw) (c : Config Raw)
    (valid : Valid engine input c) (complete : c.rest = [])
    (selected : engine.select (c.states.headD 0) none = some (none, .accept)) :
    ∃ terminals, Derives engine.grammar [.nonterminal engine.start] terminals ∧
      Relates engine.realizesToken terminals input := by
  have action := engine.selected _ _ _ _ selected
  have accepted := engine.supported _ _ _ action
  have core := engine.covered _ _ accepted.2.2
  have top : c.states.head? = some (c.states.headD 0) := by
    cases h : c.states with
    | nil => have impossible := Viable.stack_depth valid.path; simp [h] at impossible
    | cons state rest => rfl
  have single := (Viable.accepting_stack engine.constructed engine.foreign
    engine.start engine.shape valid.path top core).1
  cases framesEq : c.frames with
  | nil => simp [framesEq, frameSymbols] at single
  | cons root tail =>
    have mapped : root.grammar.symbol :: frameSymbols tail = [.nonterminal engine.start] := by
      simpa [framesEq, frameSymbols] using single
    have rootKind := (List.cons.inj mapped).1
    have emptyMap := (List.cons.inj mapped).2
    have tailEmpty : tail = [] := by
      have length := congrArg List.length emptyMap
      simpa [frameSymbols] using length
    have singleFrames : c.frames = [root] := by rw [framesEq, tailEmpty]
    refine ⟨root.grammar.terminals, ?_, ?_⟩
    · have derived := valid.grammar root.grammar (by simp [singleFrames])
      simpa [rootKind] using derived
    · have rawEq : root.raw = input := by simpa [rawYield, Request.terminalYield,
        singleFrames, complete] using valid.conserved
      simpa [word, rawYield, Request.terminalYield, singleFrames, rawEq] using valid.realizes

theorem complete_request (engine : Engine Raw) (input : List Raw) (last : Config Raw)
    (trace : Trace engine (initial engine input) last) (complete : last.rest = [])
    (selected : engine.select (last.states.headD 0) none = some (none, .accept)) :
    ∃ terminals, Derives engine.grammar [.nonterminal engine.start] terminals ∧
      Relates engine.realizesToken terminals input :=
  accepted_derivation engine input last (trace_valid (initial_valid engine input) trace) complete selected

namespace Entry
/-- Left-to-right maximum used by lr-initial-checkpoint/selected. The decoder
admits nonnegative token ends; arbitrary Scheme values are not covered. -/
def scanEnd (endOffset : Raw → Nat) : List Raw → Nat → Nat
  | [], current => current
  | token :: rest, current => scanEnd endOffset rest (max current (endOffset token))

theorem scanEnd_le_iff (endOffset : Raw → Nat) (tokens : List Raw) (current bound : Nat) :
    scanEnd endOffset tokens current ≤ bound ↔
      current ≤ bound ∧ ∀ token ∈ tokens, endOffset token ≤ bound := by
  induction tokens generalizing current with
  | nil => simp [scanEnd]
  | cons token rest ih => simp [scanEnd, ih, Nat.max_le, and_assoc]

theorem token_end_bounded (endOffset : Raw → Nat) (tokens : List Raw)
    (token : Raw) (member : token ∈ tokens) :
    endOffset token ≤ scanEnd endOffset tokens 0 :=
  ((scanEnd_le_iff endOffset tokens 0 _).mp (Nat.le_refl _)).2 token member

/-- Initial fields of the selected non-layout entry. Frames denote decoded
semantic values; decoding the empty native value list is an explicit boundary.
This record does not model Scheme heap allocation or nonempty AST decoding. -/
structure Checkpoint (Runtime Raw : Type) where
  runtime : Runtime
  input : List Raw
  inputEnd : Nat
  config : Config Raw
  actions : Nat
  shifts : Nat
  recognition : Option (List (Frame Raw))

def construct (runtime : Runtime) (endOffset : Raw → Nat) (tokens : List Raw)
    (observe layout : Bool) : Checkpoint Runtime Raw :=
  ⟨runtime, tokens, scanEnd endOffset tokens 0, ⟨[0], [], tokens⟩, 0, 0,
    if observe && !layout then some [] else none⟩

/-- Entry state zero is an actual constructor literal, not inferred from an
arbitrary graph. The graph decoder must establish its matching entry. -/
theorem config_exact (engine : Engine Raw) (endOffset : Raw → Nat) (tokens : List Raw)
    (observe layout : Bool) (entryZero : engine.graph.entry = 0) :
    (construct engine endOffset tokens observe layout).config = initial engine tokens := by
  simp [construct, initial, entryZero]

theorem constructed_valid (engine : Engine Raw) (endOffset : Raw → Nat) (tokens : List Raw)
    (observe layout : Bool) (entryZero : engine.graph.entry = 0) :
    Valid engine tokens (construct engine endOffset tokens observe layout).config := by
  rw [config_exact engine endOffset tokens observe layout entryZero]
  exact initial_valid engine tokens

/-- Selection identity and all dynamic counters/stacks are set by this call.
No preceding request state is an argument to this constructor. -/
theorem fresh_fields (runtime : Runtime) (endOffset : Raw → Nat) (tokens : List Raw)
    (observe layout : Bool) :
    let checkpoint := construct runtime endOffset tokens observe layout
    checkpoint.runtime = runtime ∧ checkpoint.input = tokens ∧ checkpoint.config.frames = [] ∧
      checkpoint.config.rest = tokens ∧ checkpoint.actions = 0 ∧ checkpoint.shifts = 0 ∧
      checkpoint.recognition = (if observe && !layout then some [] else none) := by
  simp [construct]

/-- Compose the concrete constructor adapter with the existing arbitrary
request theorem. Runtime/graph/table/selector certificates remain in Engine;
this does not prove native lowering or host execution. -/
theorem complete_from_constructed (engine : Engine Raw) (endOffset : Raw → Nat)
    (tokens : List Raw) (observe : Bool) (entryZero : engine.graph.entry = 0)
    (last : Config Raw)
    (trace : Trace engine (construct engine endOffset tokens observe false).config last)
    (complete : last.rest = [])
    (selected : engine.select (last.states.headD 0) none = some (none, .accept)) :
    ∃ terminals, Derives engine.grammar [.nonterminal engine.start] terminals ∧
      Relates engine.realizesToken terminals tokens := by
  rw [config_exact engine endOffset tokens observe false entryZero] at trace
  exact complete_request engine tokens last trace complete selected
end Entry

namespace Backend
/-- Branch order of lr-runtime-for-current-semantic-backend. Cache publication
and allocation are external to this pure adapter, not silently proved here. -/
def select (enabled alreadyEvent : Bool) (runtime : Runtime)
    (cached : Option Runtime) (eligible : Bool) (build : Runtime → Runtime) : Runtime :=
  if !enabled || alreadyEvent then runtime else
    match cached with
    | some selected => selected
    | none => if !eligible then runtime else build runtime

/-- A cache hit requires the same decoded parser engine as its owner. The
native cache field has no validation guard; its producer must establish this.
Semantic reducers/AST allocation are deliberately outside the engine view. -/
theorem kernel_preserved (kernel : Runtime → Kernel) (enabled alreadyEvent : Bool)
    (runtime : Runtime) (cached : Option Runtime) (eligible : Bool) (build : Runtime → Runtime)
    (cacheSound : ∀ selected, cached = some selected → kernel selected = kernel runtime)
    (buildSound : kernel (build runtime) = kernel runtime) :
    kernel (select enabled alreadyEvent runtime cached eligible build) = kernel runtime := by
  unfold select
  split
  · rfl
  · cases cached with
    | some selected => exact cacheSound selected rfl
    | none =>
      dsimp
      split
      · rfl
      · exact buildSound

/-- The cached branch precedes eligibility. This is a correspondence fact,
not an admission rule for stale or foreign cache entries. -/
theorem cached_before_eligibility (runtime selected : Runtime) (eligible : Bool)
    (build : Runtime → Runtime) :
    select true false runtime (some selected) eligible build = selected := by
  simp [select]

/-- Compose backend selection, the actual zero-state constructor adapter and
arbitrary accepted traces. Native engine decoding, cache ownership, table and
selector certificates, and physical host execution remain explicit boundaries. -/
theorem complete_request (kernel : Runtime → Engine Raw)
    (enabled alreadyEvent : Bool) (runtime : Runtime) (cached : Option Runtime)
    (eligible : Bool) (build : Runtime → Runtime)
    (cacheSound : ∀ selected, cached = some selected → kernel selected = kernel runtime)
    (buildSound : kernel (build runtime) = kernel runtime)
    (endOffset : Raw → Nat) (tokens : List Raw) (observe : Bool)
    (entryZero : (kernel runtime).graph.entry = 0) (last : Config Raw)
    (trace : Trace (kernel (select enabled alreadyEvent runtime cached eligible build))
      (Entry.construct (select enabled alreadyEvent runtime cached eligible build)
        endOffset tokens observe false).config last)
    (complete : last.rest = [])
    (accepted : (kernel (select enabled alreadyEvent runtime cached eligible build)).select
      (last.states.headD 0) none = some (none, .accept)) :
    ∃ terminals, Derives (kernel runtime).grammar [.nonterminal (kernel runtime).start] terminals ∧
      Relates (kernel runtime).realizesToken terminals tokens := by
  have same := kernel_preserved kernel enabled alreadyEvent runtime cached eligible build
    cacheSound buildSound
  rw [same] at trace accepted
  have startEq : (Entry.construct
      (select enabled alreadyEvent runtime cached eligible build) endOffset tokens observe false).config =
      initial (kernel runtime) tokens := by
    simp [Entry.construct, initial, entryZero]
  rw [startEq] at trace
  exact RawRequest.complete_request (kernel runtime) tokens last trace complete accepted
end Backend

end ParserEngine.RawRequest
