import ParserEngine.Grammar

/-! The LR graph obligations that derive, rather than assume, RHS agreement
and the singleton start stack. Lookahead is erased only for core provenance;
raw-token selection and its lexical observation remain separate owners. -/
namespace ParserEngine.Viable
open Grammar

structure Core where
  production : Production
  dot : Nat
  deriving DecidableEq, Repr

def advance (before : Core) (symbol : Symbol) (after : Core) : Prop :=
  after = {before with dot := before.dot + 1} ∧
    before.production.rhs[before.dot]? = some symbol

/-- make-item-layout allocates width + 1 consecutive slots per production.
These functions model both its offset loop and its owner-vector fill loop. -/
def denseOffset : List Nat → Nat → Nat
  | [], _ => 0
  | _ :: _, 0 => 0
  | width :: rest, production + 1 => width + 1 + denseOffset rest production

def denseCoordinates : List Nat → List (Nat × Nat)
  | [] => []
  | width :: rest => (List.range (width + 1)).map (fun dot => (0, dot)) ++
      (denseCoordinates rest).map (fun coordinate => (coordinate.1 + 1, coordinate.2))

/-- Arbitrary production count and RHS width, including empty productions:
every valid encoded item decodes to its original physical production and dot.
No maximum-width padding or sample grammar is assumed. -/
theorem dense_coordinate_roundtrip {widths : List Nat} {production width dot : Nat}
    (row : widths[production]? = some width) (valid : dot ≤ width) :
    (denseCoordinates widths)[denseOffset widths production + dot]? = some (production, dot) := by
  induction widths generalizing production with
  | nil => simp at row
  | cons head tail ih =>
    cases production with
    | zero =>
      have same : head = width := by simpa using row
      subst head
      simp only [denseCoordinates, denseOffset, Nat.zero_add]
      rw [List.getElem?_append_left (by simp; omega)]
      simp [List.getElem?_range (by omega : dot < width + 1)]
    | succ production =>
      have next : tail[production]? = some width := by simpa using row
      simp only [denseCoordinates, denseOffset]
      rw [List.getElem?_append_right (by simp; omega)]
      have offset : head + 1 + denseOffset tail production + dot -
          ((List.range (head + 1)).map (fun dot => (0, dot))).length =
          denseOffset tail production + dot := by simp; omega
      rw [offset, List.getElem?_map, ih next]
      rfl

/-- The native +1 goto operation cannot cross a production boundary when the
after-dot symbol exists. Both coordinates are recovered by the same decoder. -/
theorem dense_advance_roundtrip {widths : List Nat} {production width dot : Nat}
    (row : widths[production]? = some width) (next : dot < width) :
    (denseCoordinates widths)[denseOffset widths production + dot + 1]? =
      some (production, dot + 1) := by
  simpa [Nat.add_assoc] using dense_coordinate_roundtrip row (by omega : dot + 1 ≤ width)

/-- Decode the physical catalog through the actual ordered production table. -/
def decodeCore (g : CFG) (code : Nat) : Option Core := do
  let coordinate ← (denseCoordinates (g.map (fun rule => rule.rhs.length)))[code]?
  let rule ← g[coordinate.1]?
  return ⟨rule, coordinate.2⟩

theorem decode_core_roundtrip {g : CFG} {production dot : Nat} {rule : Production}
    (row : g[production]? = some rule) (valid : dot ≤ rule.rhs.length) :
    decodeCore g (denseOffset (g.map (fun rule => rule.rhs.length)) production + dot) =
      some ⟨rule, dot⟩ := by
  have width : (g.map (fun rule => rule.rhs.length))[production]? = some rule.rhs.length := by
    simp [row]
  simp [decodeCore, dense_coordinate_roundtrip width valid, row]

/-- The native integer increment used in goto decodes to the logical advance
relation. Its premise is the actual after-dot symbol, so completed items do
not accidentally shift into the following production. -/
theorem dense_goto_refines_advance {g : CFG} {production dot : Nat} {rule : Production}
    {symbol : Symbol} (row : g[production]? = some rule)
    (next : rule.rhs[dot]? = some symbol) :
    ∃ before after,
      decodeCore g (denseOffset (g.map (fun rule => rule.rhs.length)) production + dot) = some before ∧
      decodeCore g (denseOffset (g.map (fun rule => rule.rhs.length)) production + dot + 1) = some after ∧
      advance before symbol after := by
  have within : dot < rule.rhs.length := by
    by_cases bound : dot < rule.rhs.length
    · exact bound
    · simp [List.getElem?_eq_none (by omega : rule.rhs.length ≤ dot)] at next
  refine ⟨⟨rule, dot⟩, ⟨rule, dot + 1⟩, decode_core_roundtrip row (by omega), ?_, rfl, next⟩
  simpa [Nat.add_assoc] using decode_core_roundtrip row (by omega : dot + 1 ≤ rule.rhs.length)

/-- Closure starts from a kernel and only introduces dot-zero predictions. -/
inductive Closure (g : CFG) (seed : Core → Prop) : Core → Prop where
  | seed {item} (member : seed item) : Closure g seed item
  | predict {parent name} (known : Closure g seed parent)
      (next : parent.production.rhs[parent.dot]? = some (.nonterminal name))
      (rule : Production) (member : rule ∈ g) (lhs : rule.lhs = name) :
      Closure g seed ⟨rule, 0⟩

/-- An operational prediction history starts with the kernel and appends only
fresh dot-zero predictions justified by an item already published. The native
bitset solver may batch these steps; physical traversal order is irrelevant. -/
inductive ClosureTrace (g : CFG) (kernel : List Core) : List Core → Prop where
  | initial : ClosureTrace g kernel kernel
  | predict {items parent name rule}
      (earlier : ClosureTrace g kernel items) (known : parent ∈ items)
      (next : parent.production.rhs[parent.dot]? = some (.nonterminal name))
      (member : rule ∈ g) (lhs : rule.lhs = name)
      (fresh : (⟨rule, 0⟩ : Core) ∉ items) :
      ClosureTrace g kernel (items ++ [⟨rule, 0⟩])

theorem closure_trace_supported {g kernel items} (history : ClosureTrace g kernel items) :
    ∀ item ∈ items, Closure g (fun core => core ∈ kernel) item := by
  induction history with
  | initial => exact fun _ member => Closure.seed member
  | predict earlier known next member lhs fresh ih =>
    intro item belongs
    rcases List.mem_append.mp belongs with old | added
    · exact ih item old
    · have same := List.mem_singleton.mp added
      subst item
      exact Closure.predict (ih _ known) next _ member lhs

theorem closure_trace_retains_kernel {g kernel items} (history : ClosureTrace g kernel items) :
    ∀ item ∈ kernel, item ∈ items := by
  induction history with
  | initial => exact fun _ member => member
  | predict earlier known next member lhs fresh ih =>
    exact fun item belongs => List.mem_append_left _ (ih item belongs)

/-- Quiescence is expressed as closure under the actual prediction operation,
not as an assumed equality with the desired result. Combined with the history
it proves both directions for arbitrary grammars and arbitrarily long traces. -/
theorem closure_trace_exact {g kernel items} (history : ClosureTrace g kernel items)
    (quiescent : ∀ parent ∈ items, ∀ name,
      parent.production.rhs[parent.dot]? = some (.nonterminal name) →
      ∀ rule ∈ g, rule.lhs = name → (⟨rule, 0⟩ : Core) ∈ items) :
    ∀ item, item ∈ items ↔ Closure g (fun core => core ∈ kernel) item := by
  intro item
  constructor
  · exact closure_trace_supported history item
  · intro derivation
    induction derivation with
    | seed member => exact closure_trace_retains_kernel history _ member
    | predict known next rule member lhs ih => exact quiescent _ ih _ next _ member lhs

def moved (items : List Core) (symbol : Symbol) (item : Core) : Prop :=
  ∃ before ∈ items, advance before symbol item

def erase (item : Item) : Core := ⟨item.production, item.dot⟩

/-- LR(1) closure retains FIRST/nullable lookahead conditions from Grammar.
Erasure drops only that lookahead, never the production or dot coordinate. -/
inductive LR1Closure (g : CFG) (seed : Item → Prop) : Item → Prop where
  | seed {item} (member : seed item) : LR1Closure g seed item
  | predict {parent child} (known : LR1Closure g seed parent)
      (prediction : Predicts g parent child) : LR1Closure g seed child

/-- LR(1) histories retain the full suffix-FIRST/inherited-lookahead premise;
core erasure is performed only after this construction is justified. -/
inductive LR1Trace (g : CFG) (kernel : List Item) : List Item → Prop where
  | initial : LR1Trace g kernel kernel
  | predict {items parent child} (earlier : LR1Trace g kernel items)
      (known : parent ∈ items) (prediction : Predicts g parent child)
      (fresh : child ∉ items) : LR1Trace g kernel (items ++ [child])

theorem lr1_trace_supported {g kernel items} (history : LR1Trace g kernel items) :
    ∀ item ∈ items, LR1Closure g (fun seed => seed ∈ kernel) item := by
  induction history with
  | initial => exact fun _ member => LR1Closure.seed member
  | predict earlier known prediction fresh ih =>
    intro item member
    rcases List.mem_append.mp member with old | added
    · exact ih item old
    · have equal := List.mem_singleton.mp added
      subst item
      exact LR1Closure.predict (ih _ known) prediction

theorem lr1_trace_retains_kernel {g kernel items} (history : LR1Trace g kernel items) :
    ∀ item ∈ kernel, item ∈ items := by
  induction history with
  | initial => exact fun _ member => member
  | predict earlier known prediction fresh ih =>
    exact fun item member => List.mem_append_left _ (ih item member)

theorem lr1_trace_exact {g kernel items} (history : LR1Trace g kernel items)
    (quiescent : ∀ parent ∈ items, ∀ child, Predicts g parent child → child ∈ items) :
    ∀ item, item ∈ items ↔ LR1Closure g (fun seed => seed ∈ kernel) item := by
  intro item
  constructor
  · exact lr1_trace_supported history item
  · intro derivation
    induction derivation with
    | seed member => exact lr1_trace_retains_kernel history _ member
    | predict known prediction ih => exact quiescent _ ih _ prediction

def lr1GotoKernel (items : List Item) (symbol : Symbol) : List Item :=
  items.filterMap fun before =>
    if before.production.rhs[before.dot]? = some symbol then
      some {before with dot := before.dot + 1} else none

theorem lr1_goto_kernel_exact {items symbol item} :
    item ∈ lr1GotoKernel items symbol ↔ ∃ before ∈ items, Advances before symbol item := by
  constructor
  · intro member
    obtain ⟨before, belongs, emitted⟩ := List.mem_filterMap.mp member
    split at emitted
    next actual => exact ⟨before, belongs, actual, (Option.some.inj emitted).symm⟩
    next absent => cases emitted
  · rintro ⟨before, member, next, equal⟩
    exact List.mem_filterMap.mpr ⟨before, member, by simp [next, equal]⟩

theorem lr1_closure_erases {g : CFG} {seed : Item → Prop} {item : Item}
    (member : LR1Closure g seed item) :
    Closure g (fun core => ∃ original, seed original ∧ erase original = core) (erase item) := by
  induction member with
  | seed member => exact Closure.seed ⟨_, member, rfl⟩
  | @predict parent child known prediction ih =>
    obtain ⟨name, suffix, next, rule, lhs, dot, lookahead⟩ := prediction
    have atSymbol : parent.production.rhs[parent.dot]? = some (.nonterminal name) := by
      have head := congrArg List.head? next
      simpa only [List.head?_drop, List.head?_cons] using head
    have result := Closure.predict ih atSymbol child.production rule lhs
    simpa [erase, dot] using result

theorem closure_mono {g : CFG} {left right : Core → Prop}
    (inclusion : ∀ core, left core → right core) {item : Core}
    (member : Closure g left item) : Closure g right item := by
  induction member with
  | seed member => exact Closure.seed (inclusion _ member)
  | predict known next rule member lhs ih => exact Closure.predict ih next rule member lhs

theorem positive_closure_has_seed {g : CFG} {seed : Core → Prop} {item : Core}
    (member : Closure g seed item) (positive : 0 < item.dot) : seed item := by
  cases member with
  | seed member => exact member
  | predict known next rule member lhs => simp at positive

theorem closure_no_foreign_zero {g : CFG} {seed : Core → Prop} (rule : Production)
    (foreign : rule ∉ g) (absent : ¬ seed ⟨rule, 0⟩) : ¬ Closure g seed ⟨rule, 0⟩ := by
  intro member
  cases member with
  | seed member => exact absent member
  | predict known next rule member lhs => exact foreign member

structure Graph where
  entry : Nat
  items : Nat → List Core
  goto : Nat → Symbol → Option Nat

/-- An executable constructor must establish this membership relation.
Subset suffices, so empty-lookahead predictions may be omitted. This is not
an assumption that native packed IDs or refined states already refine it. -/
structure Constructed (g : CFG) (augmented : Production) (graph : Graph) : Prop where
  initial : ∀ item ∈ graph.items graph.entry,
    Closure g (fun seed => seed = ⟨augmented, 0⟩) item
  targets : ∀ state symbol target, graph.goto state symbol = some target →
    ∀ item ∈ graph.items target, Closure g (moved (graph.items state) symbol) item

/-- Goto kernels are produced by inspecting the actual symbol and incrementing
the dot, rather than accepting an arbitrary list as a supplied certificate. -/
def gotoKernel (items : List Core) (symbol : Symbol) : List Core :=
  items.filterMap fun before =>
    if before.production.rhs[before.dot]? = some symbol then
      some {before with dot := before.dot + 1} else none

theorem goto_kernel_exact {items symbol item} :
    item ∈ gotoKernel items symbol ↔ moved items symbol item := by
  constructor
  · intro member
    obtain ⟨before, belongs, emitted⟩ := List.mem_filterMap.mp member
    split at emitted
    next actual => exact ⟨before, belongs, (Option.some.inj emitted).symm, actual⟩
    next absent => cases emitted
  · rintro ⟨before, member, equal, next⟩
    exact List.mem_filterMap.mpr ⟨before, member, by simp [next, equal]⟩

/-- Operational state-construction histories derive the graph obligations used
by viable-prefix soundness. State identifiers may be interned in any order;
the caller must connect every recorded physical edge to its actual kernel. -/
theorem construction_traces_supported (g : CFG) (augmented : Production) (graph : Graph)
    (initialHistory : ClosureTrace g [⟨augmented, 0⟩] (graph.items graph.entry))
    (targets : ∀ state symbol target, graph.goto state symbol = some target →
      ClosureTrace g (gotoKernel (graph.items state) symbol) (graph.items target)) :
    Constructed g augmented graph := by
  constructor
  · intro item member
    apply closure_mono _ (closure_trace_supported initialHistory item member)
    intro core belongs
    exact List.mem_singleton.mp belongs
  · intro state symbol target edge item member
    apply closure_mono _ (closure_trace_supported (targets state symbol target edge) item member)
    exact fun _ belongs => goto_kernel_exact.mp belongs

/-- LR(1) closure/goto construction implies the core certificate used below.
Native encodings, interning and lookahead propagation still need refinement;
this theorem connects the logical FIRST/nullable owner to viable prefixes. -/
theorem lr1_graph_constructed (g : CFG) (augmented : Production) (graph : Graph)
    (items : Nat → List Item)
    (projection : ∀ state, graph.items state = (items state).map erase)
    (initial : ∀ item ∈ items graph.entry,
      LR1Closure g (fun seed => seed = ⟨augmented, 0, none⟩) item)
    (targets : ∀ source symbol target, graph.goto source symbol = some target →
      ∀ item ∈ items target, LR1Closure g
        (fun after => ∃ before ∈ items source, Advances before symbol after) item) :
    Constructed g augmented graph := by
  constructor
  · intro core member
    rw [projection] at member
    obtain ⟨item, itemMember, equal⟩ := List.mem_map.mp member
    subst core
    apply closure_mono _ (lr1_closure_erases (initial item itemMember))
    intro core seed
    obtain ⟨original, identity, erased⟩ := seed
    subst original
    simpa [erase] using erased.symm
  · intro source symbol target edge core member
    rw [projection] at member
    obtain ⟨item, itemMember, equal⟩ := List.mem_map.mp member
    subst core
    apply closure_mono _ (lr1_closure_erases (targets source symbol target edge item itemMember))
    intro core seed
    obtain ⟨after, ⟨before, beforeMember, advanced⟩, erased⟩ := seed
    obtain ⟨atSymbol, equal⟩ := advanced
    subst after
    exact ⟨erase before, by rw [projection]; exact List.mem_map.mpr ⟨before, beforeMember, rfl⟩,
      erased.symm, atSymbol⟩

/-- Full LR(1) operational histories compose through erasure with the existing
viable-prefix graph theorem. Logical prediction remains state/lookahead local;
no global FOLLOW approximation is substituted for its producer premise. -/
theorem lr1_construction_traces_supported (g : CFG) (augmented : Production) (graph : Graph)
    (items : Nat → List Item)
    (projection : ∀ state, graph.items state = (items state).map erase)
    (initialHistory : LR1Trace g [⟨augmented, 0, none⟩] (items graph.entry))
    (targets : ∀ state symbol target, graph.goto state symbol = some target →
      LR1Trace g (lr1GotoKernel (items state) symbol) (items target)) :
    Constructed g augmented graph := by
  apply lr1_graph_constructed g augmented graph items projection
  · intro item member
    have supported := lr1_trace_supported initialHistory item member
    simpa using supported
  · intro state symbol target edge item member
    have supported := lr1_trace_supported (targets state symbol target edge) item member
    have lift : ∀ seed ∈ lr1GotoKernel (items state) symbol,
        ∃ before ∈ items state, Advances before symbol seed :=
      fun _ belongs => lr1_goto_kernel_exact.mp belongs
    clear member
    induction supported with
    | seed member => exact LR1Closure.seed (lift _ member)
    | predict known prediction ih => exact LR1Closure.predict ih prediction

theorem constructed_reverse {g : CFG} {augmented : Production} {graph : Graph}
    (built : Constructed g augmented graph) {state target : Nat} {symbol : Symbol}
    (edge : graph.goto state symbol = some target) {item : Core}
    (member : item ∈ graph.items target) (positive : 0 < item.dot) :
    ∃ before ∈ graph.items state, advance before symbol item :=
  positive_closure_has_seed (built.targets state symbol target edge item member) positive

theorem constructed_initial_zero {g : CFG} {augmented : Production} {graph : Graph}
    (built : Constructed g augmented graph) {item : Core}
    (member : item ∈ graph.items graph.entry) : item.dot = 0 := by
  have closed := built.initial item member
  cases closed with
  | seed equal => cases equal; rfl
  | predict known next rule member lhs => rfl

theorem no_incoming_start_zero {g : CFG} {augmented : Production} {graph : Graph}
    (built : Constructed g augmented graph) (foreign : augmented ∉ g)
    {state target : Nat} {symbol : Symbol} (edge : graph.goto state symbol = some target) :
    (⟨augmented, 0⟩ : Core) ∉ graph.items target := by
  intro member
  apply closure_no_foreign_zero augmented foreign _ (built.targets _ _ _ edge _ member)
  rintro ⟨before, _, equal, _⟩
  have dot := congrArg Core.dot equal
  simp at dot

/-- Both lists are top-first, exactly like the native LR stacks. -/
inductive StackPath (graph : Graph) : List Nat → List Symbol → Prop where
  | start : StackPath graph [graph.entry] []
  | push {state states symbols symbol} (target : Nat)
      (before : StackPath graph (state :: states) symbols)
      (edge : graph.goto state symbol = some target) :
      StackPath graph (target :: state :: states) (symbol :: symbols)

theorem stack_depth {graph : Graph} {states symbols} (path : StackPath graph states symbols) :
    states.length = symbols.length + 1 := by
  induction path with
  | start => rfl
  | push target before edge ih => simp_all

/-- Every positive-dot item describes an actual suffix of the viable stack.
Closure's dot-zero items place no constraint on that suffix. -/
theorem item_suffix {g : CFG} {augmented : Production} {graph : Graph}
    (built : Constructed g augmented graph) {states symbols}
    (path : StackPath graph states symbols) :
    ∀ state, states.head? = some state → ∀ item ∈ graph.items state,
      item.dot ≤ item.production.rhs.length ∧
      ∃ prefixSymbols, symbols.reverse = prefixSymbols ++ item.production.rhs.take item.dot := by
  induction path with
  | start =>
    intro state top item member
    have equal : state = graph.entry := by simpa using top.symm
    subst state
    have zero := constructed_initial_zero built member
    simp [zero]
  | @push state states symbols symbol target before edge ih =>
    intro actual top item member
    have equal : actual = target := by simpa using top.symm
    subst actual
    by_cases zero : item.dot = 0
    · simp [zero]
    · have positive : 0 < item.dot := by omega
      obtain ⟨prior, priorMember, equal, atSymbol⟩ := constructed_reverse built edge member positive
      subst item
      obtain ⟨bounded, prefixSymbols, prefixEq⟩ := ih state rfl prior priorMember
      obtain ⟨indexBound, indexEq⟩ := List.getElem?_eq_some_iff.mp atSymbol
      constructor
      · simp only; omega
      · refine ⟨prefixSymbols, ?_⟩
        simp only [List.reverse_cons]
        rw [prefixEq, List.take_succ_eq_append_getElem indexBound, indexEq]
        simp [List.append_assoc]

theorem completed_rhs {g : CFG} {augmented : Production} {graph : Graph}
    (built : Constructed g augmented graph) {states symbols} (path : StackPath graph states symbols)
    {state : Nat} (top : states.head? = some state) (rule : Production)
    (member : (⟨rule, rule.rhs.length⟩ : Core) ∈ graph.items state) :
    rule.rhs.length ≤ symbols.length ∧ (symbols.take rule.rhs.length).reverse = rule.rhs := by
  obtain ⟨_, prefixSymbols, eq⟩ := item_suffix built path state top _ member
  simp only [List.take_length] at eq
  have stackEq := congrArg List.reverse eq
  simp only [List.reverse_reverse, List.reverse_append] at stackEq
  constructor
  · simp [stackEq]
  · rw [stackEq, List.take_left' (by simp), List.reverse_reverse]

/-- Popping a used prefix exposes the earlier graph path, including epsilon. -/
theorem drop_path {graph : Graph} {states symbols} (path : StackPath graph states symbols)
    (width : Nat) (fits : width ≤ symbols.length) :
    StackPath graph (states.drop width) (symbols.drop width) := by
  induction width generalizing states symbols with
  | zero => simpa using path
  | succ n ih =>
    cases path with
    | start => simp at fits
    | push target earlier edge =>
      simpa using ih earlier (by simpa using fits)

/-- A completed augmented item can only have crossed the single start edge.
The absence of augmented productions from ordinary prediction is essential. -/
theorem accepting_stack {g : CFG} {augmented : Production} {graph : Graph}
    (built : Constructed g augmented graph) (foreign : augmented ∉ g)
    (start : Nat) (shape : augmented.rhs = [.nonterminal start])
    {states symbols} (path : StackPath graph states symbols)
    {state : Nat} (top : states.head? = some state)
    (member : (⟨augmented, 1⟩ : Core) ∈ graph.items state) :
    symbols = [.nonterminal start] ∧ states.length = 2 := by
  cases path with
  | start =>
    have equal : state = graph.entry := by simpa using top.symm
    subst state
    have impossible := constructed_initial_zero built member
    contradiction
  | @push priorState priorStates priorSymbols symbol target earlier edge =>
    have equal : state = target := by simpa using top.symm
    subst state
    obtain ⟨prior, priorMember, itemEq, symbolEq⟩ := constructed_reverse built edge member (by simp)
    have productionEq := congrArg Core.production itemEq
    have dotEq := congrArg Core.dot itemEq
    simp only at productionEq dotEq
    have zero : prior.dot = 0 := by omega
    have coreEq : prior = ⟨augmented, 0⟩ := by
      cases prior with
      | mk production dot =>
        change augmented = production at productionEq
        change dot = 0 at zero
        subst production
        subst dot
        rfl
    rw [coreEq] at priorMember
    cases earlier with
    | start =>
      rw [coreEq, shape] at symbolEq
      have equal : symbol = .nonterminal start := by simpa using symbolEq.symm
      subst symbol
      exact ⟨rfl, rfl⟩
    | push target before previousEdge =>
      exact False.elim (no_incoming_start_zero built foreign previousEdge priorMember)

end ParserEngine.Viable
