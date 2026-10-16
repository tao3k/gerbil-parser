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

/-- Closure starts from a kernel and only introduces dot-zero predictions. -/
inductive Closure (g : CFG) (seed : Core → Prop) : Core → Prop where
  | seed {item} (member : seed item) : Closure g seed item
  | predict {parent name} (known : Closure g seed parent)
      (next : parent.production.rhs[parent.dot]? = some (.nonterminal name))
      (rule : Production) (member : rule ∈ g) (lhs : rule.lhs = name) :
      Closure g seed ⟨rule, 0⟩

def moved (items : List Core) (symbol : Symbol) (item : Core) : Prop :=
  ∃ before ∈ items, advance before symbol item

def erase (item : Item) : Core := ⟨item.production, item.dot⟩

/-- LR(1) closure retains FIRST/nullable lookahead conditions from Grammar.
Erasure drops only that lookahead, never the production or dot coordinate. -/
inductive LR1Closure (g : CFG) (seed : Item → Prop) : Item → Prop where
  | seed {item} (member : seed item) : LR1Closure g seed item
  | predict {parent child} (known : LR1Closure g seed parent)
      (prediction : Predicts g parent child) : LR1Closure g seed child

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
