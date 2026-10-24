import ParserEngine.Request

/-! CFG meaning underlying completion facts and LR requests. This model starts
at the admitted, unmarked grammar: DSL expansion and base-symbol erasure are
separate correspondence obligations. Terminal ordinals are selected grammar terminals, not raw token kinds or source
bytes. Fixed terminal classification is an explicit scope restriction.
-/
namespace ParserEngine.Grammar

inductive Symbol where
  | terminal (id : Nat)
  | nonterminal (id : Nat)
  deriving DecidableEq, Repr

structure Production where
  lhs : Nat
  rhs : List Symbol
  deriving DecidableEq, Repr

abbrev CFG := List Production

/-- Finite terminal derivations, retaining production membership explicitly. -/
inductive Derives (g : CFG) : List Symbol → List Nat → Prop where
  | empty : Derives g [] []
  | terminal {tail word} (token : Nat) (rest : Derives g tail word) :
      Derives g (.terminal token :: tail) (token :: word)
  | expand {tail word suffix} (rule : Production) (member : rule ∈ g)
      (body : Derives g rule.rhs word) (rest : Derives g tail suffix) :
      Derives g (.nonterminal rule.lhs :: tail) (word ++ suffix)

/-- Concatenating derivations is independent of parse-stack representation. -/
theorem derives_append {g : CFG} {left right : List Symbol} {a b : List Nat}
    (first : Derives g left a) (second : Derives g right b) :
    Derives g (left ++ right) (a ++ b) := by
  induction first with
  | empty => simpa using second
  | terminal token rest ih => simpa using Derives.terminal token ih
  | expand rule member body rest ihBody ihRest =>
    simpa [List.append_assoc] using Derives.expand rule member body ihRest

/-- Nullable facts are the least inductive closure of empty productions and
nullable RHS occurrences. A terminal is never nullable. -/
inductive Nullable (g : CFG) : List Symbol → Prop where
  | empty : Nullable g []
  | expand {tail} (rule : Production) (member : rule ∈ g)
      (body : Nullable g rule.rhs) (rest : Nullable g tail) :
      Nullable g (.nonterminal rule.lhs :: tail)

theorem nullable_derives {g : CFG} {symbols : List Symbol}
    (fact : Nullable g symbols) : Derives g symbols [] := by
  induction fact with
  | empty => exact Derives.empty
  | expand rule member body rest ihBody ihRest =>
    exact Derives.expand rule member ihBody ihRest

theorem derives_nullable {g : CFG} {symbols : List Symbol}
    (derivation : Derives g symbols []) : Nullable g symbols := by
  generalize empty : ([] : List Nat) = word at derivation
  induction derivation with
  | empty => exact Nullable.empty
  | terminal token rest ih => simp at empty
  | expand rule member body rest ihBody ihRest =>
    have parts := List.append_eq_nil_iff.mp empty.symm
    exact Nullable.expand rule member (ihBody parts.1.symm) (ihRest parts.2.symm)

theorem nullable_iff_derives {g : CFG} {symbols : List Symbol} :
    Nullable g symbols ↔ Derives g symbols [] := ⟨nullable_derives, derives_nullable⟩

/-- FIRST concerns a reachable terminal prefix. The suffix need not be
productive. The rules mirror sequence-first's nullable-prefix dependencies. -/
inductive First (g : CFG) : List Symbol → Nat → Prop where
  | terminal (token : Nat) (tail : List Symbol) : First g (.terminal token :: tail) token
  | body {tail token} (rule : Production) (member : rule ∈ g)
      (first : First g rule.rhs token) : First g (.nonterminal rule.lhs :: tail) token
  | skip {tail token} (rule : Production) (member : rule ∈ g)
      (nullable : Nullable g rule.rhs) (first : First g tail token) :
      First g (.nonterminal rule.lhs :: tail) token

/-- Context-free one-step rewriting, independent of the FIRST solver. -/
inductive Rewrite (g : CFG) : List Symbol → List Symbol → Prop where
  | rule (production : Production) (member : production ∈ g) (left right : List Symbol) :
      Rewrite g (left ++ .nonterminal production.lhs :: right)
        (left ++ production.rhs ++ right)

inductive Reach (g : CFG) : List Symbol → List Symbol → Prop where
  | refl (symbols : List Symbol) : Reach g symbols symbols
  | next {a b c} (history : Reach g a b) (step : Rewrite g b c) : Reach g a c

theorem reach_trans {g : CFG} {a b c} (first : Reach g a b) (second : Reach g b c) :
    Reach g a c := by
  induction second with
  | refl => exact first
  | next history step ih => exact Reach.next ih step

theorem rewrite_context {g : CFG} {a b} (step : Rewrite g a b) (left right : List Symbol) :
    Rewrite g (left ++ a ++ right) (left ++ b ++ right) := by
  cases step with
  | rule production member before after =>
    simpa [List.append_assoc] using Rewrite.rule production member (left ++ before) (after ++ right)

theorem reach_context {g : CFG} {a b} (history : Reach g a b) (left right : List Symbol) :
    Reach g (left ++ a ++ right) (left ++ b ++ right) := by
  induction history with
  | refl => exact Reach.refl _
  | next history step ih => exact Reach.next ih (rewrite_context step left right)

theorem derives_reach {g : CFG} {symbols word} (derivation : Derives g symbols word) :
    Reach g symbols (word.map Symbol.terminal) := by
  induction derivation with
  | empty => exact Reach.refl _
  | terminal token rest ih =>
    simpa using reach_context ih [.terminal token] []
  | @expand tail word suffix rule member body rest ihBody ihRest =>
    have begin := Reach.next (Reach.refl _) (Rewrite.rule rule member [] tail)
    have bodyStep := reach_context ihBody [] tail
    have tailStep := reach_context ihRest (word.map Symbol.terminal) []
    simp only [List.nil_append, List.append_nil] at begin bodyStep tailStep
    simpa using reach_trans (reach_trans begin bodyStep) tailStep

/-- FIRST really witnesses a reachable sentential terminal prefix, even when
that prefix has no finite terminal completion. -/
theorem first_reach {g : CFG} {symbols token} (fact : First g symbols token) :
    ∃ suffix, Reach g symbols (.terminal token :: suffix) := by
  induction fact with
  | terminal token tail => exact ⟨tail, Reach.refl _⟩
  | @body tail token rule member first ih =>
    obtain ⟨suffix, reached⟩ := ih
    refine ⟨suffix ++ tail, ?_⟩
    have begin := Reach.next (Reach.refl _) (Rewrite.rule rule member [] tail)
    simpa using reach_trans begin (reach_context reached [] tail)
  | @skip tail token rule member nullable first ih =>
    obtain ⟨suffix, reached⟩ := ih
    refine ⟨suffix, ?_⟩
    have begin := Reach.next (Reach.refl _) (Rewrite.rule rule member [] tail)
    have erased := reach_context (derives_reach (nullable_derives nullable)) [] tail
    have toTail : Reach g (.nonterminal rule.lhs :: tail) tail := by
      simpa using reach_trans begin erased
    exact reach_trans toTail reached

/-- Abstract-interpretation closure: every concrete nullable derivation belongs
in any fact environment closed under the grammar's clauses. This is leastness,
not a proof of the native occurrence worklist or its scheduling bounds. -/
theorem nullable_least {g : CFG} (facts : Nat → Prop)
    (closed : ∀ rule ∈ g,
      (∀ symbol ∈ rule.rhs, ∃ name, symbol = .nonterminal name ∧ facts name) → facts rule.lhs)
    {symbols : List Symbol} (fact : Nullable g symbols) :
    ∀ symbol ∈ symbols, ∃ name, symbol = .nonterminal name ∧ facts name := by
  induction fact with
  | empty => simp
  | expand rule member body rest ihBody ihRest =>
    intro symbol belongs
    cases belongs with
    | head => exact ⟨rule.lhs, rfl, closed rule member ihBody⟩
    | tail _ belongs => exact ihRest symbol belongs

/-- The set-level sequence operator used after nullable convergence. -/
inductive SequenceFirst (nullable : Nat → Prop) (first : Nat → Nat → Prop) :
    List Symbol → Nat → Prop where
  | terminal (token : Nat) (tail : List Symbol) :
      SequenceFirst nullable first (.terminal token :: tail) token
  | head {name tail token} (fact : first name token) :
      SequenceFirst nullable first (.nonterminal name :: tail) token
  | skip {name tail token} (empty : nullable name)
      (rest : SequenceFirst nullable first tail token) :
      SequenceFirst nullable first (.nonterminal name :: tail) token

theorem first_least {g : CFG} (nullable : Nat → Prop) (facts : Nat → Nat → Prop)
    (nullableClosed : ∀ rule ∈ g,
      (∀ symbol ∈ rule.rhs, ∃ name, symbol = .nonterminal name ∧ nullable name) → nullable rule.lhs)
    (firstClosed : ∀ rule ∈ g, ∀ token,
      SequenceFirst nullable facts rule.rhs token → facts rule.lhs token)
    {symbols : List Symbol} {token : Nat} (fact : First g symbols token) :
    SequenceFirst nullable facts symbols token := by
  induction fact with
  | terminal token tail => exact SequenceFirst.terminal token tail
  | body rule member first ih => exact SequenceFirst.head (firstClosed rule member _ ih)
  | skip rule member nullableBody first ih =>
    exact SequenceFirst.skip
      (nullableClosed rule member (nullable_least nullable nullableClosed nullableBody)) ih

def Productive (g : CFG) (name : Nat) : Prop :=
  ∃ word, Derives g [.nonterminal name] word

theorem nullable_productive {g : CFG} {name : Nat}
    (nullableProof : Nullable g [.nonterminal name]) : Productive g name :=
  ⟨[], nullable_derives nullableProof⟩

/-- Item coordinates refer to the admitted production, independent of native
packed item IDs and production-index encoding. EOF is none, not terminal zero. -/
structure Item where
  production : Production
  dot : Nat
  lookahead : Option Nat
  deriving DecidableEq, Repr

def LookaheadAllowed (g : CFG) (suffix : List Symbol)
    (inherited selected : Option Nat) : Prop :=
  (∃ token, selected = some token ∧ First g suffix token) ∨
    (Nullable g suffix ∧ selected = inherited)

/-- LR(1) prediction uses FIRST(beta, inherited lookahead), not FIRST(B).
This is the semantic obligation of the native shared suffix catalog. -/
def Predicts (g : CFG) (parent child : Item) : Prop :=
  ∃ name suffix,
    parent.production.rhs.drop parent.dot = .nonterminal name :: suffix ∧
    child.production ∈ g ∧ child.production.lhs = name ∧ child.dot = 0 ∧
    LookaheadAllowed g suffix parent.lookahead child.lookahead

def Advances (before : Item) (symbol : Symbol) (after : Item) : Prop :=
  before.production.rhs[before.dot]? = some symbol ∧
    after = {before with dot := before.dot + 1}

theorem eof_lookahead_requires_nullable {g : CFG} {suffix : List Symbol}
    {inherited : Option Nat} (admitted : LookaheadAllowed g suffix inherited none) :
    Nullable g suffix ∧ inherited = none := by
  rcases admitted with impossible | ⟨nullable, inheritedEq⟩
  · obtain ⟨token, impossible, _⟩ := impossible
    cases impossible
  · exact ⟨nullable, inheritedEq.symm⟩

theorem terminal_lookahead_has_derivation {g : CFG} {suffix : List Symbol}
    {inherited : Option Nat} {token : Nat}
    (admitted : LookaheadAllowed g suffix inherited (some token)) :
    (∃ rest, Reach g suffix (.terminal token :: rest)) ∨
      (Derives g suffix [] ∧ inherited = some token) := by
  rcases admitted with ⟨actual, equal, first⟩ | ⟨nullable, equal⟩
  · cases equal
    exact Or.inl (first_reach first)
  · exact Or.inr ⟨nullable_derives nullable, equal.symm⟩

inductive Action where
  | shift (target : Nat)
  | reduce (production : Production)
  | accept
  deriving DecidableEq, Repr

structure Table where
  items : Nat → List Item
  goto : Nat → Symbol → Option Nat
  action : Nat → Option Nat → Option Action

/-- Semantic cell support is stronger than index/schema/reachability admission.
This predicate does not assert that native validate-lr-tables checks it. -/
def CellSupported (g : CFG) (start : Nat) (augmented : Production) (table : Table)
    (state : Nat) (lookahead : Option Nat) (action : Action) : Prop :=
  match action with
  | .shift target => ∃ token, lookahead = some token ∧
      table.goto state (.terminal token) = some target ∧
      ∃ item ∈ table.items state, item.production.rhs[item.dot]? = some (.terminal token)
  | .reduce production => production ∈ g ∧
      ⟨production, production.rhs.length, lookahead⟩ ∈ table.items state
  | .accept => lookahead = none ∧ augmented.rhs = [.nonterminal start] ∧
      ⟨augmented, 1, none⟩ ∈ table.items state

def TableSupported (g : CFG) (start : Nat) (augmented : Production) (table : Table) : Prop :=
  ∀ state lookahead action, table.action state lookahead = some action →
    CellSupported g start augmented table state lookahead action

theorem supported_reduction_is_grammar_rule {g : CFG} {start : Nat}
    {augmented : Production} {table : Table} (supported : TableSupported g start augmented table)
    {state : Nat} {lookahead : Option Nat} {production : Production}
    (selected : table.action state lookahead = some (.reduce production)) : production ∈ g :=
  (supported state lookahead (.reduce production) selected).1

structure Frame where
  symbol : Symbol
  terminals : List Nat
  deriving DecidableEq, Repr

def FramesValid (g : CFG) (frames : List Frame) : Prop :=
  ∀ frame ∈ frames, Derives g [frame.symbol] frame.terminals

def symbols (frames : List Frame) : List Symbol := frames.map Frame.symbol
def terminals (frames : List Frame) : List Nat := (frames.map Frame.terminals).flatten

theorem frames_derive {g : CFG} {frames : List Frame} (valid : FramesValid g frames) :
    Derives g (symbols frames) (terminals frames) := by
  induction frames with
  | nil => exact Derives.empty
  | cons frame rest ih =>
    have head := valid frame (by simp)
    have tail := ih (fun item member => valid item (by simp [member]))
    simpa [symbols, terminals] using derives_append head tail

/-- Reduce requires the actual RHS symbols, not merely a matching width. -/
theorem reduction_derives {g : CFG} (rule : Production) (member : rule ∈ g)
    (sourceFrames : List Frame) (valid : FramesValid g sourceFrames)
    (rhsEq : symbols sourceFrames = rule.rhs) :
    Derives g [.nonterminal rule.lhs] (terminals sourceFrames) := by
  have body := frames_derive valid
  rw [rhsEq] at body
  simpa using Derives.expand rule member body Derives.empty

/-- Recognition state carries both the grammar symbol and its terminal yield.
Its projection is the previous request token-flow kernel. -/
structure Config where
  states : List Nat
  frames : List Frame
  rest : List Nat
  deriving DecidableEq, Repr

def erase (c : Config) : Request.Config Nat :=
  ⟨c.states, c.frames.map Frame.terminals, c.rest⟩

def Valid (g : CFG) (input : List Nat) (c : Config) : Prop :=
  Request.Valid input (erase c) ∧ FramesValid g c.frames

def shift (c : Config) (target token : Nat) (tail : List Nat) : Config :=
  ⟨target :: c.states, ⟨.terminal token, [token]⟩ :: c.frames, tail⟩

def reduce (c : Config) (rule : Production) (target : Nat) : Config :=
  let width := rule.rhs.length
  let sourceFrames := (c.frames.take width).reverse
  ⟨target :: c.states.drop width,
    ⟨.nonterminal rule.lhs, terminals sourceFrames⟩ :: c.frames.drop width, c.rest⟩

theorem erase_shift (c : Config) (target token : Nat) (tail : List Nat) :
    erase (shift c target token tail) = Request.shift (erase c) target token tail := rfl

theorem erase_reduce (c : Config) (rule : Production) (target : Nat) :
    erase (reduce c rule target) = Request.reduce (erase c) rule.rhs.length target := by
  simp [erase, reduce, Request.reduce, Capture.split_prefix_exact, terminals,
    List.map_take, List.map_drop, List.map_reverse]

theorem shift_valid {g : CFG} {input : List Nat} (c : Config) (target token : Nat)
    (tail : List Nat) (valid : Valid g input c) (pending : c.rest = token :: tail) :
    Valid g input (shift c target token tail) := by
  constructor
  · rw [erase_shift]
    exact Request.shift_valid input (erase c) target token tail valid.1 pending
  · intro frame member
    cases member with
    | head => exact Derives.terminal token Derives.empty
    | tail _ member => exact valid.2 frame member

theorem reduce_valid {g : CFG} {input : List Nat} (c : Config) (rule : Production)
    (target : Nat) (valid : Valid g input c) (member : rule ∈ g)
    (fits : rule.rhs.length ≤ c.frames.length)
    (rhsEq : symbols ((c.frames.take rule.rhs.length).reverse) = rule.rhs) :
    Valid g input (reduce c rule target) := by
  constructor
  · rw [erase_reduce]
    exact Request.reduce_valid input (erase c) rule.rhs.length target valid.1
      (by simpa [erase] using fits)
  · intro frame belongs
    cases belongs with
    | head =>
      apply reduction_derives rule member _ _ rhsEq
      intro child childMember
      exact valid.2 child (List.mem_of_mem_take (List.mem_reverse.mp childMember))
    | tail _ belongs => exact valid.2 frame (List.mem_of_mem_drop belongs)

def initial (state : Nat) (input : List Nat) : Config := ⟨[state], [], input⟩

theorem initial_valid (g : CFG) (state : Nat) (input : List Nat) :
    Valid g input (initial state input) := by
  constructor
  · exact Request.initial_valid state input
  · simp [FramesValid, initial]

/-- The admitted table must license the selected action/target in addition to
these grammar premises. Those producer obligations are not assumed proved. -/
inductive Step (g : CFG) : Config → Config → Prop where
  | shift (c : Config) (target token : Nat) (tail : List Nat)
      (pending : c.rest = token :: tail) : Step g c (shift c target token tail)
  | reduce (c : Config) (rule : Production) (target : Nat) (member : rule ∈ g)
      (fits : rule.rhs.length ≤ c.frames.length)
      (rhsEq : symbols ((c.frames.take rule.rhs.length).reverse) = rule.rhs) :
      Step g c (reduce c rule target)

inductive Trace (g : CFG) : Config → Config → Prop where
  | refl (c : Config) : Trace g c c
  | next {a b c : Config} (history : Trace g a b) (step : Step g b c) : Trace g a c

theorem step_valid {g : CFG} {input : List Nat} {before after : Config}
    (valid : Valid g input before) (step : Step g before after) : Valid g input after := by
  cases step with
  | shift target token tail pending => exact shift_valid _ target token tail valid pending
  | reduce rule target member fits rhsEq => exact reduce_valid _ rule target valid member fits rhsEq

theorem trace_valid {g : CFG} {input : List Nat} {before after : Config}
    (valid : Valid g input before) (trace : Trace g before after) : Valid g input after := by
  induction trace with
  | refl => exact valid
  | next history step ih => exact step_valid ih step

/-- Table-driven transitions retain the runtime's pending-input protocol.
RHS agreement is a viable-prefix obligation: native reduction does not inspect
symbol labels dynamically. A table-construction proof must discharge it. -/
inductive DrivenStep (table : Table) : Config → Config → Prop where
  | shift (c : Config) (state target token : Nat) (tail : List Nat)
      (top : c.states.head? = some state) (pending : c.rest = token :: tail)
      (selected : table.action state (some token) = some (.shift target)) :
      DrivenStep table c (shift c target token tail)
  | reduce (c : Config) (state target entry : Nat) (production : Production)
      (top : c.states.head? = some state)
      (selected : table.action state c.rest.head? = some (.reduce production))
      (fits : production.rhs.length ≤ c.frames.length)
      (rhsEq : symbols ((c.frames.take production.rhs.length).reverse) = production.rhs)
      (base : (c.states.drop production.rhs.length).head? = some entry)
      (targetEq : table.goto entry (.nonterminal production.lhs) = some target) :
      DrivenStep table c (reduce c production target)

inductive DrivenTrace (table : Table) : Config → Config → Prop where
  | refl (c : Config) : DrivenTrace table c c
  | next {a b c} (history : DrivenTrace table a b) (step : DrivenStep table b c) :
      DrivenTrace table a c

theorem driven_step_valid {g : CFG} {start : Nat} {augmented : Production}
    {table : Table} (supported : TableSupported g start augmented table)
    {input : List Nat} {before after : Config}
    (valid : Valid g input before) (step : DrivenStep table before after) : Valid g input after := by
  cases step with
  | shift state target token tail top pending selected =>
    exact shift_valid _ target token tail valid pending
  | reduce state target entry production top selected fits rhsEq base targetEq =>
    exact reduce_valid _ production target valid
      (supported_reduction_is_grammar_rule supported selected) fits rhsEq

theorem driven_trace_valid {g : CFG} {start : Nat} {augmented : Production}
    {table : Table} (supported : TableSupported g start augmented table)
    {input : List Nat} {before after : Config}
    (valid : Valid g input before) (trace : DrivenTrace table before after) :
    Valid g input after := by
  induction trace with
  | refl => exact valid
  | next history step ih => exact driven_step_valid supported ih step

/-- Acceptance soundness composes grammatical frames with exact consumption.
Singleton start/empty remainder remain explicit acceptance obligations. -/
theorem accepted_derivation {g : CFG} {input : List Nat} (c : Config)
    (start : Nat) (root : Frame) (valid : Valid g input c)
    (single : c.frames = [root]) (kind : root.symbol = .nonterminal start)
    (complete : c.rest = []) : Derives g [.nonterminal start] input := by
  have rootValid := valid.2 root (by simp [single])
  have yieldEq : root.terminals = input :=
    Request.complete_root input root.terminals (erase c) valid.1
      (by simp [erase, single]) complete
  simpa [kind, yieldEq] using rootValid

theorem accepted_trace_derivation (g : CFG) (input : List Nat) (entry : Nat)
    (last : Config) (start : Nat) (root : Frame)
    (trace : Trace g (initial entry input) last)
    (single : last.frames = [root]) (kind : root.symbol = .nonterminal start)
    (complete : last.rest = []) : Derives g [.nonterminal start] input :=
  accepted_derivation last start root (trace_valid (initial_valid g entry input) trace)
    single kind complete

/-- Table cells, selected actions and request conservation compose into a CFG
result. Native table support and viable-prefix/start-stack premises remain
required; schema validation alone supplies none of those semantic premises. -/
theorem table_driven_acceptance (g : CFG) (input : List Nat) (entry : Nat)
    (start : Nat) (augmented : Production) (table : Table)
    (supported : TableSupported g start augmented table) (last : Config) (root : Frame)
    (trace : DrivenTrace table (initial entry input) last)
    (single : last.frames = [root]) (kind : root.symbol = .nonterminal start)
    (complete : last.rest = [])
    (accepted : table.action (last.states.headD 0) none = some .accept) :
    CellSupported g start augmented table (last.states.headD 0) none .accept ∧
      Derives g [.nonterminal start] input :=
  ⟨supported _ _ _ accepted, accepted_derivation last start root
    (driven_trace_valid supported (initial_valid g entry input) trace) single kind complete⟩

/-- Fixed-terminal lexical-policy semantics on character lists. The policy receives
the complete remaining source, so it can demand longest/rank/order selection.
State-dependent literal/kind aliases, contextual modes, trivia and byte coordinates are not erased into
this closed-policy relation; they need a richer scanner correspondence. -/
inductive Lexes (policy : List Char → Nat → List Char → Prop) :
    List Char → List Nat → Prop where
  | empty : Lexes policy [] []
  | token {lexeme suffix tokens} (kind : Nat) (progress : lexeme ≠ [])
      (chosen : policy (lexeme ++ suffix) kind lexeme) (rest : Lexes policy suffix tokens) :
      Lexes policy (lexeme ++ suffix) (kind :: tokens)

/-- One complete source claim composes lexical selection with the same
admitted grammar, table, stack and terminal-flow obligations. Raw characters
and token ordinals remain different types throughout. -/
theorem source_request_derivation (g : CFG) (input : List Nat) (source : List Char)
    (policy : List Char → Nat → List Char → Prop) (scanned : Lexes policy source input)
    (entry start : Nat) (augmented : Production) (table : Table)
    (supported : TableSupported g start augmented table) (last : Config) (root : Frame)
    (trace : DrivenTrace table (initial entry input) last)
    (single : last.frames = [root]) (kind : root.symbol = .nonterminal start)
    (complete : last.rest = [])
    (accepted : table.action (last.states.headD 0) none = some .accept) :
    ∃ tokens, Lexes policy source tokens ∧ Derives g [.nonterminal start] tokens :=
  ⟨input, scanned, (table_driven_acceptance g input entry start augmented table supported
    last root trace single kind complete accepted).2⟩

/- The same grammar used by Quint and the independent native controls. -/
namespace Fixture

def startRule : Production := ⟨11, [.nonterminal 10, .terminal 2]⟩
def recursiveRule : Production := ⟨10, [.terminal 1, .nonterminal 10]⟩
def emptyRule : Production := ⟨10, []⟩
def rules : CFG := [startRule, recursiveRule, emptyRule]

theorem nullable_A : Nullable rules [.nonterminal 10] :=
  Nullable.expand emptyRule (by simp [rules]) Nullable.empty Nullable.empty

theorem first_S_a : First rules [.nonterminal 11] 1 :=
  First.body startRule (by simp [rules])
    (First.body recursiveRule (by simp [rules]) (First.terminal 1 [.nonterminal 10]))

theorem first_S_b : First rules [.nonterminal 11] 2 :=
  First.body startRule (by simp [rules])
    (First.skip emptyRule (by simp [rules]) Nullable.empty (First.terminal 2 []))

theorem repeated_A (count : Nat) :
    Derives rules [.nonterminal 10] (List.replicate count 1) := by
  induction count with
  | zero => exact Derives.expand emptyRule (by simp [rules]) Derives.empty Derives.empty
  | succ n ih =>
    simpa [List.replicate_succ, recursiveRule] using
      Derives.expand recursiveRule (by simp [rules]) (Derives.terminal 1 ih) Derives.empty

theorem repeated_S (count : Nat) :
    Derives rules [.nonterminal 11] (List.replicate count 1 ++ [2]) := by
  have body := derives_append (repeated_A count) (Derives.terminal 2 Derives.empty)
  simpa [startRule] using Derives.expand startRule (by simp [rules]) body Derives.empty

end Fixture

end ParserEngine.Grammar
