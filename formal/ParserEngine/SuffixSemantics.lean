import ParserEngine.Solver
import ParserEngine.Construction

/-! Reverse suffix evaluation, with FIRST interpreted as a reachable terminal
prefix. Host integers, vector operations and terminal-name decoding are external
premises; no final per-suffix semantic certificate is supplied. -/
namespace ParserEngine.SuffixSemantics
open Grammar

structure Facts where
  first : Nat → Nat
  nullable : Nat → Bool

structure Summary where
  first : Nat
  nullable : Bool
  deriving DecidableEq, Repr

def empty : Summary := ⟨0, true⟩

def prepend (facts : Facts) : Symbol → Summary → Summary
  | .terminal token, _ => ⟨2 ^ token, false⟩
  | .nonterminal name, tail =>
      ⟨if facts.nullable name then facts.first name ||| tail.first else facts.first name,
       facts.nullable name && tail.nullable⟩

/-- The accumulator for dot d is obtained from the completed accumulator at
width by visiting width-1, ..., d; this is the native backward recurrence. -/
def scan (facts : Facts) : List Symbol → Summary
  | [] => empty
  | symbol :: rest => prepend facts symbol (scan facts rest)

structure Exact (g : CFG) (facts : Facts) : Prop where
  first : ∀ name token, (facts.first name).testBit token = true ↔
    First g [.nonterminal name] token
  nullable : ∀ name, facts.nullable name = true ↔ Nullable g [.nonterminal name]

theorem nullable_cons {g : CFG} (name : Nat) (rest : List Symbol) :
    Nullable g (.nonterminal name :: rest) ↔
      Nullable g [.nonterminal name] ∧ Nullable g rest := by
  constructor
  · intro value
    cases value with
    | expand rule member body tail =>
      exact ⟨Nullable.expand rule member body Nullable.empty, tail⟩
  · rintro ⟨head, tail⟩
    cases head with
    | expand rule member body _ => exact Nullable.expand rule member body tail

theorem first_cons {g : CFG} (name token : Nat) (rest : List Symbol) :
    First g (.nonterminal name :: rest) token ↔
      First g [.nonterminal name] token ∨
        (Nullable g [.nonterminal name] ∧ First g rest token) := by
  constructor
  · intro value
    cases value with
    | body rule member body => exact Or.inl (First.body rule member body)
    | skip rule member body tail =>
      exact Or.inr ⟨Nullable.expand rule member body Nullable.empty, tail⟩
  · intro value
    rcases value with head | ⟨head, tail⟩
    · cases head with
      | body rule member body => exact First.body rule member body
      | skip rule member body tail => cases tail
    · cases head with
      | expand rule member body _ => exact First.skip rule member body tail

/-- Arbitrary RHS, including unproductive suffixes and recursive nullable
nonterminals. Both directions are derived together from name-level facts. -/
theorem scan_exact {g : CFG} {facts : Facts} (admitted : Exact g facts)
    (symbols : List Symbol) :
    (∀ token, (scan facts symbols).first.testBit token = true ↔ First g symbols token) ∧
      ((scan facts symbols).nullable = true ↔ Nullable g symbols) := by
  induction symbols with
  | nil =>
    constructor
    · intro token
      simp only [scan, empty, Nat.zero_testBit, Bool.false_eq_true, false_iff]
      intro impossible; cases impossible
    · exact ⟨fun _ => Nullable.empty, fun _ => rfl⟩
  | cons symbol rest ih =>
    cases symbol with
    | terminal terminal =>
      constructor
      · intro token
        simp only [scan, prepend, Nat.testBit_two_pow, decide_eq_true_eq]
        constructor
        · intro equal; subst token; exact First.terminal terminal rest
        · intro value; cases value; rfl
      · simp only [scan, prepend, Bool.false_eq_true, false_iff]
        intro impossible; cases impossible
    | nonterminal name =>
      constructor
      · intro token
        rw [first_cons]
        simp only [scan, prepend]
        cases known : facts.nullable name with
        | false =>
          have absent : ¬ Nullable g [.nonterminal name] := by
            intro value
            have := (admitted.nullable name).mpr value
            simp [known] at this
          simp [admitted.first, absent]
        | true =>
          have present := (admitted.nullable name).mp known
          simp [Nat.testBit_or, admitted.first, ih.1, present]
      · simp only [scan, prepend, Bool.and_eq_true]
        exact (and_congr (admitted.nullable name) ih.2).trans (nullable_cons name rest).symm

/-- The complete catalogue contains the empty suffix slot as well as every
predecessor. Sharing the already computed tail mirrors the source accumulator. -/
def catalogue (facts : Facts) : List Symbol → List Summary
  | [] => [empty]
  | symbol :: rest =>
      let tail := catalogue facts rest
      prepend facts symbol (scan facts rest) :: tail

@[simp] theorem catalogue_length (facts : Facts) (symbols : List Symbol) :
    (catalogue facts symbols).length = symbols.length + 1 := by
  induction symbols with
  | nil => rfl
  | cons symbol rest ih => simp [catalogue, ih]

theorem catalogue_at (facts : Facts) (symbols : List Symbol) (dot : Nat)
    (bound : dot ≤ symbols.length) :
    (catalogue facts symbols)[dot]? = some (scan facts (symbols.drop dot)) := by
  induction symbols generalizing dot with
  | nil =>
    have zero : dot = 0 := by simpa using bound
    subst dot
    rfl
  | cons symbol rest ih =>
    cases dot with
    | zero => rfl
    | succ dot => simpa [catalogue] using ih dot (by simpa using bound)

/-- A compiled suffix read has the paper's FIRST/nullable meaning at any valid
dot, rather than only at a selected fixture's suffix. -/
theorem catalogue_semantic {g : CFG} {facts : Facts} (admitted : Exact g facts)
    (symbols : List Symbol) (dot : Nat) (bound : dot ≤ symbols.length) :
    ∃ result, (catalogue facts symbols)[dot]? = some result ∧
      (∀ token, result.first.testBit token = true ↔ First g (symbols.drop dot) token) ∧
      (result.nullable = true ↔ Nullable g (symbols.drop dot)) := by
  exact ⟨scan facts (symbols.drop dot), catalogue_at facts symbols dot bound,
    scan_exact admitted _⟩

/-- State of the native backward loop: the current tail summary and the
already published suffix rows. Each step consumes exactly one preceding symbol. -/
def execute (facts : Facts) : List Symbol → Summary → List Summary → Summary × List Summary
  | [], tail, rows => (tail, rows)
  | symbol :: pending, tail, rows =>
      let current := prepend facts symbol tail
      execute facts pending current (current :: rows)

theorem execute_exact (facts : Facts) (pending suffix : List Symbol) :
    execute facts pending (scan facts suffix) (catalogue facts suffix) =
      (scan facts (pending.reverse ++ suffix), catalogue facts (pending.reverse ++ suffix)) := by
  induction pending generalizing suffix with
  | nil => rfl
  | cons symbol pending ih =>
    change execute facts pending (scan facts (symbol :: suffix)) (catalogue facts (symbol :: suffix)) = _
    rw [ih]
    simp [List.reverse_cons, List.append_assoc]

theorem reverse_execution (facts : Facts) (symbols : List Symbol) :
    execute facts symbols.reverse empty [empty] = (scan facts symbols, catalogue facts symbols) := by
  simpa [scan, catalogue] using execute_exact facts symbols.reverse []

/-- Flattening follows the same width+1 production layout as make-core-item. -/
def packed (facts : Facts) (g : CFG) : List Summary :=
  g.flatMap fun rule => catalogue facts rule.rhs

theorem packed_at (facts : Facts) (before after : CFG) (rule : Production)
    (dot : Nat) (valid : dot ≤ rule.rhs.length) :
    (packed facts (before ++ rule :: after))[
      Viable.denseOffset (before.map (fun p => p.rhs.length)) before.length + dot]? =
      some (scan facts (rule.rhs.drop dot)) := by
  induction before with
  | nil =>
    simp only [List.nil_append, List.map_nil, List.length_nil, Viable.denseOffset,
      Nat.zero_add, packed, List.flatMap_cons]
    rw [List.getElem?_append_left (by simp; omega)]
    exact catalogue_at facts rule.rhs dot valid
  | cons head before ih =>
    simp only [List.cons_append, List.map_cons, List.length_cons,
      Viable.denseOffset, packed, List.flatMap_cons]
    rw [List.getElem?_append_right (by simp; omega)]
    simp only [catalogue_length]
    rw [show head.rhs.length + 1 +
      Viable.denseOffset (before.map (fun p => p.rhs.length)) before.length + dot -
      (head.rhs.length + 1) =
      Viable.denseOffset (before.map (fun p => p.rhs.length)) before.length + dot by omega]
    simpa only [packed, List.flatMap_append, List.flatMap_cons] using ih

/-- Raw vectors start at zero/false; valid completed slots are overwritten by
the producer. Out-of-range reads are not used by the following bounded proof. -/
def readPacked (facts : Facts) (g : CFG) (code : Nat) : Summary :=
  ((packed facts g)[code]?).getD ⟨0, false⟩

theorem after_dot_read (facts : Facts) (before after : CFG) (rule : Production)
    (dot : Nat) (next : dot < rule.rhs.length) :
    readPacked facts (before ++ rule :: after)
      (Viable.denseOffset (before.map (fun p => p.rhs.length)) before.length + dot + 1) =
      scan facts (rule.rhs.drop (dot + 1)) := by
  have read := packed_at facts before after rule (dot + 1) (by omega)
  simpa [readPacked, Nat.add_assoc] using congrArg (fun value => value.getD ⟨0, false⟩) read

/-- Construct masks from the completed FIRST worklist, preserving ordinal zero
and arbitrary-width natural masks. -/
def encode : List Nat → Nat
  | [] => 0
  | token :: rest => 2 ^ token ||| encode rest

@[simp] theorem encode_bit (tokens : List Nat) (token : Nat) :
    (encode tokens).testBit token = true ↔ token ∈ tokens := by
  induction tokens with
  | nil => simp [encode]
  | cons head rest ih =>
    simp only [encode, Nat.testBit_or, Bool.or_eq_true, Nat.testBit_two_pow,
      decide_eq_true_eq, ih, List.mem_cons]
    exact or_congr eq_comm Iff.rfl

def fromCompleted (nullable : List Nat) (first : List (Nat × Nat)) : Facts :=
  ⟨fun name => encode ((first.filter (fun row => row.1 == name)).map Prod.snd),
   fun name => decide (name ∈ nullable)⟩

theorem from_completed_exact (g : CFG) (nullable : Worklist.State Nat)
    (first : Worklist.State (Nat × Nat))
    (nullableHistory : Worklist.Trace (Solver.nullableProgram g)
      (Worklist.initial (Solver.nullableProgram g)) nullable)
    (nullableFinished : nullable.pending = [])
    (firstHistory : Worklist.Trace (Solver.firstProgram g nullable.known (Solver.terminalCatalog g))
      (Worklist.initial (Solver.firstProgram g nullable.known (Solver.terminalCatalog g))) first)
    (firstFinished : first.pending = []) : Exact g (fromCompleted nullable.known first.known) := by
  obtain ⟨emptyExact, firstExact⟩ := Solver.preparation_pipeline_exact g nullable first
    nullableHistory nullableFinished firstHistory firstFinished
  constructor
  · intro name token
    simp only [fromCompleted, encode_bit, List.mem_map, List.mem_filter, beq_iff_eq]
    constructor
    · rintro ⟨⟨actual, value⟩, ⟨member, equal⟩, tokenEqual⟩
      dsimp at equal tokenEqual
      subst actual; subst value
      exact (firstExact name token).mp member
    · intro value
      exact ⟨(name, token), ⟨(firstExact name token).mpr value, rfl⟩, rfl⟩
  · intro name
    simpa [fromCompleted] using emptyExact name

/-- Source suffix facts discharge the previously supplied semantic FIRST and
nullable premises of the generic activation-guarded delivery theorem. -/
theorem delivered_allowed {g : CFG} {facts : Facts} (admitted : Exact g facts)
    (symbols : List Symbol) (inherited selected : Option Nat)
    (bit delta : Nat) (activated : Bool)
    (terminalDecode : (scan facts symbols).first.testBit bit = true → selected = some bit)
    (inheritedBits : delta.testBit bit = true → selected = inherited)
    (present : (Construction.suffixEvidence (scan facts symbols).first
      (scan facts symbols).nullable activated delta).testBit bit = true) :
    LookaheadAllowed g symbols inherited selected := by
  rcases (Construction.suffix_evidence_exact _ _ _ _ _).mp present with first | nullable
  · exact Or.inl ⟨bit, terminalDecode first.2, ((scan_exact admitted symbols).1 bit).mp first.2⟩
  · exact Or.inr ⟨(scan_exact admitted symbols).2.mp nullable.1, inheritedBits nullable.2⟩

end ParserEngine.SuffixSemantics
