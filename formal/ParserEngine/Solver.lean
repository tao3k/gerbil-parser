import ParserEngine.Encoding

/-! Connect the operational Horn worklist to the nullable fragment of the
native completion solver for every CFG, including cycles and missing names. -/
namespace ParserEngine.Solver
open Grammar

def dependencies (symbols : List Symbol) : List Nat :=
  symbols.filterMap fun symbol => match symbol with
    | .terminal _ => none
    | .nonterminal name => some name

def terminalFree (symbols : List Symbol) : Bool :=
  symbols.all fun symbol => match symbol with
    | .terminal _ => false
    | .nonterminal _ => true

theorem dependencies_member (symbols : List Symbol) (name : Nat) :
    name ∈ dependencies symbols ↔ .nonterminal name ∈ symbols := by
  simp only [dependencies, List.mem_filterMap]
  constructor
  · rintro ⟨symbol, member, equal⟩
    cases symbol with
    | terminal token => cases equal
    | nonterminal actual => cases equal; exact member
  · intro member
    exact ⟨.nonterminal name, member, rfl⟩

def nullableProgram (g : CFG) : List (Worklist.Clause Nat) :=
  g.filterMap fun rule => if terminalFree rule.rhs then some ⟨dependencies rule.rhs, rule.lhs⟩ else none

theorem nullable_rule_sound {g : CFG} {clause : Worklist.Clause Nat}
    (member : clause ∈ nullableProgram g)
    (body : ∀ name ∈ clause.dependencies, Nullable g [.nonterminal name]) :
    Nullable g [.nonterminal clause.head] := by
  obtain ⟨rule, ruleMember, result⟩ := List.mem_filterMap.mp member
  split at result
  next admitted =>
    cases result
    apply Nullable.expand rule ruleMember _ Nullable.empty
    apply Completion.nullable_from_members
    intro symbol belongs
    have allowed := List.all_eq_true.mp admitted symbol belongs
    cases symbol with
    | terminal token => simp at allowed
    | nonterminal name => exact ⟨name, rfl, body name ((dependencies_member rule.rhs name).mpr belongs)⟩
  next rejected => cases result

theorem nullable_horn_exact (g : CFG) (name : Nat) :
    Worklist.Entails (nullableProgram g) name ↔ Nullable g [.nonterminal name] := by
  constructor
  · intro derivation
    induction derivation with
    | rule clause member body ih => exact nullable_rule_sound member ih
  · intro semantic
    have closed : ∀ rule ∈ g,
        (∀ symbol ∈ rule.rhs, ∃ name, symbol = .nonterminal name ∧ Worklist.Entails (nullableProgram g) name) →
          Worklist.Entails (nullableProgram g) rule.lhs := by
      intro rule member body
      have admitted : terminalFree rule.rhs = true := by
        apply List.all_eq_true.mpr
        intro symbol belongs
        obtain ⟨name, rfl, known⟩ := body symbol belongs
        rfl
      apply Worklist.Entails.rule (⟨dependencies rule.rhs, rule.lhs⟩ : Worklist.Clause Nat)
      · exact List.mem_filterMap.mpr ⟨rule, member, by simp [admitted]⟩
      · intro name belongs
        obtain ⟨actual, identity, known⟩ := body (.nonterminal name) ((dependencies_member rule.rhs name).mp belongs)
        cases identity
        exact known
    have least := nullable_least (Worklist.Entails (nullableProgram g)) closed semantic
    obtain ⟨actual, equal, known⟩ := least (.nonterminal name) (by simp)
    cases equal
    exact known

/-- Native clause counters may store remaining occurrences. Decrementing once
per registered occurrence has exactly the same enabled test as the model. -/
theorem notification_enables_exactly (dependencies delivered : List Nat) (source : Nat)
    (fresh : source ∉ delivered) :
    Completion.remaining dependencies delivered - Completion.occurrences dependencies source = 0 ↔
      ∀ name ∈ dependencies, name ∈ source :: delivered := by
  have count := Completion.fresh_notification_counts_occurrences dependencies delivered source fresh
  have equal : Completion.remaining dependencies delivered - Completion.occurrences dependencies source =
      Completion.remaining dependencies (source :: delivered) := by omega
  rw [equal]
  exact Completion.zero_remaining _ _

/-- All grammar-specific premises are compiled from the actual RHS shape.
The caller supplies execution history and an empty pending queue, not a
successful semantic certificate or final fixed-point assumption. -/
theorem nullable_control_flow_exact (g : CFG) (last : Worklist.State Nat)
    (history : Worklist.Trace (nullableProgram g) (Worklist.initial (nullableProgram g)) last)
    (finished : last.pending = []) :
    ∀ name, name ∈ last.known ↔ Nullable g [.nonterminal name] := by
  intro name
  exact (Worklist.quiescent_exact (nullableProgram g) last history finished name).trans (nullable_horn_exact g name)

def prefixSources (nullable : List Nat) : List Symbol → List Symbol
  | [] => []
  | .terminal token :: _ => [.terminal token]
  | .nonterminal name :: tail => .nonterminal name ::
      if name ∈ nullable then prefixSources nullable tail else []

theorem prefix_sequence (nullable : List Nat) (facts : Nat → Nat → Prop)
    (symbols : List Symbol) (token : Nat) :
    SequenceFirst (fun name => name ∈ nullable) facts symbols token ↔
      ∃ symbol ∈ prefixSources nullable symbols,
        symbol = .terminal token ∨ ∃ name, symbol = .nonterminal name ∧ facts name token := by
  induction symbols with
  | nil => constructor; intro impossible; cases impossible; simp [prefixSources]
  | cons symbol tail ih =>
    cases symbol with
    | terminal actual =>
      constructor
      · intro value; cases value; exact ⟨_, by simp [prefixSources], Or.inl rfl⟩
      · rintro ⟨symbol, member, value⟩
        have equal : symbol = .terminal actual := by simpa [prefixSources] using member
        subst symbol
        rcases value with equal | ⟨name, impossible, fact⟩
        · cases equal; exact SequenceFirst.terminal _ _
        · cases impossible
    | nonterminal name =>
      constructor
      · intro value
        cases value with
        | head fact => exact ⟨_, by simp [prefixSources], Or.inr ⟨name, rfl, fact⟩⟩
        | skip empty rest =>
          obtain ⟨symbol, member, fact⟩ := ih.mp rest
          exact ⟨symbol, by simp [prefixSources, empty, member], fact⟩
      · rintro ⟨symbol, member, fact⟩
        simp only [prefixSources, List.mem_cons] at member
        rcases member with equal | rest
        · subst symbol
          rcases fact with impossible | ⟨actual, equal, known⟩
          · cases impossible
          · cases equal; exact SequenceFirst.head known
        · split at rest
          next empty => exact SequenceFirst.skip empty (ih.mpr ⟨symbol, rest, fact⟩)
          next absent => simp at rest

def firstClauses (owner : Nat) (catalog : List Nat) : Symbol → List (Worklist.Clause (Nat × Nat))
  | .terminal token => [⟨[], (owner, token)⟩]
  | .nonterminal name => catalog.map fun token => ⟨[(name, token)], (owner, token)⟩

def firstProgram (g : CFG) (nullable catalog : List Nat) : List (Worklist.Clause (Nat × Nat)) :=
  g.flatMap fun rule => (prefixSources nullable rule.rhs).flatMap (firstClauses rule.lhs catalog)

theorem sequence_semantic {g : CFG} {nullable : Nat → Prop} {facts : Nat → Nat → Prop}
    (nullableSound : ∀ name, nullable name → Nullable g [.nonterminal name])
    (firstSound : ∀ name token, facts name token → First g [.nonterminal name] token)
    {symbols token} (value : SequenceFirst nullable facts symbols token) : First g symbols token := by
  induction value with
  | terminal token tail => exact First.terminal token tail
  | head known =>
    have first := firstSound _ _ known
    cases first with
    | body rule member known => exact First.body rule member known
    | skip rule member nullableBody rest => cases rest
  | skip emptyPrefix rest ih =>
    have emptyProof := nullableSound _ emptyPrefix
    cases emptyProof with
    | expand rule member body rest => exact First.skip rule member body ih

theorem first_rule_sound {g : CFG} {nullable catalog : List Nat}
    (nullableExact : ∀ name, name ∈ nullable ↔ Nullable g [.nonterminal name])
    {clause : Worklist.Clause (Nat × Nat)} (member : clause ∈ firstProgram g nullable catalog)
    (body : ∀ fact ∈ clause.dependencies, First g [.nonterminal fact.1] fact.2) :
    First g [.nonterminal clause.head.1] clause.head.2 := by
  obtain ⟨rule, ruleMember, member⟩ := List.mem_flatMap.mp member
  obtain ⟨symbol, sourceMember, member⟩ := List.mem_flatMap.mp member
  cases symbol with
  | terminal token =>
    have equal : clause = ⟨[], (rule.lhs, token)⟩ := by simpa [firstClauses] using member
    subst clause
    apply First.body rule ruleMember
    apply sequence_semantic (fun name member => (nullableExact name).mp member)
      (fun name token known => known)
    exact (prefix_sequence nullable (fun name token => First g [.nonterminal name] token) rule.rhs token).mpr
      ⟨_, sourceMember, Or.inl rfl⟩
  | nonterminal name =>
    obtain ⟨token, tokenMember, equal⟩ := List.mem_map.mp member
    subst clause
    apply First.body rule ruleMember
    apply sequence_semantic (fun name member => (nullableExact name).mp member)
      (fun name token known => known)
    exact (prefix_sequence nullable (fun name token => First g [.nonterminal name] token) rule.rhs token).mpr
      ⟨_, sourceMember, Or.inr ⟨name, rfl, body (name, token) (by simp)⟩⟩

theorem first_closed_at (g : CFG) (nullable catalog : List Nat) (token : Nat)
    (inCatalog : token ∈ catalog) :
    ∀ rule ∈ g, SequenceFirst (fun name => name ∈ nullable)
      (fun name token => Worklist.Entails (firstProgram g nullable catalog) (name, token)) rule.rhs token →
      Worklist.Entails (firstProgram g nullable catalog) (rule.lhs, token) := by
  intro rule member semantic
  obtain ⟨symbol, belongs, fact⟩ := (prefix_sequence _ _ _ _).mp semantic
  rcases fact with rfl | ⟨name, rfl, known⟩
  · apply Worklist.Entails.rule (⟨[], (rule.lhs, token)⟩ : Worklist.Clause (Nat × Nat))
    · exact List.mem_flatMap.mpr ⟨rule, member, List.mem_flatMap.mpr ⟨_, belongs, by simp [firstClauses]⟩⟩
    · simp
  · apply Worklist.Entails.rule (⟨[(name, token)], (rule.lhs, token)⟩ : Worklist.Clause (Nat × Nat))
    · exact List.mem_flatMap.mpr ⟨rule, member, List.mem_flatMap.mpr
        ⟨_, belongs, List.mem_map.mpr ⟨token, inCatalog, rfl⟩⟩⟩
    · intro fact belongs
      have equal : fact = (name, token) := by simpa using belongs
      subst fact
      exact known

theorem first_horn_sound (g : CFG) (nullable catalog : List Nat)
    (nullableExact : ∀ name, name ∈ nullable ↔ Nullable g [.nonterminal name])
    (fact : Nat × Nat) (derived : Worklist.Entails (firstProgram g nullable catalog) fact) :
    First g [.nonterminal fact.1] fact.2 := by
  induction derived with
  | rule clause member body ih => exact first_rule_sound nullableExact member ih

theorem first_horn_exact (g : CFG) (nullable catalog : List Nat)
    (nullableExact : ∀ name, name ∈ nullable ↔ Nullable g [.nonterminal name])
    (name token : Nat) (inCatalog : token ∈ catalog) :
    Worklist.Entails (firstProgram g nullable catalog) (name, token) ↔ First g [.nonterminal name] token := by
  constructor
  · exact first_horn_sound g nullable catalog nullableExact (name, token)
  · intro semantic
    have translate {symbols token} (semantic : First g symbols token) (inCatalog : token ∈ catalog) :
        SequenceFirst (fun name => name ∈ nullable)
          (fun name token => Worklist.Entails (firstProgram g nullable catalog) (name, token)) symbols token := by
      induction semantic with
      | terminal token tail => exact SequenceFirst.terminal token tail
      | body rule member first ih => exact SequenceFirst.head (first_closed_at g nullable catalog _ inCatalog rule member (ih inCatalog))
      | skip rule member nullableBody first ih =>
        exact SequenceFirst.skip ((nullableExact rule.lhs).mpr (Nullable.expand rule member nullableBody Nullable.empty)) (ih inCatalog)
    have result := translate semantic inCatalog
    cases result with
    | head member => exact member
    | skip empty rest => cases rest

def terminalCatalog (g : CFG) : List Nat :=
  (g.flatMap fun rule => rule.rhs.filterMap fun symbol => match symbol with
    | .terminal token => some token
    | .nonterminal _ => none).eraseDups

theorem first_origin {g : CFG} {symbols token} (fact : First g symbols token) :
    .terminal token ∈ symbols ∨ ∃ rule ∈ g, .terminal token ∈ rule.rhs := by
  induction fact with
  | terminal token tail => exact Or.inl (by simp)
  | body rule member first ih =>
    rcases ih with direct | origin
    · exact Or.inr ⟨rule, member, direct⟩
    · exact Or.inr origin
  | skip rule member nullableBody first ih =>
    rcases ih with direct | origin
    · exact Or.inl (List.mem_cons_of_mem _ direct)
    · exact Or.inr origin

theorem named_first_in_catalog {g : CFG} {name token}
    (fact : First g [.nonterminal name] token) : token ∈ terminalCatalog g := by
  rcases first_origin fact with impossible | ⟨rule, member, origin⟩
  · simp at impossible
  · apply List.mem_eraseDups.mpr
    exact List.mem_flatMap.mpr ⟨rule, member, List.mem_filterMap.mpr ⟨.terminal token, origin, rfl⟩⟩

theorem first_control_flow_exact (g : CFG) (nullable : List Nat)
    (nullableExact : ∀ name, name ∈ nullable ↔ Nullable g [.nonterminal name])
    (last : Worklist.State (Nat × Nat))
    (history : Worklist.Trace (firstProgram g nullable (terminalCatalog g))
      (Worklist.initial (firstProgram g nullable (terminalCatalog g))) last)
    (finished : last.pending = []) :
    ∀ name token, (name, token) ∈ last.known ↔ First g [.nonterminal name] token := by
  intro name token
  rw [Worklist.quiescent_exact _ last history finished]
  constructor
  · exact first_horn_sound g nullable (terminalCatalog g) nullableExact (name, token)
  · intro semantic
    exact (first_horn_exact g nullable (terminalCatalog g) nullableExact name token
      (named_first_in_catalog semantic)).mpr semantic

/-- Compose the actual preparation order: FIRST reads the completed nullable
publication. Both conclusions follow from operational histories, with no
external equality certificate or final closed-set premise. -/
theorem preparation_pipeline_exact (g : CFG) (nullable : Worklist.State Nat)
    (first : Worklist.State (Nat × Nat))
    (nullableHistory : Worklist.Trace (nullableProgram g) (Worklist.initial (nullableProgram g)) nullable)
    (nullableFinished : nullable.pending = [])
    (firstHistory : Worklist.Trace (firstProgram g nullable.known (terminalCatalog g))
      (Worklist.initial (firstProgram g nullable.known (terminalCatalog g))) first)
    (firstFinished : first.pending = []) :
    (∀ name, name ∈ nullable.known ↔ Nullable g [.nonterminal name]) ∧
      (∀ name token, (name, token) ∈ first.known ↔ First g [.nonterminal name] token) := by
  have nullableExact := nullable_control_flow_exact g nullable nullableHistory nullableFinished
  exact ⟨nullableExact, first_control_flow_exact g nullable.known nullableExact first firstHistory firstFinished⟩

/-- Batched completion and FIRST retain the same source semantics. The masks
adapter constructs BatchTrace steps by clear_owner_partition; final semantic
closure is derived here rather than supplied as a certificate. -/
theorem batched_preparation_exact (g : CFG) (nullable : Worklist.State Nat)
    (first : Worklist.State (Nat × Nat))
    (nullableHistory : Worklist.BatchTrace (nullableProgram g) (Worklist.initial (nullableProgram g)) nullable)
    (nullableFinished : nullable.pending = [])
    (firstHistory : Worklist.BatchTrace (firstProgram g nullable.known (terminalCatalog g))
      (Worklist.initial (firstProgram g nullable.known (terminalCatalog g))) first)
    (firstFinished : first.pending = []) :
    (∀ name, name ∈ nullable.known ↔ Nullable g [.nonterminal name]) ∧
      (∀ name token, (name, token) ∈ first.known ↔ First g [.nonterminal name] token) := by
  have nullableExact : ∀ name, name ∈ nullable.known ↔ Nullable g [.nonterminal name] := by
    intro name
    rw [Worklist.batch_quiescent_exact _ nullable nullableHistory nullableFinished]
    exact nullable_horn_exact g name
  refine ⟨nullableExact, ?_⟩
  intro name token
  rw [Worklist.batch_quiescent_exact _ first firstHistory firstFinished]
  constructor
  · exact first_horn_sound g nullable.known (terminalCatalog g) nullableExact (name, token)
  · intro semantic
    exact (first_horn_exact g nullable.known (terminalCatalog g) nullableExact name token
      (named_first_in_catalog semantic)).mpr semantic

end ParserEngine.Solver
