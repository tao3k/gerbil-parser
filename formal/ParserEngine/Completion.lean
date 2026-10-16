import ParserEngine.Grammar

/-! Publication certificates: finite rounds supply sound witnesses and closure
supplies completeness. Exhausted fuel never implies a successful certificate. -/
namespace ParserEngine.Completion
open Grammar

/-- Counts occurrences, not distinct dependencies. Undefined names remain
obligations even though they have no publisher in the native worklist. -/
def remaining (dependencies known : List Nat) : Nat :=
  (dependencies.filter fun name => decide (name ∉ known)).length

def occurrences (dependencies : List Nat) (name : Nat) : Nat :=
  (dependencies.filter fun other => decide (other = name)).length

theorem zero_remaining (dependencies known : List Nat) :
    remaining dependencies known = 0 ↔ ∀ name ∈ dependencies, name ∈ known := by
  simp [remaining, List.filter_eq_nil_iff]

theorem fresh_notification_counts_occurrences (dependencies known : List Nat) (name : Nat)
    (fresh : name ∉ known) :
    remaining dependencies known = remaining dependencies (name :: known) + occurrences dependencies name := by
  induction dependencies with
  | nil => simp [remaining, occurrences]
  | cons head tail ih =>
    by_cases equal : head = name
    · subst head
      simp [remaining, occurrences, fresh] at ih ⊢
      omega
    · by_cases present : head ∈ known
      · simpa [remaining, occurrences, equal, present] using ih
      · simp [remaining, occurrences, equal, present] at ih ⊢
        omega

theorem duplicate_notification_changes_nothing (dependencies known : List Nat) (name : Nat)
    (already : name ∈ known) : remaining dependencies (name :: known) = remaining dependencies known := by
  unfold remaining
  apply congrArg List.length
  apply List.filter_congr
  intro other member
  by_cases equal : other = name
  · subst other
    simp [already]
  · simp [equal]

def sameSet [DecidableEq α] (left right : List α) : Bool :=
  left.all (fun item => right.contains item) && right.all (fun item => left.contains item)

theorem sameSet_members [DecidableEq α] {left right : List α}
    (equal : sameSet left right = true) : ∀ item, item ∈ left ↔ item ∈ right := by
  simp only [sameSet, Bool.and_eq_true, List.all_eq_true, List.contains_iff_mem] at equal
  exact fun item => ⟨fun member => equal.1 item member, fun member => equal.2 item member⟩

def nullableTest (facts : List Nat) (symbols : List Symbol) : Bool :=
  symbols.all fun symbol => match symbol with
    | .terminal _ => false
    | .nonterminal name => facts.contains name

theorem nullableTest_members (facts : List Nat) (symbols : List Symbol) :
    nullableTest facts symbols = true ↔
      ∀ symbol ∈ symbols, ∃ name, symbol = .nonterminal name ∧ name ∈ facts := by
  simp only [nullableTest, List.all_eq_true]
  constructor
  · intro valid symbol member
    have known := valid symbol member
    cases symbol with
    | terminal token => simp at known
    | nonterminal name => exact ⟨name, rfl, by simpa using known⟩
  · intro valid symbol member
    obtain ⟨name, rfl, known⟩ := valid symbol member
    simpa using known

theorem nullable_from_members {g : CFG} {symbols : List Symbol}
    (known : ∀ symbol ∈ symbols, ∃ name, symbol = .nonterminal name ∧ Nullable g [.nonterminal name]) :
    Nullable g symbols := by
  induction symbols with
  | nil => exact Nullable.empty
  | cons symbol tail ih =>
    obtain ⟨name, rfl, head⟩ := known symbol (by simp)
    have rest := ih (fun symbol member => known symbol (by simp [member]))
    cases head with
    | expand rule member body restProof => exact Nullable.expand rule member body rest

def nullableRound (g : CFG) (facts : List Nat) : List Nat :=
  (facts ++ g.filterMap fun rule => if nullableTest facts rule.rhs then some rule.lhs else none).eraseDups

def nullableRounds (g : CFG) : Nat → List Nat
  | 0 => []
  | fuel + 1 => nullableRound g (nullableRounds g fuel)

theorem nullable_round_sound {g : CFG} {facts : List Nat}
    (sound : ∀ name ∈ facts, Nullable g [.nonterminal name]) :
    ∀ name ∈ nullableRound g facts, Nullable g [.nonterminal name] := by
  intro name member
  simp only [nullableRound, List.mem_eraseDups] at member
  rcases List.mem_append.mp member with old | fresh
  · exact sound name old
  · obtain ⟨rule, ruleMember, published⟩ := List.mem_filterMap.mp fresh
    split at published
    next test =>
      have equal : rule.lhs = name := Option.some.inj published
      subst name
      apply Nullable.expand rule ruleMember _ Nullable.empty
      apply nullable_from_members
      intro symbol member
      obtain ⟨name, equal, known⟩ := (nullableTest_members facts rule.rhs).mp test symbol member
      exact ⟨name, equal, sound name known⟩
    next absent => cases published

theorem nullable_rounds_sound (g : CFG) (fuel : Nat) :
    ∀ name ∈ nullableRounds g fuel, Nullable g [.nonterminal name] := by
  induction fuel with
  | zero => simp [nullableRounds]
  | succ fuel ih => exact nullable_round_sound ih

def checkNullable (g : CFG) (facts : List Nat) (fuel : Nat) : Bool :=
  sameSet facts (nullableRounds g fuel) &&
    g.all (fun rule => !nullableTest facts rule.rhs || facts.contains rule.lhs)

theorem nullable_certificate_exact (g : CFG) (facts : List Nat) (fuel : Nat)
    (checked : checkNullable g facts fuel = true) :
    ∀ name, name ∈ facts ↔ Nullable g [.nonterminal name] := by
  have parts : sameSet facts (nullableRounds g fuel) = true ∧
      (g.all fun rule => !nullableTest facts rule.rhs || facts.contains rule.lhs) = true :=
    by simpa only [checkNullable, Bool.and_eq_true] using checked
  have equal := sameSet_members parts.1
  have closed : ∀ rule ∈ g,
      (∀ symbol ∈ rule.rhs, ∃ name, symbol = .nonterminal name ∧ name ∈ facts) → rule.lhs ∈ facts := by
    intro rule member body
    have test := (nullableTest_members facts rule.rhs).mpr body
    have check := List.all_eq_true.mp parts.2 rule member
    simpa [test] using check
  intro name
  constructor
  · intro member
    exact nullable_rounds_sound g fuel name ((equal name).mp member)
  · intro semantic
    have least := nullable_least (fun name => name ∈ facts) closed semantic
    obtain ⟨actual, identity, member⟩ := least (.nonterminal name) (by simp)
    cases identity
    exact member

def lookupFirst (facts : List (Nat × Nat)) (name : Nat) : List Nat :=
  facts.filterMap fun pair => if pair.1 = name then some pair.2 else none

theorem lookupFirst_member (facts : List (Nat × Nat)) (name token : Nat) :
    token ∈ lookupFirst facts name ↔ (name, token) ∈ facts := by
  simp only [lookupFirst, List.mem_filterMap]
  constructor
  · rintro ⟨⟨owner, terminal⟩, member, result⟩
    split at result
    next equal => simp_all
    next absent => cases result
  · intro member
    exact ⟨(name, token), member, by simp⟩

def sequenceFirst (nullable : List Nat) (facts : List (Nat × Nat)) : List Symbol → List Nat
  | [] => []
  | .terminal token :: _ => [token]
  | .nonterminal name :: tail => lookupFirst facts name ++
      if nullable.contains name then sequenceFirst nullable facts tail else []

theorem sequenceFirst_member (nullable : List Nat) (facts : List (Nat × Nat))
    (symbols : List Symbol) (token : Nat) :
    token ∈ sequenceFirst nullable facts symbols ↔
      SequenceFirst (fun name => name ∈ nullable) (fun name token => (name, token) ∈ facts) symbols token := by
  induction symbols with
  | nil => constructor; simp [sequenceFirst]; intro impossible; cases impossible
  | cons symbol tail ih =>
    cases symbol with
    | terminal actual =>
      constructor
      · intro member
        have equal : token = actual := by simpa [sequenceFirst] using member
        subst token
        exact SequenceFirst.terminal actual tail
      · intro semantic
        cases semantic
        simp [sequenceFirst]
    | nonterminal name =>
      constructor
      · intro member
        rcases List.mem_append.mp member with head | rest
        · exact SequenceFirst.head ((lookupFirst_member facts name token).mp head)
        · split at rest
          next empty => exact SequenceFirst.skip (by simpa using empty) (ih.mp rest)
          next absent => simp at rest
      · intro semantic
        cases semantic with
        | head known => exact List.mem_append.mpr (Or.inl ((lookupFirst_member facts name token).mpr known))
        | skip empty rest =>
          exact List.mem_append.mpr (Or.inr (by simpa [empty] using ih.mpr rest))

theorem first_sequence_sound {g : CFG} {nullable : List Nat} {facts : List (Nat × Nat)}
    (nullableSound : ∀ name ∈ nullable, Nullable g [.nonterminal name])
    (firstSound : ∀ name token, (name, token) ∈ facts → First g [.nonterminal name] token)
    {symbols token}
    (known : SequenceFirst (fun name => name ∈ nullable) (fun name token => (name, token) ∈ facts) symbols token) :
    First g symbols token := by
  induction known with
  | terminal token tail => exact First.terminal token tail
  | head known =>
    have semantic := firstSound _ _ known
    cases semantic with
    | body rule member first => exact First.body rule member first
    | skip rule member nullable first => cases first
  | skip empty known ih =>
    have semantic := nullableSound _ empty
    cases semantic with
    | expand rule member body rest => exact First.skip rule member body ih

def firstRound (g : CFG) (nullable : List Nat) (facts : List (Nat × Nat)) : List (Nat × Nat) :=
  (facts ++ g.flatMap fun rule => (sequenceFirst nullable facts rule.rhs).map (rule.lhs, ·)).eraseDups

def firstRounds (g : CFG) (nullable : List Nat) : Nat → List (Nat × Nat)
  | 0 => []
  | fuel + 1 => firstRound g nullable (firstRounds g nullable fuel)

theorem first_round_sound {g : CFG} {nullable : List Nat} {facts : List (Nat × Nat)}
    (nullableSound : ∀ name ∈ nullable, Nullable g [.nonterminal name])
    (sound : ∀ name token, (name, token) ∈ facts → First g [.nonterminal name] token) :
    ∀ name token, (name, token) ∈ firstRound g nullable facts → First g [.nonterminal name] token := by
  intro name token member
  simp only [firstRound, List.mem_eraseDups] at member
  rcases List.mem_append.mp member with old | fresh
  · exact sound name token old
  · obtain ⟨rule, ruleMember, result⟩ := List.mem_flatMap.mp fresh
    obtain ⟨actual, first, equal⟩ := List.mem_map.mp result
    have owner := congrArg Prod.fst equal
    have terminal := congrArg Prod.snd equal
    simp only at owner terminal
    subst name
    subst token
    exact First.body rule ruleMember (first_sequence_sound nullableSound sound
      ((sequenceFirst_member nullable facts rule.rhs actual).mp first))

theorem first_rounds_sound (g : CFG) (nullable : List Nat)
    (nullableSound : ∀ name ∈ nullable, Nullable g [.nonterminal name]) (fuel : Nat) :
    ∀ name token, (name, token) ∈ firstRounds g nullable fuel → First g [.nonterminal name] token := by
  induction fuel with
  | zero => simp [firstRounds]
  | succ fuel ih => exact first_round_sound nullableSound ih

def checkFirst (g : CFG) (nullable : List Nat) (facts : List (Nat × Nat)) (fuel : Nat) : Bool :=
  sameSet facts (firstRounds g nullable fuel) &&
    g.all (fun rule => (sequenceFirst nullable facts rule.rhs).all (fun token => facts.contains (rule.lhs, token)))

theorem first_certificate_exact (g : CFG) (nullable : List Nat) (facts : List (Nat × Nat))
    (nullableFuel firstFuel : Nat) (nullableChecked : checkNullable g nullable nullableFuel = true)
    (checked : checkFirst g nullable facts firstFuel = true) :
    ∀ name token, (name, token) ∈ facts ↔ First g [.nonterminal name] token := by
  have nullableExact := nullable_certificate_exact g nullable nullableFuel nullableChecked
  have parts : sameSet facts (firstRounds g nullable firstFuel) = true ∧
      (g.all (fun rule => (sequenceFirst nullable facts rule.rhs).all (fun token => facts.contains (rule.lhs, token)))) = true :=
    by simpa only [checkFirst, Bool.and_eq_true] using checked
  have equal := sameSet_members parts.1
  have nullableClosed : ∀ rule ∈ g,
      (∀ symbol ∈ rule.rhs, ∃ name, symbol = .nonterminal name ∧ name ∈ nullable) → rule.lhs ∈ nullable := by
    intro rule member body
    apply (nullableExact rule.lhs).mpr
    exact Nullable.expand rule member (nullable_from_members (by
      intro symbol member
      obtain ⟨name, identity, known⟩ := body symbol member
      exact ⟨name, identity, (nullableExact name).mp known⟩)) Nullable.empty
  have firstClosed : ∀ rule ∈ g, ∀ token,
      SequenceFirst (fun name => name ∈ nullable) (fun name token => (name, token) ∈ facts) rule.rhs token →
        (rule.lhs, token) ∈ facts := by
    intro rule member token semantic
    have belongs := (sequenceFirst_member nullable facts rule.rhs token).mpr semantic
    have row := List.all_eq_true.mp parts.2 rule member
    simpa using List.all_eq_true.mp row token belongs
  intro name token
  constructor
  · intro member
    exact first_rounds_sound g nullable (fun name member => (nullableExact name).mp member)
      firstFuel name token ((equal (name, token)).mp member)
  · intro semantic
    have least := first_least (fun name => name ∈ nullable) (fun name token => (name, token) ∈ facts)
      nullableClosed firstClosed semantic
    cases least with
    | head member => exact member
    | skip empty rest => cases rest

-- Closed cycles cannot certify invented facts; empty fuel cannot hide missing
-- consequences. FIRST remains a prefix property even for an unproductive tail.
example : checkNullable [⟨0, [.nonterminal 0]⟩] [0] 4 = false := by decide
example : checkNullable [⟨0, []⟩] [] 0 = false := by decide
example : checkFirst [⟨0, [.nonterminal 0]⟩] [] [(0, 1)] 4 = false := by decide
example : checkFirst [⟨0, [.terminal 1, .nonterminal 1]⟩, ⟨1, [.nonterminal 1]⟩]
    [] [] 0 = false := by decide
example : checkFirst [⟨0, [.terminal 1, .nonterminal 1]⟩, ⟨1, [.nonterminal 1]⟩]
    [] [(0, 1)] 4 = true := by decide

end ParserEngine.Completion
