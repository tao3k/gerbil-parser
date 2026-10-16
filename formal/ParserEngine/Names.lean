import ParserEngine.Lowering

/-! Physical name allocation and reverse production emission. The reserved
input includes authored names, all references (even undefined references),
and the requested root. Success and retry correspond to claim-name/fresh.
No assumption about the shape of a printed helper name is used for safety. -/
namespace ParserEngine.Names

structure State (Name : Type) where
  used : List Name
  bindings : List (Name × Lowering.Owner)
  attempt : Nat

def initial (reserved : List Name) : State Name := ⟨reserved, [], 0⟩

inductive Trace : State Name → State Name → Prop where
  | refl (s : State Name) : Trace s s
  | retry {start current name} (history : Trace start current) (collision : name ∈ current.used) :
      Trace start {current with attempt := current.attempt + 1}
  | claim {start current name owner} (history : Trace start current) (fresh : name ∉ current.used) :
      Trace start ⟨name :: current.used, (name, owner) :: current.bindings, current.attempt + 1⟩

theorem trace_used {a b : State Name} (history : Trace a b) :
    ∀ name ∈ a.used, name ∈ b.used := by
  induction history with
  | refl => exact fun _ h => h
  | retry history collision ih => exact ih
  | claim history fresh ih => exact fun name h => List.mem_cons_of_mem _ (ih name h)

theorem bindings_used {reserved : List Name} {last : State Name}
    (history : Trace (initial reserved) last) : ∀ pair ∈ last.bindings, pair.1 ∈ last.used := by
  induction history with
  | refl => simp [initial]
  | retry history collision ih => exact ih
  | claim history fresh ih =>
    intro pair h
    rcases List.mem_cons.mp h with rfl | h
    · simp
    · exact List.mem_cons_of_mem _ (ih pair h)

theorem unique_names {reserved : List Name} {last : State Name}
    (history : Trace (initial reserved) last) : (last.bindings.map Prod.fst).Nodup := by
  induction history with
  | refl => simp [initial]
  | retry history collision ih => exact ih
  | claim history fresh ih =>
    simp only [List.map_cons, List.nodup_cons]
    constructor
    · intro h
      obtain ⟨pair, member, equal⟩ := List.mem_map.mp h
      exact fresh (equal ▸ bindings_used history pair member)
    · exact ih

theorem reserved_not_helper {reserved : List Name} {last : State Name}
    (history : Trace (initial reserved) last) :
    ∀ pair ∈ last.bindings, pair.1 ∉ reserved := by
  induction history with
  | refl => simp [initial]
  | retry history collision ih => exact ih
  | claim history fresh ih =>
    intro pair h
    rcases List.mem_cons.mp h with rfl | h
    · intro reservedMember
      exact fresh (trace_used history _ reservedMember)
    · exact ih pair h

/-- A failed claim consumes a counter value but never changes bindings. This
models arbitrary collisions with authored helper-looking or undefined names. -/
theorem attempts_monotone {a b : State Name} (history : Trace a b) : a.attempt ≤ b.attempt := by
  induction history with
  | refl => exact Nat.le_refl _
  | retry history collision ih => exact Nat.le_trans ih (Nat.le_succ _)
  | claim history fresh ih => exact Nat.le_trans ih (Nat.le_succ _)

/-- Symbol lookup is unambiguous, even if different private helpers have the
same expression owner. Injectivity of owners is neither true nor required. -/
theorem binding_functional [DecidableEq Name] {reserved : List Name} {last : State Name}
    (history : Trace (initial reserved) last) {name : Name} {a b : Lowering.Owner}
    (first : (name, a) ∈ last.bindings) (second : (name, b) ∈ last.bindings) : a = b := by
  induction history with
  | refl => simp [initial] at first
  | retry history collision ih => exact ih first second
  | @claim current freshName owner history fresh ih =>
    rcases List.mem_cons.mp first with equal | oldFirst
    · cases equal
      rcases List.mem_cons.mp second with equal | oldSecond
      · exact (Prod.mk.inj equal).2.symm
      · exact False.elim (fresh (bindings_used history _ oldSecond))
    · rcases List.mem_cons.mp second with equal | oldSecond
      · cases equal
        exact False.elim (fresh (bindings_used history _ oldFirst))
      · exact ih oldFirst oldSecond

def references : Lowering.Expr → List Nat
  | .reference name => [name]
  | .sequence a b | .choice a b => references a ++ references b
  | .optional child | .repeat child | .repeat1 child | .annotation _ child => references child
  | _ => []

def reservedNames (root : Nat) (rules : Lowering.Rules) : List Nat :=
  root :: rules.flatMap (fun row => row.1 :: references row.2)

/-- Includes undefined references and declarations after their first use. -/
theorem source_names_reserved (root : Nat) (rules : Lowering.Rules) {name expression}
    (row : (name, expression) ∈ rules) :
    name ∈ reservedNames root rules ∧ ∀ target ∈ references expression, target ∈ reservedNames root rules := by
  constructor
  · exact List.mem_cons_of_mem _ (List.mem_flatMap.mpr ⟨(name, expression), row, by simp⟩)
  · intro target member
    exact List.mem_cons_of_mem _ (List.mem_flatMap.mpr ⟨(name, expression), row, by simp [member]⟩)

theorem helpers_avoid_source {root rules} {last : State Nat}
    (history : Trace (initial (reservedNames root rules)) last) {pair}
    (member : pair ∈ last.bindings) :
    pair.1 ≠ root ∧ ∀ name expression, (name, expression) ∈ rules →
      pair.1 ≠ name ∧ pair.1 ∉ references expression := by
  have excluded := reserved_not_helper history pair member
  constructor
  · intro equal; apply excluded; simp [reservedNames, equal]
  · intro name expression row
    have reserved := source_names_reserved root rules row
    constructor
    · intro equal; exact excluded (equal ▸ reserved.1)
    · intro reference; exact excluded (reserved.2 _ reference)

/-- Every callback records the exact production and its source occurrence.
Production zero (augmentation) is installed separately by lower-rules. -/
structure Emission (Origin : Type) where
  id : Nat
  production : Grammar.Production
  origin : Origin
  deriving DecidableEq

structure Tape (Origin : Type) where
  next : Nat
  reverseProductions : List (Emission Origin)
  reverseObserved : List (Emission Origin)

def emptyTape : Tape Origin := ⟨1, [], []⟩

def emit (t : Tape Origin) (production : Grammar.Production) (origin : Origin) : Tape Origin :=
  let row : Emission Origin := ⟨t.next, production, origin⟩
  ⟨t.next + 1, row :: t.reverseProductions, row :: t.reverseObserved⟩

inductive Emitted : Tape Origin → Prop where
  | empty : Emitted emptyTape
  | step {t production origin} (history : Emitted t) : Emitted (emit t production origin)

theorem observer_alignment {t : Tape Origin} (history : Emitted t) :
    t.reverseObserved = t.reverseProductions := by
  induction history with
  | empty => rfl
  | step history ih => simp [emit, ih]

theorem ordinal_alignment {t : Tape Origin} (history : Emitted t) :
    t.next = t.reverseProductions.length + 1 ∧
      t.reverseProductions.reverse.map Emission.id = List.range' 1 t.reverseProductions.length := by
  induction history with
  | empty => simp [emptyTape]
  | @step t production origin history ih =>
    constructor
    · simp [emit, ih.1]
    · simp only [emit, List.reverse_cons, List.map_append, List.map_cons, List.map_nil,
        List.length_cons]
      rw [ih.2, List.range'_1_concat]
      simp [ih.1, Nat.add_comm]

/-- Per-emission origin obligations compose for an arbitrary emission history,
regardless of ordinal allocation or the diagnostic observer's reverse order. -/
def OriginSupported (rules : Lowering.Rules) (decode : Nat → Lowering.Owner)
    (production : Grammar.Production) : Prop :=
  Lowering.Expansion rules [.nonterminal (decode production.lhs)]
    (production.rhs.map (Lowering.nativeSymbol decode))

inductive SupportedEmissions (rules : Lowering.Rules) (decode : Nat → Lowering.Owner) :
    Tape Origin → Prop where
  | empty : SupportedEmissions rules decode emptyTape
  | step {t production origin} (history : SupportedEmissions rules decode t)
      (supported : OriginSupported rules decode production) :
      SupportedEmissions rules decode (emit t production origin)

theorem emitted_origins {rules decode} {t : Tape Origin}
    (history : SupportedEmissions rules decode t) :
    ∀ production ∈ t.reverseProductions.reverse.map Emission.production,
      OriginSupported rules decode production := by
  induction history with
  | empty => simp [emptyTape]
  | step history supported ih =>
    intro production member
    simp only [emit, List.reverse_cons, List.map_append, List.map_cons, List.map_nil,
      List.mem_append, List.mem_singleton] at member
    rcases member with member | rfl
    · exact ih production member
    · exact supported

/-- Locally enumerable schema rules. This checker does not evaluate a language
or compare a bounded sample of accepted words. -/
def schemaRows (rules : Lowering.Rules) : Lowering.Owner → List (List Lowering.Symbol)
  | .named name => rules.filterMap fun row => if row.1 = name then some [Lowering.body row.2] else none
  | .expression expression => match expression with
    | .fail => []
    | .empty => [[]]
    | .terminal token => [[.terminal token]]
    | .reference name => [[Lowering.named name]]
    | .sequence a b => [[Lowering.body a, Lowering.body b]]
    | .choice a b => [[Lowering.body a], [Lowering.body b]]
    | .optional child => [[], [Lowering.body child]]
    | .repeat child => [[], [Lowering.body (.repeat child), Lowering.body child]]
    | .repeat1 child => [[Lowering.body child], [Lowering.body (.repeat1 child), Lowering.body child]]
    | .annotation _ child => [[Lowering.body child]]

theorem schema_row_sound {rules owner rhs} (member : rhs ∈ schemaRows rules owner) :
    Lowering.Production rules owner rhs := by
  cases owner with
  | named name =>
    obtain ⟨⟨actual, expression⟩, belongs, output⟩ := List.mem_filterMap.mp member
    simp only [schemaRows] at member
    split at output
    next equal =>
      cases Option.some.inj output
      change actual = name at equal
      subst actual
      exact Lowering.Production.named belongs
    next different => cases output
  | expression expression =>
    cases expression <;> simp only [schemaRows, List.mem_cons, List.not_mem_nil,
      or_false] at member
    all_goals first
      | contradiction
      | (rcases member with rfl | rfl <;> constructor)
      | (subst rhs; constructor)

/-- Explicit expansion recipes support inlining, annotations and private loop
owners. Invalid schema steps return none. Successful checking constructs a
kernel proof for arbitrary recipe depth and grammar size. -/
inductive Recipe where
  | identity (symbols : List Lowering.Symbol)
  | expand (owner : Lowering.Owner) (children rest : Recipe)
  | append (left right : Recipe)
  | foldNamed (name : Nat) (expression : Lowering.Expr)
  deriving Repr

abbrev Checked (rules : Lowering.Rules) :=
  {pair : List Lowering.Symbol × List Lowering.Symbol // Lowering.Expansion rules pair.1 pair.2}

def verify (rules : Lowering.Rules) : Recipe → Option (Checked rules)
  | .identity symbols => some ⟨(symbols, symbols), .refl symbols⟩
  | .append left right => do
    let a ← verify rules left
    let b ← verify rules right
    return ⟨(a.val.1 ++ b.val.1, a.val.2 ++ b.val.2), .append a.property b.property⟩
  | .expand owner children rest => do
    let a ← verify rules children
    let b ← verify rules rest
    if valid : a.val.1 ∈ schemaRows rules owner then
      return ⟨(.nonterminal owner :: b.val.1, a.val.2 ++ b.val.2),
        .expand (schema_row_sound valid) a.property b.property⟩
    else none
  | .foldNamed name expression =>
    if member : (name, expression) ∈ rules then
      if unique : rules.all (fun row => row.1 != name || row.2 == expression) then
        some ⟨([Lowering.body expression], [Lowering.named name]), .foldNamed ⟨member, by
          intro other belongs
          have h := List.all_eq_true.mp unique (name, other) belongs
          simpa using h⟩⟩
      else none
    else none

/-- Compares the checked endpoints with the actual native rule, including its
physical-name decoding. A copied origin label alone cannot pass admission. -/
def admitOrigin (rules : Lowering.Rules) (decode : Nat → Lowering.Owner)
    (production : Grammar.Production) (recipe : Recipe) : Bool :=
  match verify rules recipe with
  | none => false
  | some checked => decide (checked.val.1 = [.nonterminal (decode production.lhs)] ∧
      checked.val.2 = production.rhs.map (Lowering.nativeSymbol decode))

theorem admitted_origin_sound {rules decode production recipe}
    (accepted : admitOrigin rules decode production recipe = true) :
    OriginSupported rules decode production := by
  unfold admitOrigin at accepted
  split at accepted
  · contradiction
  next checked h =>
    have ends := of_decide_eq_true accepted
    have proof := checked.property
    change Lowering.Expansion rules _ _
    rw [ends.1, ends.2] at proof
    exact proof

/-- Universal native derivation admission: every checked physical production
is composed with the existing source DSL theorem, not a finite word oracle. -/
theorem checked_native_root_sound (rules : Lowering.Rules) (native : Grammar.CFG)
    (decode : Nat → Lowering.Owner) (recipes : Grammar.Production → Recipe)
    (accepted : ∀ production ∈ native, admitOrigin rules decode production (recipes production) = true)
    (root source : Nat) (word : List Nat) (identity : decode root = .named source)
    (derivation : Grammar.Derives native [.nonterminal root] word) :
    Lowering.Eval rules (.reference source) word := by
  exact Lowering.native_root_sound rules native decode
    (fun production member => admitted_origin_sound (accepted production member))
    root source word identity derivation

/-- Admission consumes both lists completely; dropped or extra native rules
cannot be hidden by zip truncation. Augmentation is a separate start rule. -/
def admitNative (rules : Lowering.Rules) (decode : Nat → Lowering.Owner) :
    Grammar.CFG → List Recipe → Bool
  | [], [] => true
  | production :: rest, recipe :: recipes =>
      admitOrigin rules decode production recipe && admitNative rules decode rest recipes
  | _, _ => false

theorem admitted_native_sound {rules decode native recipes}
    (accepted : admitNative rules decode native recipes = true) :
    ∀ production ∈ native, OriginSupported rules decode production := by
  induction native generalizing recipes with
  | nil => simp
  | cons first rest ih =>
    cases recipes with
    | nil => simp [admitNative] at accepted
    | cons recipe recipes =>
      have checks : admitOrigin rules decode first recipe = true ∧
          admitNative rules decode rest recipes = true := by
        simpa only [admitNative, Bool.and_eq_true] using accepted
      intro production member
      rcases List.mem_cons.mp member with rfl | member
      · exact admitted_origin_sound checks.1
      · exact ih checks.2 production member

/-- Complete checked publication composes with the arbitrary-word source
semantics theorem. The native augmented start row is excluded from publication
and is handled by the separately established root-identity premise. -/
theorem admitted_native_root_sound {rules decode native recipes}
    (accepted : admitNative rules decode native recipes = true)
    (root source : Nat) (word : List Nat) (identity : decode root = .named source)
    (derivation : Grammar.Derives native [.nonterminal root] word) :
    Lowering.Eval rules (.reference source) word :=
  Lowering.native_root_sound rules native decode (admitted_native_sound accepted)
    root source word identity derivation

/-- A producer derivation records the local schema choice and its recursive
operands. This is a syntactic generation judgment, not a language-equivalence
premise or an assertion that an arbitrary supplied recipe already passed. -/
inductive Produced (rules : Lowering.Rules) : Recipe → List Lowering.Symbol → List Lowering.Symbol → Prop where
  | identity (symbols) : Produced rules (.identity symbols) symbols symbols
  | append {left right a b c d} (first : Produced rules left a b)
      (rest : Produced rules right c d) : Produced rules (.append left right) (a ++ c) (b ++ d)
  | expand {owner children rest rhs tail a b}
      (row : rhs ∈ schemaRows rules owner)
      (first : Produced rules children rhs a) (last : Produced rules rest tail b) :
      Produced rules (.expand owner children rest) (.nonterminal owner :: tail) (a ++ b)
  | foldNamed {name expression} (unique : Lowering.UniqueBody rules name expression) :
      Produced rules (.foldNamed name expression) [Lowering.body expression] [Lowering.named name]

theorem produced_recipe_checked {rules recipe before after}
    (generation : Produced rules recipe before after) :
    ∃ checked, verify rules recipe = some checked ∧ checked.val = (before, after) := by
  induction generation with
  | identity symbols => exact ⟨_, rfl, rfl⟩
  | append first rest ihFirst ihRest =>
    obtain ⟨a, ha, ea⟩ := ihFirst
    obtain ⟨b, hb, eb⟩ := ihRest
    simp only [verify, ha, hb, Option.pure_def]
    exact ⟨_, rfl, by simp [ea, eb]⟩
  | @expand owner children rest rhs tail outA outB row first last ihFirst ihLast =>
    obtain ⟨a, ha, ea⟩ := ihFirst
    obtain ⟨b, hb, eb⟩ := ihLast
    have valid : a.val.1 ∈ schemaRows rules owner := by simpa [ea] using row
    refine ⟨⟨(_, _), Lowering.Expansion.expand (schema_row_sound valid) a.property b.property⟩, ?_, ?_⟩
    · simp [verify, ha, hb, valid]
    · simp [ea, eb]
  | @foldNamed name expression unique =>
    have allRows : rules.all (fun row => row.1 != name || row.2 == expression) = true := by
      apply List.all_eq_true.mpr
      rintro ⟨actual, body⟩ member
      by_cases same : actual = name
      · have equal := unique.2 body (by simpa only [same] using member)
        simp [equal]
      · simp [same]
    simp only [verify, unique.1, dite_eq_left, allRows]
    exact ⟨_, rfl, rfl⟩

/-- The native sequence branch right-folds operand proofs, including the
zero-operand empty production. This construction works at arbitrary arity. -/
def sequenceRecipe : List (Lowering.Expr × Recipe) → Recipe
  | [] => .expand (.expression .empty) (.identity []) (.identity [])
  | (expression, recipe) :: rest =>
      .expand (.expression (.sequence expression (Lowering.sequenceList (rest.map Prod.fst))))
        (.append recipe (sequenceRecipe rest)) (.identity [])

theorem sequence_recipe_produced (rules : Lowering.Rules)
    (operands : List (Lowering.Expr × Recipe))
    (output : Lowering.Expr × Recipe → List Lowering.Symbol)
    (aligned : ∀ operand ∈ operands,
      Produced rules operand.2 [Lowering.body operand.1] (output operand)) :
    Produced rules (sequenceRecipe operands)
      [Lowering.body (Lowering.sequenceList (operands.map Prod.fst))]
      (operands.map output).flatten := by
  induction operands with
  | nil =>
    simpa [sequenceRecipe, Lowering.sequenceList, Lowering.body] using
      Produced.expand (rules := rules) (by simp [schemaRows])
        (Produced.identity []) (Produced.identity [])
  | cons operand operands ih =>
    have first := aligned operand (by simp)
    have rest := ih (fun op member => aligned op (by simp [member]))
    have row : [Lowering.body operand.1,
      Lowering.body (Lowering.sequenceList (operands.map Prod.fst))] ∈
      schemaRows rules (.expression (.sequence operand.1
        (Lowering.sequenceList (operands.map Prod.fst)))) := by simp [schemaRows]
    simpa [sequenceRecipe, Lowering.sequenceList, Lowering.body] using
      Produced.expand row (Produced.append first rest) (Produced.identity [])

/-- Atomic operands are erased directly; complex operands retain their
intrinsic helper owner. Native annotation erasure is an earlier projection. -/
def operandSymbol : Lowering.Expr → Lowering.Symbol
  | .terminal token => .terminal token
  | .reference name => Lowering.named name
  | expression => Lowering.body expression

def operandRecipe : Lowering.Expr → Recipe
  | .terminal token => .expand (.expression (.terminal token))
      (.identity [.terminal token]) (.identity [])
  | .reference name => .expand (.expression (.reference name))
      (.identity [Lowering.named name]) (.identity [])
  | expression => .identity [Lowering.body expression]

theorem operand_recipe_produced (rules : Lowering.Rules) (expression : Lowering.Expr) :
    Produced rules (operandRecipe expression) [Lowering.body expression] [operandSymbol expression] := by
  cases expression with
  | terminal token =>
    simpa [operandRecipe, operandSymbol, Lowering.body] using
      Produced.expand (rules := rules) (by simp [schemaRows])
        (Produced.identity [.terminal token]) (Produced.identity [])
  | reference name =>
    simpa [operandRecipe, operandSymbol, Lowering.body] using
      Produced.expand (rules := rules) (by simp [schemaRows])
        (Produced.identity [Lowering.named name]) (Produced.identity [])
  | _ => exact Produced.identity _

/-- An executable producer for every normalized sequence, with no supplied
per-operand proof or final language-equality premise. -/
theorem native_sequence_recipe_produced (rules : Lowering.Rules)
    (expressions : List Lowering.Expr) :
    Produced rules
      (sequenceRecipe (expressions.map (fun e => (e, operandRecipe e))))
      [Lowering.body (Lowering.sequenceList expressions)] (expressions.map operandSymbol) := by
  have generated := sequence_recipe_produced rules
    (expressions.map (fun e => (e, operandRecipe e))) (fun operand => [operandSymbol operand.1])
    (by
      intro operand member
      obtain ⟨expression, _, equal⟩ := List.mem_map.mp member
      subst operand
      exact operand_recipe_produced rules expression)
  have flatten : (expressions.map (fun e => [operandSymbol e])).flatten =
      expressions.map operandSymbol := by
    clear generated
    induction expressions with
    | nil => rfl
    | cons head tail ih => simpa using congrArg (List.cons (operandSymbol head)) ih
  simpa [List.map_map, Function.comp_def, flatten] using generated

/-- Native n-ary choice descends through right branches to one authored child.
The empty choice has no selected branch and therefore no generated rule. -/
def choiceRecipe : List Lowering.Expr → Nat → Recipe → Recipe
  | [], _, _ => .identity []
  | head :: tail, 0, recipe =>
      .expand (.expression (.choice head (Lowering.choiceList tail))) recipe (.identity [])
  | head :: tail, index + 1, recipe =>
      .expand (.expression (.choice head (Lowering.choiceList tail)))
        (choiceRecipe tail index recipe) (.identity [])

theorem choice_recipe_produced {rules expressions index expression recipe output}
    (selected : expressions[index]? = some expression)
    (child : Produced rules recipe [Lowering.body expression] output) :
    Produced rules (choiceRecipe expressions index recipe)
      [Lowering.body (Lowering.choiceList expressions)] output := by
  induction expressions generalizing index with
  | nil => simp at selected
  | cons head tail ih =>
    cases index with
    | zero =>
      have equal : head = expression := by simpa using selected
      subst head
      simpa [choiceRecipe, Lowering.choiceList, Lowering.body] using
        Produced.expand (by simp [schemaRows]) child (Produced.identity [])
    | succ index =>
      have row : [Lowering.body (Lowering.choiceList tail)] ∈
          schemaRows rules (.expression (.choice head (Lowering.choiceList tail))) := by
        simp [schemaRows]
      have rest := ih (by simpa using selected)
      simpa [choiceRecipe, Lowering.choiceList, Lowering.body] using
        Produced.expand row rest (Produced.identity [])

/-- The two loop branches use the same proof constructor. A shared choice
owner cannot produce a recurrence witness merely from one of its alternatives;
the native producer must bind a private helper or supply a unique named fold. -/
theorem repeat_recipe_produced {rules child recurrence operand loopOutput childOutput}
    (loop : Produced rules recurrence [Lowering.body (.repeat child)] loopOutput)
    (element : Produced rules operand [Lowering.body child] childOutput) :
    Produced rules
      (.expand (.expression (.repeat child)) (.append recurrence operand) (.identity []))
      [Lowering.body (.repeat child)] (loopOutput ++ childOutput) := by
  simpa [Lowering.body] using Produced.expand (by simp [schemaRows])
    (Produced.append loop element) (Produced.identity [])

theorem repeat1_recipe_produced {rules child recurrence operand loopOutput childOutput}
    (loop : Produced rules recurrence [Lowering.body (.repeat1 child)] loopOutput)
    (element : Produced rules operand [Lowering.body child] childOutput) :
    Produced rules
      (.expand (.expression (.repeat1 child)) (.append recurrence operand) (.identity []))
      [Lowering.body (.repeat1 child)] (loopOutput ++ childOutput) := by
  simpa [Lowering.body] using Produced.expand (by simp [schemaRows])
    (Produced.append loop element) (Produced.identity [])

namespace Controls

theorem invalid_schema_rejected :
    (verify [] (.expand (.expression (.repeat (.terminal 0)))
      (.identity [.terminal 1]) (.identity []))).isNone = true := by decide

theorem ambiguous_named_fold_rejected :
    (verify [(0, .empty), (0, .terminal 0)] (.foldNamed 0 .empty)).isNone = true := by decide

theorem valid_origin_checked :
    admitOrigin [] (fun _ => .expression (.optional (.terminal 0))) ⟨0, []⟩
      (.expand (.expression (.optional (.terminal 0))) (.identity []) (.identity [])) = true := by decide

theorem changed_endpoint_rejected :
    admitOrigin [] (fun _ => .expression (.optional (.terminal 0))) ⟨0, [.terminal 0]⟩
      (.expand (.expression (.optional (.terminal 0))) (.identity []) (.identity [])) = false := by decide


theorem missing_recipe_rejected :
    admitNative [] (fun _ => .expression .empty) [⟨0, []⟩] [] = false := by decide

theorem extra_recipe_rejected :
    admitNative [] (fun _ => .expression .empty) [] [.identity []] = false := by decide

theorem changed_helper_binding_rejected :
    admitOrigin [] (fun _ => .expression (.terminal 1)) ⟨0, [.terminal 0]⟩
      (.expand (.expression (.terminal 0)) (.identity [.terminal 0]) (.identity [])) = false := by decide

end Controls
end ParserEngine.Names
