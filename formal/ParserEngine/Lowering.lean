import ParserEngine.Grammar

/-! A derivational lowering theorem, quantified over recursive grammars and
all finite words. Repetition has a private production owner. Annotations here
preserve terminal language only, not AST effects, layout or precedence policy. -/
namespace ParserEngine.Lowering

inductive Expr where
  | fail
  | empty
  | terminal (token : Nat)
  | reference (name : Nat)
  | sequence (left right : Expr)
  | choice (left right : Expr)
  | optional (child : Expr)
  | repeat (child : Expr)
  | repeat1 (child : Expr)
  | annotation (tag : Nat) (child : Expr)
  deriving DecidableEq, Repr

abbrev Rules := List (Nat × Expr)

inductive Eval (rules : Rules) : Expr → List Nat → Prop where
  | empty : Eval rules .empty []
  | terminal (token : Nat) : Eval rules (.terminal token) [token]
  | reference {name body word} (member : (name, body) ∈ rules)
      (value : Eval rules body word) : Eval rules (.reference name) word
  | sequence {left right a b} (first : Eval rules left a) (second : Eval rules right b) :
      Eval rules (.sequence left right) (a ++ b)
  | left {left right word} (value : Eval rules left word) : Eval rules (.choice left right) word
  | right {left right word} (value : Eval rules right word) : Eval rules (.choice left right) word
  | optionalEmpty (child : Expr) : Eval rules (.optional child) []
  | optionalSome {child word} (value : Eval rules child word) : Eval rules (.optional child) word
  | repeatEmpty (child : Expr) : Eval rules (.repeat child) []
  | repeatSnoc {child a b} (earlier : Eval rules (.repeat child) a) (last : Eval rules child b) :
      Eval rules (.repeat child) (a ++ b)
  | repeat1One {child word} (value : Eval rules child word) : Eval rules (.repeat1 child) word
  | repeat1Snoc {child a b} (earlier : Eval rules (.repeat1 child) a) (last : Eval rules child b) :
      Eval rules (.repeat1 child) (a ++ b)
  | annotation {child word} (tag : Nat) (value : Eval rules child word) : Eval rules (.annotation tag child) word

/-- Intrinsic owners prevent a repetition from calling a union of alternatives.
Physical native names need a fresh-name/inline correspondence to these owners. -/
inductive Owner where
  | named (name : Nat)
  | expression (body : Expr)
  deriving DecidableEq, Repr

inductive Symbol where
  | terminal (token : Nat)
  | nonterminal (owner : Owner)
  deriving DecidableEq, Repr

def body (expression : Expr) : Symbol := .nonterminal (.expression expression)
def named (name : Nat) : Symbol := .nonterminal (.named name)

/-- Production schema of the lowering target. Only finitely many expression
owners are reachable from any finite source grammar and root expression. -/
inductive Production (rules : Rules) : Owner → List Symbol → Prop where
  | named {name expression} (member : (name, expression) ∈ rules) :
      Production rules (.named name) [body expression]
  | empty : Production rules (.expression .empty) []
  | terminal (token : Nat) : Production rules (.expression (.terminal token)) [.terminal token]
  | reference (name : Nat) : Production rules (.expression (.reference name)) [named name]
  | sequence (left right : Expr) :
      Production rules (.expression (.sequence left right)) [body left, body right]
  | left (left right : Expr) : Production rules (.expression (.choice left right)) [body left]
  | right (left right : Expr) : Production rules (.expression (.choice left right)) [body right]
  | optionalEmpty (child : Expr) : Production rules (.expression (.optional child)) []
  | optionalSome (child : Expr) : Production rules (.expression (.optional child)) [body child]
  | repeatEmpty (child : Expr) : Production rules (.expression (.repeat child)) []
  | repeatSnoc (child : Expr) :
      Production rules (.expression (.repeat child)) [body (.repeat child), body child]
  | repeat1One (child : Expr) : Production rules (.expression (.repeat1 child)) [body child]
  | repeat1Snoc (child : Expr) :
      Production rules (.expression (.repeat1 child)) [body (.repeat1 child), body child]
  | annotation (tag : Nat) (child : Expr) :
      Production rules (.expression (.annotation tag child)) [body child]

inductive Derives (rules : Rules) : List Symbol → List Nat → Prop where
  | empty : Derives rules [] []
  | terminal {tail word} (token : Nat) (rest : Derives rules tail word) :
      Derives rules (.terminal token :: tail) (token :: word)
  | expand {owner rhs tail a b} (production : Production rules owner rhs)
      (first : Derives rules rhs a) (rest : Derives rules tail b) :
      Derives rules (.nonterminal owner :: tail) (a ++ b)

theorem derives_append {rules : Rules} {left right a b}
    (first : Derives rules left a) (second : Derives rules right b) :
    Derives rules (left ++ right) (a ++ b) := by
  induction first with
  | empty => simpa using second
  | terminal token rest ih => simpa using Derives.terminal token ih
  | expand production first rest ihFirst ihRest =>
    simpa [List.append_assoc] using Derives.expand production first ihRest

theorem emit {rules : Rules} {owner rhs word} (production : Production rules owner rhs)
    (value : Derives rules rhs word) : Derives rules [.nonterminal owner] word := by
  simpa using Derives.expand production value Derives.empty

theorem lowering_complete {rules : Rules} {expression word} (value : Eval rules expression word) :
    Derives rules [body expression] word := by
  induction value with
  | empty => exact emit Production.empty Derives.empty
  | terminal token => exact emit (Production.terminal token) (Derives.terminal token Derives.empty)
  | reference member value ih => exact emit (Production.reference _) (emit (Production.named member) ih)
  | sequence first second ihFirst ihSecond => exact emit (Production.sequence _ _) (derives_append ihFirst ihSecond)
  | left value ih => exact emit (Production.left _ _) ih
  | right value ih => exact emit (Production.right _ _) ih
  | optionalEmpty child => exact emit (Production.optionalEmpty child) Derives.empty
  | optionalSome value ih => exact emit (Production.optionalSome _) ih
  | repeatEmpty child => exact emit (Production.repeatEmpty child) Derives.empty
  | repeatSnoc earlier last ihPrefix ihLast => exact emit (Production.repeatSnoc _) (derives_append ihPrefix ihLast)
  | repeat1One value ih => exact emit (Production.repeat1One _) ih
  | repeat1Snoc earlier last ihPrefix ihLast => exact emit (Production.repeat1Snoc _) (derives_append ihPrefix ihLast)
  | annotation tag value ih => exact emit (Production.annotation tag _) ih

def ownerExpr : Owner → Expr
  | .named name => .reference name
  | .expression expression => expression

inductive Values (rules : Rules) : List Symbol → List Nat → Prop where
  | empty : Values rules [] []
  | terminal {tail word} (token : Nat) (rest : Values rules tail word) :
      Values rules (.terminal token :: tail) (token :: word)
  | nonterminal {owner tail a b} (first : Eval rules (ownerExpr owner) a)
      (rest : Values rules tail b) : Values rules (.nonterminal owner :: tail) (a ++ b)

theorem singleton_value {rules : Rules} {owner word} (value : Values rules [.nonterminal owner] word) :
    Eval rules (ownerExpr owner) word := by
  cases value with
  | nonterminal first rest => cases rest; simpa using first

theorem pair_values {rules : Rules} {left right word}
    (value : Values rules [.nonterminal left, .nonterminal right] word) :
    ∃ a b, word = a ++ b ∧ Eval rules (ownerExpr left) a ∧ Eval rules (ownerExpr right) b := by
  cases value with
  | nonterminal first rest => exact ⟨_, _, rfl, first, singleton_value rest⟩

theorem production_sound {rules : Rules} {owner rhs word}
    (production : Production rules owner rhs) (value : Values rules rhs word) :
    Eval rules (ownerExpr owner) word := by
  cases production with
  | named member => exact Eval.reference member (singleton_value value)
  | empty => cases value; exact Eval.empty
  | terminal token => cases value with | terminal token rest => cases rest; exact Eval.terminal token
  | reference name => exact singleton_value (owner := .named name) value
  | sequence left right =>
    obtain ⟨a, b, rfl, first, second⟩ := pair_values value
    exact Eval.sequence first second
  | left left right => exact Eval.left (singleton_value value)
  | right left right => exact Eval.right (singleton_value value)
  | optionalEmpty child => cases value; exact Eval.optionalEmpty child
  | optionalSome child => exact Eval.optionalSome (singleton_value value)
  | repeatEmpty child => cases value; exact Eval.repeatEmpty child
  | repeatSnoc child =>
    obtain ⟨a, b, rfl, earlier, last⟩ := pair_values value
    exact Eval.repeatSnoc earlier last
  | repeat1One child => exact Eval.repeat1One (singleton_value value)
  | repeat1Snoc child =>
    obtain ⟨a, b, rfl, earlier, last⟩ := pair_values value
    exact Eval.repeat1Snoc earlier last
  | annotation tag child => exact Eval.annotation tag (singleton_value value)

theorem lowering_sound_list {rules : Rules} {symbols word} (value : Derives rules symbols word) :
    Values rules symbols word := by
  induction value with
  | empty => exact Values.empty
  | terminal token rest ih => exact Values.terminal token ih
  | expand production first rest ihFirst ihRest =>
    exact Values.nonterminal (production_sound production ihFirst) ihRest

theorem lowering_exact (rules : Rules) (expression : Expr) (word : List Nat) :
    Derives rules [body expression] word ↔ Eval rules expression word :=
  ⟨fun value => singleton_value (lowering_sound_list value), lowering_complete⟩

theorem named_lowering_exact (rules : Rules) (name : Nat) (word : List Nat) :
    Derives rules [named name] word ↔ Eval rules (.reference name) word := by
  constructor
  · exact fun value => singleton_value (lowering_sound_list value)
  · intro value
    cases value with
    | reference member body => exact emit (Production.named member) (lowering_complete body)

def sequenceList : List Expr → Expr
  | [] => .empty
  | head :: tail => .sequence head (sequenceList tail)

def choiceList : List Expr → Expr
  | [] => .fail
  | head :: tail => .choice head (choiceList tail)

inductive SequenceEval (rules : Rules) : List Expr → List Nat → Prop where
  | empty : SequenceEval rules [] []
  | cons {head tail a b} (first : Eval rules head a) (rest : SequenceEval rules tail b) :
      SequenceEval rules (head :: tail) (a ++ b)

theorem sequence_list_exact (rules : Rules) (expressions : List Expr) (word : List Nat) :
    Eval rules (sequenceList expressions) word ↔ SequenceEval rules expressions word := by
  induction expressions generalizing word with
  | nil => constructor; intro value; cases value; exact SequenceEval.empty
           intro value; cases value; exact Eval.empty
  | cons head tail ih =>
    constructor
    · intro value
      cases value with
      | sequence first rest => exact SequenceEval.cons first ((ih _).mp rest)
    · intro value
      cases value with
      | cons first rest => exact Eval.sequence first ((ih _).mpr rest)

theorem choice_list_exact (rules : Rules) (expressions : List Expr) (word : List Nat) :
    Eval rules (choiceList expressions) word ↔ ∃ expression ∈ expressions, Eval rules expression word := by
  induction expressions with
  | nil => constructor; intro impossible; cases impossible; simp
  | cons head tail ih =>
    constructor
    · intro value
      cases value with
      | left value => exact ⟨head, by simp, value⟩
      | right value =>
        obtain ⟨expression, member, value⟩ := ih.mp value
        exact ⟨expression, by simp [member], value⟩
    · rintro ⟨expression, member, value⟩
      rcases List.mem_cons.mp member with rfl | member
      · exact Eval.left value
      · exact Eval.right (ih.mpr ⟨expression, member, value⟩)

/-- Folding an authored name is justified only by its unique defining body,
not by one alternative of that body. This is the native loop-isolation premise. -/
def UniqueBody (rules : Rules) (name : Nat) (expression : Expr) : Prop :=
  (name, expression) ∈ rules ∧ ∀ other, (name, other) ∈ rules → other = expression

theorem unique_body_value {rules : Rules} {name expression word}
    (unique : UniqueBody rules name expression) (value : Eval rules (.reference name) word) :
    Eval rules expression word := by
  cases value with
  | reference member body =>
    have equal := unique.2 _ member
    subst expression
    exact body

/-- Finite syntactic production expansions admit unit inlining and atomic
operand erasure. This premise is about emitted rules, not language equality. -/
inductive Expansion (rules : Rules) : List Symbol → List Symbol → Prop where
  | refl (symbols : List Symbol) : Expansion rules symbols symbols
  | foldNamed {name expression} (unique : UniqueBody rules name expression) :
      Expansion rules [body expression] [named name]
  | append {a b c d} (first : Expansion rules a b) (rest : Expansion rules c d) :
      Expansion rules (a ++ c) (b ++ d)
  | terminal {tail result} (token : Nat) (rest : Expansion rules tail result) :
      Expansion rules (.terminal token :: tail) (.terminal token :: result)
  | expand {owner rhs tail a b} (production : Production rules owner rhs)
      (first : Expansion rules rhs a) (rest : Expansion rules tail b) :
      Expansion rules (.nonterminal owner :: tail) (a ++ b)

theorem values_append_split {rules : Rules} {left right word} (value : Values rules (left ++ right) word) :
    ∃ a b, word = a ++ b ∧ Values rules left a ∧ Values rules right b := by
  induction left generalizing word with
  | nil => exact ⟨[], word, rfl, Values.empty, value⟩
  | cons symbol tail ih =>
    cases symbol with
    | terminal token =>
      cases value with
      | terminal token rest =>
        obtain ⟨a, b, rfl, first, second⟩ := ih rest
        exact ⟨token :: a, b, rfl, Values.terminal token first, second⟩
    | nonterminal owner =>
      cases value with
      | nonterminal first rest =>
        obtain ⟨a, b, rfl, middle, last⟩ := ih rest
        exact ⟨_ ++ a, b, by simp [List.append_assoc], Values.nonterminal first middle, last⟩

theorem values_append {rules : Rules} {left right a b}
    (first : Values rules left a) (second : Values rules right b) : Values rules (left ++ right) (a ++ b) := by
  induction first with
  | empty => exact second
  | terminal token rest ih => exact Values.terminal token ih
  | nonterminal first rest ih => simpa [List.append_assoc] using Values.nonterminal first ih

theorem expansion_sound {rules : Rules} {before after word} (expansion : Expansion rules before after)
    (value : Values rules after word) : Values rules before word := by
  induction expansion generalizing word with
  | refl => exact value
  | @foldNamed name expression unique =>
    have current := singleton_value (owner := .named name) value
    have result := unique_body_value unique current
    simpa [body, ownerExpr] using Values.nonterminal (owner := .expression expression) result Values.empty
  | append first rest ihFirst ihRest =>
    obtain ⟨a, b, rfl, left, right⟩ := values_append_split value
    exact values_append (ihFirst left) (ihRest right)
  | terminal token rest ih =>
    cases value with
    | terminal token value => exact Values.terminal token (ih value)
  | expand production first rest ihFirst ihRest =>
    obtain ⟨a, b, rfl, left, right⟩ := values_append_split value
    exact Values.nonterminal (production_sound production (ihFirst left)) (ihRest right)

/-- The production emitted for an isolated authored repetition is valid for
arbitrary child expressions, recursive source rules and finite word lengths.
A choice branch cannot supply this unique-body witness for its shared owner. -/
theorem isolated_repeat_origin (rules : Rules) (name : Nat) (child : Expr)
    (unique : UniqueBody rules name (.repeat child)) :
    Expansion rules [named name] [named name, body child] := by
  have recurrence : Expansion rules [body (.repeat child)] [named name, body child] := by
    have operands := Expansion.append (Expansion.foldNamed unique) (Expansion.refl [body child])
    simpa [body, named] using Expansion.expand (Production.repeatSnoc child) operands (Expansion.refl [])
  simpa [body, named] using Expansion.expand (Production.named unique.1) recurrence (Expansion.refl [])

theorem isolated_repeat1_origin (rules : Rules) (name : Nat) (child : Expr)
    (unique : UniqueBody rules name (.repeat1 child)) :
    Expansion rules [named name] [named name, body child] := by
  have recurrence : Expansion rules [body (.repeat1 child)] [named name, body child] := by
    have operands := Expansion.append (Expansion.foldNamed unique) (Expansion.refl [body child])
    simpa [body, named] using Expansion.expand (Production.repeat1Snoc child) operands (Expansion.refl [])
  simpa [body, named] using Expansion.expand (Production.named unique.1) recurrence (Expansion.refl [])

def nativeSymbol (decode : Nat → Owner) : Grammar.Symbol → Symbol
  | .terminal token => .terminal token
  | .nonterminal name => .nonterminal (decode name)

/-- All native derivations refine the source language if each emitted rule
has a finite syntactic expansion. Helper identities and inline choices are
represented by decode; no desired per-input language equivalence is assumed. -/
theorem native_derivation_sound (rules : Rules) (native : Grammar.CFG) (decode : Nat → Owner)
    (origins : ∀ rule ∈ native, Expansion rules [.nonterminal (decode rule.lhs)]
      (rule.rhs.map (nativeSymbol decode))) {symbols word}
    (value : Grammar.Derives native symbols word) : Values rules (symbols.map (nativeSymbol decode)) word := by
  induction value with
  | empty => exact Values.empty
  | terminal token rest ih => exact Values.terminal token ih
  | expand rule member first rest ihFirst ihRest =>
    have root := singleton_value (expansion_sound (origins rule member) ihFirst)
    exact Values.nonterminal root ihRest

theorem native_root_sound (rules : Rules) (native : Grammar.CFG) (decode : Nat → Owner)
    (origins : ∀ rule ∈ native, Expansion rules [.nonterminal (decode rule.lhs)]
      (rule.rhs.map (nativeSymbol decode))) (root source : Nat) (word : List Nat)
    (identity : decode root = .named source) (value : Grammar.Derives native [.nonterminal root] word) :
    Eval rules (.reference source) word := by
  have result := singleton_value (native_derivation_sound rules native decode origins value)
  simpa [identity, ownerExpr] using result

end ParserEngine.Lowering
