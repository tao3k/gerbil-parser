import ParserEngine.Completion
import ParserEngine.ConstructionEncoding
import ParserEngine.WalkSource
import Init.Data.List.Monadic

/-! Strict reader-data decoding. Fuel is derived from the complete datum node count;
malformed or truncated inputs return none rather than default productions. -/
namespace ParserEngine.GrammarWitnessDecoder
open WalkSource

deriving instance DecidableEq for Names.Recipe

def natural : Datum → Option Nat | .integer n => some n | _ => none
def boolean : Datum → Option Bool | .boolean b => some b | _ => none
def array (read : Datum → Option α) : Datum → Option (List α)
  | .list values => values.mapM read | _ => none

def expr : Nat → Datum → Option Lowering.Expr
  | 0, _ => none
  | fuel + 1, value => match value with
    | .list [.symbol "empty"] => some .empty
    | .list [.symbol "terminal", .integer n] => some (.terminal n)
    | .list [.symbol "reference", .integer n] => some (.reference n)
    | .list (.symbol "sequence" :: children) => (children.mapM (expr fuel)).map Lowering.sequenceList
    | .list (.symbol "choice" :: children) => (children.mapM (expr fuel)).map Lowering.choiceList
    | .list [.symbol "optional", child] => (expr fuel child).map Lowering.Expr.optional
    | .list [.symbol "repeat", child] => (expr fuel child).map Lowering.Expr.repeat
    | .list [.symbol "repeat1", child] => (expr fuel child).map Lowering.Expr.repeat1
    | _ => none

mutual
  def budget : Datum → Nat
    | .list values => budgets values + 1
    | _ => 1
  def budgets : List Datum → Nat
    | [] => 0
    | head :: tail => budget head + budgets tail
end

def expression (value : Datum) := expr (budget value) value

def owner : Datum → Option Lowering.Owner
  | .list [.symbol "named", .integer n] => some (.named n)
  | .list [.symbol "expression", value] => (expression value).map Lowering.Owner.expression
  | _ => none

def ownedSymbol : Datum → Option Lowering.Symbol
  | .list [.symbol "terminal", .integer n] => some (.terminal n)
  | .list [.symbol "nonterminal", value] => (owner value).map Lowering.Symbol.nonterminal
  | _ => none

def recipe : Nat → Datum → Option Names.Recipe
  | 0, _ => none
  | fuel + 1, value => match value with
    | .list [.symbol "identity", values] => (array ownedSymbol values).map Names.Recipe.identity
    | .list [.symbol "expand", key, children, rest] => do
      return .expand (← owner key) (← recipe fuel children) (← recipe fuel rest)
    | .list [.symbol "append", left, right] => do return .append (← recipe fuel left) (← recipe fuel right)
    | .list [.symbol "fold-named", .integer name, body] => (expression body).map (.foldNamed name)
    | _ => none

def recipeValue (value : Datum) := recipe (budget value) value

def rule : Datum → Option (Nat × Lowering.Expr)
  | .list [.integer name, value] => (expression value).map (name, ·)
  | _ => none

def grammarSymbol : Datum → Option Grammar.Symbol
  | .list [.symbol "terminal", .integer n] => some (.terminal n)
  | .list [.symbol "nonterminal", .integer n] => some (.nonterminal n)
  | _ => none

def production : Datum → Option Grammar.Production
  | .list [.integer lhs, rhs] => (array grammarSymbol rhs).map (⟨lhs, ·⟩)
  | _ => none

def pair : Datum → Option (Nat × Nat)
  | .list [.integer a, .integer b] => some (a, b) | _ => none

structure Completion where
  grammar : Grammar.CFG
  nullable : List Nat
  first : List (Nat × Nat)
  fuel : Nat
  deriving DecidableEq

def completion : Datum → Option Completion
  | .list [grammar, nullable, first, fuel] => do
    return ⟨← array production grammar, ← array natural nullable, ← array pair first, ← natural fuel⟩
  | _ => none

def operand : Datum → Option ProducerAdmission.Operand
  | .list [.symbol "eof"] => some ⟨.terminal .eof, []⟩
  | .list [.symbol "nonterminal", .integer n] => some ⟨.nonterminal n, []⟩
  | .list [.symbol "token", .integer n] => some ⟨.terminal (.token n), []⟩
  | .list [.symbol "literal", .integer n] => some ⟨.terminal (.literal n), []⟩
  | .list [.symbol "layout-start", .integer n] => some ⟨.terminal (.layoutStart n), []⟩
  | .list [.symbol "layout-next", .integer n] => some ⟨.terminal (.layoutNext n), []⟩
  | _ => none

def record : Datum → Option ConstructionGrammar.Record
  | .list [.integer id, .integer lhs, rhs] =>
    (array operand rhs).map fun rhs => ⟨id, ⟨lhs, rhs, .ordinary 0⟩⟩
  | _ => none

structure Origin where
  rules : Lowering.Rules
  bindings : List (Nat × Lowering.Expr)
  native : Grammar.CFG
  recipes : List Names.Recipe
  records : List ConstructionGrammar.Record
  augmented : Nat
  admitted : Bool
  projectable : Bool
  deriving DecidableEq

def origin : Datum → Option Origin
  | .list [rules, bindings, native, recipes, records, augmented, admitted, projectable] => do
    return ⟨← array rule rules, ← array rule bindings, ← array production native,
      ← array recipeValue recipes, ← array record records, ← natural augmented,
      ← boolean admitted, ← boolean projectable⟩
  | _ => none

def Origin.decode (value : Origin) (name : Nat) : Lowering.Owner :=
  match value.bindings.lookup name with | some body => .expression body | none => .named name

def Origin.input (value : Origin) : ConstructionGrammar.Input :=
  ⟨value.records, value.augmented, 0, 0, value.rules, value.decode, value.recipes⟩

/-- Whole-list decoding retains every source position; failures cannot be
silently dropped and duplicate records cannot collapse. -/
theorem array_positions (read : Datum → Option α) (raw : List Datum) (decoded : List α)
    (accepted : array read (.list raw) = some decoded) :
    raw.length = decoded.length ∧ ∀ (index : Nat) (value : Datum), raw[index]? = some value →
      ∃ result, decoded[index]? = some result ∧ read value = some result :=
  ConstructionEncoding.mapM_trace read raw decoded accepted

/-- Malformed entries reject the whole corpus, including a valid prefix.
No fallback grammar, unknown tag or exhausted recursive decoder is admitted. -/
theorem malformed_rejected :
    array completion (.list [.list [.list [], .list [], .list [], .integer 1], .symbol "invalid"]) = none ∧
    origin (.list []) = none ∧
    record (.list [.integer 0, .integer 0, .list [.list [.symbol "unknown", .integer 1]]]) = none ∧
    recipeValue (.list [.symbol "append", .list [.symbol "identity", .list []]]) = none ∧
    expr 0 (.list [.symbol "empty"]) = none := by decide

end ParserEngine.GrammarWitnessDecoder
