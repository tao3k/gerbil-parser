import ParserEngine.LoweringTraversal
import ParserEngine.LoweringTraversalData
import ParserEngine.GrammarWitnessDecoder

/-! Finite native controls complement the generic recursive producer theorem.
Reader data comes from actual lower-rules callbacks, including physical helper
bindings. Exact recipe and full ordered output equality detect truncation,
helper-order changes and erasure of precedence operand barriers. -/
namespace ParserEngine.LoweringTraversalWitness
open WalkSource GrammarWitnessDecoder LoweringTraversal

def decodeSyntax : Nat → Datum → Option LoweringTraversal.Syntax
  | 0, _ => none
  | fuel + 1, value => match value with
    | .list [.symbol "empty"] => some .empty
    | .list [.symbol "layout-end"] => some .layoutEnd
    | .list [.symbol "terminal", .integer key] => some (.terminal key)
    | .list [.symbol "reference", .integer name] => some (.reference name)
    | .list (.symbol "sequence" :: children) => (children.mapM (decodeSyntax fuel)).map .sequence
    | .list (.symbol "choice" :: children) => (children.mapM (decodeSyntax fuel)).map .choice
    | .list [.symbol "optional", child] => (decodeSyntax fuel child).map .optional
    | .list [.symbol "repeat", child] => (decodeSyntax fuel child).map .repeat
    | .list [.symbol "repeat1", child] => (decodeSyntax fuel child).map .repeat1
    | .list [.symbol "field", .integer label, child] => (decodeSyntax fuel child).map (.field label)
    | .list [.symbol "alias", .integer label, child] => (decodeSyntax fuel child).map (.alias label)
    | .list [.symbol "precedence", .integer direction, .integer rank, child] =>
        (decodeSyntax fuel child).map (.precedence direction rank)
    | _ => none

structure Fixture where
  rules : List (Nat × LoweringTraversal.Syntax)
  observed : Origin

def authored : Datum → Option (Nat × LoweringTraversal.Syntax)
  | .list [.integer name, expression] =>
      (decodeSyntax (budget expression) expression).map (name, ·)
  | _ => none

def fixture : Datum → Option Fixture
  | .list [rules, observed] => do
      return ⟨← array authored rules, ← origin observed⟩
  | _ => none

def fixtures : List Fixture := (array fixture corpus).getD []

def check (value : Fixture) : Bool :=
    let generated := declarations value.rules
    decide (value.observed.rules = normalized value.rules ∧
      value.observed.native.length = value.observed.recipes.length ∧
      generated.map (fun row => (row.owner, row.rhs, row.recipe)) =
        (value.observed.native.zip value.observed.recipes).map (fun (row, recipe) =>
          (value.observed.decode row.lhs,
            row.rhs.map (Lowering.nativeSymbol value.observed.decode), recipe)))

set_option maxRecDepth 16384
set_option maxHeartbeats 1600000

theorem decoded : (array fixture corpus).isSome = true := by decide
theorem cardinality : fixtures.length = 23 := by decide
theorem compared : fixtures.all check = true := by decide_cbv

theorem actual_trace (value : Fixture) (member : value ∈ fixtures) : check value = true :=
  List.all_eq_true.mp compared value member

/-- An operand precedence wrapper must allocate/emit its helper even when the
wrapped child is atomic. Transparent semantic projection alone loses this. -/
theorem precedence_barrier :
    (LoweringTraversal.operand [] (.precedence 0 0 (.terminal 1))).helpers.length = 1 ∧
    (LoweringTraversal.operand [] (.terminal 1)).helpers.length = 0 := by decide_cbv

theorem truncated_rejected : fixtures.all (fun value =>
    if value.observed.native.isEmpty then true else
      !check { value with observed := { value.observed with native := value.observed.native.tail } }) = true :=
  by decide_cbv

theorem malformed_rejected :
    (decodeSyntax 3 (.list [.symbol "sequence", .symbol "bad"])).isNone = true ∧
    (fixture (.list [])).isNone = true := by decide

end ParserEngine.LoweringTraversalWitness
