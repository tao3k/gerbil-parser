import ParserEngine.LoweringDispatchSyntax

/-! Complete symbolic branch selection for lower-to, lower-operand and
lower-symbol, composed with actual Scheme selector and chosen-body evaluation.
Branch-body simulation must still discharge recursive calls and native emit. -/
namespace ParserEngine.LoweringDispatch
open WalkSource SchemeTableEvaluation LoweringDispatchSyntax NativeActions.Source CompilerSemanticsSource
open LoweringBranchSource LoweringNamesSource

/-- Independent source schemas already used by the complete source gate. -/
def lowerBranch : LoweringTraversal.Syntax → List Datum
  | .precedence _ _ _ => body precedenceBranch
  | .choice _ => body choiceBranch
  | .sequence _ => body sequenceBranch
  | .optional _ => body optionalBranch
  | .repeat _ | .repeat1 _ => body repeatBranch
  | .field _ _ => body (annotationBranch "field")
  | .alias _ _ => body (annotationBranch "alias")
  | .empty => [emptyEmit]
  | .layoutEnd => [emit emptyList
      (app "if" [app "null?" [call "cdr" ["expression"]], quoted "layout-end", s "expression"])
      (stepProof (s "expression") identityEmpty)]
  | .terminal _ | .reference _ => [emit (app "list" [call "lower-symbol" ["owner", "expression", "precedence", "path"]])
      (quoted "pass") (proof operandProof)]

def helperBranch : List Datum := body ((clauses LoweringNamesSource.symbol).getLast?.getD (.boolean false))
def symbolBranch (encoding : Encoding) : LoweringTraversal.Syntax → List Datum
  | .terminal key => match encoding.terminalTag key with
    | .token => [app "list" [quoted "terminal", quoted "token", call "cadr" ["expression"]]]
    | _ => [app "list" [quoted "terminal", call "car" ["expression"], call "cadr" ["expression"]]]
  | .reference _ => [app "list" [quoted "nonterminal", call "cadr" ["expression"]]]
  | _ => helperBranch

def operandBranchBody : LoweringTraversal.Syntax → List Datum
  | .field _ _ => body (operandBranch "field")
  | .alias _ _ => body (operandBranch "alias")
  | _ => [call "lower-symbol" ["owner", "expression", "precedence", "path"]]

set_option maxRecDepth 16384 in
theorem lower_selected (encoding : Encoding) (expression : LoweringTraversal.Syntax) :
    selectCase (tag encoding expression) (clauses LoweringBranchSource.lowerToSource) = some (lowerBranch expression) := by
  cases expression <;> try rfl
  rename_i key
  cases kind : encoding.terminalTag key <;> simp only [tag, kind, TerminalTag.name] <;> rfl

set_option maxRecDepth 16384 in
theorem operand_selected (encoding : Encoding) (expression : LoweringTraversal.Syntax) :
    selectCase (tag encoding expression) (clauses LoweringNamesSource.operandSource) = some (operandBranchBody expression) := by
  cases expression <;> try rfl
  rename_i key
  cases kind : encoding.terminalTag key <;> simp only [tag, kind, TerminalTag.name] <;> rfl

set_option maxRecDepth 16384 in
theorem symbol_selected (encoding : Encoding) (expression : LoweringTraversal.Syntax) :
    selectCase (tag encoding expression) (clauses LoweringNamesSource.symbolSource) = some (symbolBranch encoding expression) := by
  cases expression <;> try rfl
  rename_i key
  cases kind : encoding.terminalTag key <;> simp only [tag, symbolBranch, kind, TerminalTag.name] <;> rfl

/-- Modular composition: the chosen branch consumes the selector's heap.
Unselected branches have no execution premise, including their callbacks. -/
theorem lower_eval (encoding : Encoding) (expression : LoweringTraversal.Syntax)
    (read : resolve env before "expression" = some (datumValue (encode encoding expression)))
    (unshadowed : env "car" = none)
    (branch : SchemeTableEvaluation.Sequence env before (lowerBranch expression) result after) :
    Eval env before (caseSource LoweringBranchSource.lowerToSource) result after := by
  exact .caseSymbol (selector_eval encoding expression read unshadowed) (lower_selected encoding expression) branch

theorem operand_eval (encoding : Encoding) (expression : LoweringTraversal.Syntax)
    (read : resolve env before "expression" = some (datumValue (encode encoding expression)))
    (unshadowed : env "car" = none)
    (branch : SchemeTableEvaluation.Sequence env before (operandBranchBody expression) result after) :
    Eval env before (caseSource LoweringNamesSource.operandSource) result after := by
  exact .caseSymbol (selector_eval encoding expression read unshadowed) (operand_selected encoding expression) branch

theorem symbol_eval (encoding : Encoding) (expression : LoweringTraversal.Syntax)
    (read : resolve env before "expression" = some (datumValue (encode encoding expression)))
    (unshadowed : env "car" = none)
    (branch : SchemeTableEvaluation.Sequence env before (symbolBranch encoding expression) result after) :
    Eval env before (caseSource LoweringNamesSource.symbolSource) result after := by
  exact .caseSymbol (selector_eval encoding expression read unshadowed) (symbol_selected encoding expression) branch

/-- All normal executions of the full source case agree with the constructed
selected-branch execution; this is stronger than source-envelope equality. -/
theorem lower_unique (encoding : Encoding) (expression : LoweringTraversal.Syntax)
    (read : resolve env before "expression" = some (datumValue (encode encoding expression)))
    (unshadowed : env "car" = none)
    (branch : SchemeTableEvaluation.Sequence env before (lowerBranch expression) result after)
    (actual : Eval env before (caseSource LoweringBranchSource.lowerToSource) other last) :
    other = result ∧ last = after :=
  actual.deterministic (lower_eval encoding expression read unshadowed branch)

end ParserEngine.LoweringDispatch
