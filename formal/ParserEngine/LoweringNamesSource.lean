import ParserEngine.LoweringCallStack
import ParserEngine.LoweringTraversalData

/-! Independent complete schemas for allocation and helper call/return. These
admit the restricted parser protocol; they do not provide general Scheme or
compiler semantics and do not admit the remaining lower-to dispatch body. -/
namespace ParserEngine.LoweringNamesSource
open WalkSource NativeActions.Source CompilerSemanticsSource

def claim : Datum := defn "claim-name" ["name"] [
  app "and" [app "not" [app "table-ref" [s "used-names", s "name", b false]],
    app "begin" [app "table-set!" [s "used-names", s "name", b true], s "name"]]]

def fresh : Datum := defn "fresh" ["owner"] [
  app "let" [s "loop", l [], app "let" [binding "name"
    (app "string->symbol" [app "string-append" [.text "$", call "symbol->string" ["owner"],
      .text ".", call "number->string" ["next-synthetic"]]]),
    app "set!" [s "next-synthetic", app "+" [s "next-synthetic", n 1]],
    app "or" [call "claim-name" ["name"], app "loop" []]]]]

def symbol : Datum := defn "lower-symbol" ["owner", "expression", "precedence", "path"] [
  app "case" [app "car" [s "expression"],
    l [names ["literal", "layout-start", "layout-next"],
      app "list" [quoted "terminal", call "car" ["expression"], call "cadr" ["expression"]]],
    l [names ["token"], app "list" [quoted "terminal", quoted "token", call "cadr" ["expression"]]],
    l [names ["reference"], app "list" [quoted "nonterminal", call "cadr" ["expression"]]],
    l [s "else", app "let" [binding "name" (call "fresh" ["owner"]),
      app "when" [s "observe-proof", app "observe-proof" [quoted "binding", s "name", s "expression"]],
      app "lower-to" [s "name", s "owner", s "expression", s "precedence", s "path", b true,
        app "and" [s "observe-proof", app "lambda" [names ["proof"], s "proof"]],
        app "and" [s "observe-proof", app "identity" [app "list" [call "body" ["expression"]]]]],
      app "list" [quoted "nonterminal", s "name"]]]]]

def operandBranch (tag : String) : Datum :=
  l [names [tag], app "operand-add-action" [
    app "lower-operand" [s "owner", call "caddr" ["expression"], s "precedence", app "child-path" [s "path", n 0]],
    app "list" [quoted tag, call "cadr" ["expression"]]]]

def operand : Datum := defn "lower-operand" ["owner", "expression", "precedence", "path"] [
  app "case" [call "car" ["expression"], operandBranch "field", operandBranch "alias",
    l [s "else", call "lower-symbol" ["owner", "expression", "precedence", "path"]]]]

def admitted : Bool := decide (freshSource = fresh ∧ claimSource = claim ∧
  symbolSource = symbol ∧ operandSource = operand)

set_option maxRecDepth 8192 in
theorem source_checked : admitted = true := by
  simp only [admitted, decide_eq_true_eq]
  exact ⟨rfl, rfl, rfl, rfl⟩

theorem changed_claim_rejected (replacement : Datum) (changed : replacement ≠ claim) :
    decide (replacement = claim) = false := by simp [changed]

end ParserEngine.LoweringNamesSource
