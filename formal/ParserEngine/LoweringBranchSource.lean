import ParserEngine.LoweringProgram
import ParserEngine.LoweringNamesSource

/-! Complete independent recursive dispatch and recipe schemas. Admission is
for this restricted normal-return lowering protocol. Host primitive semantics,
compiler execution, callback termination and AST actions remain separate. -/
namespace ParserEngine.LoweringBranchSource
open WalkSource NativeActions.Source CompilerSemanticsSource LoweringNamesSource

def proof (value : Datum) : Datum := app "and" [s "observe-proof", value]
def identityEmpty : Datum := app "identity" [emptyList]
def stepProof (expression children : Datum) : Datum := proof (app "step" [expression, children])
def operandProof : Datum := call "operand-proof" ["expression"]
def childOperandProof : Datum := app "operand-proof" [call "cadr" ["expression"]]
def childOperand : Datum := app "lower-operand" [s "owner", call "cadr" ["expression"], s "precedence",
  app "child-path" [s "path", n 0]]
def emit (rhs action recipe : Datum) : Datum := app "emit" [s "lhs", rhs, action, s "precedence", recipe]
def emptyEmit : Datum := emit emptyList (quoted "concat") (stepProof (s "expression") identityEmpty)
def oneEmit : Datum := emit (app "list" [s "child"]) (quoted "pass") (stepProof (s "expression") childOperandProof)
def repeatTest : Datum := app "eq?" [call "car" ["expression"], quoted "repeat"]

def precedenceBranch : Datum := l [names ["precedence"], app "let" [l [
  binding "direction" (call "cadr" ["expression"]), binding "rank" (call "caddr" ["expression"])],
  app "when" [app "and" [app "eq?" [s "direction", quoted "dynamic"], app "not" [s "allow-dynamic?"]],
    app "error" [.text "dynamic precedence requires selective GLR admission", s "owner", s "rank"]],
  app "lower-to" [s "lhs", s "owner", call "cadddr" ["expression"], call "list" ["direction", "rank"],
    app "child-path" [s "path", n 0], s "isolated?", s "context", s "recurrence"]]]

def choiceBranch : Datum := l [names ["choice"], app "if" [call "or" ["observe-origin", "observe-proof"],
  app "let" [s "loop", l [binding "children" (call "cdr" ["expression"]), binding "index" (n 0)],
    app "unless" [call "null?" ["children"],
      app "lower-to" [s "lhs", s "owner", call "car" ["children"], s "precedence",
        app "child-path" [s "path", s "index"], b false,
        proof (app "lambda" [names ["proof"], app "context" [
          app "choice-proof" [call "cdr" ["expression"], s "index", s "proof"]]]), b false],
      app "loop" [call "cdr" ["children"], app "+" [s "index", n 1]]]],
  app "for-each" [app "lambda" [names ["child"],
    app "lower-to" [s "lhs", s "owner", s "child", s "precedence", b false, b false]], call "cdr" ["expression"]]]]

def sequenceBranch : Datum := l [names ["sequence"], emit
  (app "map-children" [app "lambda" [names ["child", "path"],
    call "lower-operand" ["owner", "child", "precedence", "path"]], call "cdr" ["expression"], s "path"])
  (quoted "concat") (proof (app "sequence-proof" [call "cdr" ["expression"]]))]

def optionalBranch : Datum := l [names ["optional"], emptyEmit,
  emit (app "list" [childOperand]) (quoted "pass") (stepProof (s "expression") childOperandProof)]

def repeatBranch : Datum := l [names ["repeat", "repeat1"], app "if" [app "not" [s "isolated?"],
  emit (app "list" [call "lower-operand" ["owner", "expression", "precedence", "path"]]) (quoted "pass")
    (proof (app "identity" [app "list" [call "body" ["expression"]]])),
  app "begin" [app "when" [repeatTest, emptyEmit],
    app "let" [binding "child" childOperand, app "unless" [repeatTest, oneEmit],
      emit (app "list" [app "list" [quoted "nonterminal", s "lhs"], s "child"]) (quoted "concat")
        (stepProof (s "expression") (app "list" [quoted "append", s "recurrence", childOperandProof]))]]]]

def annotationBranch (tag : String) : Datum := l [names [tag],
  emit (app "list" [call "lower-operand" ["owner", "expression", "precedence", "path"]])
    (quoted "pass") (proof operandProof)]

def lowerTo : Datum := l [s "def", l [s "lower-to", s "lhs", s "owner", s "expression", s "precedence", s "path",
  l [s "isolated?", b true], l [s "context", b false], l [s "recurrence", b false]],
  defn "emit" ["lhs", "rhs", "action", "precedence", "proof"] [
    app "emit-production" [s "lhs", s "rhs", s "action", s "precedence", s "owner", s "path", s "expression",
      proof (call "context" ["proof"])]],
  app "case" [call "car" ["expression"], precedenceBranch, choiceBranch,
    l [names ["empty"], emptyEmit],
    l [names ["layout-end"], emit emptyList
      (app "if" [app "null?" [call "cdr" ["expression"]], quoted "layout-end", s "expression"])
      (stepProof (s "expression") identityEmpty)],
    sequenceBranch, optionalBranch, repeatBranch, annotationBranch "field", annotationBranch "alias",
    l [names ["literal", "layout-start", "layout-next", "token", "reference"],
      emit (app "list" [call "lower-symbol" ["owner", "expression", "precedence", "path"]]) (quoted "pass") (proof operandProof)],
    l [s "else", app "error" [.text "unsupported GrammarExpr in LR lowering", s "owner", s "expression"]]]]

def identityDefinition : Datum := defn "identity" ["symbols"] [app "list" [quoted "identity", s "symbols"]]
def bodyDefinition : Datum := defn "body" ["expression"] [app "list" [quoted "expression", s "expression"]]
def stepDefinition : Datum := defn "step" ["expression", "children"] [
  app "list" [quoted "expand", call "body" ["expression"], s "children", identityEmpty]]

def operandDefinition : Datum := defn "operand-proof" ["expression"] [
  app "case" [call "car" ["expression"],
    l [names ["field", "alias"], app "operand-proof" [call "caddr" ["expression"]]],
    l [names ["precedence"], app "identity" [app "list" [call "body" ["expression"]]]],
    l [names ["literal", "layout-start", "layout-next", "token"], app "step" [s "expression",
      app "identity" [app "list" [app "list" [quoted "terminal", call "car" ["expression"], call "cadr" ["expression"]]]]]],
    l [names ["reference"], app "step" [s "expression", app "identity" [app "list" [
      app "list" [quoted "named", call "cadr" ["expression"]]]]]],
    l [s "else", app "identity" [app "list" [call "body" ["expression"]]]]]]

def sequenceDefinition : Datum := defn "sequence-proof" ["children"] [app "if" [call "null?" ["children"],
  app "step" [app "quote" [names ["sequence"]], identityEmpty],
  app "step" [app "cons" [quoted "sequence", s "children"],
    app "list" [quoted "append", app "operand-proof" [call "car" ["children"]], app "sequence-proof" [call "cdr" ["children"]]]]]]

def choiceDefinition : Datum := defn "choice-proof" ["children", "index", "proof"] [
  app "step" [app "cons" [quoted "choice", s "children"],
    app "if" [call "zero?" ["index"], s "proof",
      app "choice-proof" [call "cdr" ["children"], app "-" [s "index", n 1], s "proof"]]]]

def childPath : Datum := defn "child-path" ["path", "index"] [
  app "and" [s "observe-origin", call "cons" ["index", "path"]]]
def mapChildren : Datum := defn "map-children" ["procedure", "children", "path"] [
  app "if" [s "observe-origin", app "let" [s "loop", l [binding "rest" (s "children"), binding "index" (n 0)],
    app "if" [call "null?" ["rest"], emptyList,
      app "cons" [app "procedure" [call "car" ["rest"], app "child-path" [s "path", s "index"]],
        app "loop" [call "cdr" ["rest"], app "+" [s "index", n 1]]]]],
    app "map" [app "lambda" [names ["child"], app "procedure" [s "child", b false]], s "children"]]]

def reserveReferences : Datum := defn "reserve-references" ["expression"] [
  app "when" [call "pair?" ["expression"],
    app "when" [app "eq?" [call "car" ["expression"], quoted "reference"],
      app "table-set!" [s "used-names", call "cadr" ["expression"], b true]],
    call "for-each" ["reserve-references", "expression"]]]

def reserveRules : Datum := app "for-each" [app "lambda" [names ["row"],
  app "table-set!" [s "used-names", call "car" ["row"], b true],
  app "table-set!" [s "rule-counts", call "car" ["row"],
    app "+" [n 1, app "table-ref" [s "rule-counts", call "car" ["row"], n 0]]],
  app "reserve-references" [call "cadr" ["row"]]], s "rules"]

def authoredCalls : Datum := app "for-each" [app "lambda" [names ["row"],
  app "lower-to" [call "car" ["row"], call "car" ["row"], call "cadr" ["row"], b false, emptyList,
    app "=" [app "table-ref" [s "rule-counts", call "car" ["row"]], n 1],
    proof (app "lambda" [names ["proof"], app "list" [quoted "expand",
      app "list" [quoted "named", call "car" ["row"]], s "proof", identityEmpty]]),
    proof (app "list" [quoted "fold-named", call "car" ["row"], call "cadr" ["row"]])]], s "rules"]

/-- Covers the whole lexical declaration, not just selected subdefinitions:
unknown new effects, changed reservation order and changed call arguments fail. -/
def root : Datum := l [s "def", l [s "lower-rules", s "rules", s "root", l [s "allow-dynamic?", b false],
  l [s "observe-origin", b false], l [s "observe-proof", b false], l [s "declare-name", b false]],
  app "let" [LoweringPublication.Source.initializer, LoweringPublication.Source.emitter,
    reserveReferences, claim, fresh, app "table-set!" [s "used-names", s "root", b true], reserveRules,
    childPath, mapChildren, identityDefinition, bodyDefinition, stepDefinition, operandDefinition,
    sequenceDefinition, choiceDefinition, symbol, operand, lowerTo, authoredCalls, LoweringPublication.Source.finalizer]]

def admitted : Bool := LoweringNamesSource.admitted && decide (lowerToSource = lowerTo ∧
  recipeSources = l [identityDefinition, bodyDefinition, stepDefinition, operandDefinition, sequenceDefinition, choiceDefinition] ∧
  pathSources = l [childPath, mapChildren] ∧ rootSource = root)

set_option maxRecDepth 16384 in
theorem source_checked : admitted = true := by
  rw [admitted, LoweringNamesSource.source_checked]
  simp only [Bool.true_and, decide_eq_true_eq]
  exact ⟨rfl, rfl, rfl, rfl⟩

end ParserEngine.LoweringBranchSource
