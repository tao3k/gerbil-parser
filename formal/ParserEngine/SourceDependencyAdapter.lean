import ParserEngine.SourceDependencyWitness
import ParserEngine.SchemeEvaluationDeterminism

/-! Generic evaluation of the real adapters' lambda-data construction. The upstream reader and transform
protocols are composed by SourceDeclarationTransformation; no body length is sampled. -/
namespace ParserEngine.SourceDependencyAdapter
open WalkSource SchemeTableEvaluation

def sharedConstruction : Datum :=
  match SourceDependencyWitness.adapterSource with
  | .list [.symbol "def", _, .list [.symbol "let", _,
      .list [.symbol "source-expression-procedure", expression]]] => expression
  | _ => s "unsupported"

def eventConstruction : Datum :=
  match SourceDependencyWitness.eventAdapterSource with
  | .list [.symbol "def", _, .list [.symbol "source-expression-procedure", expression]] => expression
  | _ => s "unsupported"

def parameterExpression : Datum := app "cdr" [app "cadr" [s "declaration"]]
def bodyExpression : Datum := app "cddr" [s "declaration"]

theorem extracted_shared : sharedConstruction = app "cons" [quoted "lambda",
    app "cons" [app "append" [parameterExpression, s "dependencies"], bodyExpression]] := rfl

theorem extracted_event : eventConstruction = app "cons" [quoted "lambda",
    app "cons" [parameterExpression, bodyExpression]] := rfl

structure Primitives (env : Environment) : Prop where
  carTail : env "cdr" = none
  second : env "cadr" = none
  bodyTail : env "cddr" = none
  append : env "append" = none
  cons : env "cons" = none

/-- Header and body items are opaque Scheme values; every trailing item remains. -/
theorem pieces (env : Environment) (heap : Heap) (owner : String)
    (parameters body : List Value) (ready : Primitives env)
    (read : resolve env heap "declaration" = some (.list (.symbol "def" :: .list (.symbol owner :: parameters) :: body))) :
    Eval env heap parameterExpression (.list parameters) heap ∧
      Eval env heap bodyExpression (.list body) heap := by
  have declaration : Eval env heap (s "declaration")
      (.list (.symbol "def" :: .list (.symbol owner :: parameters) :: body)) heap := .variable read
  have header : Eval env heap (app "cadr" [s "declaration"])
      (.list (.symbol owner :: parameters)) heap :=
    .apply (.cons declaration .nil) rfl ready.second
  exact ⟨.apply (.cons header .nil) rfl ready.carTail,
    .apply (.cons declaration .nil) rfl ready.bodyTail⟩

theorem shared_keeps_whole_body (env : Environment) (heap : Heap) (owner : String)
    (parameters dependencies body : List Value) (ready : Primitives env)
    (read : resolve env heap "declaration" = some (.list (.symbol "def" :: .list (.symbol owner :: parameters) :: body)))
    (supplied : resolve env heap "dependencies" = some (.list dependencies)) :
    Eval env heap sharedConstruction
      (.list (.symbol "lambda" :: .list (parameters ++ dependencies) :: body)) heap := by
  obtain ⟨arguments, allBody⟩ := pieces env heap owner parameters body ready read
  have added : Eval env heap (app "append" [parameterExpression, s "dependencies"])
      (.list (parameters ++ dependencies)) heap :=
    .apply (.cons arguments (.cons (.variable supplied) .nil)) rfl ready.append
  have tail : Eval env heap (app "cons" [app "append" [parameterExpression, s "dependencies"], bodyExpression])
      (.list (.list (parameters ++ dependencies) :: body)) heap :=
    .apply (.cons added (.cons allBody .nil)) rfl ready.cons
  rw [extracted_shared]
  exact .apply (.cons .quoteSymbol (.cons tail .nil)) rfl ready.cons

theorem event_keeps_whole_body (env : Environment) (heap : Heap) (owner : String)
    (parameters body : List Value) (ready : Primitives env)
    (read : resolve env heap "declaration" = some (.list (.symbol "def" :: .list (.symbol owner :: parameters) :: body))) :
    Eval env heap eventConstruction
      (.list (.symbol "lambda" :: .list parameters :: body)) heap := by
  obtain ⟨arguments, allBody⟩ := pieces env heap owner parameters body ready read
  have tail : Eval env heap (app "cons" [parameterExpression, bodyExpression])
      (.list (.list parameters :: body)) heap :=
    .apply (.cons arguments (.cons allBody .nil)) rfl ready.cons
  rw [extracted_event]
  exact .apply (.cons .quoteSymbol (.cons tail .nil)) rfl ready.cons

/-- Model the actual project-owned declaration-to-lambda construction before
invoking the external compiler. The compiler never receives an opaque def. -/
def lambdaData (declaration : Datum) (dependencies : List String) : Datum :=
  match declaration with
  | .list (.symbol "def" :: .list (.symbol _ :: parameters) :: body) =>
    l (s "lambda" :: l (parameters ++ dependencies.map s) :: body)
  | _ => s "unsupported"

theorem datumValues_map (items : List Datum) : datumValues items = items.map datumValue := by
  induction items with
  | nil => rfl
  | cons first rest ih => simp [datumValues, ih]

/-- Arbitrary complete reader bodies, including keyword data, execute through
the existing extracted Scheme constructor. This is not a compiler premise. -/
theorem lambda_data_evaluates (env : Environment) (heap : Heap) (owner : String)
    (parameters body : List Datum) (dependencies : List String) (ready : Primitives env)
    (read : resolve env heap "declaration" =
      some (datumValue (l (s "def" :: l (s owner :: parameters) :: body))))
    (supplied : resolve env heap "dependencies" = some (.list (dependencies.map Value.symbol))) :
    Eval env heap sharedConstruction
      (datumValue (lambdaData (l (s "def" :: l (s owner :: parameters) :: body)) dependencies)) heap := by
  have execution := shared_keeps_whole_body env heap owner (datumValues parameters)
    (dependencies.map Value.symbol) (datumValues body) ready
    (by simpa [datumValue, datumValues] using read) supplied
  simpa [lambdaData, datumValue, datumValues, datumValues_map, List.map_append, List.map_map, Function.comp_def]
    using execution

/-- The old caddr projection gives the same first form for distinct tails;
therefore no proof over that projection can establish body preservation. -/
theorem first_body_projection_loses_tail (first trailing : Value) :
    ([first, trailing] : List Value).head? = [first].head? ∧
      ([first, trailing] : List Value) ≠ [first] := by simp

end ParserEngine.SourceDependencyAdapter
