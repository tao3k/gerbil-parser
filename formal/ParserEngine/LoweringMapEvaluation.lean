import ParserEngine.LoweringDispatch

/-! Source map-children traverses its actual operand list with one shared heap.
Ordered map callbacks are an explicit installed-host contract, as are the
existing argument-evaluation and primitive contracts. No SDK is reimplemented. -/
namespace ParserEngine.LoweringMapEvaluation
open WalkSource SchemeTableEvaluation NativeActions.Source CompilerSemanticsSource

/-- Results and effects retain source order across every finite partition. -/
theorem append
    (first : Mapping env name expressions before values results middle)
    (last : Mapping env name expressions middle tail outputs after) :
    Mapping env name expressions before (values ++ tail) (results ++ outputs) after := by
  induction values generalizing before middle results with
  | nil => cases first; exact last
  | cons value values ih =>
      cases first with
      | cons head rest => exact .cons head (ih rest last)

theorem length (run : Mapping env name expressions before values results after) :
    results.length = values.length := by
  induction values generalizing before results with
  | nil => cases run; rfl
  | cons value values ih =>
      cases run with
      | cons head tail => exact congrArg Nat.succ (ih tail)

theorem deterministic
    (first : Mapping env name expressions before values results after)
    (second : Mapping env name expressions before values others last) : results = others ∧ after = last := by
  induction values generalizing before results after others last with
  | nil => cases first; cases second; exact ⟨rfl, rfl⟩
  | cons value values ih =>
    cases first with
    | cons head tail =>
      cases second with
      | cons other rest =>
        obtain ⟨rfl, rfl⟩ := (Eval.begin head).deterministic (.begin other)
        obtain ⟨rfl, rfl⟩ := ih tail rest
        exact ⟨rfl, rfl⟩

def callback : Datum := app "procedure" [s "child", b false]
def fastBody : Datum := app "map" [app "lambda" [names ["child"], callback], s "children"]

def sourceBody : Datum :=
  match LoweringBranchSource.pathSources with
  | .list [_, .list [_, _, expression]] => expression
  | _ => b false

set_option maxRecDepth 16384 in
theorem source_body : sourceBody = app "if" [s "observe-origin",
    app "let" [s "loop", l [binding "rest" (s "children"), binding "index" (n 0)],
      app "if" [call "null?" ["rest"], emptyList,
        app "cons" [app "procedure" [call "car" ["rest"], app "child-path" [s "path", s "index"]],
          app "loop" [call "cdr" ["rest"], app "+" [s "index", n 1]]]]], fastBody] := rfl

/-- Actual source execution of the unobserved map path. The recursive operand
simulation supplies per-child Sequence witnesses, not a final trace equality. -/
theorem fast_eval (origin : resolve env before "observe-origin" = some (.boolean false))
    (children : resolve env before "children" = some (.list values)) (unshadowed : env "map" = none)
    (mapping : Mapping env "child" [callback] before values results after) :
    Eval env before sourceBody (.list results) after := by
  rw [source_body]
  apply Eval.conditionalFalse (.variable origin) rfl
  exact .mapLambda (.variable children) mapping unshadowed

/-- Combine independently derived recursive operand prefixes and suffixes;
source execution receives the original complete list in one map invocation. -/
theorem complete_list
    (origin : resolve env before "observe-origin" = some (.boolean false))
    (children : resolve env before "children" = some (.list (values ++ tail))) (unshadowed : env "map" = none)
    (first : Mapping env "child" [callback] before values results middle)
    (last : Mapping env "child" [callback] middle tail outputs after) :
    Eval env before sourceBody (.list (results ++ outputs)) after ∧
      (results ++ outputs).length = (values ++ tail).length := by
  have combined := append first last
  exact ⟨fast_eval origin children unshadowed combined, length combined⟩

end ParserEngine.LoweringMapEvaluation
