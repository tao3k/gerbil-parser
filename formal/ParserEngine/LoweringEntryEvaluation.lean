import ParserEngine.LoweringMapEvaluation
import ParserEngine.LoweringRootScope

/-! Actual root-module lower-to invocation includes its local emit declaration.
The branch proof executes under that closure's scope, never a replacement
emitter or a caller-supplied whole-trace correspondence certificate. -/
namespace ParserEngine.LoweringEntryEvaluation
open WalkSource SchemeTableEvaluation LoweringDispatchSyntax NativeActions.Source
open CompilerSemanticsSource

def definition : Procedure :=
  ((LoweringClaimEvaluation.procedure LoweringBranchSource.lowerToSource).map Prod.snd).getD ⟨[], []⟩

def emitter : Procedure := ⟨[s "lhs", s "rhs", s "action", s "precedence", s "proof"],
  [app "emit-production" [s "lhs", s "rhs", s "action", s "precedence", s "owner", s "path", s "expression",
    LoweringBranchSource.proof (call "context" ["proof"])]]⟩

def entered (env : Environment) : Environment := tie [("emit", emitter)] env

set_option maxRecDepth 16384 in
theorem root_lookup : LoweringRootScope.group.lookup "lower-to" = some definition := rfl

set_option maxRecDepth 16384 in
theorem source_body : definition.body =
    [.list (.symbol "def" :: .list (.symbol "emit" :: emitter.parameters) :: emitter.body),
      caseSource LoweringBranchSource.lowerToSource] := rfl

theorem captures : entered env "emit" = some (.recursive [("emit", emitter)] "emit" env) := by
  simp [entered, tie]

/-- The caller's mutable cells are retained in the captured environment. -/
theorem captured_expression : entered env "expression" = env "expression" := rfl

/-- This executes the complete procedure-body envelope, including binding the
source emit closure before branch selection and branch execution. -/
theorem body_eval (encoding : Encoding) (expression : LoweringTraversal.Syntax)
    (read : resolve env before "expression" = some (datumValue (encode encoding expression)))
    (unshadowed : env "car" = none)
    (branch : SchemeTableEvaluation.Sequence (entered env) before (LoweringDispatch.lowerBranch expression) result after) :
    SchemeTableEvaluation.Sequence env before definition.body result after := by
  rw [source_body]
  apply SchemeTableEvaluation.Sequence.localProcedure
  apply SchemeTableEvaluation.Sequence.single
  apply LoweringDispatch.lower_eval encoding expression _ _ branch
  · change resolve env before "expression" = _
    exact read
  · change env "car" = none
    exact unshadowed

/-- Real root lookup, argument/default binding, local closure capture and case
execution compose at every call. Recursive branch derivations belong to the
parser simulation; they are not discharged by a host/compiler premise. -/
theorem root_invoke (encoding : Encoding) (expression : LoweringTraversal.Syntax)
    (bound : bind definition.parameters values (tie LoweringRootScope.group captured) = some env)
    (read : resolve env before "expression" = some (datumValue (encode encoding expression)))
    (unshadowed : env "car" = none)
    (branch : SchemeTableEvaluation.Sequence (entered env) before (LoweringDispatch.lowerBranch expression) result after) :
    Invoke before (.recursive LoweringRootScope.group "lower-to" captured) values result after :=
  .recursive root_lookup bound (body_eval encoding expression read unshadowed branch)

end ParserEngine.LoweringEntryEvaluation
