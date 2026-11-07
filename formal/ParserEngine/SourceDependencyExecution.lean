import ParserEngine.SourceDependencies
import ParserEngine.SourceDependencyWitness

/-! Generic Scheme heap execution of the actual extracted recording closure.
The SDK supplies the callback sequence; no finite reference inventory is assumed. -/
namespace ParserEngine.SourceDependencyExecution
open WalkSource SchemeTableEvaluation

/-- Admit the complete producer scope and both the callback and readback. -/
def extractRecord : Datum → Option (Datum × Datum)
  | .list [.symbol "def", .list [.symbol "expanded-source-dependencies", .symbol "core"],
      .list [.symbol "let", .list [.symbol "references", .list [.symbol "quote", .list []]],
        .list [.symbol "collect-source-dependencies", .symbol "core", .keyword "record",
          .list [.symbol "lambda", .list [.symbol "name"], body]],
        readback]] => some (body, readback)
  | _ => none

def recordBody : Datum :=
  ((extractRecord SourceDependencyWitness.recordSource).getD (s "unsupported", s "unsupported")).1

def readbackBody : Datum :=
  ((extractRecord SourceDependencyWitness.recordSource).getD (s "unsupported", s "unsupported")).2

theorem extracted_record : recordBody = app "set!" [s "references", app "cons" [s "name", s "references"]] := rfl

theorem extracted_readback : readbackBody = app "reverse" [s "references"] := rfl

def collectorDeclarations : Datum → List Datum
  | .list forms => forms.filter (fun form => match form with
      | .list (.symbol "defcompile-method" :: _) => true
      | _ => false)
  | _ => []

/-- The project actually inherits the SDK's complete expression visitor and
registers both leaf overrides. This project-owned choice is not a host premise. -/
theorem extracted_collector : collectorDeclarations SourceDependencyWitness.moduleSource =
    [app "defcompile-method" [app "collect-source-dependencies" [.keyword "record", s "record"],
      l [s "::source-dependency-collector", s "::collect-expression-refs"], l [s "record"],
      .keyword "final", app "%#ref" [s "source-dependency-reference%"],
      app "%#set!" [s "source-dependency-assignment%"]]] := rfl

/-- The project verifies an actual host primitive slot. It neither accepts
all ## names nor treats an ambient global function as a primitive. -/
theorem extracted_resolution : SourceDependencyWitness.resolutionSource =
    app "def" [l [s "source-reference-resolved?", s "id"],
      app "or" [app "resolve-identifier" [s "id"],
        app "let" [l [s "name", app "stx-e" [s "id"]],
          app "and" [app "symbol?" [s "name"],
            app "string-prefix?" [.text "##", app "symbol->string" [s "name"]],
            app "procedure?" [app "##global-var-primitive-ref" [app "##make-global-var" [s "name"]]]]]]] := rfl

def literalGuard : Datum := app "unless" [
  app "and" [app "stx-pair?" [s "core"],
    app "memq" [app "stx-e" [app "stx-car" [s "core"]], app "quote" [l [s "%#lambda", s "%#case-lambda"]]]],
  app "error" [.text "source witness must expand to a procedure literal"]]

def missingGuard : Datum := app "unless" [app "null?" [s "missing"],
  app "error" [.text "source procedure has undeclared dependencies", s "missing"]]

/-- The concrete preparation inspects and evaluates the same immutable core,
with both rejection guards between expansion and evaluation. -/
theorem extracted_preparation : SourceDependencyWitness.prepareSource =
    app "def" [l [s "prepare-source-procedure", s "expression"],
      app "let*" [l [l [s "core", app "core-expand" [app "datum->syntax" [b false, s "expression"]]],
                    l [s "missing", app "expanded-source-dependencies" [s "core"]]],
        literalGuard, missingGuard,
        app "let" [l [s "procedure", app "eval-syntax*" [s "core"]],
          app "unless" [app "procedure?" [s "procedure"], app "error" [.text "source witness is not a procedure"]],
          s "procedure"]]] := rfl

def recordReference : Datum := app "unless" [
  app "source-reference-resolved?" [app "syntax" [s "id"]],
  l [app "@" [s "self", s "record"], app "stx-e" [app "syntax" [s "id"]]]]

def methodSource (name : String) (pattern body : List Datum) : Datum :=
  app "def" [l [s name, s "self", s "stx"], app "ast-case" [s "stx", l [], l (l pattern :: body)]]

/-- The concrete reference method records precisely unresolved identifiers. -/
theorem extracted_reference_method : SourceDependencyWitness.referenceSource =
    methodSource "source-dependency-reference%" [s "_", s "id"] [recordReference] := rfl

/-- The concrete assignment method records its target before traversing RHS
with the same SDK visitor, so the abstract assignment obligations apply. -/
theorem extracted_assignment_method : SourceDependencyWitness.assignmentSource =
    methodSource "source-dependency-assignment%" [s "_", s "id", s "expression"]
      [recordReference, app "compile-e" [s "self", app "syntax" [s "expression"]]] := rfl

/-- The callback reads the current cell and writes one cons, rather than using
an assumed pure copy of the recording algorithm. -/
theorem callback_step (env : Environment) (heap : Heap) (address : Nat)
    (name : String) (reversed : List String)
    (held : env "references" = some (.cell address)) (unshadowed : env "cons" = none)
    (stored : heap address 0 = some (.list (reversed.map Value.symbol))) :
    Eval (extend env "name" (.symbol name)) heap recordBody .unit
      (write heap address 0 (.list ((name :: reversed).map Value.symbol))) := by
  rw [extracted_record]
  have argument : Eval (extend env "name" (.symbol name)) heap (s "name") (.symbol name) heap := by
    exact .variable (by simp [resolve, extend])
  have captured : Eval (extend env "name" (.symbol name)) heap (s "references")
      (.list (reversed.map Value.symbol)) heap := by
    apply Eval.variable
    simpa only [resolve, extend, ite_eq_right (by decide : "references" ≠ "name"), held] using stored
  exact .assignment (by simpa [extend] using held)
    (.apply (.cons argument (.cons captured .nil)) rfl (by simpa [extend] using unshadowed))

/-- The SDK invokes the retained lexical closure, not a copied callback body. -/
theorem callback_invocation (env : Environment) (heap : Heap) (address : Nat)
    (name : String) (reversed : List String)
    (held : env "references" = some (.cell address)) (unshadowed : env "cons" = none)
    (stored : heap address 0 = some (.list (reversed.map Value.symbol))) :
    Invoke heap (.closure [s "name"] [recordBody] env) [.symbol name] .unit
      (write heap address 0 (.list ((name :: reversed).map Value.symbol))) :=
  .closure rfl (.single (callback_step env heap address name reversed held unshadowed stored))

/-- Foreign visitation ignores callback return values but preserves invocation
order and the shared captured heap. -/
inductive CallbackTrace (procedure : Value) : Heap → List String → Heap → Prop where
  | nil : CallbackTrace procedure heap [] heap
  | cons : Invoke before procedure [.symbol name] value middle →
      CallbackTrace procedure middle names after → CallbackTrace procedure before (name :: names) after

theorem each_is_closure_calls (env : Environment) (names : List String)
    (executed : Each env "name" [recordBody] before (names.map Value.symbol) after) :
    CallbackTrace (.closure [s "name"] [recordBody] env) before names after := by
  induction names generalizing before with
  | nil => cases executed; exact .nil
  | cons head rest ih =>
    cases executed with
    | cons body tail => exact .cons (.closure rfl body) (ih tail)

/-- Arbitrary many SDK callback invocations execute the extracted body in the
existing Scheme semantics, and preserve exactly the cons-accumulator contents. -/
theorem callback_sequence (env : Environment) (heap : Heap) (address : Nat)
    (names reversed : List String)
    (held : env "references" = some (.cell address)) (unshadowed : env "cons" = none)
    (stored : heap address 0 = some (.list (reversed.map Value.symbol))) :
    ∃ after, Each env "name" [recordBody] heap (names.map Value.symbol) after ∧
      after address 0 = some (.list ((SourceDependencies.record names reversed).map Value.symbol)) := by
  induction names generalizing heap reversed with
  | nil => exact ⟨heap, .nil, stored⟩
  | cons head rest ih =>
    let middle := write heap address 0 (.list ((head :: reversed).map Value.symbol))
    have updated : middle address 0 = some (.list ((head :: reversed).map Value.symbol)) := by
      simp [middle, write]
    obtain ⟨after, remaining, contents⟩ := ih middle (head :: reversed) updated
    refine ⟨after, .cons (.single (callback_step env heap address head reversed held unshadowed stored)) remaining, ?_⟩
    simpa [SourceDependencies.record, List.foldl_cons] using contents

/-- The real final reverse restores the compiler's complete visit order.
Initial accumulator contents are retained as the corresponding prefix. -/
theorem complete_readback (env : Environment) (heap : Heap) (address : Nat)
    (names reversed : List String)
    (held : env "references" = some (.cell address))
    (consPrimitive : env "cons" = none) (reversePrimitive : env "reverse" = none)
    (stored : heap address 0 = some (.list (reversed.map Value.symbol))) :
    ∃ after, Each env "name" [recordBody] heap (names.map Value.symbol) after ∧
      Eval env after readbackBody (.list ((reversed.reverse ++ names).map Value.symbol)) after := by
  obtain ⟨after, callbacks, contents⟩ := callback_sequence env heap address names reversed held consPrimitive stored
  refine ⟨after, callbacks, ?_⟩
  rw [extracted_readback]
  have captured : Eval env after (s "references")
      (.list ((SourceDependencies.record names reversed).map Value.symbol)) after :=
    .variable (by simpa only [resolve, held] using contents)
  exact .apply (.cons captured .nil)
    (by simp only [primitive, ← List.map_reverse, SourceDependencies.record_exact]) reversePrimitive

/-- Complete native callback invocation trace followed by the actual readback. -/
theorem closure_complete_readback (env : Environment) (heap : Heap) (address : Nat)
    (names reversed : List String)
    (held : env "references" = some (.cell address))
    (consPrimitive : env "cons" = none) (reversePrimitive : env "reverse" = none)
    (stored : heap address 0 = some (.list (reversed.map Value.symbol))) :
    ∃ after, CallbackTrace (.closure [s "name"] [recordBody] env) heap names after ∧
      Eval env after readbackBody (.list ((reversed.reverse ++ names).map Value.symbol)) after := by
  obtain ⟨after, callbacks, readback⟩ := complete_readback env heap address names reversed held consPrimitive reversePrimitive stored
  exact ⟨after, each_is_closure_calls env names callbacks, readback⟩

end ParserEngine.SourceDependencyExecution
