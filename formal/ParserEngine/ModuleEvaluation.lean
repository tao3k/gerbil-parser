import ParserEngine.ModuleWrapper
import LeanPoo.Functional.CertifiedObservation

/-! The host supplies its actual finite force tree, including cache decisions,
phase selection and recursive source evaluation. Observation never derives an
execution order from an import directory. SDK promises, concurrent forcing,
compiler correctness and host cache semantics remain external premises. -/
namespace ParserEngine.ModuleEvaluation
open LeanPoo.Functional
universe u v w

/-- `force` denotes an actual SDK body invocation, not a declared dependency.
Cached requests have no new force node. Effects may precede imports. -/
inductive Program (Name : Type u) (Error : Type v) (Value : Type w) where
  | done (result : Except Error Value)
  | effect (name : Name) (next : Program Name Error Value)
  | force (name : Name) (body next : Program Name Error Value)
  | report (name : Name) (next : Program Name Error Value)

def run : Program Name Error Value → Except Error Value × List Name
  | .done result => (result, [])
  | .effect name next => let result := run next; (result.1, name :: result.2)
  | .report _ next => run next
  | .force _ body next =>
      let first := run body
      match first.1 with
      | .error error => (.error error, first.2)
      | .ok _ => let last := run next; (last.1, first.2 ++ last.2)

/-- Reporting occurs at the actual force site. A failing body skips the end
report and its continuation; no dependency is executed by installation. -/
def observe : Program Name Error Value → Program Name Error Value
  | .done result => .done result
  | .effect name next => .effect name (observe next)
  | .report name next => .report name (observe next)
  | .force name body next =>
      .report name (.force name (observe body) (.report name (observe next)))

/-- Arbitrary nested source calls retain all values, first failures and the
complete ordered effect trace, including effects before an import. -/
theorem observation_preserves (program : Program Name Error Value) :
    run (observe program) = run program := by
  induction program with
  | done result => rfl
  | effect name next ih => simp only [observe, run, ih]
  | report name next ih => simp only [observe, run, ih]
  | force name body next ihBody ihNext => simp only [observe, run, ihBody, ihNext]

/-- Eagerly admitting a directory entry violates first-error ordering. This
counterexample is independent of the SDK's particular module names. -/
theorem eager_import_counterexample (name : Name) (error : Error)
    (value : Value) :
    run (.force name (.done (.error error)) (.effect name (.done (.ok value)))) =
      (.error error, []) ∧
    run (.effect name (.force name (.done (.error error)) (.done (.ok value)))) =
      (.error error, [name]) := by
  simp [run]

/-- Reuse LeanPoo's joint observation contract across an arbitrary capability
family. Every prepared force tree, including duplicate keys, satisfies the
same value/failure/effect law; no second provider registry is introduced. -/
def expose (_ : Context) (_ : Key) (program : Program Name Error Value) := run (observe program)

theorem built_observation
    (factories : Requirements.Factories Context
      (fun _ (_ : Key) => Program Name Error Value) keys) (context : Context) :
    Requirements.build (Requirements.observe expose factories) context =
      Requirements.observeResults (fun _ _ program => run program)
        (Requirements.build factories context) := by
  rw [Requirements.build_observe]
  have same : (expose : ∀ (_ : Context) (_ : Key), Program Name Error Value → _) =
      (fun _ _ program => run program) := by
    funext context key program
    exact observation_preserves program
  rw [same]

/-- A missing native helper cannot be replaced by its undeclared source or
static owner. Reuse LeanPoo selection for any capability catalog. -/
theorem missing_helper_not_ready {Context : Type u} {Key : Type v} [DecidableEq Key]
    {Payload : Context → Key → Type w} (provider : Provider Context Key Payload)
    (keys : List Key) (helper : Key) (required : helper ∈ keys)
    (missing : provider helper = none)
    (factories : Requirements.Factories Context Payload keys) :
    Requirements.prepare provider keys ≠ .ok factories := by
  intro ready
  have selected := (Requirements.prepare_ok_iff _ _ _).mp ready
  have found := Requirements.factoryAt_selected provider keys factories selected helper required
  rw [missing] at found
  contradiction

/-- SDK cache states are supplied by the host contract, not an alternative
module registry in the parser. Only the SDK's actual request owns this update. -/
inductive Cache (Error : Type u) (Value : Type v) where
  | cold
  | ready (value : Value)
  | failed (error : Error)

def request : Cache Error Value → Except Error Value → Except Error Value × Cache Error Value
  | .cold, .ok value => (.ok value, .ready value)
  | .cold, .error error => (.error error, .failed error)
  | .ready value, _ => (.ok value, .ready value)
  | .failed error, _ => (.error error, .failed error)

/-- Returning a speculative child exception before requesting the parent loses
its failed state. Actual parent delegation preserves failure across child
recovery; the SDK's explicit parent reload is required for a new attempt. -/
theorem parent_failure_cache (error : Error) (recovered : Value) :
    request (Value := Value) .cold (.error error) ≠ ((.error error), .cold) ∧
    request (request .cold (.error error)).2 (.ok recovered) =
      ((.error error), .failed error) := by
  simp [request]

/-- Native bodies must finish in the original evaluator context before a
closed source wrapper replays their SDK requests. Nothing after the first
cached failure executes, so its tail need not be ready. -/
inductive SettledPrefix : List (Cache Error Value) → Prop where
  | nil : SettledPrefix []
  | ready (value : Value) : SettledPrefix rest → SettledPrefix (.ready value :: rest)
  | failed (error : Error) (rest : List (Cache Error Value)) : SettledPrefix (.failed error :: rest)

def replay (fresh : Nat → Except Error Value) : List (Cache Error Value) → Nat → Except Error (List Value)
  | [], _ => .ok []
  | state :: rest, index => do
      let value ← (request state (fresh index)).1
      let tail ← replay fresh rest (index + 1)
      pure (value :: tail)

/-- Arbitrary evaluator-dependent fresh outcomes cannot affect a settled
prefix. This retains values and the exact first failure, not just success. -/
theorem settled_replay_context {Error : Type u} {Value : Type v}
    {states : List (Cache Error Value)} (settled : SettledPrefix states)
    (original scheme : Nat → Except Error Value) (index : Nat) :
    replay original states index = replay scheme states index := by
  induction settled generalizing index with
  | nil => rfl
  | ready value _ ih => simp [replay, request, ih]
  | failed error rest => rfl

end ParserEngine.ModuleEvaluation
