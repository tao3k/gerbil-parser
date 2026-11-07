import ParserEngine.SchemeEvaluationDeterminism
import LeanPoo.Functional.Access

/-! Source extraction must retain lexical dependencies as well as syntax.
Factories read mutable cells at invocation, never at preparation. The actual
Gerbil binding extractor and host primitive interpretation remain premises. -/
namespace ParserEngine.SourceEnvironment
open LeanPoo.Functional SchemeTableEvaluation
variable {name : String} {heap : Heap} {value : Value} {address : Nat}

/-- Only explicit lexical dependencies are admitted here. Unshadowed host
primitives are covered by the separate Scheme evaluation rules. -/
def provider (env : Environment) : Provider Heap String (fun _ _ => Option Value) :=
  fun name => (env name).map (fun _ heap => resolve env heap name)

/-- A prepared lexical factory preserves the actual Scheme resolve operation
at every heap, including missing cell contents and updates after preparation. -/
theorem selected_resolves (env : Environment) (keys : List String)
    {factories : Requirements.Factories Heap (fun _ (_ : String) => Option Value) keys}
    (ready : Requirements.prepare (provider env) keys = .ok factories)
    (member : name ∈ keys) (heap : Heap) :
    Requirements.factoryAt factories name member heap = resolve env heap name := by
  have selected := Requirements.factoryAt_selected (provider env) keys factories
    ((Requirements.prepare_ok_iff _ _ _).mp ready) name member
  unfold provider at selected
  cases found : env name with
  | none => simp [found] at selected
  | some value =>
    simp only [found, Option.map_some, Option.some.injEq] at selected
    exact congrFun selected.symm heap

/-- Missing dependencies cannot be hidden by an otherwise successful tuple,
regardless of caller order, duplicates, or unrelated ambient bindings. -/
theorem missing_rejects (env : Environment) (keys : List String)
    {factories : Requirements.Factories Heap (fun _ (_ : String) => Option Value) keys}
    (absent : env name = none) (member : name ∈ keys) :
    Requirements.prepare (provider env) keys ≠ .ok factories := by
  intro ready
  have selected := Requirements.factoryAt_selected (provider env) keys factories
    ((Requirements.prepare_ok_iff _ _ _).mp ready) name member
  simp [provider, absent] at selected

/-- Compose LeanPoo's retained capability with the existing source evaluator,
not a second evaluator for variables. -/
theorem variable_evaluates (env : Environment) (keys : List String)
    {factories : Requirements.Factories Heap (fun _ (_ : String) => Option Value) keys}
    (ready : Requirements.prepare (provider env) keys = .ok factories)
    (member : name ∈ keys)
    (read : Requirements.factoryAt factories name member heap = some value) :
    Eval env heap (.symbol name) value heap := by
  exact .variable ((selected_resolves env keys ready member heap).symm.trans read)

/-- Preparation of a cell binding is insufficient to prove a runtime read.
This is a genuine extra precondition of extracted closure execution. -/
theorem bound_cell_can_be_uninitialized (env : Environment)
    (bound : env name = some (.cell address)) (empty : heap address 0 = none) :
    provider env name = some (fun heap => resolve env heap name) ∧
      resolve env heap name = none := by
  constructor
  · simp only [provider, bound, Option.map_some]
  · simpa only [resolve, bound] using empty

/-- A retained factory observes later writes to the same physical cell. No
source/metadata preparation cache may freeze its earlier value. -/
theorem retained_cell_reads_write (env : Environment) (keys : List String)
    {factories : Requirements.Factories Heap (fun _ (_ : String) => Option Value) keys}
    (ready : Requirements.prepare (provider env) keys = .ok factories)
    (member : name ∈ keys) (bound : env name = some (.cell address)) :
    Requirements.factoryAt factories name member (write heap address 0 value) = some value := by
  rw [selected_resolves env keys ready member]
  simp [resolve, bound, write]

end ParserEngine.SourceEnvironment
