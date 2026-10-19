import ParserEngine.SourceDeclarationPredicate
import ParserEngine.SourceDependencyAdapter
import ParserEngine.SourceDependencies

/-! A transform is an actual Scheme closure call with a threaded heap, not an
uninterpreted declaration value. Interface preservation and complete dependency
admission are distinct from semantic equivalence of instrumentation callbacks. -/
namespace ParserEngine.SourceDeclarationTransformation
open WalkSource SchemeTableEvaluation LeanPoo.Functional

/-- A successful guard keeps the name and complete parameter interface. -/
def interface : Datum → Option Datum
  | .list (.symbol "def" :: .list (.symbol owner :: parameters) :: _ :: _) =>
    some (.list (.symbol owner :: parameters))
  | _ => none

def accept (original transformed : Datum) : Bool :=
  match interface original, interface transformed with
  | some left, some right => left == right
  | _, _ => false

theorem accepted_interface (accepted : accept original transformed = true) :
    ∃ header, interface original = some header ∧ interface transformed = some header := by
  unfold accept at accepted
  split at accepted
  · rename_i left right a b
    have same : left = right := by simpa using accepted
    exact ⟨left, a, same ▸ b⟩
  · contradiction

/-- A header-changing transformation cannot turn a free name into a new
parameter and thereby hide that obligation from the dependency collector. -/
theorem changed_interface_rejects
    (left : interface original = some originalHeader)
    (right : interface transformed = some transformedHeader)
    (changed : originalHeader ≠ transformedHeader) : accept original transformed = false := by
  simp [accept, left, right, changed]

/-- If the guard snapshot aliases callback storage, a write destroys the
old guard value. This holds for any value and any original heap. -/
theorem aliased_snapshot_overwritten (heap : Heap) (address : Nat)
    (changed : SchemeTableEvaluation.Value) :
    write heap address 0 changed address 0 = some changed := by simp [write]

/-- Physical separation is the required host-copy contract. Threading a
callback write preserves every independently owned snapshot cell. -/
theorem distinct_snapshot_retained (heap : Heap) (snapshot callback : Nat)
    (changed : SchemeTableEvaluation.Value) (distinct : snapshot ≠ callback) :
    write heap callback 0 changed snapshot 0 = heap snapshot 0 := by simp [write, distinct]

/-- The literal call extracted from the real adapter is evaluated exactly once. -/
def invocation : Datum := app "transform" [s "declaration"]

def invocationFromSource : Datum :=
  match SourceDependencyWitness.transformSource with
  | .list [.symbol "def", _, _, .list [.symbol "let*", _,
      .list [.symbol "let", .list [_, call], _, _]]] => call
  | _ => s "unsupported"

theorem extracted_invocation : invocationFromSource = invocation := rfl

def originalGuard : Datum := app "unless" [app "and" [
  app "list?" [s "declaration"], app ">=" [app "length" [s "declaration"], n 3],
  app "eq?" [app "car" [s "declaration"], quoted "def"],
  app "pair?" [app "cadr" [s "declaration"]],
  app "symbol?" [app "caadr" [s "declaration"]],
  app "list?" [app "cdadr" [s "declaration"]]],
  app "error" [.text "source witness requires a procedure declaration"]]

def transformedGuard : Datum := app "unless" [app "and" [
  app "list?" [s "transformed"], app ">=" [app "length" [s "transformed"], n 3],
  app "eq?" [app "car" [s "transformed"], quoted "def"],
  app "equal?" [app "cadr" [s "transformed"], s "header"]],
  app "error" [.text "source transformation changed the declaration interface"]]

/-- Both guards surround the single invocation in the entire real procedure. -/
theorem extracted_transform : SourceDependencyWitness.transformSource =
    app "def" [l [s "transform-source-declaration", s "declaration", s "transform"],
      originalGuard, app "let*" [l [
        l [s "header", app "copy-source-data" [app "cadr" [s "declaration"]]],
        l [s "declaration", app "copy-source-data" [s "declaration"]]],
        app "let" [l [s "transformed", invocation], transformedGuard, s "transformed"]]] := rfl

/-- Project use of the installed host's object-graph codec is checked. Codec
round-trip and storage separation are host premises, not invented copy code. -/
theorem extracted_snapshot : SourceDependencyWitness.snapshotSource =
    app "def" [l [s "copy-source-data", s "value"],
      app "u8vector->object" [app "object->u8vector" [s "value"]]] := rfl

/-- Full closure bodies may mutate captured storage. Successful evaluation of
that body generates the one call; the resulting heap goes to later admission. -/
theorem invoke_closure (env captured entered : Environment) (before after : Heap)
    (parameters body : List Datum) (original transformed : Value)
    (transformRead : resolve env before "transform" = some (.closure parameters body captured))
    (sourceRead : resolve env before "declaration" = some original)
    (bound : bind parameters [original] captured = some entered)
    (execution : SchemeTableEvaluation.Sequence entered before body transformed after) :
    Eval env before invocation transformed after := by
  exact .call rfl (.variable transformRead) (.cons (.variable sourceRead) .nil)
    (.closure bound execution)

/-- Once retained, a closure result is unique even with effects. Re-evaluating
against the updated heap is not justified by this theorem. -/
theorem unique_execution (first : Eval env before invocation value after)
    (second : Eval env before invocation other last) : value = other ∧ after = last := by
  exact Eval.deterministic first second

/-- A successful project guard retains the result of the actual call. Syntax
conversion is the same literal representation used by the source evaluator. -/
structure GuardedCall (env : Environment) (before after : Heap)
    (original transformed : Datum) where
  originalValue : SchemeTableEvaluation.Value
  transformedValue : SchemeTableEvaluation.Value
  input : literal original = some originalValue
  output : literal transformed = some transformedValue
  sourceRead : resolve env before "declaration" = some originalValue
  execution : Eval env before invocation transformedValue after
  accepted : accept original transformed = true

/-- The effectful closure body and syntax conversion construct a guarded call;
there is no function from an assumed transform result to a pretend execution. -/
def checked_closure (env captured entered : Environment) (before after : Heap)
    (parameters body : List Datum) (original transformed : Datum)
    (transformRead : resolve env before "transform" = some (.closure parameters body captured))
    (sourceRead : resolve env before "declaration" = some (datumValue original))
    (bound : bind parameters [datumValue original] captured = some entered)
    (bodyExecution : SchemeTableEvaluation.Sequence entered before body (datumValue transformed) after)
    (accepted : accept original transformed = true) : GuardedCall env before after original transformed :=
  ⟨datumValue original, datumValue transformed, literal_total original, literal_total transformed, sourceRead,
    invoke_closure env captured entered before after parameters body (datumValue original) (datumValue transformed)
      transformRead sourceRead bound bodyExecution, accepted⟩

/-- Compose the entire read-selection and transformed-output admission path
with LeanPoo's typed factory preparation. The source is selected by the reader
trace, rather than being an admitted input declaration. Expansion of the retained
transformed value into tree remains an installed-SDK semantic premise. -/
theorem read_transform_prepare {Context : Type} {Value : Context → String → Type}
    (provider : Provider Context String Value) (name : String) (forms : List Datum)
    (original transformed : Datum) (selected : SourceDeclarationReading.readDeclaration name forms = some original)
    (env : Environment) (before after : Heap)
    (called : GuardedCall env before after original transformed)
    (expand : Datum → Option (SourceDependencies.Tree String)) (tree : SourceDependencies.Tree String)
    (expanded : expand transformed = some tree)
    (collected : SourceDependencies.dependencies tree = some names)
    {factories : Requirements.Factories Context Value names}
    (ready : Requirements.prepare provider names = .ok factories) :
    (original ∈ forms ∧ SourceDeclarationReading.selects name original = true) ∧
    (∃ header, interface original = some header ∧ interface transformed = some header) ∧
    Eval env before invocation called.transformedValue after ∧
    expand transformed = some tree ∧
    (∀ key, SourceDependencies.Requires key tree → ∃ factory, provider key = some factory) := by
  exact ⟨SourceDeclarationReading.selected_occurs selected, accepted_interface called.accepted, called.execution, expanded,
    fun _ required => SourceDependencies.prepared_covers_source provider tree collected ready required⟩

end ParserEngine.SourceDeclarationTransformation
