import LeanPoo.Functional.Requirements
import ParserEngine.WalkSource

/-! Closed loader wrappers are an effect language, not arbitrary Scheme.
The SDK owns actual evaluation, module state, exceptions and initializer
memoization. Parsing Scheme data and recognizing SDK-generated declarations
are source extraction obligations, exercised separately by real file controls. -/
namespace ParserEngine.ModuleWrapper
open LeanPoo.Functional
universe u v

/-- Variadic begin is normalized to ordered binary composition; empty begin
and a recognized compiler declaration have no loader effects. Opaque code is
never interpreted by this dependency extractor. -/
inductive Form (Module : Type u) where
  | empty
  | load (name : Module)
  | seq (left right : Form Module)
  | quoted (data : Form Module)
  | deferred (body : Form Module)
  | unknown

def extract : Form Module → Option (List Module)
  | .empty => some []
  | .load name => some [name]
  | .seq left right => do
      let leading ← extract left
      let trailing ← extract right
      pure (leading ++ trailing)
  | .quoted _ | .deferred _ | .unknown => none

/-- Only closed terms have a claimed execution trace. In particular the model
makes no execution claim for an arbitrary opaque Scheme body. -/
inductive Executes : Form Module → List Module → Prop where
  | empty : Executes .empty []
  | load : Executes (.load name) [name]
  | seq : Executes left leading → Executes right trailing →
      Executes (.seq left right) (leading ++ trailing)

theorem extraction_sound (form : Form Module) (names : List Module)
    (accepted : extract form = some names) : Executes form names := by
  induction form generalizing names with
  | empty => cases accepted; exact .empty
  | load name => cases accepted; exact .load
  | seq left right ihLeft ihRight =>
    cases hLeft : extract left with
    | none => simp [extract, hLeft] at accepted
    | some leading =>
      cases hRight : extract right with
      | none => simp [extract, hLeft, hRight] at accepted
      | some trailing =>
        simp [extract, hLeft, hRight] at accepted
        subst names
        exact .seq (ihLeft _ hLeft) (ihRight _ hRight)
  | quoted _ _ => simp [extract] at accepted
  | deferred _ _ => simp [extract] at accepted
  | unknown => simp [extract] at accepted

theorem extraction_complete (execution : Executes form names) :
    extract form = some names := by
  induction execution with
  | empty => rfl
  | load => rfl
  | seq _ _ ihLeft ihRight => simp [extract, ihLeft, ihRight]

/-- Reuse the existing capability selection law. A successful source extraction
selects exactly the trace's factories, including order and first missing
provider; no independent module registry is introduced. -/
theorem selected_trace
    {Context : Type u} {Value : Context → Module → Type v}
    (provider : Provider Context Module Value)
    (extracted : extract form = some names)
    (execution : Executes form trace) :
    HEq (Requirements.prepare provider names) (Requirements.prepare provider trace) := by
  have same := (extraction_complete execution).symm.trans extracted
  cases Option.some.inj same
  rfl

/-- The old recursive data walk descends into quote and lambda alike. Its trace
is not an execution trace: data and deferred calls are not dependencies now. -/
def dataWalk : Form Module → List Module
  | .empty | .unknown => []
  | .load name => [name]
  | .seq left right => dataWalk left ++ dataWalk right
  | .quoted body | .deferred body => dataWalk body

theorem data_walk_counterexample (name : Module) :
    dataWalk (.quoted (.load name)) = [name] ∧
    extract (.quoted (.load name)) = none ∧
    dataWalk (.deferred (.load name)) = [name] ∧
    extract (.deferred (.load name)) = none := by
  simp [dataWalk, extract]

/-- An unsupported sibling rejects the entire preloading plan. Retaining an
accepted leading would run effects before an earlier opaque body can fail. -/
theorem opaque_rejects_whole_plan (leading trailing : Form Module) :
    extract (.seq leading (.seq .unknown trailing)) = none := by
  cases h : extract leading <;> simp [extract, h]

open WalkSource (Datum)
mutual
  /-- Reuse the existing reader datum representation. The only accepted
  statement constructors are exactly the generated SDK wrapper vocabulary. -/
  def rawExtract (native : Bool) : Datum → Option (List String)
    | .list [.symbol "load-module", .text name] => some [name]
    | .list (.symbol "begin" :: forms) => rawForms native forms
    | .list [.symbol "declare", .list [.symbol "block"],
        .list [.symbol "standard-bindings"], .list [.symbol "extended-bindings"]] => if native then some [] else none
    | _ => none
  termination_by value => sizeOf value
  def rawForms (native : Bool) : List Datum → Option (List String)
    | [] => some []
    | form :: rest => do
        let leading ← rawExtract native form
        let trailing ← rawForms native rest
        pure (leading ++ trailing)
  termination_by values => sizeOf values
end

def encode : Form String → Datum
  | .empty => .list [.symbol "begin"]
  | .load name => .list [.symbol "load-module", .text name]
  | .seq left right => .list [.symbol "begin", encode left, encode right]
  | .quoted body => .list [.symbol "quote", encode body]
  | .deferred body => .list [.symbol "lambda", .list [], encode body]
  | .unknown => .symbol "unsupported"

/-- Arbitrary recursive loader plans, quoted data and deferred bodies agree
with extraction from their reader-level Scheme encoding. This is not limited
to a finite source inventory. The SDK reader itself remains external. -/
theorem raw_encoding (native : Bool) (form : Form String) : rawExtract native (encode form) = extract form := by
  induction form with
  | empty => simp [rawExtract, rawForms, encode, extract]
  | load name => simp [rawExtract, encode, extract]
  | seq left right ihLeft ihRight =>
    cases hLeft : extract left <;> cases hRight : extract right <;>
      simp [encode, rawExtract, rawForms, extract, ihLeft, ihRight, hLeft, hRight]
  | quoted body _ => simp [rawExtract, encode, extract]
  | deferred body _ => simp [rawExtract, encode, extract]
  | unknown => simp [rawExtract, encode, extract]

theorem raw_execution (native : Bool) (execution : Executes form names) :
    rawExtract native (encode form) = some names := by
  rw [raw_encoding, extraction_complete execution]

/-- A native compiler declaration is not admitted as executable source.
The direct SDK probe rejects this form in the active Gerbil context. -/
theorem declaration_context :
    rawExtract false (.list [.symbol "declare", .list [.symbol "block"],
      .list [.symbol "standard-bindings"], .list [.symbol "extended-bindings"]]) = none ∧
    rawExtract true (.list [.symbol "declare", .list [.symbol "block"],
      .list [.symbol "standard-bindings"], .list [.symbol "extended-bindings"]]) = some [] := by
  simp [rawExtract]

end ParserEngine.ModuleWrapper
