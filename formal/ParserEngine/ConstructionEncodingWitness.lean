import ParserEngine.ConstructionEncoding
import ParserEngine.NativeFacts
import ParserEngine.RawGrammarData

/-! Raw data are emitted by the shared Scheme source-data declaration macro.
This finite source check supplements the generic encoding theorems; it does
not synthesize proofs in the producer or assume native IDs are correct. -/
namespace ParserEngine.ConstructionEncodingWitness
open ConstructionEncoding

def expected : List ConstructionGrammar.Input := NativeFacts.origins.map GrammarWitnessDecoder.Origin.input

def fields (input : ConstructionGrammar.Input) :=
  (input.records, input.augmented, input.root, input.source, input.declarations)

/-- The real witness compares declaration order and multiplicity as well as
rows and roots; a declaration mismatch cannot hide in ignored metadata. -/
theorem fields_declarations (left right : ConstructionGrammar.Input)
    (same : fields left = fields right) : left.declarations = right.declarations :=
  congrArg (fun value => value.2.2.2.2) same

def frameCheck (frame : WalkSource.Datum) (target : ConstructionGrammar.Input) : Bool :=
  match frame, ConstructionEncoding.read frame target with
  | .list [_, _, _, _, _, .boolean valid], some raw =>
    let observed := (normalize raw).map fields
    if valid then decide (observed = some (fields target)) else observed.isNone
  | _, _ => false

theorem declaration_mismatch_rejected (names rows augmented root declarations : WalkSource.Datum)
    (target normalized : ConstructionGrammar.Input) (raw : Input)
    (parsed : ConstructionEncoding.read
      (.list [names, rows, augmented, root, declarations, .boolean true]) target = some raw)
    (converted : normalize raw = some normalized)
    (different : normalized.declarations ≠ target.declarations) :
    frameCheck (.list [names, rows, augmented, root, declarations, .boolean true]) target = false := by
  have unequal : fields normalized ≠ fields target := fun same => different (fields_declarations _ _ same)
  simp [frameCheck, parsed, converted, unequal]

def check (frames : List WalkSource.Datum) (targets : List ConstructionGrammar.Input) : Bool :=
  decide (frames.length = targets.length) && (frames.zip targets).all fun pair => frameCheck pair.1 pair.2

def frames : List WalkSource.Datum := match RawGrammarData.corpus with | .list values => values | _ => []

set_option maxRecDepth 8192 in
set_option maxHeartbeats 800000 in
theorem source_checked : check frames expected = true := by decide

theorem no_truncation (frames : List WalkSource.Datum) (targets : List ConstructionGrammar.Input)
    (accepted : check frames targets = true) : frames.length = targets.length := by
  simp only [check, Bool.and_eq_true, decide_eq_true_eq] at accepted
  exact accepted.1

end ParserEngine.ConstructionEncodingWitness
