import ParserEngine.LRConstructionSource

/-! Source-checked entry points compose raw metadata extraction, actual FIFO
replay and semantic meaning. They do not assume final-mask correctness. -/
namespace ParserEngine.ConstructionAdmission
open WalkSource LRConstructionSource

def predictionReplay (source : Datum) (keys : List Nat) (index : Nat → Option Nat)
    (g : Grammar.CFG) (witness : List Nat) : Option PredictionQueue.MState :=
  if source = prediction then
    PredictionQueue.replay (PredictionCatalog.parents (PredictionExtraction.rows index g) (PredictionExtraction.names keys))
      (PredictionQueue.initial (PredictionCatalog.seeds (PredictionExtraction.rows index g) (PredictionExtraction.names keys))) witness
  else none

theorem prediction_exact (source : Datum) (keys : List Nat) (index : Nat → Option Nat)
    (valid : PredictionExtraction.NameIndex keys index) (g : Grammar.CFG)
    (covered : ∀ rule ∈ g, ∀ key, rule.rhs[0]? = some (.nonterminal key) → key ∈ keys)
    (node id : Nat) (selected : node ∈ PredictionExtraction.names keys)
    (witness : List Nat) (after : PredictionQueue.MState)
    (checked : predictionReplay source keys index g witness = some after) (finished : after.queue.work = []) :
    (after.queue.cells node).known.testBit id = true ↔
      ∃ rule, g[id]? = some rule ∧ LRStateSemantics.Prediction g (PredictionExtraction.name keys node) rule := by
  unfold predictionReplay at checked
  split at checked
  next matched =>
    exact PredictionCatalog.checked_prediction
      (PredictionExtraction.bounded keys index valid g) (PredictionExtraction.decoded keys index valid g covered)
      node id selected witness after checked finished
  next mismatch => cases checked

def stateIntern [DecidableEq Key] (growSource appendSource : Datum)
    (compute : Key → List Nat × Template) (before : StateInterning.Registry Key Template) (key : Key) :
    Option (StateInterning.Result Key Template) :=
  if growSource = grow ∧ appendSource = appendState then some (StateInterning.intern compute before key) else none

theorem state_preserves [DecidableEq Key] (growSource appendSource : Datum)
    (compute : Key → List Nat × Template) (before : StateInterning.Registry Key Template) (key : Key)
    (valid : StateInterning.Valid compute before) (after : StateInterning.Result Key Template)
    (checked : stateIntern growSource appendSource compute before key = some after) :
    StateInterning.Valid compute after.registry := by
  unfold stateIntern at checked
  split at checked
  next matched => cases checked; exact StateInterning.intern_valid compute before valid key
  next mismatch => cases checked

def nodeLookup (indexSource searchSource : Datum) (rows : NodeCoordinates.Rows)
    (base limit core : Nat) : Option Nat :=
  if indexSource = ProducerAdmission.Source.itemIndex ∧ searchSource = LookaheadExecution.Source.search then
    LookaheadExecution.Search.find (NodeCoordinates.readItems rows) core base limit
  else none

theorem coordinate_exact (indexSource searchSource : Datum)
    (before after : NodeCoordinates.Rows) (row : List Nat) (core node : Nat)
    (checked : nodeLookup indexSource searchSource (before ++ row :: after)
      (NodeCoordinates.width before) (NodeCoordinates.width before + row.length) core = some node) :
    (NodeCoordinates.coordinates 0 (before ++ row :: after))[node]? = some (before.length, core) := by
  unfold nodeLookup at checked
  split at checked
  next matched => exact NodeCoordinates.lookup_sound before after row core node checked
  next mismatch => cases checked

theorem changed_prediction_rejected (source : Datum) (keys : List Nat) (index : Nat → Option Nat)
    (g : Grammar.CFG) (witness : List Nat) (changed : source ≠ prediction) :
    predictionReplay source keys index g witness = none := by simp [predictionReplay, changed]

end ParserEngine.ConstructionAdmission
