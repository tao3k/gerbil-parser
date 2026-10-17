import ParserEngine.PredictionWitness
import ParserEngine.SourceData

/-! Admit automatically recorded raw native values. Finite receipts are checked
by the same replay protocol whose soundness is proved for arbitrary records. -/
namespace ParserEngine.PredictionObservation
open WalkSource

structure Record where
  parents : List (List Nat)
  own : List Nat
  dequeues : List Nat
  finalMasks : List Nat
  catalog : List Nat
  nodes : List (Option Nat)

def natural : Datum → Option Nat | .integer value => some value | _ => none
def naturals : Datum → Option (List Nat) | .list values => values.mapM natural | _ => none
def node : Datum → Option (Option Nat)
  | .boolean false => some none
  | .integer value => some (some value)
  | _ => none

def decode : Datum → Option Record
  | .list [.list parents, own, dequeues, finalMasks, catalog, .list nodes] => do
    return ⟨← parents.mapM naturals, ← naturals own, ← naturals dequeues,
      ← naturals finalMasks, ← naturals catalog, ← nodes.mapM node⟩
  | _ => none

def seeds (record : Record) : PredictionQueue.Seeds :=
  (List.range record.own.length).reverse.map fun owner => (owner, record.own[owner]!)
def parentRows (record : Record) : PredictionQueue.Parents := fun node => (record.parents[node]?).getD []
def physical (nodes : List (Option Nat)) (masks : List Nat) : List Nat :=
  nodes.map fun node => match node with | none => 0 | some owner => (masks[owner]?).getD 0

def check (record : Record) : Bool :=
  PredictionWitness.observe (parentRows record) (seeds record) record.dequeues
    (List.range record.own.length) record.finalMasks &&
      physical record.nodes record.finalMasks == record.catalog

def checkDatum (source : Datum) : Bool := match decode source with | none => false | some record => check record

def all (source : Datum) : Bool := match source with | .list records => records.all checkDatum | _ => false

/-- Native return values and the complete observed dequeue order refine the
same replay. Coordinates with #f publish zero, while owner ID zero is retained. -/
theorem checked_record (record : Record) (accepted : check record = true) :
    ∃ after, PredictionQueue.replay (parentRows record) (PredictionQueue.initial (seeds record)) record.dequeues = some after ∧
      after.queue.work = [] ∧ record.finalMasks = (List.range record.own.length).map (fun node => (after.queue.cells node).known) ∧
      record.catalog = physical record.nodes record.finalMasks := by
  simp only [check, Bool.and_eq_true, beq_iff_eq] at accepted
  obtain ⟨after, replay, finished, masks⟩ := PredictionWitness.observed_masks _ _ _ _ _ accepted.1
  exact ⟨after, replay, finished, masks, accepted.2.symm⟩

/-- Mutations are made to observed values, not to an expected success count. -/
def corruptTrace : Datum → Datum
  | .list [parents, own, .list (_ :: rest), masks, catalog, nodes] =>
      .list [parents, own, .list (.integer 999 :: rest), masks, catalog, nodes]
  | source => source

end ParserEngine.PredictionObservation
namespace ParserEngine.PredictionObservationWitness
set_option autoImplicit false

theorem native_checked : ParserEngine.PredictionObservation.all ParserEngine.PredictionObservationWitness.observations = true := by decide +kernel

theorem changed_dequeue_rejected :
    (match ParserEngine.PredictionObservationWitness.observations with | .list (first :: _) => ParserEngine.PredictionObservation.checkDatum (ParserEngine.PredictionObservation.corruptTrace first) | _ => true) = false := by decide +kernel
end ParserEngine.PredictionObservationWitness
