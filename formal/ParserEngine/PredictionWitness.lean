import ParserEngine.PredictionExtraction

/-! Construct replay witnesses from the actual FIFO transition protocol. The
recorder and interpreter share one step; no final semantic certificate or
caller-supplied dequeue sequence is needed to generate a completed witness. -/
namespace ParserEngine.PredictionWitness
open PredictionCatalog PredictionQueue PredictionTermination

structure Run (rows : List Row) (names : List Nat) (before : MState) where
  after : MState
  dequeues : List Nat
  checked : replay (parents rows names) before dequeues = some after
  finished : after.queue.work = []

def execute (rows : List Row) (names : List Nat) (bounded : Bounded rows names)
    (state : MState) (history : Trace (parents rows names) (seeds rows names) state) : Run rows names state :=
  match front : state.queue.work with
  | [] => ⟨state, [], rfl, front⟩
  | node :: tail =>
    let later := execute rows names bounded (step (parents rows names) state node tail) (Trace.next history front)
    ⟨later.after, node :: later.dequeues, by simp only [replay, front, ↓reduceIte]; exact later.checked, later.finished⟩
termination_by LookaheadExecution.Termination.rank state (domain rows names)
decreasing_by exact finite_rank rows names bounded state history node tail front

def generate (rows : List Row) (names : List Nat) (bounded : Bounded rows names) :
    Run rows names (initial (seeds rows names)) := execute rows names bounded _ Trace.start

theorem generated_exact {g rows names name} (bounded : Bounded rows names)
    (decoded : Decoded g rows names name) (node id : Nat) (selected : node ∈ names) :
    ((generate rows names bounded).after.queue.cells node).known.testBit id = true ↔
      ∃ rule, g[id]? = some rule ∧ LRStateSemantics.Prediction g (name node) rule :=
  checked_prediction bounded decoded node id selected _ _ (generate rows names bounded).checked
    (generate rows names bounded).finished

/-- Check observed native output by replaying its raw dequeue sequence and
comparing physical masks. No grammar-meaning assertion appears in the input. -/
def observe (parents : Parents) (seeds : Seeds) (dequeues : List Nat) (owners masks : List Nat) : Bool :=
  match replay parents (initial seeds) dequeues with
  | none => false
  | some after => after.queue.work.isEmpty &&
      (owners.map fun node => (after.queue.cells node).known) == masks

theorem observed_masks (parents : Parents) (seeds : Seeds) (dequeues owners masks : List Nat)
    (accepted : observe parents seeds dequeues owners masks = true) :
    ∃ after, replay parents (initial seeds) dequeues = some after ∧ after.queue.work = [] ∧
      masks = owners.map (fun node => (after.queue.cells node).known) := by
  unfold observe at accepted
  cases result : replay parents (initial seeds) dequeues with
  | none => simp [result] at accepted
  | some after =>
    simp only [result, Bool.and_eq_true, List.isEmpty_iff, beq_iff_eq] at accepted
    exact ⟨after, rfl, accepted.1, accepted.2.symm⟩

end ParserEngine.PredictionWitness
