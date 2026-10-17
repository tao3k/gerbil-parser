import ParserEngine.PredictionCatalog

/-! The finite owner/production domain is derived from the raw catalogue;
termination does not require a caller-supplied bound for every publication. -/
namespace ParserEngine.PredictionTermination
open PredictionCatalog PredictionQueue LookaheadExecution.Termination

def domain (rows : List Row) (names : List Nat) : List (Nat × Nat) :=
  names.flatMap fun node => (List.range rows.length).map fun id => (node, id)

theorem predicted_id_bound {rows : List Row} {node id : Nat} (predicted : Predict rows node id) :
    id < rows.length := by
  induction predicted with
  | @own id node row read owner =>
    by_cases bound : id < rows.length
    · exact bound
    · simp [List.getElem?_eq_none (by omega : rows.length ≤ id)] at read
  | via read owner first known ih => exact ih

theorem finite_rank (rows : List Row) (names : List Nat) (bounded : Bounded rows names)
    (state : MState) (history : Trace (parents rows names) (seeds rows names) state)
    (node : Nat) (tail : List Nat) (front : state.queue.work = node :: tail) :
    rank (step (parents rows names) state node tail) (domain rows names) < rank state (domain rows names) := by
  apply step_rank _ state node tail front (domain rows names)
  intro target edge bit pending
  have valid := trace_valid history
  have reached := valid.basic.sound node bit (valid.basic.pending node bit pending)
  have predicted := (reach_exact rows names bounded node bit).mp reached
  apply List.mem_flatMap.mpr
  refine ⟨target, ((parents_member rows names node target).mp edge).1, ?_⟩
  exact List.mem_map.mpr ⟨bit, List.mem_range.mpr (predicted_id_bound predicted), rfl⟩

/-- Every source-shaped prediction history has a finite exhausted extension.
This is model termination, with host execution still an external premise. -/
theorem finishes (rows : List Row) (names : List Nat) (bounded : Bounded rows names)
    (state : MState) (history : Trace (parents rows names) (seeds rows names) state) :
    ∃ after, Trace (parents rows names) (seeds rows names) after ∧ after.queue.work = [] := by
  generalize size : rank state (domain rows names) = fuel
  induction fuel using Nat.strongRecOn generalizing state with
  | ind fuel ih =>
    cases front : state.queue.work with
    | nil => exact ⟨state, history, front⟩
    | cons node tail =>
      have smaller := finite_rank rows names bounded state history node tail front
      rw [size] at smaller
      exact ih _ smaller _ (Trace.next history front) rfl

end ParserEngine.PredictionTermination
