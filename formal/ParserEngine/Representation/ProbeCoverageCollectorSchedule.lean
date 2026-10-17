import ParserEngine.Representation.RootsMasks
import ParserEngine.Operational
import ParserEngine.Encoding
import Init.Data.BitVec.Lemmas

/-! Representation adapters shared by root discovery and compiler scheduling.
These algorithms expose descriptor, equality and dependency assumptions; they
are not a verification of the host collector or compiler's GC-map generator. -/
namespace ParserEngine.Representation

namespace ProbeCoverage
/-- One descending circuit, following native step=1 and wraparound. -/
def position (capacity start turn : Nat) : Nat :=
  if turn ≤ start then start - turn else capacity + start - turn

def previous (capacity slot : Nat) : Nat := if slot = 0 then capacity - 1 else slot - 1

theorem position_bound {capacity start turn : Nat} (initial : start < capacity) (turnBound : turn < capacity) :
    position capacity start turn < capacity := by unfold position; split <;> omega

theorem position_first {capacity start : Nat} : position capacity start 0 = start := by simp [position]

theorem position_next {capacity start turn : Nat} (initial : start < capacity) (bound : turn + 1 < capacity) :
    position capacity start (turn + 1) = previous capacity (position capacity start turn) := by
  unfold position previous
  split <;> split <;> split <;> omega

theorem position_injective {capacity start left right : Nat} (_initial : start < capacity)
    (l : left < capacity) (r : right < capacity)
    (same : position capacity start left = position capacity start right) : left = right := by
  unfold position at same
  split at same <;> split at same <;> omega

theorem position_covers {capacity start target : Nat} (initial : start < capacity) (valid : target < capacity) :
    ∃ turn, turn < capacity ∧ position capacity start turn = target := by
  by_cases before : target ≤ start
  · exact ⟨start - target, by omega, by simp [position]; omega⟩
  · refine ⟨capacity + start - target, by omega, ?_⟩
    unfold position
    split <;> omega

def observations (capacity start : Nat) (slots : Nat → Hash.Slot Key Value) : List (Hash.Slot Key Value) :=
  (List.range capacity).map (fun turn => slots (position capacity start turn))

theorem observed_slot {capacity start target : Nat} (slots : Nat → Hash.Slot Key Value)
    (initial : start < capacity) (valid : target < capacity) :
    slots target ∈ observations capacity start slots := by
  obtain ⟨turn, bound, same⟩ := position_covers initial valid
  exact List.mem_map.mpr ⟨turn, List.mem_range.mpr bound, by rw [same]⟩

inductive Result (Value : Type) where
  | found (value : Value)
  | absent
  | exhausted
  deriving DecidableEq, Repr

/-- Unlike an Option-only projection, exhausting a corrupt/full probe is not
reported as a legitimate UNUSED miss. -/
def scan [DecidableEq Key] (key : Key) : List (Hash.Slot Key Value) → Result Value
  | [] => .exhausted
  | .unused :: _ => .absent
  | .deleted :: rest => scan key rest
  | .entry candidate value :: rest => if key = candidate then .found value else scan key rest

theorem unused_prevents_exhaustion [DecidableEq Key] (key : Key) (rows : List (Hash.Slot Key Value))
    (vacant : Hash.Slot.unused ∈ rows) : scan key rows ≠ .exhausted := by
  induction rows with
  | nil => simp at vacant
  | cons row rest ih =>
    cases row with
    | unused => simp [scan]
    | deleted => exact ih (by simpa using vacant)
    | entry candidate value =>
      by_cases same : key = candidate
      · simp [scan, same]
      · simpa [scan, same] using ih (by simpa using vacant)

theorem physical_probe_terminates [DecidableEq Key] {capacity start : Nat} (key : Key)
    (slots : Nat → Hash.Slot Key Value) (initial : start < capacity)
    (space : ∃ target, target < capacity ∧ slots target = .unused) :
    scan key (observations capacity start slots) ≠ .exhausted := by
  obtain ⟨target, bound, empty⟩ := space
  apply unused_prevents_exhaustion
  exact empty ▸ observed_slot slots initial bound

theorem hash_start_bound (width mixed capacity : Nat) (positive : 0 < capacity) :
    ((mixed % 2 ^ width) * capacity) / 2 ^ width < capacity := by
  apply (Nat.div_lt_iff_lt_mul (Nat.two_pow_pos width)).mpr
  have bound := Nat.mul_lt_mul_of_pos_right (Nat.mod_lt mixed (Nat.two_pow_pos width)) positive
  simpa [Nat.mul_comm] using bound

/-- Generic equal? tables use fixnum-modulo, including negative custom hashes;
C eq? tables use the bounded high-product route above. Both then step by one
slot (two physical fields). -/
theorem generic_start_bound (hash : Int) (capacity : Nat) (positive : 0 < capacity) :
    (hash % (capacity : Int)).toNat < capacity := by
  have low := Int.emod_nonneg hash (show (capacity : Int) ≠ 0 by omega)
  have high := Int.emod_lt_of_pos hash (show (0 : Int) < capacity by omega)
  omega

theorem physical_step (capacity slot : Nat) :
    2 * previous capacity slot = if slot = 0 then 2 * capacity - 2 else 2 * slot - 2 := by
  unfold previous
  split <;> omega

def optionResult : Result Value → Option Value
  | .found value => some value
  | .absent | .exhausted => none

theorem scan_refines_probe [DecidableEq Key] (key : Key) (rows : List (Hash.Slot Key Value)) :
    optionResult (scan key rows) = Hash.probe key rows := by
  induction rows with
  | nil => rfl
  | cons row rest ih =>
    cases row with
    | unused => rfl
    | deleted => exact ih
    | entry candidate value =>
      by_cases same : key = candidate
      · simp [scan, Hash.probe, same, optionResult]
      · simpa only [scan, Hash.probe, same, ite_false] using ih

end ProbeCoverage

namespace CollectorSchedule
/-- A reserved object remains a root obligation while scan/mark_array runs.
Popping it from a queue does not make this worker idle. -/
structure Worker where
  active : Bool
  queued : List Nat
  held : List Nat

def work (worker : Worker) : List Nat := worker.queued ++ worker.held

def Safe (worker : Worker) : Prop := worker.active = false → work worker = []

structure State where
  workers : List Worker
  count : Nat

def Valid (state : State) : Prop :=
  state.count = state.workers.countP Worker.active ∧ ∀ worker ∈ state.workers, Safe worker

def pending (state : State) : List Nat := state.workers.flatMap work

/-- Mutex-protected counter updates must correspond to actual active workers.
An inactive worker cannot receive locally published chunks. -/
theorem replace_active {earlierWorkers laterWorkers : List Worker} {old fresh : Worker} {count : Nat}
    (valid : Valid ⟨earlierWorkers ++ old :: laterWorkers, count⟩)
    (active : old.active = true) (same : fresh.active = true) :
    Valid ⟨earlierWorkers ++ fresh :: laterWorkers, count⟩ := by
  constructor
  · simpa [List.countP_append, List.countP_cons, active, same] using valid.1
  · intro worker member
    rcases List.mem_append.mp member with earlier | later
    · exact valid.2 worker (List.mem_append_left _ earlier)
    · rcases List.mem_cons.mp later with rfl | later
      · intro inactive; simp [same] at inactive
      · exact valid.2 worker (List.mem_append_right _ (List.mem_cons_of_mem _ later))

def publish (worker : Worker) (objects : List Nat) : Worker :=
  {worker with queued := objects ++ worker.queued}

def reserve (worker : Worker) (object : Nat) (rest : List Nat) : Worker :=
  {worker with queued := rest, held := object :: worker.held}

theorem publish_valid {earlierWorkers laterWorkers : List Worker} {worker : Worker} {count : Nat}
    (valid : Valid ⟨earlierWorkers ++ worker :: laterWorkers, count⟩) (active : worker.active = true) (objects : List Nat) :
    Valid ⟨earlierWorkers ++ publish worker objects :: laterWorkers, count⟩ :=
  replace_active valid active active

theorem reserve_valid {earlierWorkers laterWorkers : List Worker} {worker : Worker} {count object : Nat} {rest : List Nat}
    (valid : Valid ⟨earlierWorkers ++ worker :: laterWorkers, count⟩) (active : worker.active = true) :
    Valid ⟨earlierWorkers ++ reserve worker object rest :: laterWorkers, count⟩ :=
  replace_active valid active active

theorem reservation_retains_work (worker : Worker) (object : Nat) (rest : List Nat)
    (head : worker.queued = object :: rest) : List.Perm (work (reserve worker object rest)) (work worker) := by
  simp only [work, reserve, head, List.cons_append]
  exact List.perm_middle

theorem idle_valid {earlierWorkers laterWorkers : List Worker} {worker : Worker} {count : Nat}
    (valid : Valid ⟨earlierWorkers ++ worker :: laterWorkers, count⟩) (active : worker.active = true)
    (drained : work worker = []) :
    Valid ⟨earlierWorkers ++ {worker with active := false} :: laterWorkers, count - 1⟩ := by
  constructor
  · have old := valid.1
    simp only [List.countP_append, List.countP_cons, active, ite_true] at old
    simp only [List.countP_append, List.countP_cons, Bool.false_eq_true, ite_false, Nat.add_zero]
    omega
  · intro other member
    rcases List.mem_append.mp member with earlier | later
    · exact valid.2 other (List.mem_append_left _ earlier)
    · rcases List.mem_cons.mp later with rfl | later
      · exact fun _ => drained
      · exact valid.2 other (List.mem_append_right _ (List.mem_cons_of_mem _ later))

theorem wake_valid {earlierWorkers laterWorkers : List Worker} {worker : Worker} {count : Nat}
    (valid : Valid ⟨earlierWorkers ++ worker :: laterWorkers, count⟩) (inactive : worker.active = false) :
    Valid ⟨earlierWorkers ++ {worker with active := true} :: laterWorkers, count + 1⟩ := by
  constructor
  · have old := valid.1
    simp only [List.countP_append, List.countP_cons, inactive, Bool.false_eq_true, ite_false, Nat.add_zero] at old
    simp only [List.countP_append, List.countP_cons, ite_true]
    omega
  · intro other member
    rcases List.mem_append.mp member with earlier | later
    · exact valid.2 other (List.mem_append_left _ earlier)
    · rcases List.mem_cons.mp later with rfl | later
      · intro impossible; simp at impossible
      · exact valid.2 other (List.mem_append_right _ (List.mem_cons_of_mem _ later))

theorem reservation_preserves_pending {earlier later : List Worker} {worker : Worker} {count object : Nat} {rest : List Nat}
    (head : worker.queued = object :: rest) (fact : Nat) :
    fact ∈ pending ⟨earlier ++ reserve worker object rest :: later, count⟩ ↔
      fact ∈ pending ⟨earlier ++ worker :: later, count⟩ := by
  simp [pending, work, reserve, head, or_left_comm]

/-- Stealing removes a complete chunk from its victim before scanning it on
an already-active thief. Both queue ownership and reservations remain visible. -/
theorem steal_valid {earlier middle later : List Worker} {victim thief : Worker} {count : Nat} {chunk rest : List Nat}
    (valid : Valid ⟨earlier ++ victim :: (middle ++ thief :: later), count⟩)
    (victimActive : victim.active = true) (thiefActive : thief.active = true) :
    Valid ⟨earlier ++ {victim with queued := rest} ::
      (middle ++ {thief with held := chunk ++ thief.held} :: later), count⟩ := by
  have first : Valid ⟨earlier ++ {victim with queued := rest} :: (middle ++ thief :: later), count⟩ :=
    replace_active valid victimActive victimActive
  have second := replace_active (earlierWorkers := earlier ++ {victim with queued := rest} :: middle)
    (laterWorkers := later) (old := thief) (fresh := {thief with held := chunk ++ thief.held})
    (count := count) (by simpa [List.append_assoc] using first) thiefActive thiefActive
  simpa [List.append_assoc] using second

theorem steal_preserves_pending {earlier middle later : List Worker} {victim thief : Worker} {count : Nat} {chunk rest : List Nat}
    (head : victim.queued = chunk ++ rest) (fact : Nat) :
    fact ∈ pending ⟨earlier ++ {victim with queued := rest} ::
      (middle ++ {thief with held := chunk ++ thief.held} :: later), count⟩ ↔
    fact ∈ pending ⟨earlier ++ victim :: (middle ++ thief :: later), count⟩ := by
  simp [pending, work, head, or_left_comm]


theorem counter_bound (state : State) (valid : Valid state) : state.count ≤ state.workers.length := by
  rw [valid.1]
  exact List.countP_le_length

theorem counter_fits (state : State) (valid : Valid state) (limit : Nat)
    (processors : state.workers.length ≤ limit) : state.count ≤ limit :=
  Nat.le_trans (counter_bound state valid) processors

/-- The C termination counter is global. A locally empty queue, a reserved
object, or an uncounted active worker cannot justify termination. -/
theorem zero_workers_drained (state : State) (valid : Valid state) (zero : state.count = 0) :
    pending state = [] := by
  have none : state.workers.countP Worker.active = 0 := valid.1.symm.trans zero
  apply List.flatMap_eq_nil_iff.mpr
  intro worker member
  have inactive : worker.active = false := by
    have h := List.countP_eq_zero.mp none worker member
    cases value : worker.active <;> simp_all
  exact valid.2 worker member inactive

/-- Refinement records queues AND in-flight scans. Counter zero then supplies
the genuine pending-empty premise of the compiled strong-marking theorem. -/
theorem zero_workers_marking_exact {heap : List Roots.Object} {banks : Roots.Banks}
    (state : State) (logical : Worklist.State Nat) (valid : Valid state)
    (ownership : ∀ object, object ∈ logical.pending ↔ object ∈ pending state)
    (history : Worklist.Trace (Roots.program heap banks) (Worklist.initial (Roots.program heap banks)) logical)
    (zero : state.count = 0) : ∀ address, address ∈ logical.known ↔ Roots.Marked heap banks address := by
  have stopped := zero_workers_drained state valid zero
  have empty : logical.pending = [] := by
    apply List.eq_nil_iff_forall_not_mem.mpr
    intro object member
    have present := (ownership object).mp member
    simp [stopped] at present
  exact Roots.marking_control_flow_exact heap banks logical history empty
end CollectorSchedule


end ParserEngine.Representation
