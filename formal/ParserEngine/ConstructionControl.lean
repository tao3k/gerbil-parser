import ParserEngine.ConstructionStorage
import ParserEngine.ConstructionExtraction

/-! A restricted control interpreter for the actual grow! declaration. It
decodes guards, allocation initializers, forward loop controls and ordered
owner assignments. Heap primitives and normal-return host evaluation remain
external contracts; this is not an interpreter for arbitrary Scheme. -/
namespace ParserEngine.ConstructionControl
open WalkSource ConstructionStorage

inductive Field where | states | rows | plans deriving DecidableEq, Repr
structure Program where
  allocations : List Field
  copies : List Field
  owners : List Field
  deriving DecidableEq

def canonical : Program := ⟨[.states, .rows, .plans], [.states, .rows, .plans], [.states, .rows, .plans]⟩

def allocation : Datum → Option Field
  | .list [.symbol "next-states", .list [.symbol "make-vector", .list [.symbol "*", .integer 2, .symbol "state-count"], .boolean false]] => some .states
  | .list [.symbol "next-transitions", .list [.symbol "make-vector", .list [.symbol "*", .integer 2, .symbol "state-count"], .list [.symbol "quote", .list []]]] => some .rows
  | .list [.symbol "next-plans", .list [.symbol "make-vector", .list [.symbol "*", .integer 2, .symbol "state-count"], .boolean false]] => some .plans
  | _ => none

def copyWrite : Datum → Option Field
  | .list [.symbol "vector-set!", .symbol "next-states", .symbol "index", .list [.symbol "vector-ref", .symbol "states", .symbol "index"]] => some .states
  | .list [.symbol "vector-set!", .symbol "next-transitions", .symbol "index", .list [.symbol "vector-ref", .symbol "state-transitions", .symbol "index"]] => some .rows
  | .list [.symbol "vector-set!", .symbol "next-plans", .symbol "index", .list [.symbol "vector-ref", .symbol "kernel-plans", .symbol "index"]] => some .plans
  | _ => none

def ownerWrite : Datum → Option Field
  | .list [.symbol "set!", .symbol "states", .symbol "next-states"] => some .states
  | .list [.symbol "set!", .symbol "state-transitions", .symbol "next-transitions"] => some .rows
  | .list [.symbol "set!", .symbol "kernel-plans", .symbol "next-plans"] => some .plans
  | _ => none

def copyLoop : Datum → Option (List Field)
  | .list [.symbol "let", .symbol "copy", .list [.list [.symbol "index", .integer 0]],
      .list [.symbol "when", .list [.symbol "<", .symbol "index", .symbol "state-count"], one, two, three,
        .list [.symbol "copy", .list [.symbol "+", .symbol "index", .integer 1]]]] =>
    [one, two, three].mapM copyWrite
  | _ => none

def decode : Datum → Option Program
  | .list [.symbol "def", .list [.symbol "grow!"],
      .list [.symbol "when", .list [.symbol "=", .symbol "state-count", .list [.symbol "vector-length", .symbol "states"]],
        .list [.symbol "let", .list [one, two, three], loop, first, second, third]]] => do
      let allocations ← [one, two, three].mapM allocation
      let copies ← copyLoop loop
      let owners ← [first, second, third].mapM ownerWrite
      pure ⟨allocations, copies, owners⟩
  | _ => none

def allocateField (capacity : Nat) (heap : Heap) : Field → Heap
  | .states => {heap with states := List.replicate capacity none}
  | .rows => {heap with rows := List.replicate capacity none}
  | .plans => {heap with plans := List.replicate capacity none}

/-- The native let initializers need not acquire an extra evaluation-order
premise: independent fresh vector allocations commute at this representation. -/
theorem allocations_commute (capacity : Nat) (heap : Heap) (one two : Field) :
    allocateField capacity (allocateField capacity heap one) two =
      allocateField capacity (allocateField capacity heap two) one := by
  cases one <;> cases two <;> rfl

theorem allocation_permutation (capacity : Nat) (one two : List Field) (order : one.Perm two) (heap : Heap) :
    one.foldl (allocateField capacity) heap = two.foldl (allocateField capacity) heap := by
  induction order generalizing heap with
  | nil => rfl
  | cons field _ ih => exact ih _
  | swap one two rest => simp only [List.foldl_cons, allocations_commute]
  | trans _ _ first second => exact (first heap).trans (second heap)

def copyField (source : Heap) (index : Nat) (target : Heap) : Field → Heap
  | .states => {target with states := target.states.set index (source.states[index]?.join)}
  | .rows => {target with rows := target.rows.set index (source.rows[index]?.join)}
  | .plans => {target with plans := target.plans.set index (source.plans[index]?.join)}

def ownerField (fresh : Heap) (active : Heap) : Field → Heap
  | .states => {active with states := fresh.states}
  | .rows => {active with rows := fresh.rows}
  | .plans => {active with plans := fresh.plans}

structure Fits (heap : Heap) (limit : Nat) : Prop where
  states : limit ≤ heap.states.length
  rows : limit ≤ heap.rows.length
  plans : limit ≤ heap.plans.length

theorem fits_prefix (fits : Fits heap limit) (inside : index < limit) : Fits heap (index + 1) :=
  ⟨Nat.le_trans (Nat.succ_le_of_lt inside) fits.states,
    Nat.le_trans (Nat.succ_le_of_lt inside) fits.rows, Nat.le_trans (Nat.succ_le_of_lt inside) fits.plans⟩

theorem copy_field_fits (source target : Heap) (index limit : Nat) (fits : Fits target limit) (field : Field) :
    Fits (copyField source index target field) limit := by
  cases field <;> exact ⟨by simpa [copyField] using fits.states,
    by simpa [copyField] using fits.rows, by simpa [copyField] using fits.plans⟩

theorem copy_fields_fits (source target : Heap) (index limit : Nat) (fits : Fits target limit) (fields : List Field) :
    Fits (fields.foldl (copyField source index) target) limit := by
  induction fields generalizing target with
  | nil => exact fits
  | cons field rest ih => exact ih _ (copy_field_fits source target index limit fits field)

/-- The source loop is tail recursive with a forward cursor and a strict
count bound. Active owners remain unchanged throughout this local copy. -/
def copyWhile (program : Program) (source target : Heap) (index limit : Nat) : Heap :=
  if inside : index < limit then
    copyWhile program source (program.copies.foldl (copyField source index) target) (index + 1) limit
  else target
termination_by limit - index
decreasing_by omega

inductive CopyRun (program : Program) (source : Heap) (limit : Nat) : Heap → Nat → Heap → Prop where
  | done (target : Heap) (index : Nat) (finished : limit ≤ index) : CopyRun program source limit target index target
  | next {target index result} (inside : index < limit) (reads : Fits source (index + 1)) (writes : Fits target (index + 1))
      (later : CopyRun program source limit (program.copies.foldl (copyField source index) target) (index + 1) result) :
      CopyRun program source limit target index result

/-- Every executed loop iteration has valid reads and writes in all vectors;
list-set's out-of-bounds no-op is never used to justify native behavior. -/
theorem loop_safe (program : Program) (source target : Heap) (index limit : Nat)
    (reads : Fits source limit) (writes : Fits target limit) :
    CopyRun program source limit target index (copyWhile program source target index limit) := by
  rw [copyWhile]
  split
  · next inside =>
      exact .next inside (fits_prefix reads inside) (fits_prefix writes inside)
        (loop_safe program source _ (index + 1) limit reads (copy_fields_fits source target index limit writes program.copies))
  · next finished => exact .done target index (Nat.le_of_not_gt finished)
termination_by limit - index
decreasing_by omega

def fresh (program : Program) (count : Nat) : Heap := program.allocations.foldl (allocateField (2 * count)) ⟨[], [], []⟩

def GrowthSafe (program : Program) (heap : Heap) (count : Nat) : Prop := count = heap.states.length →
  CopyRun program heap count (fresh program count) 0 (copyWhile program heap (fresh program count) 0 count)

theorem growth_safe (heap : Heap) (count : Nat) (shape : Shape heap count) : GrowthSafe canonical heap count := by
  intro full
  have reads : Fits heap count := ⟨by omega, by rw [shape.rows]; omega, by rw [shape.plans]; omega⟩
  have writes : Fits (fresh canonical count) count := by
    constructor <;> simp only [fresh, canonical, List.foldl_cons, List.foldl_nil, allocateField, List.length_replicate] <;> omega
  exact loop_safe canonical heap (fresh canonical count) 0 count reads writes

def grow (program : Program) (heap : Heap) (count : Nat) : Heap :=
  if count = heap.states.length then
    let fresh := program.allocations.foldl (allocateField (2 * count)) ⟨[], [], []⟩
    let copied := copyWhile program heap fresh 0 count
    program.owners.foldl (ownerField copied) heap
  else heap

def append (program : Program) (heap : Heap) (count : Nat) (state : List Nat) (plan : PlanStorage.Plan) : Heap :=
  let prepared := grow program heap count
  let withState := {prepared with states := prepared.states.set count (some state)}
  {withState with plans := withState.plans.set count (some plan)}

theorem forward_prefix (source target : Heap) (index limit : Nat) (bounded : index ≤ limit) :
    copyWhile canonical source (ConstructionStorage.copy source target index) index limit =
      ConstructionStorage.copy source target limit := by
  rw [copyWhile]
  split
  · have step : canonical.copies.foldl (copyField source index) (ConstructionStorage.copy source target index) =
        ConstructionStorage.copy source target (index + 1) := rfl
    rw [step]
    exact forward_prefix source target (index + 1) limit (by omega)
  · have same : index = limit := by omega
    subst index
    rfl
termination_by limit - index
decreasing_by omega

theorem copy_exact (source target : Heap) (count : Nat) :
    copyWhile canonical source target 0 count = ConstructionStorage.copy source target count :=
  forward_prefix source target 0 count (Nat.zero_le count)

/-- Publishing owners separately is intentional: the state owner switches
first, while row and plan owners still refer to the old vectors. -/
theorem owner_stages (old fresh : Heap) :
    ownerField fresh old .states = ⟨fresh.states, old.plans, old.rows⟩ ∧
      ownerField fresh (ownerField fresh old .states) .rows = ⟨fresh.states, old.plans, fresh.rows⟩ ∧
      canonical.owners.foldl (ownerField fresh) old = fresh := by
  cases fresh
  exact ⟨rfl, rfl, rfl⟩

theorem grow_exact (heap : Heap) (count : Nat) :
    grow canonical heap count = if count = heap.states.length then ConstructionStorage.grow heap else heap := by
  unfold grow
  split
  · next full =>
      dsimp only
      rw [copy_exact, (owner_stages heap _).2.2]
      simp only [canonical, List.foldl_cons, List.foldl_nil, allocateField]
      simp only [ConstructionStorage.grow, full]
  · rfl

theorem append_exact (heap : Heap) (count : Nat) (state : List Nat) (plan : PlanStorage.Plan) :
    append canonical heap count state plan = ConstructionStorage.append heap count state plan := by
  simp only [append, grow_exact, ConstructionStorage.append]

set_option maxRecDepth 8192 in
theorem schema_decoded : decode (LRConstructionSource.growWith 2) = some canonical := rfl

theorem source_decoded (builder growth installation template : Datum) (recipe : ConstructionExtraction.Recipe)
    (admitted : ConstructionExtraction.extract builder growth installation template = some recipe) :
    decode growth = some canonical := by
  have current := (ConstructionExtraction.extracted_sources _ _ _ _ recipe admitted).2.1
  rw [recipe.doubled] at current
  rw [current]
  exact schema_decoded

inductive Action where
  | append (state : List Nat) (plan : PlanStorage.Plan)
  | publish (node : Nat) (row : Row)

def execute (program : Program) : List Action → Nat → Heap → Nat × Heap
  | [], count, heap => (count, heap)
  | .append state plan :: rest, count, heap => execute program rest (count + 1) (append program heap count state plan)
  | .publish node row :: rest, count, heap => execute program rest count (ConstructionStorage.publish heap node row)

theorem execute_append (program : Program) (first second : List Action) (count : Nat) (heap : Heap) :
    execute program (first ++ second) count heap =
      execute program second (execute program first count heap).1 (execute program first count heap).2 := by
  induction first generalizing count heap with
  | nil => rfl
  | cons action rest ih => cases action <;> simp only [List.cons_append, execute, ih]

def Safe (program : Program) : List Action → Nat → Heap → Prop
  | [], _, _ => True
  | .append state plan :: rest, count, heap => Shape heap count ∧ GrowthSafe program heap count ∧ Safe program rest (count + 1) (append program heap count state plan)
  | .publish node row :: rest, count, heap => node < count ∧ Safe program rest count (ConstructionStorage.publish heap node row)

theorem safe_append (program : Program) (first second : List Action) (count : Nat) (heap : Heap)
    (before : Safe program first count heap)
    (after : Safe program second (execute program first count heap).1 (execute program first count heap).2) :
    Safe program (first ++ second) count heap := by
  induction first generalizing count heap with
  | nil => exact after
  | cons action rest ih =>
    cases action with
    | append state plan => exact ⟨before.1, before.2.1, ih _ _ before.2.2 after⟩
    | publish node row => exact ⟨before.1, ih _ _ before.2 after⟩

def Realizes (layout : PlanStorage.Layout) (count : Nat) (heap : Heap) : Prop :=
  ∃ actions, Safe canonical actions 0 (ConstructionStorage.empty layout) ∧
    execute canonical actions 0 (ConstructionStorage.empty layout) = (count, heap)

def executeSource (source : Datum) (actions : List Action) (layout : PlanStorage.Layout) : Option (Nat × Heap) :=
  (decode source).bind fun program =>
  if program = canonical then some (execute program actions 0 (ConstructionStorage.empty layout)) else none

theorem malformed_rejected (source : Datum) (actions : List Action) (layout : PlanStorage.Layout)
    (rejected : decode source = none) : executeSource source actions layout = none := by
  simp only [executeSource, rejected, Option.bind_none]

theorem unsupported_rejected (source : Datum) (actions : List Action) (layout : PlanStorage.Layout)
    (program : Program) (decoded : decode source = some program) (different : program ≠ canonical) :
    executeSource source actions layout = none := by
  simp only [executeSource, decoded, Option.bind_some, different, ↓reduceIte]

/-- A nonadvancing source loop is rejected, even when its three mutation
bodies are arbitrary. The interpreter cannot silently replace it by +1. -/
theorem nonadvancing_rejected (one two three : Datum) :
    copyLoop (.list [.symbol "let", .symbol "copy", .list [.list [.symbol "index", .integer 0]],
      .list [.symbol "when", .list [.symbol "<", .symbol "index", .symbol "state-count"], one, two, three,
        .list [.symbol "copy", .list [.symbol "+", .symbol "index", .integer 0]]]]) = none := rfl

/-- Every complete joint history has a forward-control execution witness;
the caller does not supply final-vector equality or a source execution trace. -/
theorem trace_realizes (trace : ConstructionStorage.Trace layout count heap) : Realizes layout count heap := by
  induction trace with
  | start => exact ⟨[], trivial, rfl⟩
  | append prior state plan ih =>
    obtain ⟨actions, safe, execution⟩ := ih
    refine ⟨actions ++ [.append state plan], ?_, ?_⟩
    · apply safe_append canonical actions _ _ _ safe
      rw [execution]
      exact ⟨trace_shape prior, growth_safe _ _ (trace_shape prior), trivial⟩
    · rw [execute_append, execution]
      simp only [execute, append_exact]
  | publish _ node row inside ih =>
    obtain ⟨actions, safe, execution⟩ := ih
    refine ⟨actions ++ [.publish node row], ?_, ?_⟩
    · apply safe_append canonical actions _ _ _ safe
      rw [execution]
      exact ⟨inside, trivial⟩
    · rw [execute_append, execution]
      rfl

theorem source_realizes (builder growth installation template : Datum) (recipe : ConstructionExtraction.Recipe)
    (admitted : ConstructionExtraction.extract builder growth installation template = some recipe)
    (trace : ConstructionStorage.Trace recipe.layout count heap) :
    ∃ actions, Safe canonical actions 0 (ConstructionStorage.empty recipe.layout) ∧
      executeSource growth actions recipe.layout = some (count, heap) := by
  obtain ⟨actions, safe, execution⟩ := trace_realizes trace
  refine ⟨actions, safe, ?_⟩
  simp only [executeSource, source_decoded builder growth installation template recipe admitted,
    Option.bind_some, ↓reduceIte, execution]

end ParserEngine.ConstructionControl
