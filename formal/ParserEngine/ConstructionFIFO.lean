import ParserEngine.ConstructionInitialization
import ParserEngine.ConstructionHeap

/-! Source-order target callbacks separate append installation, row cons and
conditional enqueue. The outer instruction loop owns dequeue, accounting and
one reversed-row publication. Primitives and normal-return diagnostics remain
external; these are parser effects, not arbitrary Scheme evaluation. -/
namespace ParserEngine.ConstructionFIFO
open WalkSource ConstructionQueue CachedConstruction ConstructionStorage

inductive TargetInstruction where | install | row | enqueue deriving DecidableEq
inductive QueueInstruction where | dequeue | process | groups | publish deriving DecidableEq
structure Program where
  seed : Nat
  target : List TargetInstruction
  queue : List QueueInstruction
  deriving DecidableEq
def canonical : Program := ⟨0, [.install, .row, .enqueue], [.dequeue, .process, .groups, .publish]⟩

def targetBody : Datum → Option (List TargetInstruction)
  | .list [.symbol "for-each", .list [.symbol "lambda", .list [.symbol "kernel"],
      .list [.symbol "let*", .list [
        .list [.symbol "symbol", .list [.symbol "vector-ref", .symbol "symbols", .list [.symbol "vector-ref", .symbol "kernel", .integer 1]]],
        .list [.symbol "mask", .list [.symbol "vector-ref", .symbol "kernel", .integer 2]],
        .list [.symbol "existing", .list [.symbol "table-ref", .symbol "state-index", .symbol "mask", .boolean false]],
        .list [.symbol "target-index", .list [.symbol "or", .symbol "existing", .list [.symbol "append-state!", .symbol "mask"]]]],
        .list [.symbol "set!", .symbol "rows", .list [.symbol "cons", .list [.symbol "cons", .symbol "symbol", .symbol "target-index"], .symbol "rows"]],
        .list [.symbol "unless", .symbol "existing", .list [.symbol "enqueue!", .symbol "queue", .symbol "target-index"]]]],
      .list [.symbol "lr0-state-kernels", .list [.symbol "car", .symbol "plan"], .list [.symbol "cdr", .symbol "plan"],
        .symbol "symbol-ids", .symbol "masks", .symbol "first-items"]] => some [.install, .row, .enqueue]
  | _ => none

def queueBody : Datum → Option Program
  | .list [.symbol "let", .list [.symbol "queue", .list [.symbol "make-Queue"]],
      .list [.symbol "enqueue!", .symbol "queue", .integer seed],
      .list [.symbol "let", .symbol "loop", .list [],
        .list [.symbol "if", .list [.symbol "queue-empty?", .symbol "queue"],
          .list [.symbol "values", .symbol "states", .symbol "state-transitions", .symbol "state-count",
            .symbol "productions-by-lhs", .symbol "processed-count"],
          .list [.symbol "let", .list [.symbol "index", .list [.symbol "dequeue!", .symbol "queue"]],
            .list [.symbol "set!", .symbol "processed-count", .list [.symbol "+", .symbol "processed-count", .integer 1]],
            trace, .list [.symbol "let", .list [
              .list [.symbol "rows", .list [.symbol "quote", .list []]],
              .list [.symbol "plan", .list [.symbol "vector-ref", .symbol "kernel-plans", .symbol "index"]]],
              targets, .list [.symbol "vector-set!", .symbol "state-transitions", .symbol "index", .list [.symbol "reverse", .symbol "rows"]]],
            .list [.symbol "loop"]]]]] =>
    (targetBody targets).bind fun target =>
    if trace = LRTransitionSource.traceProgress then
      some ⟨seed, target, [.dequeue, .process, .groups, .publish]⟩ else none
  | _ => none

def decode (builder : Datum) : Option Program :=
  (ConstructionInitialization.envelope builder).bind fun parts => queueBody parts.queue

set_option maxRecDepth 8192 in
theorem schema_decoded (initial : Nat) : decode (LRTransitionSource.builderWith initial) = some canonical := by
  change queueBody LRTransitionSource.workQueue = some canonical
  change (targetBody LRTransitionSource.targets).bind (fun target =>
    if LRTransitionSource.traceProgress = LRTransitionSource.traceProgress then
      some ⟨0, target, [.dequeue, .process, .groups, .publish]⟩ else none) = some canonical
  simp only [↓reduceIte]
  rfl

theorem source_decoded (builder growth installation template : Datum) (recipe : ConstructionExtraction.Recipe)
    (admitted : ConstructionExtraction.extract builder growth installation template = some recipe) :
    decode builder = some canonical := by
  rw [(ConstructionExtraction.extracted_sources _ _ _ _ recipe admitted).1]
  exact schema_decoded recipe.initial

structure TargetFrame (g : Grammar.CFG) where
  registry : StateInterning.Registry Nat (List Nat)
  cache : Cache g
  rows : Row
  id : Nat
  fresh : Bool
  actions : List ConstructionControl.Action

def appendAction (g : Grammar.CFG) (cache : Cache g) (kernel : Nat) : ConstructionControl.Action :=
  .append (KernelConstruction.compute g kernel).1 ⟨kernel,
    (PlanStorage.fetch (TemplateCapabilities.materialize g) cache.memory (TemplateCapabilities.predictionMask g kernel)).1⟩

def targetStep (g : Grammar.CFG) (symbol kernel : Nat) (frame : TargetFrame g) : TargetInstruction → TargetFrame g
  | .install =>
    match frame.registry.index kernel with
    | some id => {frame with id := id, fresh := false}
    | none =>
      let installed := ConstructionInstallation.append g frame.cache frame.registry kernel
      {frame with
        registry := installed.registry, cache := installed.cache, id := installed.id, fresh := true,
        actions := frame.actions ++ [appendAction g frame.cache kernel]}
  | .row => {frame with rows := (symbol, frame.id) :: frame.rows}
  | .enqueue => if frame.fresh then {frame with registry := {frame.registry with pending := frame.registry.pending ++ [frame.id]}} else frame

def target (program : Program) (g : Grammar.CFG) (cache : Cache g)
    (before : StateInterning.Registry Nat (List Nat)) (symbol kernel : Nat) (rows : Row) : TargetFrame g :=
  program.target.foldl (targetStep g symbol kernel) ⟨before, cache, rows, 0, false, []⟩

theorem target_existing (g : Grammar.CFG) (cache : Cache g) (before : StateInterning.Registry Nat (List Nat))
    (symbol kernel id : Nat) (rows : Row) (found : before.index kernel = some id) :
    target canonical g cache before symbol kernel rows = ⟨before, cache, (symbol, id) :: rows, id, false, []⟩ := by
  simp [target, canonical, targetStep, found]

theorem target_before_enqueue (g : Grammar.CFG) (cache : Cache g) (before : StateInterning.Registry Nat (List Nat))
    (symbol kernel : Nat) (rows : Row) (fresh : before.index kernel = none) :
    let frame := target {canonical with target := [.install, .row]} g cache before symbol kernel rows
    frame.registry.pending = before.pending ∧ frame.rows = (symbol, before.keys.length) :: rows := by
  simp only [target, List.foldl_cons, List.foldl_nil, targetStep, fresh]
  exact ⟨(ConstructionInstallation.append_queue g cache before kernel).1,
    by rw [ConstructionInstallation.append_id]⟩

theorem target_exact (g : Grammar.CFG) (cache : Cache g) (before : StateInterning.Registry Nat (List Nat))
    (symbol kernel : Nat) (rows : Row) :
    target canonical g cache before symbol kernel rows =
      let installed := CachedConstruction.intern g cache before kernel
      ⟨installed.1.registry, installed.2, (symbol, installed.1.id) :: rows, installed.1.id, installed.1.fresh,
        if installed.1.fresh then [appendAction g cache kernel] else []⟩ := by
  cases found : before.index kernel with
  | some id => simp [target, canonical, targetStep, CachedConstruction.intern, found]
  | none =>
    simp [target, canonical, targetStep, found, ConstructionInstallation.append_exact g cache before kernel found,
      CachedConstruction.intern, StateInterning.intern]

structure Groups (g : Grammar.CFG) where
  registry : StateInterning.Registry Nat (List Nat)
  cache : Cache g
  rows : Row
  actions : List ConstructionControl.Action

def groups (program : Program) (g : Grammar.CFG) (cache : Cache g) (before : StateInterning.Registry Nat (List Nat)) :
    List (Nat × Nat) → Row → Groups g
  | [], rows => ⟨before, cache, rows, []⟩
  | (symbol, kernel) :: rest, rows =>
    let installed := target program g cache before symbol kernel rows
    let later := groups program g installed.cache installed.registry rest installed.rows
    {later with actions := installed.actions ++ later.actions}

theorem groups_exact (g : Grammar.CFG) (cache : Cache g) (before : StateInterning.Registry Nat (List Nat))
    (inputs : List (Nat × Nat)) (rows : Row) :
    (groups canonical g cache before inputs rows).registry = (CachedConstruction.publish g cache before inputs).1.registry ∧
      (groups canonical g cache before inputs rows).cache = (CachedConstruction.publish g cache before inputs).2 ∧
      (groups canonical g cache before inputs rows).rows = (CachedConstruction.publish g cache before inputs).1.rows.reverse ++ rows := by
  induction inputs generalizing cache before rows with
  | nil => exact ⟨rfl, rfl, rfl⟩
  | cons group rest ih =>
    obtain ⟨registry, cacheEq, rowsEq⟩ := ih (CachedConstruction.intern g cache before group.2).2
      (CachedConstruction.intern g cache before group.2).1.registry
      ((group.1, (CachedConstruction.intern g cache before group.2).1.id) :: rows)
    simp only [groups, target_exact, CachedConstruction.publish]
    exact ⟨registry, cacheEq, by simpa [List.reverse_cons, List.append_assoc] using rowsEq⟩

theorem execute_concat (first second : List ConstructionControl.Action) (count : Nat) (heap : Heap) :
    ConstructionAppend.execute ConstructionControl.canonical ConstructionAppend.canonical (first ++ second) count heap =
      let prior := ConstructionAppend.execute ConstructionControl.canonical ConstructionAppend.canonical first count heap
      ConstructionAppend.execute ConstructionControl.canonical ConstructionAppend.canonical second prior.1 prior.2 := by
  simp only [ConstructionAppend.execute_exact, ConstructionControl.execute_append]

theorem groups_registry (g : Grammar.CFG) (cache : Cache g) (before : StateInterning.Registry Nat (List Nat))
    (inputs : List (Nat × Nat)) (rows : Row) :
    (groups canonical g cache before inputs rows).registry = (CachedConstruction.publish g cache before inputs).1.registry :=
  (groups_exact g cache before inputs rows).1

theorem groups_cache (g : Grammar.CFG) (cache : Cache g) (before : StateInterning.Registry Nat (List Nat))
    (inputs : List (Nat × Nat)) (rows : Row) :
    (groups canonical g cache before inputs rows).cache = (CachedConstruction.publish g cache before inputs).2 :=
  (groups_exact g cache before inputs rows).2.1

theorem groups_rows (g : Grammar.CFG) (cache : Cache g) (before : StateInterning.Registry Nat (List Nat))
    (inputs : List (Nat × Nat)) (rows : Row) :
    (groups canonical g cache before inputs rows).rows = (CachedConstruction.publish g cache before inputs).1.rows.reverse ++ rows :=
  (groups_exact g cache before inputs rows).2.2

theorem intern_replay (g : Grammar.CFG) (cache : Cache g) (before : StateInterning.Registry Nat (List Nat)) (kernel : Nat) :
    let installed := CachedConstruction.intern g cache before kernel
    ConstructionAppend.execute ConstructionControl.canonical ConstructionAppend.canonical
      (if installed.1.fresh then [appendAction g cache kernel] else []) before.keys.length cache.storage =
        (installed.1.registry.keys.length, installed.2.storage) := by
  cases found : before.index kernel <;>
    simp [CachedConstruction.intern, found, StateInterning.intern, appendAction,
      ConstructionAppend.execute, ConstructionAppend.run_exact, TemplateCapabilities.build_storage]

theorem groups_replay (g : Grammar.CFG) (cache : Cache g) (before : StateInterning.Registry Nat (List Nat))
    (inputs : List (Nat × Nat)) (rows : Row) :
    let result := groups canonical g cache before inputs rows
    ConstructionAppend.execute ConstructionControl.canonical ConstructionAppend.canonical result.actions before.keys.length cache.storage =
      (result.registry.keys.length, result.cache.storage) := by
  induction inputs generalizing cache before rows with
  | nil => rfl
  | cons group rest ih =>
    simp only [groups, target_exact]
    rw [execute_concat, intern_replay]
    exact ih _ _ _

structure Output (g : Grammar.CFG) where
  machine : Machine
  cache : Cache g
  actions : List ConstructionControl.Action

structure QueueFrame (g : Grammar.CFG) extends Output g where
  node : Nat
  rows : Row

def queueStep (program : Program) (g : Grammar.CFG) (traversal : Traversal) (frame : QueueFrame g) : QueueInstruction → QueueFrame g
  | .dequeue => match frame.machine.registry.pending with
    | [] => frame
    | node :: tail => {frame with node := node, machine := {frame.machine with registry := {frame.machine.registry with pending := tail}}}
  | .process => {frame with machine := {frame.machine with registry := {frame.machine.registry with processed := frame.machine.registry.processed + 1}}}
  | .groups => match frame.cache.readTemplate frame.node with
    | none => frame
    | some (kernel, template) =>
      let grouped := CachedConstruction.runGroup g traversal frame.cache kernel template
      let result := groups program g grouped.2 frame.machine.registry (grouped.1.map fun row => (row.key, row.mask)) []
      {frame with
        machine := {frame.machine with registry := result.registry}, cache := result.cache,
        rows := result.rows, actions := frame.actions ++ result.actions}
  | .publish =>
    let rows := frame.rows.reverse
    {frame with
      machine := {frame.machine with transitions := ProducerAdmission.Storage.update frame.machine.transitions frame.node rows},
      cache := {frame.cache with storage := ConstructionStorage.publish frame.cache.storage frame.node rows},
      actions := frame.actions ++ [.publish frame.node rows]}

def tick (program : Program) (g : Grammar.CFG) (traversal : Traversal) (cache : Cache g) (before : Machine) : Output g :=
  if before.registry.pending = [] then ⟨before, cache, []⟩ else
    (program.queue.foldl (queueStep program g traversal) ⟨⟨before, cache, []⟩, 0, []⟩).toOutput

/-- Before grouping, dequeue and accounting affect only their owned fields.
This is an equality of the entire frame, including the untouched cache,
transition table and empty action stream. -/
theorem counted_prefix (g : Grammar.CFG) (traversal : Traversal) (cache : Cache g)
    (before : Machine) (node : Nat) (tail : List Nat)
    (front : before.registry.pending = node :: tail) :
    ([QueueInstruction.dequeue, .process].foldl (queueStep canonical g traversal)
      ⟨⟨before, cache, []⟩, 0, []⟩) =
      ⟨⟨{before with registry := {before.registry with pending := tail, processed := before.registry.processed + 1}},
        cache, []⟩, node, []⟩ := by
  simp [List.foldl, queueStep, front]

theorem target_processed (g : Grammar.CFG) (cache : Cache g)
    (before : StateInterning.Registry Nat (List Nat)) (symbol kernel : Nat) (rows : Row) :
    (target canonical g cache before symbol kernel rows).registry.processed = before.processed := by
  cases found : before.index kernel with
  | some id => simp [target, canonical, targetStep, found]
  | none =>
    simp only [target, canonical, List.foldl_cons, List.foldl_nil, targetStep, found]
    exact (ConstructionInstallation.append_queue g cache before kernel).2

/-- Any number of fresh or reused targets leaves outer accounting alone. -/
theorem groups_processed (g : Grammar.CFG) (cache : Cache g)
    (before : StateInterning.Registry Nat (List Nat)) (inputs : List (Nat × Nat)) (rows : Row) :
    (groups canonical g cache before inputs rows).registry.processed = before.processed := by
  induction inputs generalizing cache before rows with
  | nil => rfl
  | cons edge rest ih =>
    simp only [groups]
    rw [ih, target_processed]

/-- Group callbacks retain the unpublished table. Only publish owns the
transition write; callbacks cannot account for another dequeued state. -/
theorem groups_boundary (g : Grammar.CFG) (traversal : Traversal) (frame : QueueFrame g) :
    let after := queueStep canonical g traversal frame .groups
    after.machine.registry.processed = frame.machine.registry.processed ∧
      after.machine.transitions = frame.machine.transitions ∧ after.node = frame.node := by
  simp only [queueStep]
  cases frame.cache.readTemplate frame.node with
  | none => exact ⟨rfl, rfl, rfl⟩
  | some plan => exact ⟨groups_processed _ _ _ _ _, rfl, rfl⟩

/-- The actual tick factors through the accounted frame and unpublished group
result. This connects the prefix laws to the same loop retained by LeanPoo. -/
theorem tick_phases (g : Grammar.CFG) (traversal : Traversal) (cache : Cache g)
    (before : Machine) (node : Nat) (tail : List Nat)
    (front : before.registry.pending = node :: tail) :
    let counted : QueueFrame g :=
      ⟨⟨{before with registry := {before.registry with pending := tail, processed := before.registry.processed + 1}},
        cache, []⟩, node, []⟩
    let grouped := queueStep canonical g traversal counted .groups
    grouped.machine.registry.processed = before.registry.processed + 1 ∧
      grouped.machine.transitions = before.transitions ∧
      tick canonical g traversal cache before = (queueStep canonical g traversal grouped .publish).toOutput := by
  dsimp only
  refine ⟨(groups_boundary _ _ _).1, (groups_boundary _ _ _).2.1, ?_⟩
  simp only [tick, front, List.cons_ne_nil, ↓reduceIte]
  change (([QueueInstruction.dequeue, .process] ++ [QueueInstruction.groups, .publish]).foldl
    (queueStep canonical g traversal) ⟨⟨before, cache, []⟩, 0, []⟩).toOutput = _
  rw [List.foldl_append, counted_prefix g traversal cache before node tail front]
  rfl

theorem tick_exact (g : Grammar.CFG) (traversal : Traversal) (cache : Cache g) (before : Machine)
    (admitted : Invariant g before) (aligned : Aligned g cache before.registry) :
    ((tick canonical g traversal cache before).machine, (tick canonical g traversal cache before).cache) =
      CachedConstruction.tick g traversal cache before := by
  cases front : before.registry.pending with
  | nil => simp [tick, front, CachedConstruction.tick]
  | cons node tail =>
    have bound := admitted.valid.bound node (by rw [front]; simp)
    let kernel := before.registry.keys[node]
    have read : before.registry.keys[node]? = some kernel := List.getElem?_eq_getElem bound
    have plan := aligned node kernel read
    simp only [tick, front, List.cons_ne_nil, ↓reduceIte,
      show canonical.queue = [.dequeue, .process, .groups, .publish] from rfl,
      List.foldl_cons, List.foldl_nil, queueStep,
      TemplateCapabilities.Cache.readTemplate, CachedConstruction.tick]
    simp only [TemplateCapabilities.Cache.readTemplate] at plan
    simp only [plan, groups_registry, groups_cache, groups_rows, List.append_nil, List.reverse_reverse, TransitionPublication.drain]

theorem tick_replay (g : Grammar.CFG) (traversal : Traversal) (cache : Cache g) (before : Machine)
    (admitted : Invariant g before) (aligned : Aligned g cache before.registry) :
    let result := tick canonical g traversal cache before
    ConstructionAppend.execute ConstructionControl.canonical ConstructionAppend.canonical result.actions
      before.registry.keys.length cache.storage = (result.machine.registry.keys.length, result.cache.storage) := by
  cases front : before.registry.pending with
  | nil => simp [tick, front, ConstructionAppend.execute]
  | cons node tail =>
    have bound := admitted.valid.bound node (by rw [front]; simp)
    let kernel := before.registry.keys[node]
    have read : before.registry.keys[node]? = some kernel := List.getElem?_eq_getElem bound
    have plan := aligned node kernel read
    simp only [tick, front, List.cons_ne_nil, ↓reduceIte,
      show canonical.queue = [.dequeue, .process, .groups, .publish] from rfl,
      List.foldl_cons, List.foldl_nil, queueStep, plan, List.nil_append]
    rw [execute_concat]
    let grouped := runGroup g traversal cache kernel (TemplateCapabilities.materialize g (TemplateCapabilities.predictionMask g kernel))
    let drained := TransitionPublication.drain before.registry tail
    let inputs := grouped.1.map fun row => (row.key, row.mask)
    let result := groups canonical g grouped.2 drained inputs []
    change (let prior := (ConstructionAppend.execute ConstructionControl.canonical ConstructionAppend.canonical
        result.actions drained.keys.length grouped.2.storage);
      ConstructionAppend.execute ConstructionControl.canonical ConstructionAppend.canonical
        [.publish node result.rows.reverse] prior.1 prior.2) =
      (result.registry.keys.length, ConstructionStorage.publish result.cache.storage node result.rows.reverse)
    rw [show ConstructionAppend.execute ConstructionControl.canonical ConstructionAppend.canonical
      result.actions drained.keys.length grouped.2.storage = (result.registry.keys.length, result.cache.storage) from
      groups_replay g grouped.2 drained inputs []]
    rfl

def finish (g : Grammar.CFG) (traversal : Traversal) (cache : Cache g) (before : Machine)
    (admitted : Invariant g before) (covered : Covered g cache before.registry) (aligned : Aligned g cache before.registry) : Output g :=
  if _empty : before.registry.pending = [] then ⟨before, cache, []⟩ else
    let later := tick canonical g traversal cache before
    have same := tick_exact g traversal cache before admitted aligned
    have valid : Invariant g later.machine := by
      rw [show later.machine = (CachedConstruction.tick g traversal cache before).1 from congrArg Prod.fst same,
        CachedConstruction.tick_exact g traversal cache before admitted covered aligned]
      exact tick_invariant g before admitted
    have retained : Covered g later.cache later.machine.registry := by
      rw [show later.cache = (CachedConstruction.tick g traversal cache before).2 from congrArg Prod.snd same,
        show later.machine = (CachedConstruction.tick g traversal cache before).1 from congrArg Prod.fst same]
      exact tick_covered g traversal cache before covered
    have matching : Aligned g later.cache later.machine.registry := by
      rw [show later.cache = (CachedConstruction.tick g traversal cache before).2 from congrArg Prod.snd same,
        show later.machine = (CachedConstruction.tick g traversal cache before).1 from congrArg Prod.fst same]
      exact tick_aligned g traversal cache before aligned
    let result := finish g traversal later.cache later.machine valid retained matching
    {result with actions := later.actions ++ result.actions}
termination_by capacity g - before.registry.processed
decreasing_by
  rw [show (tick canonical g traversal cache before).machine = (CachedConstruction.tick g traversal cache before).1 from
    congrArg Prod.fst (tick_exact g traversal cache before admitted aligned),
    CachedConstruction.tick_exact g traversal cache before admitted covered aligned]
  exact tick_rank g before admitted _empty

theorem finish_exact (g : Grammar.CFG) (traversal : Traversal) (cache : Cache g) (before : Machine)
    (admitted : Invariant g before) (covered : Covered g cache before.registry) (aligned : Aligned g cache before.registry) :
    ((finish g traversal cache before admitted covered aligned).machine,
      (finish g traversal cache before admitted covered aligned).cache) =
        CachedConstruction.finish g traversal cache before admitted covered aligned := by
  rw [finish, CachedConstruction.finish]
  split
  · rfl
  · dsimp only
    rw [finish_exact]
    have same := tick_exact g traversal cache before admitted aligned
    simp only [show (tick canonical g traversal cache before).machine = (CachedConstruction.tick g traversal cache before).1 from congrArg Prod.fst same,
      show (tick canonical g traversal cache before).cache = (CachedConstruction.tick g traversal cache before).2 from congrArg Prod.snd same]
termination_by capacity g - before.registry.processed
decreasing_by
  rw [show (tick canonical g traversal cache before).machine = (CachedConstruction.tick g traversal cache before).1 from
    congrArg Prod.fst (tick_exact g traversal cache before admitted aligned),
    CachedConstruction.tick_exact g traversal cache before admitted covered aligned]
  exact tick_rank g before admitted (by assumption)

theorem finish_replay (g : Grammar.CFG) (traversal : Traversal) (cache : Cache g) (before : Machine)
    (admitted : Invariant g before) (covered : Covered g cache before.registry) (aligned : Aligned g cache before.registry) :
    let result := finish g traversal cache before admitted covered aligned
    ConstructionAppend.execute ConstructionControl.canonical ConstructionAppend.canonical result.actions
      before.registry.keys.length cache.storage = (result.machine.registry.keys.length, result.cache.storage) := by
  rw [finish]
  split
  · rfl
  · dsimp only
    rw [execute_concat, tick_replay g traversal cache before admitted aligned]
    exact finish_replay g traversal _ _ _ _ _
termination_by capacity g - before.registry.processed
decreasing_by
  rw [show (tick canonical g traversal cache before).machine = (CachedConstruction.tick g traversal cache before).1 from
    congrArg Prod.fst (tick_exact g traversal cache before admitted aligned),
    CachedConstruction.tick_exact g traversal cache before admitted covered aligned]
  exact tick_rank g before admitted (by assumption)

def start (program : Program) (initialization : List ConstructionAppend.Instruction)
    (g : Grammar.CFG) (layout : PlanStorage.Layout) : Machine × Cache g :=
  let installed := ConstructionInitialization.initial initialization g layout
  (⟨{installed.1.registry with pending := installed.1.registry.pending ++ [program.seed]}, installed.1.transitions⟩, installed.2)

theorem start_exact (g : Grammar.CFG) (layout : PlanStorage.Layout) :
    start canonical ConstructionInitialization.canonical g layout = CachedConstruction.initial g layout := by
  rw [start, ConstructionInitialization.initial_exact]
  simp only [canonical, List.nil_append, CachedConstruction.initial, CachedConstruction.intern,
    ConstructionQueue.empty, StateInterning.intern]
  rfl

def construct (g : Grammar.CFG) (nonempty : g ≠ []) (traversal : Traversal) (layout : PlanStorage.Layout) : Output g :=
  let initial := start canonical ConstructionInitialization.canonical g layout
  have same : initial = CachedConstruction.initial g layout := start_exact g layout
  have valid : Invariant g initial.1 := by rw [same, CachedConstruction.initial_exact]; exact initial_invariant g nonempty
  have covered : Covered g initial.2 initial.1.registry := by rw [same]; exact initial_covered g layout
  have aligned : Aligned g initial.2 initial.1.registry := by rw [same]; exact initial_aligned g layout
  let result := finish g traversal initial.2 initial.1 valid covered aligned
  {result with actions := appendAction g (TemplateCapabilities.empty g layout) 1 :: result.actions}

theorem construct_exact (g : Grammar.CFG) (nonempty : g ≠ []) (traversal : Traversal) (layout : PlanStorage.Layout) :
    ((construct g nonempty traversal layout).machine, (construct g nonempty traversal layout).cache) =
      ((CachedConstruction.construct g nonempty traversal layout).1.machine,
        (CachedConstruction.construct g nonempty traversal layout).2) := by
  simp only [construct, finish_exact, start_exact, CachedConstruction.construct]

theorem construct_replay (g : Grammar.CFG) (nonempty : g ≠ []) (traversal : Traversal) (layout : PlanStorage.Layout) :
    let result := construct g nonempty traversal layout
    ConstructionAppend.execute ConstructionControl.canonical ConstructionAppend.canonical result.actions 0
      (ConstructionStorage.empty layout) = (result.machine.registry.keys.length, result.cache.storage) := by
  let initial := start canonical ConstructionInitialization.canonical g layout
  have valid : Invariant g initial.1 := by dsimp only [initial]; rw [start_exact, CachedConstruction.initial_exact]; exact initial_invariant g nonempty
  have covered : Covered g initial.2 initial.1.registry := by dsimp only [initial]; rw [start_exact]; exact initial_covered g layout
  have aligned : Aligned g initial.2 initial.1.registry := by dsimp only [initial]; rw [start_exact]; exact initial_aligned g layout
  have replay := finish_replay g traversal initial.2 initial.1 valid covered aligned
  have count : initial.1.registry.keys.length = 1 := by dsimp only [initial]; rw [start_exact]; rfl
  have heap : initial.2.storage = ConstructionStorage.append (ConstructionStorage.empty layout) 0
      (KernelConstruction.compute g 1).1 ⟨1, (PlanStorage.fetch (TemplateCapabilities.materialize g)
        (TemplateCapabilities.empty g layout).memory (TemplateCapabilities.predictionMask g 1)).1⟩ := by
    dsimp only [initial]
    rw [start_exact]
    simp only [CachedConstruction.initial, CachedConstruction.intern, ConstructionQueue.empty,
      TemplateCapabilities.build_storage, TemplateCapabilities.empty]
    rfl
  rw [count, heap] at replay
  simpa only [construct, ConstructionAppend.execute, appendAction, ConstructionAppend.run_exact] using replay

structure Built (g : Grammar.CFG) where
  construction : ConstructionProtocol.Product g × Cache g
  actions : List ConstructionControl.Action

/-- The retained factory executes the source-order producer once. Semantic
certificates are transported to its actual machine and cache, not substituted
by a second independent algorithm run. -/
def build (g : Grammar.CFG) (nonempty : g ≠ []) (traversal : Traversal) (layout : PlanStorage.Layout) : Built g :=
  let result := construct g nonempty traversal layout
  have same : result.machine = (ConstructionProtocol.construct g nonempty).machine :=
    (congrArg Prod.fst (construct_exact g nonempty traversal layout)).trans (CachedConstruction.construct_exact g nonempty traversal layout)
  ⟨(⟨result.machine, same.symm ▸ (ConstructionProtocol.construct g nonempty).invariant,
      same.symm ▸ (ConstructionProtocol.construct g nonempty).published,
      same.symm ▸ (ConstructionProtocol.construct g nonempty).empty⟩, result.cache), result.actions⟩

theorem build_exact (g : Grammar.CFG) (nonempty : g ≠ []) (traversal : Traversal) (layout : PlanStorage.Layout) :
    (build g nonempty traversal layout).construction = CachedConstruction.construct g nonempty traversal layout := by
  apply Prod.ext
  · have ext (a b : ConstructionProtocol.Product g) (same : a.machine = b.machine) : a = b := by
      cases a; cases b; cases same; rfl
    apply ext
    exact congrArg (fun pair : Machine × Cache g => pair.1) (construct_exact g nonempty traversal layout)
  · exact congrArg (fun pair : Machine × Cache g => pair.2) (construct_exact g nonempty traversal layout)

theorem build_replay (g : Grammar.CFG) (nonempty : g ≠ []) (traversal : Traversal) (layout : PlanStorage.Layout) :
    let result := build g nonempty traversal layout
    ConstructionAppend.execute ConstructionControl.canonical ConstructionAppend.canonical result.actions 0
      (ConstructionStorage.empty layout) = (result.construction.1.machine.registry.keys.length, result.construction.2.storage) :=
  construct_replay g nonempty traversal layout

def executeSource (builder growth installation template : Datum) (g : Grammar.CFG) (nonempty : g ≠ [])
    (traversal : Traversal) : Option ((Machine × Cache g) × List ConstructionControl.Action) :=
  (ConstructionExtraction.extract builder growth installation template).bind fun recipe =>
  (ConstructionInitialization.decode builder).bind fun initial =>
  (decode builder).bind fun queue =>
  (ConstructionControl.decode growth).bind fun expansion =>
  (ConstructionAppend.decode installation).bind fun append =>
  if initial = ConstructionInitialization.canonical ∧ queue = canonical ∧
      expansion = ConstructionControl.canonical ∧ append = ConstructionAppend.canonical then
    let result := construct g nonempty traversal recipe.layout
    some ((result.machine, result.cache), result.actions)
  else none

theorem source_execution (builder growth installation template : Datum) (recipe : ConstructionExtraction.Recipe)
    (admitted : ConstructionExtraction.extract builder growth installation template = some recipe)
    (g : Grammar.CFG) (nonempty : g ≠ []) (traversal : Traversal) :
    executeSource builder growth installation template g nonempty traversal =
      some (((build g nonempty traversal recipe.layout).construction.1.machine,
        (build g nonempty traversal recipe.layout).construction.2), (build g nonempty traversal recipe.layout).actions) := by
  simp only [executeSource, admitted, Option.bind_some, ConstructionInitialization.source_decoded _ _ _ _ recipe admitted,
    source_decoded _ _ _ _ recipe admitted, ConstructionControl.source_decoded _ _ _ _ recipe admitted,
    ConstructionAppend.source_decoded _ _ _ _ recipe admitted, Option.bind_some, and_self, ↓reduceIte]
  rfl

theorem rejected (builder growth installation template : Datum) (g : Grammar.CFG) (nonempty : g ≠ []) (traversal : Traversal)
    (failure : ConstructionExtraction.extract builder growth installation template = none) :
    executeSource builder growth installation template g nonempty traversal = none := by
  simp only [executeSource, failure, Option.bind_none]

end ParserEngine.ConstructionFIFO
