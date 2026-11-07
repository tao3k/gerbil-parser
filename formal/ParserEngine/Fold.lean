import Lean

namespace ParserEngine

/- Responsibility boundary: this file owns pure denotational refinement and
operation-count bounds. FoldPlan.qnt owns scan invalidation and consumer
interleaving histories. Neither layer establishes physical throughput, full
FFI ownership, or correctness of the compiler's complete IR analysis. -/

/-- Pure source classification for two predicates over one immutable frame.
The counter counts source visits, not CPU time, table lookups or allocations. -/
def sharedProbe (p q : α → Bool) : List α → (Bool × Bool) × Nat
  | [] => ((false, false), 0)
  | head :: tail =>
    let rest := sharedProbe p q tail
    ((p head || rest.1.1, q head || rest.1.2), rest.2 + 1)

theorem shared_probe_exact (p q : α → Bool) (source : List α) :
    sharedProbe p q source = ((source.any p, source.any q), source.length) := by
  induction source with
  | nil => rfl
  | cons head tail ih => simp [sharedProbe, ih, List.any_cons]

/-- Full scans only: separate early-exit probes may visit fewer than 2*n bytes.
This inequality must never be advertised as a guaranteed twofold speedup. -/
theorem shared_probe_visit_bound (p q : α → Bool) (source : List α) :
    (sharedProbe p q source).2 ≤ 2 * source.length := by
  rw [shared_probe_exact]
  simp only
  omega

/-- Consumers are arbitrary ordered state transitions; classification does
not run them or mutate their immutable source frame. Compiler eligibility
must separately establish this source/bounds stability assumption. -/
theorem shared_probe_consumers (p q : α → Bool) (source : List α)
    (first : Bool → σ → σ) (second : Bool → σ → σ) (state : σ) :
    second (sharedProbe p q source).1.2
      (first (sharedProbe p q source).1.1 state) =
    second (source.any q) (first (source.any p) state) := by
  rw [shared_probe_exact]

/-- A necessary first byte can precede total Boolean state reads. This does
not authorize moving queries, range errors, mutation or allocating helpers. -/
theorem necessary_byte_guard (state first rest : Bool) :
    (state && (first && rest)) = (first && (state && rest)) := by
  cases state <;> cases first <;> cases rest <;> rfl

def guardedStateReads (first : Bool) : Nat := if first then 1 else 0

theorem irrelevant_state_reads_eliminated : guardedStateReads false = 0 := rfl

theorem relevant_state_read_retained : guardedStateReads true = 1 := rfl

/-- Standard-library reuse is an implementation choice, not evidence that a
runtime gate passed. Order, effects, ownership and actual timing remain gates. -/
structure LibraryAdmission where
  standardLibrary : Bool
  preservesOrder : Bool
  preservesEffects : Bool
  preservesOwnership : Bool
  matchedRuntimeGate : Bool

def LibraryAdmission.accepted (a : LibraryAdmission) : Bool :=
  a.preservesOrder && a.preservesEffects && a.preservesOwnership && a.matchedRuntimeGate

theorem library_choice_cannot_replace_runtime_gate (a : LibraryAdmission)
    (failed : a.matchedRuntimeGate = false) : a.accepted = false := by
  simp [LibraryAdmission.accepted, failed]

/-- Ordered emission has the List.flatten contract. This models sequence
semantics, not the Gerbil macro's heap implementation or elapsed time. -/
theorem ordered_builder_contract (chunks : List (List α)) :
    chunks.foldr List.append [] = chunks.flatten := by
  induction chunks with
  | nil => rfl
  | cons head tail ih => simp [List.foldr, ih]

/-- Every alternative retains its suffix test; membership is only necessary. -/
theorem necessary_choice_guard (a b x y : Bool) :
    ((a && x) || (b && y)) = ((a || b) && ((a && x) || (b && y))) := by
  cases a <;> cases b <;> cases x <;> cases y <;> rfl

/-- A cursor byte is loaded without a second range check only within the
validated loop frame. This does not justify reusing it in a nested frame. -/
theorem validated_loop_byte (start cursor limit size : Nat)
    (_lower : start ≤ cursor) (upper : cursor < limit) (extent : limit ≤ size) :
    cursor < size := Nat.lt_of_lt_of_le upper extent

/-- Dependence/effect summaries are compiler obligations, not facts inferred
from successful model tests. The implementation must compute these flags. -/
def probeEligible (sameBounds boundWrite opaqueEffect helper frameChange : Bool) : Bool :=
  sameBounds && !boundWrite && !opaqueEffect && !helper && !frameChange

theorem source_frame_change_blocks_shared_probe (sameBounds boundWrite opaqueEffect helper : Bool) :
    probeEligible sameBounds boundWrite opaqueEffect helper true = false := by
  simp [probeEligible]

theorem bound_write_blocks_shared_probe (sameBounds opaqueEffect helper frameChange : Bool) :
    probeEligible sameBounds true opaqueEffect helper frameChange = false := by
  simp [probeEligible]

theorem opaque_call_blocks_shared_probe (sameBounds boundWrite helper frameChange : Bool) :
    probeEligible sameBounds boundWrite true helper frameChange = false := by
  simp [probeEligible]

/-- Code interning is not value reuse: invoking one shared procedure k times
still performs k scans. Counts here measure full-scan source visits only. -/
def repeatedScanVisits (requests bytes : Nat) : Nat := requests * bytes

theorem interned_code_does_not_reduce_scans (calls bytes : Nat) :
    repeatedScanVisits calls bytes = calls * bytes := rfl

/-- A dominating immutable scan shared by two ordered consumers removes one
full traversal. Early exits, lookup costs and clock time remain unmodeled. -/
theorem dominating_scan_visit_saving (bytes : Nat) :
    repeatedScanVisits 2 bytes - repeatedScanVisits 1 bytes = bytes := by
  simp [repeatedScanVisits]
  omega

/-- Cross-consumer reuse supplies the same immutable observation to each ordered
consumer. Source/frame equality is a compiler obligation; ownership remains
in the state passed through the fold, not in the observation. -/
theorem shared_consumer_observation {α β σ : Type} (observe : α → β) (source : α)
    (consumers : List (β → σ → σ)) (state : σ) :
    consumers.foldl (fun s consume => consume (observe source) s) state =
    (let fact := observe source
     consumers.foldl (fun s consume => consume fact s) state) := rfl

/-- Representation changes must commute with every admitted transition.
This obligation includes writes, stack operations and parameter overrides;
it is not discharged by simply replacing vector reads with local variables. -/
theorem frame_representation_refinement {σ τ α : Type}
    (encode : σ → τ) (oldStep : σ → α → σ) (newStep : τ → α → τ)
    (stepLaw : ∀ state action, encode (oldStep state action) =
                                newStep (encode state) action)
    (actions : List α) (state : σ) :
    encode (actions.foldl oldStep state) = actions.foldl newStep (encode state) := by
  induction actions generalizing state with
  | nil => rfl
  | cons action rest ih =>
    simp only [List.foldl_cons]
    rw [ih, stepLaw]

-- Quint owns owner/frame lifecycle histories, not this transition algebra.
#print axioms ParserEngine.frame_representation_refinement

/-- A typed Boolean subset with observable query traces. This is executable
model semantics, not semantics of every Fold instruction or Scheme exception. -/
inductive FoldPredicate where
  | literal : Bool → FoldPredicate
  | slot : Nat → FoldPredicate
  | byte : Nat → FoldPredicate
  | query : Bool → Nat → FoldPredicate
  | conjunction : FoldPredicate → FoldPredicate → FoldPredicate
  | choice : FoldPredicate → FoldPredicate → FoldPredicate

def FoldPredicate.evaluate (slots : Nat → Bool) (cursorByte : Nat) :
    FoldPredicate → Bool × List Nat
  | .literal value => (value, [])
  | .slot key => (slots key, [])
  | .byte expected => (cursorByte == expected, [])
  | .query result tag => (result, [tag])
  | .conjunction left right =>
    let first := left.evaluate slots cursorByte
    if first.1 then
      let second := right.evaluate slots cursorByte
      (second.1, first.2 ++ second.2)
    else (false, first.2)
  | .choice left right =>
    let first := left.evaluate slots cursorByte
    if first.1 then (true, first.2)
    else
      let second := right.evaluate slots cursorByte
      (second.1, first.2 ++ second.2)

theorem byte_guard_preserves_query_trace (slots : Nat → Bool) (key expected byte : Nat)
    (rest : FoldPredicate) :
    (FoldPredicate.conjunction (.slot key) (.conjunction (.byte expected) rest)).evaluate slots byte =
    (FoldPredicate.conjunction (.byte expected) (.conjunction (.slot key) rest)).evaluate slots byte := by
  cases slotValue : slots key <;> cases byteValue : (byte == expected) <;>
    simp [FoldPredicate.evaluate, slotValue, byteValue]

/-- Mutation witness: a query cannot move behind a mismatching byte guard. -/
theorem moving_query_is_observable :
    (FoldPredicate.conjunction (.query true 7) (.byte 1)).evaluate (fun _ => false) 0 ≠
    (FoldPredicate.conjunction (.byte 1) (.query true 7)).evaluate (fun _ => false) 0 := by
  decide

/-- The validated byte can eliminate a mismatching state-dependent branch,
including arbitrary queries in its suffix, without executing those queries. -/
theorem mismatching_byte_eliminates_state (slots : Nat → Bool) (key expected byte : Nat)
    (rest : FoldPredicate) (mismatch : (byte == expected) = false) :
    (FoldPredicate.conjunction (.slot key) (.conjunction (.byte expected) rest)).evaluate slots byte =
    (false, []) := by
  cases value : slots key <;> simp [FoldPredicate.evaluate, value, mismatch]

/-- Cofactoring preserves a sequence of arbitrary state transitions once each
step has its independently established mismatch/effect obligation. This does
not prove that the compiler computed that obligation for every IR form. -/
def executeSteps (steps : List (α → α)) (state : α) : α :=
  steps.foldl (fun current step => step current) state

theorem ordered_region_refinement (steps : List (α → α))
    (transform : (α → α) → (α → α))
    (refinement : ∀ step ∈ steps, ∀ state, transform step state = step state)
    (state : α) : executeSteps (steps.map transform) state = executeSteps steps state := by
  induction steps generalizing state with
  | nil => rfl
  | cons step rest ih =>
    simp only [List.map_cons, executeSteps, List.foldl_cons]
    rw [refinement step (by simp) state]
    apply ih
    intro next member current
    exact refinement next (by simp [member]) current

/-- A negative shared recognition result selects the original else transition
without evaluating irrelevant pure state guards. Necessity and effect freedom
must be established by IR admission; this is not a grammar-wide speed proof. -/
theorem negative_line_consumer {σ : Type} (trigger condition : Bool)
    (necessary : condition = true → trigger = true) (negative : trigger = false)
    (yes no : σ → σ) (state : σ) :
    (if condition then yes state else no state) = no state := by
  cases condition with
  | false => rfl
  | true => have impossible := necessary rfl; simp [negative] at impossible

/-- Scheme conditions consume truthiness, but Boolean slots publish actual
Booleans. This models that distinction without erasing observable traces. -/
inductive ConditionValue where
  | falseValue
  | trueValue
  | payload : Nat → ConditionValue
  deriving DecidableEq

def ConditionValue.truth : ConditionValue → Bool
  | .falseValue => false
  | _ => true

def ConditionValue.normalize (value : ConditionValue) : ConditionValue :=
  if value.truth then .trueValue else .falseValue

theorem condition_normalization_not_required (value : ConditionValue) :
    value.normalize.truth = value.truth := by
  cases value <;> rfl

theorem condition_preserves_ordered_trace (value : ConditionValue)
    (priorTrace onTrue onFalse : List Nat) :
    priorTrace ++ (if value.normalize.truth then onTrue else onFalse) =
    priorTrace ++ (if value.truth then onTrue else onFalse) := by
  rw [condition_normalization_not_required]

theorem boolean_publication_cannot_be_erased :
    ConditionValue.normalize (.payload 7) ≠ ConditionValue.payload 7 := by
  decide

/-- The idle branch is a guarded specialization, never a cached result.
The compiler must prove equivalence for every state satisfying its guard;
outside that domain the original ordered transition is retained. This theorem
does not prove the Scheme IR transformation or physical timing. -/
def guardedIdle (guard : σ → Bool) (specialized original : σ → σ) (state : σ) : σ :=
  if guard state then specialized state else original state

theorem guarded_idle_refinement (guard : σ → Bool) (specialized original : σ → σ)
    (eligible : ∀ state, guard state = true → specialized state = original state)
    (state : σ) : guardedIdle guard specialized original state = original state := by
  unfold guardedIdle
  split
  next h => exact eligible state h
  next => rfl

theorem zero_mode_guard (mode : Nat) (idle : mode = 0) : (0 < mode) = False := by
  subst mode
  simp

/-- Consumer-local selector facts require an admitted unsigned cell. This
does not apply to a signed integer or an opaque predicate. -/
theorem consumer_false_positive_zero (mode : Nat) (branch : ¬ 0 < mode) : mode = 0 := by
  exact Nat.eq_zero_of_not_pos branch

/-- Fact support is independent: refining one consumer never assigns the
other consumer's state. Runtime mutation/invalidation histories belong to Quint. -/
theorem consumer_zero_support (mode other : Nat) (branch : ¬ 0 < mode) :
    (mode, other) = (0, other) := by
  rw [consumer_false_positive_zero mode branch]

/-- Reusing a selector's branch truth requires its extracted read support to
be sound. This proves the disjoint-write rule, not the IR dependency extractor. -/
theorem consumer_disjoint_write (reads : List Nat) (predicate : (Nat → Nat) → Bool)
    (support : ∀ before after, (∀ name, name ∈ reads → before name = after name) →
       predicate before = predicate after)
    (state : Nat → Nat) (written value : Nat) (disjoint : written ∉ reads) :
    predicate state = predicate (fun name => if name = written then value else state name) := by
  apply support
  intro name member
  have different : name ≠ written := by
    intro same
    subst name
    exact disjoint member
  simp [different]

/-- Removing internal owner-valued results preserves ordered effects when
each transition retains the same private owner. This is not a proof of mutable
alias admission, the generated compiler, or physical continuation allocation. -/
def liftPrivateOwner {σ ω : Type} (effect : σ → σ) (owned : σ × ω) : σ × ω :=
  (effect owned.1, owned.2)

theorem effect_only_owner_threading {σ ω : Type} (steps : List (σ → σ))
    (state : σ) (owner : ω) :
    executeSteps (steps.map liftPrivateOwner) (state, owner) =
      (executeSteps steps state, owner) := by
  induction steps generalizing state with
  | nil => rfl
  | cons step rest ih =>
    simp only [List.map_cons, executeSteps, List.foldl_cons, liftPrivateOwner]
    exact ih (step state)

/-- A validated cursor and clamped forward steps stay ordered in its frame.
This arithmetic fact does not admit a reversed range or reuse across frames. -/
theorem clamped_forward_range (start cursor stop : Nat)
    (lower : start ≤ cursor) (upper : cursor ≤ stop) :
    start ≤ cursor ∧ cursor ≤ min stop (cursor + 1) ∧
      min stop (cursor + 1) ≤ stop := by
  omega

/-- Erase a discarded condition only after its observation is total and pure.
The Scheme effect checker and its contextual admission remain separate duties. -/
theorem discarded_pure_condition (observe : σ → Bool) (state : σ) :
    (if observe state then state else state) = state := by
  split <;> rfl

/-- A fixed overwrite family depends on input, never on prior written cells.
The untouched context includes source, admission guards and the event owner.
This model does not prove the Scheme admission checker or compiler lowering. -/
def overwriteOnly {κ ρ α : Type} (encode : α → ρ) (state : κ × ρ) (input : α) : κ × ρ :=
  (state.1, encode input)

theorem overwrite_run_last {κ ρ α : Type} (encode : α → ρ) (state : κ × ρ)
    (earlier : List α) (last : α) :
    (earlier ++ [last]).foldl (overwriteOnly encode) state =
      overwriteOnly encode state last := by
  induction earlier generalizing state with
  | nil => rfl
  | cons head rest ih =>
    simp only [List.cons_append, List.foldl_cons]
    rw [ih]
    rfl

/-- Independent consumer selectors read only untouched context. Active and
inactive consumers therefore coalesce without an all-idle assumption. The
Scheme checker must establish that context/write separation; it is not assumed
sound merely because this conditional transfer theorem is proven. -/
def selectiveOverwrite {κ ρ α : Type} (enabled : κ → Bool) (encode : κ → α → ρ)
    (state : κ × ρ) (input : α) : κ × ρ :=
  (state.1, if enabled state.1 then encode state.1 input else state.2)

theorem selective_run_last {κ ρ α : Type} (enabled : κ → Bool) (encode : κ → α → ρ)
    (state : κ × ρ) (earlier : List α) (last : α) :
    (earlier ++ [last]).foldl (selectiveOverwrite enabled encode) state =
      selectiveOverwrite enabled encode state last := by
  induction earlier generalizing state with
  | nil => rfl
  | cons head rest ih =>
    simp only [List.cons_append, List.foldl_cons]
    rw [ih]
    cases h : enabled state.1 <;> simp [selectiveOverwrite, h]

/-- Two independent consumers share one input traversal; neither needs the
other to be idle. This product theorem does not authorize shared write cells. -/
def independentConsumerStep {κ ρ τ α : Type}
    (first : κ → Bool) (firstEncode : κ → α → ρ)
    (second : κ → Bool) (secondEncode : κ → α → τ)
    (state : (κ × ρ) × (κ × τ)) (input : α) : (κ × ρ) × (κ × τ) :=
  (selectiveOverwrite first firstEncode state.1 input,
   selectiveOverwrite second secondEncode state.2 input)

theorem independent_consumer_run_last {κ ρ τ α : Type}
    (first : κ → Bool) (firstEncode : κ → α → ρ)
    (second : κ → Bool) (secondEncode : κ → α → τ)
    (state : (κ × ρ) × (κ × τ)) (earlier : List α) (last : α) :
    (earlier ++ [last]).foldl
      (independentConsumerStep first firstEncode second secondEncode) state =
      independentConsumerStep first firstEncode second secondEncode state last := by
  induction earlier generalizing state with
  | nil => rfl
  | cons head rest ih =>
    simp only [List.cons_append, List.foldl_cons]
    rw [ih]
    apply Prod.ext
    · simpa [independentConsumerStep] using
        (selective_run_last first firstEncode state.1 [head] last)
    · simpa [independentConsumerStep] using
        (selective_run_last second secondEncode state.2 [head] last)

/-- A scanner returns an ordinary prefix and leaves the first trigger for
the original transition. Refinement includes that residual transition;
dropping it would preserve final overwrite cells but lose observations. -/
theorem overwrite_run_residual {κ ρ α : Type} (encode : α → ρ)
    (state : κ × ρ) (earlier : List α) (last : α)
    (residual : κ × ρ → κ × ρ) :
    residual ((earlier ++ [last]).foldl (overwriteOnly encode) state) =
      residual (overwriteOnly encode state last) := by
  rw [overwrite_run_last]

/-- The source partition is contiguous: scanning a run never consumes the
trigger at its limit. Actual byte membership and alias admission are Scheme
and complete AOT differential obligations, not consequences of this theorem. -/
theorem run_partition (start stop limit : Nat)
    (lower : start ≤ stop) (upper : stop ≤ limit) :
    stop - start + (limit - stop) = limit - start := by omega

/-- Pure contract of the bounded leaf: split at the first trigger, not after
it. Actual C byte classification and bounds remain native AOT obligations. -/
def ordinaryPrefix (trigger : α → Bool) : List α → List α × List α
  | [] => ([], [])
  | head :: tail =>
    if trigger head then ([], head :: tail)
    else let rest := ordinaryPrefix trigger tail
         (head :: rest.1, rest.2)

theorem ordinary_prefix_partition (trigger : α → Bool) (source : List α) :
    (ordinaryPrefix trigger source).1 ++ (ordinaryPrefix trigger source).2 = source := by
  induction source with
  | nil => rfl
  | cons head tail ih =>
    cases h : trigger head <;> simp [ordinaryPrefix, h, ih]

theorem ordinary_prefix_leaves_trigger (trigger : α → Bool) (source : List α) :
    match (ordinaryPrefix trigger source).2 with
    | [] => True
    | head :: _ => trigger head = true := by
  induction source with
  | nil => trivial
  | cons head tail ih =>
    cases h : trigger head <;> simp [ordinaryPrefix, h, ih]

/-- Environment projection has a different obligation from run coalescing.
This concrete expression fragment models reads and ordered composition;
compiler name resolution and effects still require complete AOT controls. -/
inductive EnvironmentExpr (name value : Type) where
  | literal : value → EnvironmentExpr name value
  | read : name → EnvironmentExpr name value
  | combine : (value → value → value) → EnvironmentExpr name value →
      EnvironmentExpr name value → EnvironmentExpr name value

def EnvironmentExpr.evaluate (env : name → value) : EnvironmentExpr name value → value
  | .literal v => v
  | .read n => env n
  | .combine op left right => op (left.evaluate env) (right.evaluate env)

def EnvironmentExpr.reads : EnvironmentExpr name value → List name
  | .literal _ => []
  | .read n => [n]
  | .combine _ left right => left.reads ++ right.reads

theorem EnvironmentExpr.project_required (expr : EnvironmentExpr name value)
    (original projected : name → value)
    (same : ∀ n ∈ expr.reads, original n = projected n) :
    expr.evaluate original = expr.evaluate projected := by
  induction expr with
  | literal v => rfl
  | read n => exact same n (by simp [reads])
  | combine op left right ihLeft ihRight =>
    simp only [evaluate]
    rw [ihLeft (fun n hn => same n (by simp [reads, hn])),
        ihRight (fun n hn => same n (by simp [reads, hn]))]

/-- Ordinary reset/increment machines have a constant-size run summary.
No event is published inside this fragment; the residual observes the final
counter after the owner commits it. Exact byte eligibility is an AOT duty. -/
def runCountStep (count : Nat) (member : Bool) : Nat :=
  if member then count + 1 else 0

def countSummaryStep (summary : Bool × Nat) (member : Bool) : Bool × Nat :=
  if member then (summary.1, summary.2 + 1) else (false, 0)

def applyCountSummary (summary : Bool × Nat) (initial : Nat) : Nat :=
  (if summary.1 then initial else 0) + summary.2

theorem count_summary_step (summary : Bool × Nat) (initial : Nat) (member : Bool) :
    applyCountSummary (countSummaryStep summary member) initial =
      runCountStep (applyCountSummary summary initial) member := by
  cases member <;> simp [countSummaryStep, applyCountSummary, runCountStep, Nat.add_assoc]

theorem count_summary_fold (members : List Bool) (summary : Bool × Nat) (initial : Nat) :
    applyCountSummary (members.foldl countSummaryStep summary) initial =
      members.foldl runCountStep (applyCountSummary summary initial) := by
  induction members generalizing summary with
  | nil => rfl
  | cons head tail ih =>
    simp only [List.foldl_cons]
    rw [ih, count_summary_step]

theorem count_summary_identity (members : List Bool) (initial : Nat) :
    applyCountSummary (members.foldl countSummaryStep (true, 0)) initial =
      members.foldl runCountStep initial := by
  simpa [applyCountSummary] using count_summary_fold members (true, 0) initial

end ParserEngine

#print axioms ParserEngine.count_summary_identity

#print axioms ParserEngine.overwrite_run_residual
#print axioms ParserEngine.run_partition
#print axioms ParserEngine.ordinary_prefix_partition
#print axioms ParserEngine.ordinary_prefix_leaves_trigger
#print axioms ParserEngine.EnvironmentExpr.project_required

#print axioms ParserEngine.shared_probe_exact
#print axioms ParserEngine.shared_probe_visit_bound
#print axioms ParserEngine.shared_probe_consumers
#print axioms ParserEngine.necessary_byte_guard
#print axioms ParserEngine.ordered_builder_contract
#print axioms ParserEngine.necessary_choice_guard
#print axioms ParserEngine.library_choice_cannot_replace_runtime_gate
#print axioms ParserEngine.validated_loop_byte
#print axioms ParserEngine.source_frame_change_blocks_shared_probe
#print axioms ParserEngine.bound_write_blocks_shared_probe
#print axioms ParserEngine.opaque_call_blocks_shared_probe
#print axioms ParserEngine.interned_code_does_not_reduce_scans
#print axioms ParserEngine.dominating_scan_visit_saving
#print axioms ParserEngine.byte_guard_preserves_query_trace
#print axioms ParserEngine.moving_query_is_observable
#print axioms ParserEngine.mismatching_byte_eliminates_state
#print axioms ParserEngine.ordered_region_refinement
#print axioms ParserEngine.condition_normalization_not_required
#print axioms ParserEngine.condition_preserves_ordered_trace
#print axioms ParserEngine.boolean_publication_cannot_be_erased
#print axioms ParserEngine.guarded_idle_refinement
#print axioms ParserEngine.zero_mode_guard
#print axioms ParserEngine.consumer_false_positive_zero
#print axioms ParserEngine.consumer_zero_support
#print axioms ParserEngine.consumer_disjoint_write
#print axioms ParserEngine.effect_only_owner_threading
#print axioms ParserEngine.clamped_forward_range
#print axioms ParserEngine.discarded_pure_condition
#print axioms ParserEngine.overwrite_run_last
#print axioms ParserEngine.selective_run_last
#print axioms ParserEngine.independent_consumer_run_last
