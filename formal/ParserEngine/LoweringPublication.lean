import ParserEngine.ConstructionGrammar
import ParserEngine.CompilerSemanticsSource

/-! The common native emission/publication spine, shared by every recursive
lowering branch. Callback outcomes are explicit and fail before local commit.
Branch choice and helper allocation are separate producer obligations. -/
namespace ParserEngine.LoweringPublication

structure Pending where
  value : ProducerAdmission.Production
  /-- Logical origin witness supplied to qualification; the unobserved native
  branch need not allocate this recipe. Generation is a separate obligation. -/
  recipe : Names.Recipe
  origin : Nat

structure Entry where
  record : ConstructionGrammar.Record
  recipe : Names.Recipe
  origin : Nat

structure State where
  next : Nat
  reverseEntries : List Entry

def initial : State := ⟨1, []⟩

inductive Event where
  | allocate (id origin : Nat)
  | origin (id origin : Nat)
  | proof (id origin : Nat)
  | increment (before after : Nat)
  | store (id : Nat)
  deriving DecidableEq, Repr

structure Observers where
  originEnabled : Bool
  proofEnabled : Bool
  originReturns : Nat → Bool
  proofReturns : Nat → Bool

structure Result where
  state : State
  events : List Event
  returned : Bool

def commit (before : State) (pending : Pending) : State :=
  ⟨before.next + 1, ⟨⟨before.next, pending.value⟩, pending.recipe, pending.origin⟩ :: before.reverseEntries⟩

def emit (observers : Observers) (before : State) (pending : Pending) : Result :=
  let allocated := [Event.allocate before.next pending.origin]
  let origin := if observers.originEnabled then [Event.origin before.next pending.origin] else []
  let proof := if observers.proofEnabled then [Event.proof before.next pending.origin] else []
  if observers.originEnabled && !observers.originReturns before.next then
    ⟨before, allocated ++ origin, false⟩
  else if observers.proofEnabled && !observers.proofReturns before.next then
    ⟨before, allocated ++ origin ++ proof, false⟩
  else ⟨commit before pending, allocated ++ origin ++ proof ++
    [.increment before.next (before.next + 1), .store before.next], true⟩

def run (observers : Observers) : State → List Pending → Result
  | before, [] => ⟨before, [], true⟩
  | before, pending :: rest =>
    let current := emit observers before pending
    if current.returned then
      let later := run observers current.state rest
      ⟨later.state, current.events ++ later.events, later.returned⟩
    else current

def ordered (state : State) : List Entry := state.reverseEntries.reverse

/-- Any recursive declaration/helper partition has the same execution, with
the suffix skipped after failure. No final-state or per-row support premise. -/
theorem run_append (observers : Observers) (before : State) (first rest : List Pending) :
    run observers before (first ++ rest) =
      let prior := run observers before first
      if prior.returned then
        let later := run observers prior.state rest
        ⟨later.state, prior.events ++ later.events, later.returned⟩
      else prior := by
  induction first generalizing before with
  | nil => simp [run]
  | cons head tail ih =>
    cases emitted : (emit observers before head).returned with
    | false => simp [run, emitted]
    | true =>
      rw [List.cons_append, run, emitted]
      simp only [↓reduceIte]
      rw [ih]
      cases suffix : (run observers (emit observers before head).state tail).returned <;>
        simp [run, emitted, suffix, List.append_assoc]

theorem failed_prefix (observers : Observers) (before : State) (first rest : List Pending)
    (failed : (run observers before first).returned = false) :
    run observers before (first ++ rest) = run observers before first := by
  rw [run_append]
  simp [failed]

theorem emitted (observers : Observers) (before : State) (pending : Pending)
    (returned : (emit observers before pending).returned = true) :
    (emit observers before pending).state = commit before pending := by
  by_cases originFailed : (observers.originEnabled && !observers.originReturns before.next) = true
  · simp [emit, originFailed] at returned
  · by_cases proofFailed : (observers.proofEnabled && !observers.proofReturns before.next) = true
    · simp [emit, originFailed, proofFailed] at returned
    · simp [emit, originFailed, proofFailed]

theorem emission_failed (observers : Observers) (before : State) (pending : Pending)
    (failed : (emit observers before pending).returned = false) :
    (emit observers before pending).state = before := by
  by_cases originFailed : (observers.originEnabled && !observers.originReturns before.next) = true
  · simp [emit, originFailed]
  · by_cases proofFailed : (observers.proofEnabled && !observers.proofReturns before.next) = true
    · simp [emit, originFailed, proofFailed]
    · simp [emit, originFailed, proofFailed] at failed

theorem origin_failure (observers : Observers) (before : State) (pending : Pending)
    (enabled : observers.originEnabled = true) (failed : observers.originReturns before.next = false) :
    emit observers before pending =
      ⟨before, [.allocate before.next pending.origin, .origin before.next pending.origin], false⟩ := by
  simp [emit, enabled, failed]

theorem proof_failure (observers : Observers) (before : State) (pending : Pending)
    (origin : observers.originEnabled = false ∨ observers.originReturns before.next = true)
    (enabled : observers.proofEnabled = true) (failed : observers.proofReturns before.next = false) :
    (emit observers before pending).state = before ∧ (emit observers before pending).returned = false := by
  rcases origin with disabled | returned
  · simp [emit, disabled, enabled, failed]
  · simp [emit, returned, enabled, failed]

theorem run_count (observers : Observers) (before : State) (pending : List Pending)
    (returned : (run observers before pending).returned = true) :
    (run observers before pending).state.next = before.next + pending.length := by
  induction pending generalizing before with
  | nil => simp [run]
  | cons head tail ih =>
    cases accepted : (emit observers before head).returned with
    | false => simp [run, accepted] at returned
    | true =>
      have rest : (run observers (emit observers before head).state tail).returned = true := by
        simpa [run, accepted] using returned
      have count := ih _ rest
      simp only [run, accepted, ↓reduceIte]
      rw [count, emitted observers before head accepted]
      simp [commit, Nat.add_comm, Nat.add_left_comm]

def append (before : State) (pending : List Pending) : State := pending.foldl commit before

/-- Forget transient callback steps into the existing atomic emission tape.
Its observer field is a logical origin view, not a claim that disabled native
callbacks execute or allocate recipes. -/
def tape (project : ProducerAdmission.Production → Grammar.Production) (state : State) :
    Names.Tape (Names.Recipe × Nat) :=
  let entries := state.reverseEntries.map fun entry =>
    (⟨entry.record.id, project entry.record.value, (entry.recipe, entry.origin)⟩ : Names.Emission (Names.Recipe × Nat))
  ⟨state.next, entries, entries⟩

theorem tape_commit (project : ProducerAdmission.Production → Grammar.Production)
    (before : State) (pending : Pending) :
    tape project (commit before pending) =
      Names.emit (tape project before) (project pending.value) (pending.recipe, pending.origin) := rfl

theorem tape_append (project : ProducerAdmission.Production → Grammar.Production)
    (before : State) (pending : List Pending) :
    tape project (append before pending) = pending.foldl
      (fun state value => Names.emit state (project value.value) (value.recipe, value.origin)) (tape project before) := by
  induction pending generalizing before with
  | nil => rfl
  | cons head tail ih =>
    change tape project (append (commit before head) tail) = _
    rw [ih, tape_commit]
    rfl

theorem run_state (observers : Observers) (before : State) (pending : List Pending)
    (returned : (run observers before pending).returned = true) :
    (run observers before pending).state = append before pending := by
  induction pending generalizing before with
  | nil => rfl
  | cons head tail ih =>
    cases accepted : (emit observers before head).returned with
    | false => simp [run, accepted] at returned
    | true =>
      have rest : (run observers (emit observers before head).state tail).returned = true := by
        simpa [run, accepted] using returned
      simpa only [run, accepted, ↓reduceIte, append, List.foldl_cons,
        emitted observers before head accepted] using ih _ rest

/-- Physical IDs are assigned by the common emitter, not supplied by a branch. -/
theorem append_ids (before : State) (pending : List Pending) :
    (ordered (append before pending)).map (fun entry => entry.record.id) =
      (ordered before).map (fun entry => entry.record.id) ++ List.range' before.next pending.length := by
  induction pending generalizing before with
  | nil => simp [append]
  | cons head tail ih =>
    change (ordered (append (commit before head) tail)).map _ = _
    rw [ih]
    simp [ordered, commit, List.range'_succ, List.append_assoc]

theorem append_payloads (before : State) (pending : List Pending) :
    (ordered (append before pending)).map (fun entry => entry.record.value) =
      (ordered before).map (fun entry => entry.record.value) ++ pending.map Pending.value ∧
    (ordered (append before pending)).map Entry.recipe =
      (ordered before).map Entry.recipe ++ pending.map Pending.recipe := by
  induction pending generalizing before with
  | nil => simp [append]
  | cons head tail ih =>
    change (ordered (append (commit before head) tail)).map _ = _ ∧ _
    obtain ⟨values, recipes⟩ := ih (commit before head)
    simpa [ordered, commit, append, List.append_assoc] using And.intro values recipes

structure Metadata where
  augmented : Nat
  root : Nat
  source : Nat
  rules : Lowering.Rules
  decode : Nat → Lowering.Owner
  declarations : List Nat

def input (metadata : Metadata) (state : State) : ConstructionGrammar.Input :=
  ⟨⟨0, ⟨metadata.augmented, [⟨.nonterminal metadata.root, []⟩], .ordinary 0⟩⟩ ::
    (ordered state).map Entry.record, metadata.augmented, metadata.root, metadata.source,
    metadata.rules, metadata.decode, (ordered state).map Entry.recipe, metadata.declarations⟩

def publish (metadata : Metadata) (result : Result) : Option ConstructionGrammar.Input :=
  if result.returned then some (input metadata result.state) else none

theorem failed_not_published (metadata : Metadata) (result : Result) (failed : result.returned = false) :
    publish metadata result = none := by simp [publish, failed]

theorem successful_ids (metadata : Metadata) (observers : Observers) (pending : List Pending)
    (returned : (run observers initial pending).returned = true) :
    (input metadata (run observers initial pending).state).records.map ConstructionGrammar.Record.id =
      List.range (pending.length + 1) := by
  rw [run_state observers initial pending returned]
  simp only [input, List.map_cons, List.map_map, Function.comp_def]
  rw [append_ids]
  simp [initial, ordered, List.range_eq_range', List.range'_succ]

theorem successful_payloads (metadata : Metadata) (observers : Observers) (pending : List Pending)
    (returned : (run observers initial pending).returned = true) :
    (ConstructionGrammar.productions (input metadata (run observers initial pending).state)).tail =
      pending.map Pending.value ∧
    (input metadata (run observers initial pending).state).recipes = pending.map Pending.recipe := by
  have pairs := append_payloads initial pending
  rw [run_state observers initial pending returned]
  simpa [input, ConstructionGrammar.productions, initial, ordered, List.map_map, Function.comp_def] using pairs

theorem dense_ids (metadata : Metadata) (observers : Observers) (pending : List Pending)
    (returned : (run observers initial pending).returned = true) :
    let published := input metadata (run observers initial pending).state
    published.records.map ConstructionGrammar.Record.id = List.range published.records.length := by
  have ids := successful_ids metadata observers pending returned
  have length := congrArg List.length ids
  simp only [List.length_map, List.length_range] at length
  simpa only [length] using ids

namespace Source
open WalkSource NativeActions.Source CompilerSemanticsSource

def emitter : Datum := defn "emit-production"
  ["lhs", "rhs", "action", "precedence", "owner", "path", "expression", "proof"] [
  app "let" [binding "production" (call "make-production" ["next-id", "lhs", "rhs", "action", "precedence"]),
    app "when" [s "observe-origin", app "observe-origin" [s "production", s "owner", app "reverse" [s "path"], s "expression"]],
    app "when" [s "observe-proof", app "observe-proof" [app "quote" [s "production"], s "production", s "proof"]],
    app "set!" [s "next-id", app "+" [s "next-id", n 1]],
    app "set!" [s "productions", call "cons" ["production", "productions"]]]]

def initializer : Datum := l [binding "next-id" (n 1), binding "next-synthetic" (n 0),
  binding "used-names" (app "make-table" [.keyword "test", s "eq?"]),
  binding "rule-counts" (app "make-table" [.keyword "test", s "eq?"]),
  binding "productions" emptyList]

def constructor : Datum := defn "make-production" ["id", "lhs", "rhs", "action", "precedence"]
  [call "list" ["id", "lhs", "rhs", "action", "precedence"]]

def finalizer : Datum := app "cons" [
  app "make-production" [n 0, app "or" [app "claim-name" [app "quote" [s "$accept"]],
    app "fresh" [app "quote" [s "accept"]]],
    app "list" [app "list" [app "quote" [s "nonterminal"], s "root"]], app "quote" [s "pass"], b false],
  app "reverse" [s "productions"]]

end Source

/-- Publication feeds the existing retained LeanPoo construction, including its
semantic origin gate. A callback failure cannot yield a partial successful CFG. -/
structure Sources where
  emitter : WalkSource.Datum
  constructor : WalkSource.Datum
  initializer : WalkSource.Datum
  finalizer : WalkSource.Datum

def admitted (sources : Sources) : Bool := decide (sources.emitter = Source.emitter ∧ sources.constructor = Source.constructor ∧
  sources.initializer = Source.initializer ∧ sources.finalizer = Source.finalizer)

def construct (lowering : Sources) (sources : ConstructionConsumer.Sources) (metadata : Metadata) (observers : Observers)
    (pending : List Pending) (traversal : CachedConstruction.Traversal) :
    Option (Σ input, ConstructionGrammar.Output input traversal) :=
  if admitted lowering then
    (publish metadata (run observers initial pending)).bind fun input =>
      (ConstructionGrammar.construct sources input traversal).map fun output => ⟨input, output⟩
  else none

theorem source_rejected (lowering : Sources) (sources : ConstructionConsumer.Sources) (metadata : Metadata)
    (observers : Observers) (pending : List Pending) (traversal : CachedConstruction.Traversal)
    (failed : admitted lowering = false) : construct lowering sources metadata observers pending traversal = none := by
  simp [construct, failed]

theorem failed_not_constructed (lowering : Sources) (sources : ConstructionConsumer.Sources) (metadata : Metadata)
    (observers : Observers) (pending : List Pending) (traversal : CachedConstruction.Traversal)
    (failed : (run observers initial pending).returned = false) :
    construct lowering sources metadata observers pending traversal = none := by
  simp [construct, failed_not_published metadata _ failed]

theorem constructed (lowering : Sources) (sources : ConstructionConsumer.Sources) (metadata : Metadata)
    (observers : Observers) (pending : List Pending) (traversal : CachedConstruction.Traversal)
    (output : Σ input, ConstructionGrammar.Output input traversal)
    (built : construct lowering sources metadata observers pending traversal = some output) :
    admitted lowering = true ∧ (run observers initial pending).returned = true ∧
    output.1 = input metadata (run observers initial pending).state ∧
    ConstructionGrammar.construct sources output.1 traversal = some output.2 := by
  unfold construct at built
  split at built
  next allowed =>
    obtain ⟨value, published, rest⟩ := Option.bind_eq_some_iff.mp built
    obtain ⟨product, made, equal⟩ := Option.map_eq_some_iff.mp rest
    subst output
    unfold publish at published
    split at published
    next returned =>
      have identity := Option.some.inj published
      exact ⟨allowed, returned, identity.symm, made⟩
    next failed => cases published
  next denied => cases built

theorem constructed_fields (lowering : Sources) (sources : ConstructionConsumer.Sources) (metadata : Metadata)
    (observers : Observers) (pending : List Pending) (traversal : CachedConstruction.Traversal)
    (output : Σ input, ConstructionGrammar.Output input traversal)
    (built : construct lowering sources metadata observers pending traversal = some output) :
    output.1.records.map ConstructionGrammar.Record.id = List.range output.1.records.length ∧
    (ConstructionGrammar.productions output.1).tail = pending.map Pending.value ∧
    output.1.recipes = pending.map Pending.recipe := by
  obtain ⟨_, returned, identity, _⟩ := constructed lowering sources metadata observers pending traversal output built
  rw [identity]
  exact ⟨dense_ids metadata observers pending returned, successful_payloads metadata observers pending returned⟩

/-- Successful publication reaches the exact existing source FIFO factory;
all callback, ID and payload laws concern this same returned product. -/
theorem complete (lowering : Sources) (sources : ConstructionConsumer.Sources) (metadata : Metadata)
    (observers : Observers) (pending : List Pending) (traversal : CachedConstruction.Traversal)
    (output : Σ input, ConstructionGrammar.Output input traversal)
    (built : construct lowering sources metadata observers pending traversal = some output) :
    ConstructionFIFO.executeSource sources.builder sources.growth sources.append sources.template
      (ConstructionGrammar.grammar output.1) (ConstructionGrammar.nonempty output.1 output.2.receipt) traversal =
      some ((output.2.product.construction.1.machine, output.2.product.construction.2), output.2.product.actions) ∧
    ConstructionReadiness.Complete (ConstructionGrammar.grammar output.1) traversal output.2.product.layout ∧
    ConstructionReadiness.Visit (ConstructionGrammar.grammar output.1) traversal output.2.product.layout
      output.2.product.construction.1.machine output.2.product.construction.2 :=
  ConstructionGrammar.complete sources output.1 traversal output.2 (constructed lowering sources metadata observers pending traversal output built).2.2.2


end ParserEngine.LoweringPublication
