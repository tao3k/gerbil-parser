import ParserEngine.LRStateSemantics
import ParserEngine.LookaheadExecution

/-! Compose generated lookahead reachability with paper prediction and goto.
Coordinates are producer obligations: actual LR0 graph/name/key extraction is
not silently reclassified as a host assumption or a completed native proof. -/
namespace ParserEngine.PropagationSemantics
open Grammar Viable
abbrev Machine := LookaheadExecution.Machine.Graph

inductive PaperTrace (g : CFG) (transition : Nat → Symbol → Nat → Prop)
    (initial : Nat → Item → Prop) : Nat → Item → Prop where
  | seed {state item} (member : initial state item) : PaperTrace g transition initial state item
  | predict {state parent child} (known : PaperTrace g transition initial state parent)
      (step : Predicts g parent child) : PaperTrace g transition initial state child
  | shift {state target before after symbol} (known : PaperTrace g transition initial state before)
      (edge : transition state symbol target) (step : Advances before symbol after) :
      PaperTrace g transition initial target after

structure Coordinates (g : CFG) (graph : Machine) (transition : Nat → Symbol → Nat → Prop) where
  state : Nat → Nat
  core : Nat → Core
  suffix : Nat → List Symbol
  key : Nat → Option Nat
  facts : SuffixSemantics.Facts
  factsExact : SuffixSemantics.Exact g facts
  firstRow : ∀ node, graph.first node = (SuffixSemantics.scan facts (suffix node)).first
  nullableRow : ∀ node, graph.nullable node = (SuffixSemantics.scan facts (suffix node)).nullable
  terminalKey : ∀ node bit, (graph.first node).testBit bit = true → key bit = some bit
  closureShape : ∀ source target, target ∈ graph.closure source →
    ∃ name, (core source).production.rhs.drop (core source).dot = .nonterminal name :: suffix source ∧
      (core target).production ∈ g ∧ (core target).production.lhs = name ∧
      (core target).dot = 0 ∧ state target = state source
  shiftShape : ∀ source target, graph.shift source = some target →
    ∃ symbol, advance (core source) symbol (core target) ∧
      transition (state source) symbol (state target)

def label {g graph transition} (coordinates : Coordinates g graph transition) (node bit : Nat) : Item :=
  ⟨(coordinates.core node).production, (coordinates.core node).dot, coordinates.key bit⟩

theorem emitted_supported {g graph transition initial}
    (coordinates : Coordinates g graph transition) {source bit target terminal : Nat}
    (known : PaperTrace g transition initial (coordinates.state source) (label coordinates source bit))
    (emitted : LookaheadExecution.Machine.Emits graph source bit target terminal) :
    PaperTrace g transition initial (coordinates.state target) (label coordinates target terminal) := by
  cases emitted with
  | shift edge =>
    obtain ⟨symbol, ⟨shape, read⟩, transitionEdge⟩ := coordinates.shiftShape source target edge
    apply PaperTrace.shift known transitionEdge
    refine ⟨read, ?_⟩
    simp only [label]
    rw [shape]
  | follow member empty =>
    obtain ⟨name, next, production, lhs, zero, stateEqual⟩ := coordinates.closureShape source target member
    have nullable : Nullable g (coordinates.suffix source) := by
      rw [coordinates.nullableRow] at empty
      exact (SuffixSemantics.scan_exact coordinates.factsExact _).2.mp empty
    have prediction : Predicts g (label coordinates source bit) (label coordinates target bit) :=
      ⟨name, coordinates.suffix source, next, production, lhs, zero, Or.inr ⟨nullable, rfl⟩⟩
    rw [stateEqual]
    exact PaperTrace.predict known prediction
  | first member present =>
    obtain ⟨name, next, production, lhs, zero, stateEqual⟩ := coordinates.closureShape source target member
    have decoded := coordinates.terminalKey source terminal present
    rw [coordinates.firstRow] at present
    have first := ((SuffixSemantics.scan_exact coordinates.factsExact _).1 terminal).mp present
    have prediction : Predicts g (label coordinates source bit) (label coordinates target terminal) :=
      ⟨name, coordinates.suffix source, next, production, lhs, zero, Or.inl ⟨terminal, decoded, first⟩⟩
    rw [stateEqual]
    exact PaperTrace.predict known prediction

/-- All finite propagation ancestry, including cycles and transitive first
activation, has a paper prediction/goto history under the coordinate adapter. -/
theorem reach_supported {g graph transition initial}
    (coordinates : Coordinates g graph transition) (seedNode seedMask : Nat)
    (seed : ∀ bit, seedMask.testBit bit = true →
      initial (coordinates.state seedNode) (label coordinates seedNode bit))
    {node bit : Nat} (reached : LookaheadExecution.Machine.Reach graph seedNode seedMask node bit) :
    PaperTrace g transition initial (coordinates.state node) (label coordinates node bit) := by
  induction reached with
  | seed member => exact PaperTrace.seed (seed _ member)
  | emit earlier emitted ih => exact emitted_supported coordinates ih emitted

/-- No externally supplied fixed-point soundness certificate: successful FIFO
replay and exhaustion are interpreted by the existing operational solver proof. -/
theorem replay_supported {g graph transition initial}
    (coordinates : Coordinates g graph transition) (seedNode seedMask : Nat)
    (seed : ∀ bit, seedMask.testBit bit = true →
      initial (coordinates.state seedNode) (label coordinates seedNode bit))
    (witness : List Nat) (after : LookaheadExecution.Machine.State)
    (checked : LookaheadExecution.Machine.replay graph
      (LookaheadExecution.Machine.initial seedNode seedMask) witness = some after)
    (finished : after.queue.work = []) {node bit : Nat}
    (present : (after.queue.cells node).known.testBit bit = true) :
    PaperTrace g transition initial (coordinates.state node) (label coordinates node bit) := by
  apply reach_supported coordinates seedNode seedMask seed
  exact (LookaheadExecution.Machine.checked_quiescent graph seedNode seedMask witness after
    checked finished node bit).mp present

/-- The compiled graph reads core+1 from the generated physical suffix rows.
Both semantic directions are derived, rather than supplied in Coordinates. -/
theorem compiled_suffix (facts : SuffixSemantics.Facts) (before after : CFG) (rule : Production)
    (admitted : SuffixSemantics.Exact (before ++ rule :: after) facts)
    (input : LookaheadExecution.Graph.Input) (graph : Machine)
    (generated : LookaheadExecution.Graph.compile input
      (fun code => (SuffixSemantics.readPacked facts (before ++ rule :: after) code).first)
      (fun code => (SuffixSemantics.readPacked facts (before ++ rule :: after) code).nullable) = some graph)
    (index state dot : Nat) (next : dot < rule.rhs.length)
    (coordinate : input.nodes[index]? = some (state,
      denseOffset (before.map (fun p => p.rhs.length)) before.length + dot)) :
    (∀ token, (graph.first index).testBit token = true ↔ First (before ++ rule :: after)
      (rule.rhs.drop (dot + 1)) token) ∧
      (graph.nullable index = true ↔ Nullable (before ++ rule :: after) (rule.rhs.drop (dot + 1))) := by
  obtain ⟨row, built, closures, shifts, firstRow, nullableRow⟩ :=
    LookaheadExecution.Graph.compiled_node input _ _ graph generated index state _ coordinate
  have read := SuffixSemantics.after_dot_read facts before after rule dot next
  constructor
  · intro token
    rw [firstRow, read]
    exact (SuffixSemantics.scan_exact admitted _).1 token
  · rw [nullableRow, read]
    exact (SuffixSemantics.scan_exact admitted _).2

/-- Complete source-shaped graph construction and FIFO witness checking feed
paper provenance without requiring a caller-provided semantic execution trace. -/
theorem extracted_supported {g graph transition initial}
    (coordinates : Coordinates g graph transition)
    (searchSource graphSource scheduleSource : WalkSource.Datum) (input : LookaheadExecution.Graph.Input)
    (suffixFirst : Nat → Nat) (suffixNullable : Nat → Bool) (seedNode seedMask : Nat)
    (seed : ∀ bit, seedMask.testBit bit = true →
      initial (coordinates.state seedNode) (label coordinates seedNode bit))
    (dequeues : List Nat) (state : LookaheadExecution.Machine.State)
    (checked : LookaheadExecution.Admission.execute searchSource graphSource scheduleSource input
      suffixFirst suffixNullable seedNode seedMask dequeues = some (graph, state))
    (finished : state.queue.work = []) {node bit : Nat}
    (present : (state.queue.cells node).known.testBit bit = true) :
    PaperTrace g transition initial (coordinates.state node) (label coordinates node bit) := by
  have reached := (LookaheadExecution.Admission.extracted_quiescent searchSource graphSource
    scheduleSource input suffixFirst suffixNullable seedNode seedMask dequeues graph state checked finished).2
  exact reach_supported coordinates seedNode seedMask seed ((reached node bit).mp present)

end ParserEngine.PropagationSemantics
