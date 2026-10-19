import ParserEngine.LoweringCallStack

/-! Executable controls for the general allocator/stack theorems. They are not
the proof of arbitrary depth, collisions, reference stability or origin admission. -/
namespace ParserEngine.LoweringCallStackControls
open Lowering LoweringBranches LoweringNameStore LoweringScope LoweringCallStack

def rules : Rules := [(0, .optional .empty)]
def context : LoweringBranches.Context rules (.optional .empty) :=
  authoredContext (name := 0) (expression := .optional .empty) (by simp [rules])
def helper : LoweringBranches.Emission rules :=
  LoweringBranches.emit (identityContext rules .empty)
    (step (by simp [Names.schemaRows]) (identity rules []))
def parent : LoweringBranches.Emission rules :=
  LoweringBranches.emit context (step (by simp [Names.schemaRows])
    (identity rules [body .empty]))

/-- The inner helper shadows the outer lexical slot. The parent deliberately
uses the outer captured value both before and after the inner return. -/
def code : Code [0, 1] rules [] :=
  .bind 0 (.expression .empty)
    (.call .here (.emit helper .here .nil .done)
      (.bind 0 (.expression .empty)
        (.call .here (.emit helper .here .nil .done)
          (.emit parent (.authored 0 (by decide)) (.nonterminal (.there .here) .nil) .done))
        (.emit parent (.authored 0 (by decide)) (.nonterminal .here .nil) .done)))
    .done

def execution := execute (fun _ index => index) (fun _ _ _ equal => equal) code (initial [0, 1]) .nil

theorem exact_physical : emissions execution.events =
    [⟨2, []⟩, ⟨3, []⟩, ⟨0, [.nonterminal 2]⟩, ⟨0, [.nonterminal 2]⟩] := by decide_cbv

theorem final_counter : execution.after.names.attempt = 4 := by decide_cbv

theorem same_owner_distinct_names : decode execution.after.names 2 = .expression .empty ∧
    decode execution.after.names 3 = .expression .empty := by decide_cbv

def eventKind : Event → String
  | .claim _ _ _ _ => "claim"
  | .declare _ _ => "declare"
  | .enter _ => "enter"
  | .leave _ => "return"
  | .emit _ => "emit"

theorem returns_before_parent : execution.events.map eventKind =
    ["claim", "declare", "enter", "emit", "return", "claim", "declare", "enter", "emit", "return", "emit", "emit"] := by decide_cbv

theorem authored_requests : allocationRequests execution.events =
    [(0, .expression .empty), (0, .expression .empty)] := by decide_cbv

theorem ordered_callbacks : (declarations execution.events).map Prod.fst = [2, 3] := by decide_cbv

theorem origin_admitted : Names.admitNative rules (decode execution.after.names)
    (emissions execution.events) ((erase code).map LoweringBranches.Emission.recipe) = true :=
  admitted code (initial [0, 1]) execution

end ParserEngine.LoweringCallStackControls
