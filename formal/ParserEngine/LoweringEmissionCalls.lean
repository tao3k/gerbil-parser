import ParserEngine.LoweringEmitClosure

/-! Arbitrary unobserved recursive callers execute the real local/root emitter
chain. Counters, record contents and writes are generated from source execution,
not supplied as alignment predicates or final-state hypotheses. -/
namespace ParserEngine.LoweringEmissionCalls
open WalkSource SchemeTableEvaluation LoweringProductionEvaluation LoweringEmitterEvaluation

structure Request where
  context : LoweringEmitterEvaluation.Context
  value : Payload
  isolated : Bool

inductive Calls (env : Environment) (base : Nat) : Heap → List Request → Heap → Prop where
  | nil : Calls env base before [] before
  | cons (head : Invoke before (LoweringEmitClosure.function env base request.context request.value request.isolated)
      [.symbol request.value.lhs, .list request.value.rhs, datumValue request.value.action,
        datumValue request.value.precedence, .boolean false] .unit middle)
      (tail : Calls env base middle requests after) : Calls env base before (request :: requests) after

def records (next : Nat) : List Request → List Value
  | [] => []
  | request :: rest => record next request.value :: records (next + 1) rest

def recordId : Value → Option Nat
  | .list (.natural next :: _) => some next
  | _ => none

/-- Recursive helper/body/continuation partitions keep the same source order.
Each suffix starts from its predecessor's actual heap, never a reset counter. -/
theorem Calls.append (first : Calls env base before requests middle)
    (last : Calls env base middle suffix after) : Calls env base before (requests ++ suffix) after := by
  induction first with
  | nil => exact last
  | cons head tail ih => exact .cons head (ih last)

theorem records_append (next : Nat) (first last : List Request) :
    records next (first ++ last) = records next first ++ records (next + first.length) last := by
  induction first generalizing next with
  | nil => simp [records]
  | cons request rest ih =>
      simp [records, ih, Nat.add_comm, Nat.add_left_comm]

theorem record_ids (next : Nat) (requests : List Request) :
    (records next requests).map recordId = (List.range' next requests.length).map some := by
  induction requests generalizing next with
  | nil => rfl
  | cons request rest ih => simpa [records, recordId, record, List.range'_succ] using congrArg (List.cons (some next)) (ih (next + 1))

/-- Every finite request list, arbitrary initial counter and nonempty prefix.
All constructor fields and both root-cell writes are derived by the real
closure invocation. Other owned stores are preserved by a general frame law. -/
theorem source_history (requests : List Request) (next : Nat)
    (listPrimitive : env "list" = none) (consPrimitive : env "cons" = none) (plusPrimitive : env "+" = none)
    (count : before base 0 = some (.natural next))
    (contents : before (base + 4) 0 = some (.list previous)) :
    ∃ after, Calls env base before requests after ∧
      after base 0 = some (.natural (next + requests.length)) ∧
      after (base + 4) 0 = some (.list ((records next requests).reverse ++ previous)) ∧
      ∀ address key, (address ≠ base ∨ key ≠ .index 0) →
        (address ≠ base + 4 ∨ key ≠ .index 0) → after address key = before address key := by
  induction requests generalizing before next previous with
  | nil => exact ⟨before, .nil, by simpa using count, by simpa [records] using contents, fun _ _ _ _ => rfl⟩
  | cons request rest ih =>
    let middle := committed before base next previous request.value
    have emitted := LoweringEmitClosure.invoke next listPrimitive consPrimitive plusPrimitive count contents
      request.context request.value request.isolated
    have nextCount : middle base 0 = some (.natural (next + 1)) := committed_counter next
    have nextRecords : middle (base + 4) 0 = some (.list (record next request.value :: previous)) := committed_records next
    obtain ⟨after, calls, count, contents, frame⟩ := ih (next + 1) nextCount nextRecords
    refine ⟨after, .cons emitted calls, ?_, ?_, ?_⟩
    · simpa [List.length_cons, Nat.add_assoc, Nat.add_comm, Nat.add_left_comm] using count
    · simpa [records, List.reverse_cons, List.append_assoc] using contents
    · intro address key counter productions
      exact (frame address key counter productions).trans (committed_frame next counter productions)

theorem Calls.deterministic (first : Calls env base before requests after)
    (second : Calls env base before requests last) : after = last := by
  induction first generalizing last with
  | nil => cases second; rfl
  | cons head tail ih =>
      cases second with
      | cons other rest =>
          obtain ⟨_, rfl⟩ := head.deterministic other
          exact ih rest

/-- All returning physical call streams, not just the witness constructed by
source_history, have this complete counter and record-list correspondence. -/
theorem every_return (run : Calls env base before requests after) (next : Nat)
    (listPrimitive : env "list" = none) (consPrimitive : env "cons" = none) (plusPrimitive : env "+" = none)
    (count : before base 0 = some (.natural next))
    (contents : before (base + 4) 0 = some (.list previous)) :
    after base 0 = some (.natural (next + requests.length)) ∧
      after (base + 4) 0 = some (.list ((records next requests).reverse ++ previous)) := by
  obtain ⟨generated, calls, count, contents, _⟩ := source_history requests next listPrimitive consPrimitive plusPrimitive count contents
  have same := run.deterministic calls
  cases same
  exact ⟨count, contents⟩

end ParserEngine.LoweringEmissionCalls