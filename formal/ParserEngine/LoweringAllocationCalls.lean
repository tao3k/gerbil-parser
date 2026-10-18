import ParserEngine.LoweringFreshEvaluation
import ParserEngine.LoweringNameStore

/-! Composition across arbitrary source allocation calls. Shared lexical cells
are re-read after every return; new source calls consume the previous heap,
not an independently reset or caller-supplied counter snapshot. -/
namespace ParserEngine.LoweringAllocationCalls
open WalkSource SchemeTableEvaluation LoweringFreshEvaluation

def committed (heap : Heap) (counter table : Nat) (owner : String) (index : Nat) : Heap :=
  writeSymbol (write heap counter 0 (.natural (index + 1))) table (candidate owner index) (.boolean true)

theorem committed_counter : committed heap counter table owner index counter 0 = some (.natural (index + 1)) := by
  simp [committed, writeSymbol, write]

theorem committed_occupied (valid : Occupied heap table used) :
    Occupied (committed heap counter table owner index) table (candidate owner index :: used) := by
  intro name
  by_cases same : name = candidate owner index
  · subst name; simp [committed, writeSymbol]
  · simpa [committed, writeSymbol, write, same] using valid name

/-- Allocation writes only the captured counter and the claimed table key. -/
theorem committed_frame (counterSlot : address ≠ counter ∨ key ≠ .index 0)
    (tableSlot : address ≠ table ∨ key ≠ .symbol (candidate owner index)) :
    committed heap counter table owner index address key = heap address key := by
  simp only [committed, writeSymbol, write]
  split
  · grind
  · split <;> grind

inductive Calls (counter table : Nat) : Heap → List (String × Lowering.Owner) → List String → Heap → Prop where
  | nil : Calls counter table heap [] [] heap
  | cons (head : Eval (outer counter table owner) before (app "fresh" [s "owner"]) (.symbol name) middle)
      (tail : Calls counter table middle requests names after) :
      Calls counter table before ((owner, logicalOwner) :: requests) (name :: names) after

theorem Calls.deterministic (first : Calls counter table heap requests names after)
    (second : Calls counter table heap requests others last) : names = others ∧ after = last := by
  induction first generalizing others last with
  | nil => cases second; exact ⟨rfl, rfl⟩
  | cons head tail ih =>
    cases second with
    | cons other rest =>
      obtain ⟨same, shared⟩ := head.deterministic other
      cases Value.symbol.inj same
      cases shared
      obtain ⟨equalNames, equalHeap⟩ := ih rest
      exact ⟨congrArg (List.cons _) equalNames, equalHeap⟩

/-- An arbitrary request sequence generates its own physical call witnesses
and one composed abstract history. There is no supplied native trace premise. -/
theorem source_history (requests : List (String × Lowering.Owner))
    (valid : Occupied heap table before.used)
    (contents : heap counter 0 = some (.natural before.attempt))
    (injective : ∀ owner a b, candidate owner a = candidate owner b → a = b) :
    ∃ after names last, Calls counter table heap requests names last ∧
      Names.Trace before after ∧ Occupied last table after.used ∧
      last counter 0 = some (.natural after.attempt) ∧
      after.bindings = (names.zip (requests.map Prod.snd)).reverse ++ before.bindings := by
  induction requests generalizing before heap with
  | nil => exact ⟨before, [], heap, .nil, .refl _, valid, contents, rfl⟩
  | cons request rest ih =>
    obtain ⟨index, found⟩ := LoweringNames.search_total (candidate request.1) (injective request.1) before.used before.attempt
    let next := LoweringNames.claimed before (candidate request.1) request.2 index
    let middle := committed heap counter table request.1 index
    have execution := fresh_eval valid contents found
    have history := (LoweringNames.search_trace request.2 found).2.2
    have nextValid : Occupied middle table next.used := committed_occupied valid
    have nextCount : middle counter 0 = some (.natural next.attempt) := committed_counter
    obtain ⟨after, names, last, calls, suffix, occupied, count, bindings⟩ := ih nextValid nextCount
    refine ⟨after, candidate request.1 index :: names, last, .cons execution calls,
      LoweringNameStore.trans history suffix, occupied, count, ?_⟩
    simpa [next, LoweringNames.claimed, List.reverse_cons, List.append_assoc] using bindings

/-- Safety from the paper's global name history applies to all source calls,
including repeated owners, nested-helper callers and occupied candidates. -/
theorem source_unique (requests : List (String × Lowering.Owner))
    (valid : Occupied heap table reserved)
    (contents : heap counter 0 = some (.natural 0))
    (injective : ∀ owner a b, candidate owner a = candidate owner b → a = b) :
    ∃ after names last, Calls counter table heap requests names last ∧
      Names.Trace (Names.initial reserved) after ∧ (after.bindings.map Prod.fst).Nodup ∧
      ∀ binding ∈ after.bindings, binding.1 ∉ reserved := by
  obtain ⟨after, names, last, calls, history, _⟩ :=
    source_history requests (before := Names.initial reserved) valid contents injective
  exact ⟨after, names, last, calls, history, Names.unique_names history, Names.reserved_not_helper history⟩

/-- All returning executions have the generated binding history; the result is
not limited to the particular derivation constructed by source_history. -/
theorem every_return (execution : Calls counter table heap requests names last)
    (valid : Occupied heap table before.used)
    (contents : heap counter 0 = some (.natural before.attempt))
    (injective : ∀ owner a b, candidate owner a = candidate owner b → a = b) :
    ∃ after, Names.Trace before after ∧ Occupied last table after.used ∧
      last counter 0 = some (.natural after.attempt) ∧
      after.bindings = (names.zip (requests.map Prod.snd)).reverse ++ before.bindings := by
  obtain ⟨after, generated, final, calls, history, occupied, count, bindings⟩ :=
    source_history requests valid contents injective
  obtain ⟨rfl, rfl⟩ := execution.deterministic calls
  exact ⟨after, history, occupied, count, bindings⟩

end ParserEngine.LoweringAllocationCalls
