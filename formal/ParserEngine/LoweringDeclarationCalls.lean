import ParserEngine.LoweringAllocationCalls
import ParserEngine.LoweringDeclarationEvaluation

/-! Allocation and declaration must alternate in the same heap. Collecting
callbacks in a later independent replay does not establish this invariant.
The remaining dispatcher proof must generate these request lists from source. -/
namespace ParserEngine.LoweringDeclarationCalls
open WalkSource SchemeTableEvaluation LoweringFreshEvaluation LoweringAllocationCalls

inductive Calls (scope : Scope) (captured : Environment) (address : Nat) :
    Heap → List (String × Lowering.Owner) → List String → Heap → Prop where
  | nil : Calls scope captured address heap [] [] heap
  | cons (fresh : Eval (outer scope owner) before (app "fresh" [s "owner"]) (.symbol name) allocated)
      (declare : Eval (LoweringDeclarationEvaluation.environment captured address name)
        allocated LoweringDeclarationEvaluation.slot .unit middle)
      (tail : Calls scope captured address middle requests names after) :
      Calls scope captured address before ((owner, logicalOwner) :: requests) (name :: names) after

theorem Calls.deterministic (first : Calls scope captured address heap requests names after)
    (second : Calls scope captured address heap requests others last) : names = others ∧ after = last := by
  induction first generalizing others last with
  | nil => cases second; exact ⟨rfl, rfl⟩
  | cons fresh declare tail ih =>
    cases second with
    | cons otherFresh otherDeclare otherTail =>
      obtain ⟨same, rfl⟩ := fresh.deterministic otherFresh
      cases Value.symbol.inj same
      obtain ⟨_, rfl⟩ := declare.deterministic otherDeclare
      obtain ⟨rfl, rfl⟩ := ih otherTail
      exact ⟨rfl, rfl⟩

/-- Arbitrary occupied candidates, owners and request lengths. The callback
cell must not alias the counter: it is a derived root-storage responsibility,
not an assumption that allocation and callback writes commute unconditionally. -/
theorem source_history (requests : List (String × Lowering.Owner))
    (valid : Occupied heap scope.table before.used)
    (count : heap scope.counter 0 = some (.natural before.attempt))
    (collected : heap address 0 = some (.list previous))
    (separate : address ≠ scope.counter) (unshadowed : captured "cons" = none)
    (injective : ∀ owner a b, candidate owner a = candidate owner b → a = b) :
    ∃ after names last, Calls scope captured address heap requests names last ∧
      Names.Trace before after ∧ Occupied last scope.table after.used ∧
      last scope.counter 0 = some (.natural after.attempt) ∧
      after.bindings = (names.zip (requests.map Prod.snd)).reverse ++ before.bindings ∧
      last address 0 = some (.list ((names.map Value.symbol).reverse ++ previous)) := by
  induction requests generalizing before heap previous with
  | nil => exact ⟨before, [], heap, .nil, .refl _, valid, count, rfl, collected⟩
  | cons request rest ih =>
    obtain ⟨index, found⟩ := LoweringNames.search_total (candidate request.1) (injective request.1) before.used before.attempt
    let next := LoweringNames.claimed before (candidate request.1) request.2 index
    let allocated := committed heap scope.counter scope.table request.1 index
    let middle := write allocated address 0 (.list (.symbol (candidate request.1 index) :: previous))
    have fresh := fresh_eval valid count found
    have history := (LoweringNames.search_trace request.2 found).2.2
    have preserved : allocated address 0 = some (.list previous) := by
      change committed heap scope.counter scope.table request.1 index address 0 = _
      exact (committed_frame (heap := heap) (counter := scope.counter) (table := scope.table)
        (owner := request.1) (index := index) (key := 0) (Or.inl separate) (Or.inr (by simp))).trans collected
    have declare := LoweringDeclarationEvaluation.slot_eval (name := candidate request.1 index) unshadowed preserved
    have allocatedValid : Occupied allocated scope.table next.used := committed_occupied valid
    have occupied : Occupied middle scope.table next.used := by
      intro name
      simpa [middle, write] using allocatedValid name
    have counter : middle scope.counter 0 = some (.natural next.attempt) := by
      simpa [middle, write, Ne.symm separate, next, LoweringNames.claimed] using
        (committed_counter (heap := heap) (counter := scope.counter) (table := scope.table) (owner := request.1) (index := index))
    have collector : middle address 0 = some (.list (.symbol (candidate request.1 index) :: previous)) := by simp [middle, write]
    obtain ⟨after, names, last, calls, suffix, occupied, counter, bindings, collector⟩ := ih occupied counter collector
    refine ⟨after, candidate request.1 index :: names, last, .cons fresh declare calls,
      LoweringNameStore.trans history suffix, occupied, counter, ?_, ?_⟩
    · simpa [next, LoweringNames.claimed, List.reverse_cons, List.append_assoc] using bindings
    · simpa [List.reverse_cons, List.append_assoc] using collector

/-- Every returning interleaved execution has the same complete collector and
allocation history as the generated execution, rather than a supplied oracle. -/
theorem every_return (run : Calls scope captured address heap requests names last)
    (valid : Occupied heap scope.table before.used)
    (count : heap scope.counter 0 = some (.natural before.attempt))
    (collected : heap address 0 = some (.list previous))
    (separate : address ≠ scope.counter) (unshadowed : captured "cons" = none)
    (injective : ∀ owner a b, candidate owner a = candidate owner b → a = b) :
    ∃ after, Names.Trace before after ∧
      after.bindings = (names.zip (requests.map Prod.snd)).reverse ++ before.bindings ∧
      last address 0 = some (.list ((names.map Value.symbol).reverse ++ previous)) := by
  obtain ⟨after, generated, final, calls, history, _, _, bindings, collector⟩ :=
    source_history requests valid count collected separate unshadowed injective
  obtain ⟨rfl, rfl⟩ := run.deterministic calls
  exact ⟨after, history, bindings, collector⟩

/-- Recursive lowering itself generates allocator owner requests. There is no
independent caller-supplied request trace. Numeric-to-source owner encoding
remains explicit; this does not yet derive the complete dispatcher Eval. -/
theorem generated_history
    (execution : LoweringCallStack.Execution code store) (encodeOwner : Nat → String)
    (valid : Occupied heap scope.table before.used)
    (count : heap scope.counter 0 = some (.natural before.attempt))
    (collected : heap address 0 = some (.list previous))
    (separate : address ≠ scope.counter) (unshadowed : captured "cons" = none)
    (injective : ∀ owner a b, candidate owner a = candidate owner b → a = b) :
    ∃ after names last,
      Calls scope captured address heap
        ((LoweringCallStack.allocationRequests execution.events).map (fun request => (encodeOwner request.1, request.2))) names last ∧
      Names.Trace before after ∧
      after.bindings = (names.zip ((LoweringCallStack.requests code).map Prod.snd)).reverse ++ before.bindings ∧
      last address 0 = some (.list ((names.map Value.symbol).reverse ++ previous)) := by
  rw [execution.requested]
  obtain ⟨after, names, last, calls, history, _, _, bindings, collector⟩ :=
    source_history ((LoweringCallStack.requests code).map (fun request => (encodeOwner request.1, request.2)))
      valid count collected separate unshadowed injective
  exact ⟨after, names, last, calls, history, by simpa [List.map_map, Function.comp_def] using bindings, collector⟩

end ParserEngine.LoweringDeclarationCalls
