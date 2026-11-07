import ParserEngine.LoweringEmissionCalls
import ParserEngine.LoweringDeclarationEvaluation

/-! Actual helper allocation, fixed declaration collector and native emission
may interleave. Prove the publication projection in one shared heap rather than
replaying emission after helpers in an independently initialized environment. -/
namespace ParserEngine.LoweringMixedEmission
open WalkSource SchemeTableEvaluation LoweringProductionEvaluation LoweringEmitterEvaluation LoweringEmissionCalls

inductive Trace (env : Environment) (base : Nat) : Heap → List Request → Heap → Prop where
  | nil : Trace env base before [] before
  | emit (run : Invoke before (LoweringEmitClosure.function env base request.context request.value request.isolated)
      [.symbol request.value.lhs, .list request.value.rhs, datumValue request.value.action,
        datumValue request.value.precedence, .boolean false] .unit middle)
      (tail : Trace env base middle requests after) : Trace env base before (request :: requests) after
  | fresh (prelude : ∀ operation ∈ LoweringFreshEvaluation.preludeNames, globals env operation = none)
      (occupied : LoweringFreshEvaluation.Occupied before (base + 2) used)
      (counter : before (base + 1) 0 = some (.natural next))
      (injective : ∀ a b, LoweringFreshEvaluation.candidate owner a = LoweringFreshEvaluation.candidate owner b → a = b)
      (run : Eval (LoweringFreshEvaluation.outer (LoweringRootScope.scope (globals env) base prelude) owner)
        before (app "fresh" [s "owner"]) result middle)
      (tail : Trace env base middle requests after) : Trace env base before requests after
  | declare (owned : address < base) (contents : before address 0 = some (.list names))
      (unshadowed : captured "cons" = none)
      (run : Eval (LoweringDeclarationEvaluation.environment captured address name)
        before LoweringDeclarationEvaluation.slot .unit middle)
      (tail : Trace env base middle requests after) : Trace env base before requests after

/-- Recursive partitions compose without relocating their captured root cells. -/
theorem Trace.append (first : Trace env base before requests middle)
    (last : Trace env base middle suffix after) :
    Trace env base before (requests ++ suffix) after := by
  induction first with
  | nil => exact last
  | emit run tail ih => exact .emit run (ih last)
  | fresh prelude occupied counter injective run tail ih =>
      exact .fresh prelude occupied counter injective run (ih last)
  | declare owned contents unshadowed run tail ih =>
      exact .declare owned contents unshadowed run (ih last)

/-- Arbitrary mixed source traces. Only source preconditions are inputs; no
frame assertions, post-state equalities or counter-to-record agreement are
supplied by the caller. Emitter IDs count actual emissions, not helper steps. -/
theorem publication (run : Trace env base before requests after) (next : Nat)
    (listPrimitive : env "list" = none) (consPrimitive : env "cons" = none) (plusPrimitive : env "+" = none)
    (count : before base 0 = some (.natural next))
    (contents : before (base + 4) 0 = some (.list previous)) :
    after base 0 = some (.natural (next + requests.length)) ∧
      after (base + 4) 0 = some (.list ((records next requests).reverse ++ previous)) := by
  induction run generalizing next previous with
  | nil => simpa [records] using And.intro count contents
  | @emit before middle requests after request actual tail ih =>
      obtain ⟨_, same⟩ := actual.deterministic
        (LoweringEmitClosure.invoke next listPrimitive consPrimitive plusPrimitive count contents _ _ _)
      cases same
      have nextCount : committed before base next previous request.value base 0 = some (.natural (next + 1)) := committed_counter next
      have nextRecords : committed before base next previous request.value (base + 4) 0 = some (.list (record next request.value :: previous)) := committed_records next
      have result := ih (next + 1) nextCount nextRecords
      simpa [records, List.reverse_cons, List.append_assoc, Nat.add_assoc, Nat.add_comm, Nat.add_left_comm] using result
  | @fresh before used nextName owner result middle requests after prelude occupied counter injective actual tail ih =>
      obtain ⟨index, found⟩ := LoweringNames.search_total (LoweringFreshEvaluation.candidate owner) injective used nextName
      have reference := LoweringFreshEvaluation.fresh_eval
        (scope := LoweringRootScope.scope (globals env) base prelude) occupied counter found
      obtain ⟨_, same⟩ := actual.deterministic reference
      cases same
      have counterFrame := LoweringAllocationCalls.committed_frame
        (heap := before) (counter := base + 1) (table := base + 2) (owner := owner) (index := index)
        (address := base) (key := 0) (Or.inl (by omega)) (Or.inr (by simp))
      have recordsFrame := LoweringAllocationCalls.committed_frame
        (heap := before) (counter := base + 1) (table := base + 2) (owner := owner) (index := index)
        (address := base + 4) (key := 0) (Or.inl (by omega)) (Or.inr (by simp))
      exact ih next (counterFrame.trans count) (recordsFrame.trans contents)
  | @declare address before captured name middle requests after names owned helpers unshadowed actual tail ih =>
      have reference := LoweringDeclarationEvaluation.slot_eval (name := name) unshadowed helpers
      obtain ⟨_, same⟩ := actual.deterministic reference
      cases same
      have different : base ≠ address := by omega
      have recordsDifferent : base + 4 ≠ address := by omega
      apply ih next
      · simpa [write, different] using count
      · simpa [write, recordsDifferent] using contents

/-- A collector cell already present before root initialization cannot alias a
new root cell. This follows from the same allocator freshness used by root
storage, rather than an independently assumed address-separation assertion. -/
theorem collector_owned (before : SchemeStorage.Store) (fresh : SchemeStorage.Fresh before)
    (contents : before.heap address 0 = some (.list names)) : address < before.frontier := by
  by_cases inside : address < before.frontier
  · exact inside
  · have absent := fresh address (by omega) 0
    rw [contents] at absent
    cases absent

theorem collector_root_frame (before : SchemeStorage.Store) (fresh : SchemeStorage.Fresh before)
    (contents : before.heap address 0 = some (.list names)) :
    LoweringRootStorage.heap before.heap before.frontier address 0 = some (.list names) :=
  (LoweringRootStorage.root_frame before (collector_owned before fresh contents)).trans contents

end ParserEngine.LoweringMixedEmission