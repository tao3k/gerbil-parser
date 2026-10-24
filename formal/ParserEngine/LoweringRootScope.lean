import ParserEngine.LoweringRootStorage
import ParserEngine.LoweringAllocationCalls

/-! The real root module supplies the generic source allocation interface.
Every directly nested declaration is retained, including recursive dispatch,
proof helpers and the emitter. This is not a two-procedure replacement scope. -/
namespace ParserEngine.LoweringRootScope
open WalkSource SchemeTableEvaluation LoweringClaimEvaluation LoweringFreshEvaluation

def definitions : Option (List (String × Procedure)) := do
  let (_, body) ← LoweringRootStorage.rootParts LoweringBranchSource.rootSource
  return body.filterMap procedure

def group : List (String × Procedure) := definitions.getD []

set_option maxRecDepth 8192 in
theorem extracted : definitions = some group := rfl

theorem claim_lookup : group.lookup "claim-name" = some ⟨[s "name"], [claimBody]⟩ := rfl
theorem fresh_lookup : group.lookup "fresh" = some ⟨[s "owner"], [freshBody]⟩ := rfl

theorem lookup_unbound (member : operation ∈ preludeNames ∨ operation = "next-synthetic" ∨ operation = "used-names") :
    group.lookup operation = none := by
  rcases member with member | rfl | rfl
  · simp only [preludeNames, List.mem_cons, List.not_mem_nil, or_false] at member
    rcases member with rfl | rfl | rfl | rfl | rfl | rfl | rfl | rfl <;> rfl
  · rfl
  · rfl

/-- The primitive prelude is an explicit host interface; root lexical storage
and procedure lookup are discharged here from source, not host assumptions. -/
def scope (env : Environment) (base : Nat)
    (prelude : ∀ operation ∈ preludeNames, env operation = none) : Scope where
  group := group
  captured := LoweringRootStorage.environment env base
  counter := base + 1
  table := base + 2
  claimLookup := claim_lookup
  freshLookup := fresh_lookup
  next := by simp only [tie, lookup_unbound (Or.inr (Or.inl rfl))]; simp [LoweringRootStorage.environment, extend]
  occupied := by simp only [tie, lookup_unbound (Or.inr (Or.inr rfl))]; simp [LoweringRootStorage.environment, extend]
  prelude := by
    intro operation member
    simp only [tie, lookup_unbound (Or.inl member)]
    have absent := prelude operation member
    simp only [preludeNames, List.mem_cons, List.not_mem_nil, or_false] at member
    rcases member with rfl | rfl | rfl | rfl | rfl | rfl | rfl | rfl <;>
      simpa [LoweringRootStorage.environment, extend] using absent

/-- Root initialization supplies both occupancy and the captured counter read;
none of these facts are independently supplied by an allocation caller. -/
theorem initialized (fresh : SchemeStorage.Fresh before)
    (prelude : ∀ operation ∈ preludeNames, env operation = none) :
    Occupied (LoweringRootStorage.heap before.heap before.frontier) (scope env before.frontier prelude).table [] ∧
    LoweringRootStorage.heap before.heap before.frontier (scope env before.frontier prelude).counter 0 = some (.natural 0) := by
  have roots := LoweringRootStorage.roots (env := env) fresh
  constructor
  · intro name
    exact roots.2.2.2.2.2.1 (.symbol name)
  · simpa [scope, LoweringRootStorage.environment, extend, resolve] using roots.2.1

/-- Arbitrary allocation calls now execute in the entire extracted root module.
The empty occupied set is the point before reservation; prepass simulation is
still needed to connect a nonempty grammar's post-reservation state. -/
theorem root_allocations (fresh : SchemeStorage.Fresh before)
    (prelude : ∀ operation ∈ preludeNames, env operation = none)
    (injective : ∀ owner a b, candidate owner a = candidate owner b → a = b)
    (requests : List (String × Lowering.Owner)) :
    ∃ (after : Names.State String) (names : List String) (last : Heap),
      LoweringAllocationCalls.Calls (scope env before.frontier prelude)
        (LoweringRootStorage.heap before.heap before.frontier) requests names last ∧
      Names.Trace (Names.initial []) after ∧ (after.bindings.map Prod.fst).Nodup := by
  have ready := initialized fresh prelude
  obtain ⟨after, names, last, calls, history, unique, _⟩ :=
    LoweringAllocationCalls.source_unique requests ready.1 ready.2 injective
  exact ⟨after, names, last, calls, history, unique⟩

end ParserEngine.LoweringRootScope
