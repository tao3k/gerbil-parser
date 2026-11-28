import Lean

/-! Costed suffix memoization for compiler/event-fold-future.ss.

This is the forward line-search recurrence with cache publication on return.
The query indices belong to one fixed, advancing line-boundary sequence.
Arbitrary in-line source offsets require a more general successor-graph model;
this restricted recurrence does not establish that interface's physical-line bound.
The Scheme owner accumulates visited positions and publishes them at its terminal
branch; distinct advancing positions make the final cache identical. Establishing
that concrete byte/line correspondence is a separate implementation obligation.
No statement here establishes a clock bound or whole-grammar linearity.
-/
namespace ParserEngine.FutureSearch

inductive Decision where
  | next | accept | stop
  deriving DecidableEq, Repr

structure Cell where
  decision : Decision
  cached : Option Bool
  deriving DecidableEq, Repr

structure Result where
  answer : Bool
  cells : List Cell
  visits : Nat
  deriving DecidableEq, Repr

/-- The first terminal classification wins; end of source rejects. -/
def reference : List Cell → Bool
  | [] => false
  | cell :: rest => match cell.decision with
    | .next => reference rest
    | .accept => true
    | .stop => false

/-- Request/query-specific immutable-source cache validity, at every suffix. -/
def Sound : List Cell → Prop
  | [] => True
  | cell :: rest => (∀ value, cell.cached = some value →
      value = reference (cell :: rest)) ∧ Sound rest

/-- A hit does no classification. A miss publishes all newly traversed positions. -/
def search : List Cell → Result
  | [] => ⟨false, [], 0⟩
  | cell :: rest => match cell.cached with
    | some answer => ⟨answer, cell :: rest, 0⟩
    | none => match cell.decision with
      | .accept => ⟨true, {cell with cached := some true} :: rest, 1⟩
      | .stop => ⟨false, {cell with cached := some false} :: rest, 1⟩
      | .next => let result := search rest
                 ⟨result.answer, {cell with cached := some result.answer} :: result.cells,
                  result.visits + 1⟩

/-- The Scheme `visited` accumulator is in reverse traversal order. This
logical publication changes only those newly classified cells; untouched cache
entries remain in the suffix. Distinct advancing positions are the concrete
source adapter obligation, not a property of arbitrary hash-table writes. -/
def publish (visited : List Cell) (answer : Bool) : List Cell :=
  visited.reverse.map fun cell => {cell with cached := some answer}

/-- Tail-recursive search with the terminal `finish` used by the Scheme owner.
The pending positions are charged once even when the terminal is a cache hit
or end of source. The returned list is in source order, not hash write order. -/
def accumulated (visited : List Cell) : List Cell → Result
  | [] => ⟨false, publish visited false, visited.length⟩
  | cell :: rest => match cell.cached with
    | some answer => ⟨answer, publish visited answer ++ cell :: rest, visited.length⟩
    | none => match cell.decision with
      | .accept => ⟨true, publish (cell :: visited) true ++ rest, visited.length + 1⟩
      | .stop => ⟨false, publish (cell :: visited) false ++ rest, visited.length + 1⟩
      | .next => accumulated (cell :: visited) rest

/-- Delayed publication and recursive-return publication yield the same
answer, final cells and classification count. No cache soundness premise is
needed for this implementation equivalence; semantic correctness still needs it. -/
theorem accumulated_refinement (cells visited : List Cell) :
    accumulated visited cells =
      ⟨(search cells).answer,
       publish visited (search cells).answer ++ (search cells).cells,
       visited.length + (search cells).visits⟩ := by
  induction cells generalizing visited with
  | nil => simp [accumulated, search]
  | cons cell rest ih =>
    cases cell with | mk decision cached =>
      cases cached with
      | some answer => rfl
      | none =>
        cases decision <;>
          simp [accumulated, search, ih, publish, List.reverse_cons,
            List.map_append, List.append_assoc, Nat.add_assoc, Nat.add_comm]

/-- This is the actual empty-accumulator entry used for a fresh traversal. -/
theorem accumulated_search (cells : List Cell) : accumulated [] cells = search cells := by
  simpa [publish] using accumulated_refinement cells []

/-- Potential counts uncached positions, including positions never queried. -/
def potential : List Cell → Nat
  | [] => 0
  | cell :: rest => (if cell.cached.isNone then 1 else 0) + potential rest

/-- Every charged classification removes exactly one unit of potential.
No cache soundness assumption is needed for this accounting identity. -/
theorem search_accounting (cells : List Cell) :
    (search cells).visits + potential (search cells).cells = potential cells := by
  induction cells with
  | nil => rfl
  | cons cell rest ih =>
    cases cell with | mk decision cached =>
      cases cached with
      | some answer => simp [search, potential]
      | none =>
        cases decision <;> simp_all [search, potential] <;> omega

/-- Search changes only memo entries, never the immutable classifier stream. -/
theorem search_source (cells : List Cell) :
    (search cells).cells.map Cell.decision = cells.map Cell.decision := by
  induction cells with
  | nil => rfl
  | cons cell rest ih =>
    cases cell with | mk decision cached =>
      cases cached with
      | some answer => rfl
      | none => cases decision <;> simp_all [search]

/-- The reference meaning depends only on source classifications. -/
theorem reference_source (left right : List Cell)
    (same : left.map Cell.decision = right.map Cell.decision) :
    reference left = reference right := by
  induction left generalizing right with
  | nil => cases right <;> simp_all [reference]
  | cons cell rest ih =>
    cases right with
    | nil => simp at same
    | cons other tail =>
      simp only [List.map_cons, List.cons.injEq] at same
      simp only [reference, same.1]
      cases choice : other.decision with
      | next => simpa [reference, choice] using ih tail same.2
      | accept => simp
      | stop => simp

/-- Memoization preserves the uncached answer and valid suffix caches. -/
theorem search_correct (cells : List Cell) (valid : Sound cells) :
    (search cells).answer = reference cells ∧ Sound (search cells).cells := by
  induction cells with
  | nil => exact ⟨rfl, trivial⟩
  | cons cell rest ih =>
    rcases valid with ⟨head, tail⟩
    cases cell with | mk decision cached =>
      cases cached with
      | some answer => exact ⟨head answer rfl, ⟨head, tail⟩⟩
      | none =>
        have step := ih tail
        have meaning := reference_source (search rest).cells rest (search_source rest)
        cases decision <;> simp_all [search, Sound, reference]

/-- Select a suffix without reading or classifying its skipped prefix. -/
def query : Nat → List Cell → Result
  | 0, cells => search cells
  | _ + 1, [] => ⟨false, [], 0⟩
  | offset + 1, cell :: rest =>
      let result := query offset rest
      ⟨result.answer, cell :: result.cells, result.visits⟩

theorem query_accounting (offset : Nat) (cells : List Cell) :
    (query offset cells).visits + potential (query offset cells).cells = potential cells := by
  induction offset generalizing cells with
  | zero => exact search_accounting cells
  | succ offset ih =>
    cases cells with
    | nil => rfl
    | cons cell rest =>
      have step := ih rest
      simp only [query, potential]
      omega

theorem query_source (offset : Nat) (cells : List Cell) :
    (query offset cells).cells.map Cell.decision = cells.map Cell.decision := by
  induction offset generalizing cells with
  | zero => exact search_source cells
  | succ offset ih =>
    cases cells with
    | nil => rfl
    | cons cell rest => simp only [query, List.map_cons]; rw [ih]

theorem query_correct (offset : Nat) (cells : List Cell) (valid : Sound cells) :
    (query offset cells).answer = reference (cells.drop offset) ∧
      Sound (query offset cells).cells := by
  induction offset generalizing cells with
  | zero => simpa [query] using search_correct cells valid
  | succ offset ih =>
    cases cells with
    | nil => exact ⟨rfl, trivial⟩
    | cons cell rest =>
      rcases valid with ⟨head, tail⟩
      have step := ih rest tail
      have meaning := reference_source (cell :: (query offset rest).cells)
        (cell :: rest) (by simp only [List.map_cons, query_source])
      constructor
      · simpa only [query, List.drop_succ_cons] using step.1
      · exact ⟨fun value cached => (head value cached).trans meaning.symm, step.2⟩

/-- One request execution publishes every answer and counts the same steps. -/
structure RequestResult where
  answers : List Bool
  cells : List Cell
  visits : Nat
  deriving DecidableEq, Repr

/-- Repeated and out-of-order suffix queries share only their stable cache. -/
def requests : List Nat → List Cell → RequestResult
  | [], cells => ⟨[], cells, 0⟩
  | offset :: rest, cells =>
      let first := query offset cells
      let later := requests rest first.cells
      ⟨first.answer :: later.answers, later.cells, first.visits + later.visits⟩

theorem requests_correct (starts : List Nat) (cells : List Cell) (valid : Sound cells) :
    (requests starts cells).answers =
      starts.map (fun offset => reference (cells.drop offset)) := by
  induction starts generalizing cells with
  | nil => rfl
  | cons offset rest ih =>
    have first := query_correct offset cells valid
    simp only [requests, List.map_cons]
    rw [first.1, ih (query offset cells).cells first.2]
    congr 1
    apply List.map_congr_left
    intro position _
    apply reference_source
    simpa only [List.map_drop] using
      congrArg (List.drop position) (query_source offset cells)

theorem requests_accounting (starts : List Nat) (cells : List Cell) :
    (requests starts cells).visits + potential (requests starts cells).cells =
      potential cells := by
  induction starts generalizing cells with
  | nil => simp [requests]
  | cons offset rest ih =>
    have first := query_accounting offset cells
    have later := ih (query offset cells).cells
    simp only [requests]
    omega

theorem potential_bound (cells : List Cell) : potential cells ≤ cells.length := by
  induction cells with
  | nil => simp [potential]
  | cons cell rest ih => simp only [potential, List.length_cons]; split <;> omega

/-- One stable query family on this fixed boundary sequence classifies each cell
at most once, independent
of query order/count. Hash operations, byte classification and encoding have
separate costs; this does not assert that all query families share work. -/
theorem request_visit_bound (starts : List Nat) (cells : List Cell) :
    (requests starts cells).visits ≤ cells.length := by
  have accounting := requests_accounting starts cells
  have bound := potential_bound cells
  omega

/-- A new request/query family starts with no retained answers. This logical
list is not a claim that the Scheme hash cache allocates a source-sized array. -/
def fresh (source : List Decision) : List Cell := source.map fun decision => ⟨decision, none⟩

theorem fresh_sound (source : List Decision) : Sound (fresh source) := by
  induction source with
  | nil => trivial
  | cons decision rest ih => simpa [fresh, Sound] using ih

/-- Count entries into the Scheme search loop, including the EOF/cache-hit
terminal entry. This counts neither hash implementation work nor byte scans. -/
def searchIterations : List Cell → Nat
  | [] => 1
  | cell :: rest => match cell.cached with
    | some _ => 1
    | none => match cell.decision with
      | .next => 1 + searchIterations rest
      | .accept | .stop => 1

theorem search_iterations_bound (cells : List Cell) :
    searchIterations cells ≤ (search cells).visits + 1 := by
  induction cells with
  | nil => simp [searchIterations, search]
  | cons cell rest ih =>
    cases cell with | mk decision cached =>
      cases cached with
      | some answer => simp [searchIterations, search]
      | none => cases decision <;> simp_all [searchIterations, search] <;> omega

/-- Prefix selection is direct in Scheme (a byte offset); this logical list
selection is not a claim that skipped-prefix traversal is free in Lean. -/
def queryIterations : Nat → List Cell → Nat
  | 0, cells => searchIterations cells
  | _ + 1, [] => 1
  | offset + 1, _ :: rest => queryIterations offset rest

theorem query_iterations_bound (offset : Nat) (cells : List Cell) :
    queryIterations offset cells ≤ (query offset cells).visits + 1 := by
  induction offset generalizing cells with
  | zero => exact search_iterations_bound cells
  | succ offset ih => cases cells <;> simp_all [queryIterations, query]

/-- Cache hits still enter the loop. Count every request, in its actual order,
using exactly the cache produced by the preceding request. -/
def requestIterations : List Nat → List Cell → Nat
  | [], _ => 0
  | offset :: rest, cells =>
      queryIterations offset cells + requestIterations rest (query offset cells).cells

theorem request_iterations_bound (starts : List Nat) (cells : List Cell) :
    requestIterations starts cells ≤ (requests starts cells).visits + starts.length := by
  induction starts generalizing cells with
  | nil => simp [requestIterations, requests]
  | cons offset rest ih =>
    have first := query_iterations_bound offset cells
    have later := ih (query offset cells).cells
    simp only [requestIterations, requests, List.length_cons]
    omega

/-- For one immutable query family, Q requests over L admitted positions need
at most Q + L loop entries. This includes repeated hits, unlike the miss-only
bound. Byte classification, hashing and output costs remain separate. -/
theorem request_iteration_amortized_bound (starts : List Nat) (cells : List Cell) :
    requestIterations starts cells ≤ starts.length + cells.length := by
  have work := request_iterations_bound starts cells
  have visits := request_visit_bound starts cells
  omega

/-- Fresh-request correctness establishes the cache premise rather than asking
for an externally supplied cache certificate. Classifier stability remains an
explicit source-adapter boundary. -/
theorem fresh_request (starts : List Nat) (source : List Decision) :
    (requests starts (fresh source)).answers =
      starts.map (fun offset => reference ((fresh source).drop offset)) ∧
    (requests starts (fresh source)).visits ≤ source.length := by
  constructor
  · exact requests_correct starts (fresh source) (fresh_sound source)
  · simpa [fresh] using request_visit_bound starts (fresh source)

/-- Promotion keeps exactly one answer computed on the original source. -/
def seedPosition : Nat → Bool → List Cell → List Cell
  | _, _, [] => []
  | 0, answer, cell :: rest => {cell with cached := some answer} :: rest
  | offset + 1, answer, cell :: rest => cell :: seedPosition offset answer rest

theorem seed_source (offset : Nat) (answer : Bool) (cells : List Cell) :
    (seedPosition offset answer cells).map Cell.decision = cells.map Cell.decision := by
  induction offset generalizing cells with
  | zero => cases cells <;> rfl
  | succ offset ih => cases cells <;> simp [seedPosition, ih]

theorem seed_sound (offset : Nat) (answer : Bool) (cells : List Cell)
    (valid : Sound cells) (correct : answer = reference (cells.drop offset)) :
    Sound (seedPosition offset answer cells) := by
  induction offset generalizing cells with
  | zero =>
    cases cells with
    | nil => trivial
    | cons cell rest =>
      have same := reference_source (seedPosition 0 answer (cell :: rest))
        (cell :: rest) (seed_source 0 answer (cell :: rest))
      simp only [seedPosition] at same ⊢
      exact ⟨fun value cached => (Option.some.inj cached).symm.trans
        (correct.trans same.symm), valid.2⟩
  | succ offset ih =>
    cases cells with
    | nil => trivial
    | cons cell rest =>
      have same := reference_source (seedPosition (offset + 1) answer (cell :: rest))
        (cell :: rest) (seed_source (offset + 1) answer (cell :: rest))
      simp only [seedPosition] at same ⊢
      exact ⟨fun value cached => (valid.1 value cached).trans same.symm,
        ih rest valid.2 correct⟩

/-- The first scan only retains its start answer. Any number of same-start
hits have that same answer without classifying. On a distinct start the ordinary
suffix cache can consume that retained answer, and total classification work is
bounded by two passes over the admitted positions for this query family.
This models per-query promotion; byte-offset adapters and actual costs are
separate from the theorem. -/
theorem demand_promotion (first : Nat) (later : List Nat) (cells : List Cell)
    (valid : Sound cells) :
    let cold := query first cells
    let promoted := seedPosition first cold.answer cells
    cold.answer = reference (cells.drop first) ∧
      (requests later promoted).answers = later.map (fun offset => reference (cells.drop offset)) ∧
      cold.visits + (requests later promoted).visits ≤ 2 * cells.length := by
  dsimp
  have cold := query_correct first cells valid
  have seeded := seed_sound first (query first cells).answer cells valid cold.1
  have entries := seed_source first (query first cells).answer cells
  have size := congrArg List.length entries
  simp only [List.length_map] at size
  have firstBound := request_visit_bound [first] cells
  simp only [requests, Nat.add_zero] at firstBound
  have restBound := request_visit_bound later (seedPosition first (query first cells).answer cells)
  refine ⟨cold.1, ?_, by omega⟩
  rw [requests_correct later _ seeded]
  apply List.map_congr_left
  intro offset _
  apply reference_source
  simpa only [List.map_drop] using congrArg (List.drop offset) entries

#print axioms demand_promotion

example : reference [⟨.next, none⟩, ⟨.stop, none⟩, ⟨.accept, none⟩] = false := rfl
example : (requests [0, 1, 0, 2, 1] [⟨.next, none⟩, ⟨.next, none⟩, ⟨.accept, none⟩]).visits = 3 := rfl
/-- A stale cache is a semantic counterexample, even though cost stays bounded. -/
example : (search [⟨.stop, some true⟩]).answer ≠ reference [⟨.stop, some true⟩] := by decide

#print axioms request_iteration_amortized_bound
#print axioms accumulated_refinement
#print axioms accumulated_search
#print axioms fresh_request
#print axioms requests_correct
#print axioms query_correct
#print axioms search_correct
#print axioms search_accounting
#print axioms requests_accounting
#print axioms request_visit_bound

end ParserEngine.FutureSearch
