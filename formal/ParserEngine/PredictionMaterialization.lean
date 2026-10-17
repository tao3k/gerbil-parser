import ParserEngine.PredictionWitness

/-! Materialize completed prediction masks into physical dot-zero items and
compose them with the grammar closure theorem, rather than assuming a final
templateExact certificate. All offsets come from the dense production layout. -/
namespace ParserEngine.PredictionMaterialization
open Grammar Viable PredictionExtraction

def completed (keys : List Nat) (index : Nat → Option Nat) (valid : NameIndex keys index) (g : CFG) :=
  (PredictionWitness.generate (rows index g) (names keys) (bounded keys index valid g)).after

def maskFor (index : Nat → Option Nat) (state : PredictionQueue.MState) (root : Core) : Nat :=
  match root.production.rhs[root.dot]? with
  | some (.nonterminal key) => match index key with
    | some node => (state.queue.cells node).known
    | none => 0
  | _ => 0

def catalogAt (g : CFG) (index : Nat → Option Nat) (state : PredictionQueue.MState) (code : Nat) : Nat :=
  match decodeCore g code with | some root => maskFor index state root | none => 0

def aggregate (g : CFG) (index : Nat → Option Nat) (state : PredictionQueue.MState) : List Nat → Nat
  | [] => 0
  | code :: rest => catalogAt g index state code ||| aggregate g index state rest

def materialize (g : CFG) (mask : Nat) : List Nat :=
  ((List.range g.length).filter fun id => mask.testBit id).map fun id =>
    denseOffset (g.map fun rule => rule.rhs.length) id

def Kernel (g : CFG) (seeds : List Nat) (root : Core) : Prop :=
  ∃ code ∈ seeds, decodeCore g code = some root

def Covers (keys : List Nat) (g : CFG) : Prop :=
  ∀ rule ∈ g, ∀ key, Symbol.nonterminal key ∈ rule.rhs → key ∈ keys

theorem core_member {g : CFG} {code : Nat} {root : Core} (decoded : decodeCore g code = some root) :
    root.production ∈ g := by
  unfold decodeCore at decoded
  obtain ⟨coordinate, _, rest⟩ := Option.bind_eq_some_iff.mp decoded
  obtain ⟨rule, source, result⟩ := Option.bind_eq_some_iff.mp rest
  simp only [Option.pure_def, Option.some.injEq] at result
  cases result
  exact List.mem_of_getElem? source

theorem aggregate_bit (g : CFG) (index : Nat → Option Nat) (state : PredictionQueue.MState)
    (seeds : List Nat) (id : Nat) :
    (aggregate g index state seeds).testBit id = true ↔ ∃ code ∈ seeds, (catalogAt g index state code).testBit id = true := by
  induction seeds with
  | nil => simp [aggregate]
  | cons code rest ih => simp [aggregate, Nat.testBit_or, ih, Bool.or_eq_true]

theorem mask_meaning (keys : List Nat) (index : Nat → Option Nat) (valid : NameIndex keys index)
    (g : CFG) (covered : Covers keys g) (root : Core) (member : root.production ∈ g) (id : Nat) :
    (maskFor index (completed keys index valid g) root).testBit id = true ↔
      ∃ key rule, root.production.rhs[root.dot]? = some (.nonterminal key) ∧ g[id]? = some rule ∧
        LRStateSemantics.Prediction g key rule := by
  have firstCovered : ∀ rule ∈ g, ∀ key, rule.rhs[0]? = some (.nonterminal key) → key ∈ keys := by
    intro rule source key head
    exact covered rule source key (List.mem_of_getElem? head)
  cases head : root.production.rhs[root.dot]? with
  | none => simp [maskFor, head]
  | some symbol =>
    cases symbol with
    | terminal token => simp [maskFor, head]
    | nonterminal key =>
      obtain ⟨node, found⟩ := key_index valid (covered root.production member key (List.mem_of_getElem? head))
      have exactMask := PredictionWitness.generated_exact (bounded keys index valid g)
        (decoded keys index valid g firstCovered) node id (indexed_bound valid found)
      have decodedName := indexed_name valid found
      simpa [maskFor, head, found, completed, decodedName] using exactMask

theorem materialize_read (g : CFG) (mask : Nat) (item : Core) :
    (∃ code ∈ materialize g mask, decodeCore g code = some item) ↔
      ∃ id rule, g[id]? = some rule ∧ mask.testBit id = true ∧ item = ⟨rule, 0⟩ := by
  constructor
  · rintro ⟨code, member, decoded⟩
    obtain ⟨id, selected, rfl⟩ := List.mem_map.mp member
    obtain ⟨bound, present⟩ := List.mem_filter.mp selected
    have inside := List.mem_range.mp bound
    let rule := g[id]
    have source : g[id]? = some rule := List.getElem?_eq_getElem inside
    have same := decode_core_roundtrip source (Nat.zero_le rule.rhs.length)
    simp only [Nat.add_zero] at same
    have equal : item = ⟨rule, 0⟩ := Option.some.inj (decoded.symm.trans same)
    exact ⟨id, rule, source, present, equal⟩
  · rintro ⟨id, rule, source, present, rfl⟩
    have inside : id < g.length := by
      by_cases bound : id < g.length
      · exact bound
      · simp [List.getElem?_eq_none (by omega : g.length ≤ id)] at source
    refine ⟨denseOffset (g.map fun p => p.rhs.length) id, ?_, ?_⟩
    · exact List.mem_map.mpr ⟨id, List.mem_filter.mpr ⟨List.mem_range.mpr inside, present⟩, rfl⟩
    · simpa only [Nat.add_zero] using decode_core_roundtrip source (Nat.zero_le rule.rhs.length)

theorem template_exact (keys : List Nat) (index : Nat → Option Nat) (valid : NameIndex keys index)
    (g : CFG) (covered : Covers keys g) (seeds : List Nat) (item : Core) :
    (∃ code ∈ materialize g (aggregate g index (completed keys index valid g) seeds), decodeCore g code = some item) ↔
      LRStateSemantics.Template g (Kernel g seeds) item := by
  rw [materialize_read]
  constructor
  · rintro ⟨id, rule, source, bit, rfl⟩
    obtain ⟨code, seed, present⟩ := (aggregate_bit _ _ _ seeds id).mp bit
    cases read : decodeCore g code with
    | none => simp [catalogAt, read] at present
    | some root =>
      have mask : (maskFor index (completed keys index valid g) root).testBit id = true := by simpa [catalogAt, read] using present
      obtain ⟨key, actual, head, row, predicted⟩ := (mask_meaning keys index valid g covered root (core_member read) id).mp mask
      have equal : actual = rule := Option.some.inj (row.symm.trans source)
      subst actual
      exact ⟨root, key, ⟨code, seed, read⟩, head, predicted, rfl⟩
  · rintro ⟨root, key, ⟨code, seed, read⟩, head, predicted, zero⟩
    obtain ⟨id, source⟩ := PredictionCatalog.member_index (PredictionCatalog.prediction_member predicted)
    refine ⟨id, item.production, source, ?_, ?_⟩
    · apply (aggregate_bit _ _ _ seeds id).mpr
      refine ⟨code, seed, ?_⟩
      have mask := (mask_meaning keys index valid g covered root (core_member read) id).mpr
        ⟨key, item.production, head, source, predicted⟩
      simpa [catalogAt, read] using mask
    · cases item with | mk rule dot => dsimp at zero; subst dot; rfl

/-- Complete queue execution, template materialization and closure publication
compose without a final templateExact premise. -/
theorem closure_exact (keys : List Nat) (index : Nat → Option Nat) (valid : NameIndex keys index)
    (g : CFG) (covered : Covers keys g) (seeds : List Nat) (item : Core) :
    (∃ code ∈ LRStateSemantics.merge seeds
      (materialize g (aggregate g index (completed keys index valid g) seeds)) [], decodeCore g code = some item) ↔
        Closure g (Kernel g seeds) item := by
  apply LRStateSemantics.decoded_closure
  · intro core; rfl
  · intro core; exact (template_exact keys index valid g covered seeds core).symm

end ParserEngine.PredictionMaterialization
