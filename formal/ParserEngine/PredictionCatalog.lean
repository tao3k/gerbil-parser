import ParserEngine.PredictionQueue
import ParserEngine.LRStateSemantics

/-! Indexed production identities are retained through own masks and reverse
name dependencies. Equal production values at different IDs remain distinct. -/
namespace ParserEngine.PredictionCatalog
open Grammar
structure Row where
  owner : Option Nat
  first : Option Nat
  deriving DecidableEq, Repr

def owns (rows : List Row) (node : Nat) : List Nat :=
  (List.range rows.length).filter fun id => ((rows[id]?).bind Row.owner) == some node

def seeds (rows : List Row) (names : List Nat) : PredictionQueue.Seeds :=
  names.map fun node => (node, SuffixSemantics.encode (owns rows node))

def edge (rows : List Row) (parent child : Nat) : Prop :=
  ∃ (id : Nat) (row : Row), rows[id]? = some row ∧ row.owner = some parent ∧ row.first = some child
instance (rows : List Row) (parent child : Nat) : Decidable (edge rows parent child) :=
  decidable_of_iff (∃ row ∈ rows, row.owner = some parent ∧ row.first = some child) (by
    constructor
    · rintro ⟨row, member, owner, first⟩
      obtain ⟨before, after, equal⟩ := List.mem_iff_append.mp member
      exact ⟨before.length, row, by rw [equal]; simp, owner, first⟩
    · rintro ⟨id, row, read, owner, first⟩
      exact ⟨row, List.mem_of_getElem? read, owner, first⟩)

def parents (rows : List Row) (names : List Nat) (child : Nat) : List Nat :=
  names.reverse.filter fun parent => decide (edge rows parent child)

@[simp] theorem parents_member (rows : List Row) (names : List Nat) (child parent : Nat) :
    parent ∈ parents rows names child ↔ parent ∈ names ∧ edge rows parent child := by
  simp [parents]

theorem owns_member (rows : List Row) (node id : Nat) :
    id ∈ owns rows node ↔ ∃ row, rows[id]? = some row ∧ row.owner = some node := by
  simp only [owns, List.mem_filter, List.mem_range, beq_iff_eq]
  constructor
  · rintro ⟨bound, own⟩
    cases read : rows[id]? with
    | none => simp [read] at own
    | some row => exact ⟨row, rfl, by simpa [read] using own⟩
  · rintro ⟨row, read, own⟩
    have bound : id < rows.length := by
      by_cases valid : id < rows.length
      · exact valid
      · simp [List.getElem?_eq_none (by omega : rows.length ≤ id)] at read
    exact ⟨bound, by simp [read, own]⟩

inductive Predict (rows : List Row) : Nat → Nat → Prop where
  | own {id node : Nat} {row : Row} (read : rows[id]? = some row) (owner : row.owner = some node) : Predict rows node id
  | via {id node child target : Nat} {row : Row} (read : rows[id]? = some row) (owner : row.owner = some node)
      (first : row.first = some child) (known : Predict rows child target) : Predict rows node target

structure Bounded (rows : List Row) (names : List Nat) : Prop where
  owner : ∀ (id : Nat) (row : Row), rows[id]? = some row → ∀ node, row.owner = some node → node ∈ names
  first : ∀ (id : Nat) (row : Row), rows[id]? = some row → ∀ node, row.first = some node → node ∈ names

theorem reach_exact (rows : List Row) (names : List Nat) (bounded : Bounded rows names) (node id : Nat) :
    PredictionQueue.Reach (parents rows names) (seeds rows names) node id ↔ Predict rows node id := by
  constructor
  · intro reached
    induction reached with
    | @own node mask bit member present =>
      obtain ⟨name, listed, equal⟩ := List.mem_map.mp member
      have sameNode : name = node := congrArg Prod.fst equal
      have sameMask : SuffixSemantics.encode (owns rows name) = mask := congrArg Prod.snd equal
      rw [← sameMask] at present
      obtain ⟨row, read, owner⟩ := (owns_member rows name bit).mp ((SuffixSemantics.encode_bit _ _).mp present)
      have predicted := Predict.own read owner
      simpa only [sameNode] using predicted
    | parent earlier edge ih =>
      obtain ⟨member, source, row, read, owner, first⟩ := (parents_member _ _ _ _).mp edge
      exact Predict.via read owner first ih
  · intro predicted
    induction predicted with
    | @own id node row read owner =>
      have name := bounded.owner id row read node owner
      exact PredictionQueue.Reach.own (List.mem_map.mpr ⟨node, name, rfl⟩)
        ((SuffixSemantics.encode_bit _ _).mpr ((owns_member rows node id).mpr ⟨row, read, owner⟩))
    | @via source node child target row read owner first known ih =>
      exact PredictionQueue.Reach.parent ih ((parents_member _ _ _ _).mpr
        ⟨bounded.owner source row read node owner, source, row, read, owner, first⟩)

/-- Prefix an already discovered production path with the native first-symbol
edge. Source production lookup is functional, so equal IDs denote the same row. -/
theorem extend {rows : List Row} {root source child target : Nat} {row : Row}
    (earlier : Predict rows root source) (read : rows[source]? = some row) (first : row.first = some child)
    (later : Predict rows child target) : Predict rows root target := by
  induction earlier with
  | own decoded owner =>
    have same := Option.some.inj (decoded.symm.trans read)
    cases same
    exact Predict.via read owner first later
  | via decoded owner head known ih => exact Predict.via decoded owner head (ih read)

structure Decoded (g : CFG) (rows : List Row) (names : List Nat) (name : Nat → Nat) : Prop where
  source : ∀ (id : Nat) (row : Row), rows[id]? = some row → ∃ rule, g[id]? = some rule
  complete : ∀ (id : Nat) (rule : Production), g[id]? = some rule → ∃ row, rows[id]? = some row
  owner : ∀ (id : Nat) (row : Row) (rule : Production), rows[id]? = some row → g[id]? = some rule →
    ∀ node ∈ names, row.owner = some node ↔ rule.lhs = name node
  head : ∀ (id : Nat) (row : Row) (rule : Production), rows[id]? = some row → g[id]? = some rule → ∀ target,
    rule.rhs[0]? = some (.nonterminal target) ↔ ∃ child, row.first = some child ∧ name child = target

variable {g : CFG} {rows : List Row} {names : List Nat} {name : Nat → Nat}

theorem prefix_prediction {g : CFG} {name child : Nat} {parent target : Production}
    (known : LRStateSemantics.Prediction g name parent)
    (first : parent.rhs[0]? = some (.nonterminal child))
    (later : LRStateSemantics.Prediction g child target) : LRStateSemantics.Prediction g name target := by
  induction later with
  | own rule member lhs => exact LRStateSemantics.Prediction.next known first rule member lhs
  | next earlier head rule member lhs ih => exact LRStateSemantics.Prediction.next ih head rule member lhs

theorem prediction_member {g : CFG} {name : Nat} {rule : Production}
    (known : LRStateSemantics.Prediction g name rule) : rule ∈ g := by
  cases known with
  | own _ member _ => exact member
  | next _ _ _ member _ => exact member

theorem member_index {g : CFG} {rule : Production} (member : rule ∈ g) : ∃ (id : Nat), g[id]? = some rule := by
  obtain ⟨before, after, same⟩ := List.mem_iff_append.mp member
  exact ⟨before.length, by rw [same]; simp⟩

theorem decoded_sound {g rows names name} (bounded : Bounded rows names)
    (decoded : Decoded g rows names name) {node id : Nat} (known : Predict rows node id) :
    ∃ rule, g[id]? = some rule ∧ LRStateSemantics.Prediction g (name node) rule := by
  induction known with
  | @own id node row read owner =>
    obtain ⟨rule, source⟩ := decoded.source id row read
    exact ⟨rule, source, LRStateSemantics.Prediction.own rule (List.mem_of_getElem? source)
      ((decoded.owner id row rule read source node (bounded.owner id row read node owner)).mp owner)⟩
  | @via id node child target row read owner first known ih =>
    obtain ⟨rule, source⟩ := decoded.source id row read
    obtain ⟨result, output, predicted⟩ := ih
    have own := LRStateSemantics.Prediction.own rule (List.mem_of_getElem? source)
      ((decoded.owner id row rule read source node (bounded.owner id row read node owner)).mp owner)
    have head := (decoded.head id row rule read source (name child)).mpr ⟨child, first, rfl⟩
    exact ⟨result, output, prefix_prediction own head predicted⟩

theorem decoded_complete {g rows names name} (bounded : Bounded rows names)
    (decoded : Decoded g rows names name) (node : Nat) (selected : node ∈ names)
    {rule : Production} (known : LRStateSemantics.Prediction g (name node) rule)
    (id : Nat) (source : g[id]? = some rule) : Predict rows node id := by
  induction known generalizing id with
  | own rule member lhs =>
    obtain ⟨row, read⟩ := decoded.complete id rule source
    exact Predict.own read ((decoded.owner id row rule read source node selected).mpr lhs)
  | @next parent child earlier head rule member lhs ih =>
    obtain ⟨parentID, parentSource⟩ := member_index (prediction_member earlier)
    obtain ⟨parentRow, parentRead⟩ := decoded.complete parentID parent parentSource
    obtain ⟨childNode, first, sameName⟩ := (decoded.head parentID parentRow parent parentRead parentSource child).mp head
    obtain ⟨row, read⟩ := decoded.complete id rule source
    have owned : row.owner = some childNode := (decoded.owner id row rule read source childNode
      (bounded.first parentID parentRow parentRead childNode first)).mpr (lhs.trans sameName.symm)
    exact extend (ih parentID parentSource) parentRead first (Predict.own read owned)

/-- Native FIFO completion has the paper's prediction meaning for every actual
production ID, including two IDs whose semantic production values are equal. -/
theorem checked_prediction {g rows names name} (bounded : Bounded rows names)
    (decoded : Decoded g rows names name) (node id : Nat) (selected : node ∈ names)
    (witness : List Nat) (after : PredictionQueue.MState)
    (checked : PredictionQueue.replay (parents rows names) (PredictionQueue.initial (seeds rows names)) witness = some after)
    (finished : after.queue.work = []) :
    (after.queue.cells node).known.testBit id = true ↔
      ∃ rule, g[id]? = some rule ∧ LRStateSemantics.Prediction g (name node) rule := by
  rw [PredictionQueue.checked_exact _ _ witness after checked finished,
    reach_exact rows names bounded node id]
  constructor
  · exact decoded_sound bounded decoded
  · rintro ⟨rule, source, predicted⟩
    exact decoded_complete bounded decoded node selected predicted id source

end ParserEngine.PredictionCatalog
