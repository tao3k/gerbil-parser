import ParserEngine.SuffixSemantics

/-! LR0 prediction templates, the source's accumulator merge, and the paper
prediction/goto rules. This does not equate merged LALR states with canonical
LR1 states. Native production/name catalogues remain distinct adapters. -/
namespace ParserEngine.LRStateSemantics
open Grammar Viable

/-- Name-node propagation only follows the first RHS symbol, never an arbitrary
nonterminal occurring later in a production. Epsilon contributes its own rule. -/
inductive Prediction (g : CFG) (name : Nat) : Production → Prop where
  | own (rule : Production) (member : rule ∈ g) (lhs : rule.lhs = name) :
      Prediction g name rule
  | next {parent childName} (known : Prediction g name parent)
      (head : parent.rhs[0]? = some (.nonterminal childName))
      (rule : Production) (member : rule ∈ g) (lhs : rule.lhs = childName) :
      Prediction g name rule

def Template (g : CFG) (kernel : Core → Prop) (item : Core) : Prop :=
  ∃ root name, kernel root ∧
    root.production.rhs[root.dot]? = some (.nonterminal name) ∧
    Prediction g name item.production ∧ item.dot = 0

theorem prediction_supported {g : CFG} {seed : Core → Prop} {parent : Core} {name : Nat}
    (known : Closure g seed parent)
    (head : parent.production.rhs[parent.dot]? = some (.nonterminal name))
    {rule : Production} (prediction : Prediction g name rule) :
    Closure g seed ⟨rule, 0⟩ := by
  induction prediction with
  | own rule member lhs => exact Closure.predict known head rule member lhs
  | next earlier first rule member lhs ih => exact Closure.predict ih first rule member lhs

/-- Transitive prediction from a kernel is exactly LR0 closure; the forward
proof derives the witness by following the actual prediction ancestry. -/
theorem closure_exact {g : CFG} {kernel : Core → Prop} (item : Core) :
    Closure g kernel item ↔ kernel item ∨ Template g kernel item := by
  constructor
  · intro value
    induction value with
    | seed member => exact Or.inl member
    | @predict parent name known head rule member lhs ih =>
      rcases ih with root | ⟨origin, firstName, initial, next, earlier, zero⟩
      · exact Or.inr ⟨parent, name, root, head, Prediction.own rule member lhs, rfl⟩
      · have first : parent.production.rhs[0]? = some (.nonterminal name) := by
          simpa [zero] using head
        exact Or.inr ⟨origin, firstName, initial, next,
          Prediction.next earlier first rule member lhs, rfl⟩
  · intro value
    rcases value with root | ⟨origin, name, initial, next, prediction, zero⟩
    · exact Closure.seed root
    · have supported := prediction_supported (Closure.seed initial) next prediction
      cases item with
      | mk rule dot => dsimp at zero; subst dot; exact supported

/-- The source appends a reversed accumulator onto its initial tail using
(foldl cons tail found). Gerbil foldl's second argument is the initial value. -/
theorem foldl_cons_tail (found tail : List Nat) :
    found.foldl (fun accumulator item => item :: accumulator) tail = found.reverse ++ tail := by
  induction found generalizing tail with
  | nil => rfl
  | cons head rest ih => simp [ih, List.reverse_cons, List.append_assoc]

def orderedUnion : List Nat → List Nat → List Nat
  | [], right => right
  | left, [] => left
  | a :: left, b :: right =>
      if a = b then a :: orderedUnion left right
      else if a < b then a :: orderedUnion left (b :: right)
      else b :: orderedUnion (a :: left) right
termination_by left right => left.length + right.length

def merge : List Nat → List Nat → List Nat → List Nat
  | [], right, found => found.reverse ++ right
  | left, [], found => found.reverse ++ left
  | a :: left, b :: right, found =>
      if a = b then merge left right (a :: found)
      else if a < b then merge left (b :: right) (a :: found)
      else merge (a :: left) right (b :: found)
termination_by left right _ => left.length + right.length

theorem merge_accumulator (left right found : List Nat) :
    merge left right found = found.reverse ++ orderedUnion left right := by
  cases left with
  | nil => simp [merge, orderedUnion]
  | cons a left =>
    cases right with
    | nil => simp [merge, orderedUnion]
    | cons b right =>
      rw [merge, orderedUnion]
      split
      next equal =>
        rw [merge_accumulator left right (a :: found)]
        simp [List.reverse_cons, List.append_assoc]
      next unequal =>
        split
        next smaller =>
          rw [merge_accumulator left (b :: right) (a :: found)]
          simp [List.reverse_cons, List.append_assoc]
        next larger =>
          rw [merge_accumulator (a :: left) right (b :: found)]
          simp [List.reverse_cons, List.append_assoc]
termination_by left.length + right.length

theorem union_member (left right : List Nat) (item : Nat) :
    item ∈ orderedUnion left right ↔ item ∈ left ∨ item ∈ right := by
  cases left with
  | nil => simp [orderedUnion]
  | cons a left =>
    cases right with
    | nil => simp [orderedUnion]
    | cons b right =>
      rw [orderedUnion]
      split
      next equal =>
        subst b
        simp only [List.mem_cons, union_member left right item]
        simp [or_assoc, or_left_comm]
      next unequal =>
        split
        next smaller =>
          simp only [List.mem_cons, union_member left (b :: right) item]
          simp [or_assoc, or_left_comm]
        next larger =>
          simp only [List.mem_cons, union_member (a :: left) right item]
          simp [or_assoc, or_left_comm]
termination_by left.length + right.length

theorem merge_member (left right found : List Nat) (item : Nat) :
    item ∈ merge left right found ↔ item ∈ found ∨ item ∈ left ∨ item ∈ right := by
  rw [merge_accumulator]
  simp [union_member]

/-- The source merge cannot reverse a sorted slice. This is the ordering
premise needed by the already proved lookahead-node binary search. -/
theorem union_sorted (left right : List Nat)
    (leftSorted : left.Pairwise (· < ·)) (rightSorted : right.Pairwise (· < ·)) :
    (orderedUnion left right).Pairwise (· < ·) := by
  cases left with
  | nil => simpa [orderedUnion] using rightSorted
  | cons a left =>
    cases right with
    | nil => simpa [orderedUnion] using leftSorted
    | cons b right =>
      obtain ⟨leftHead, leftTail⟩ := List.pairwise_cons.mp leftSorted
      obtain ⟨rightHead, rightTail⟩ := List.pairwise_cons.mp rightSorted
      rw [orderedUnion]
      split
      next equal =>
        subst b
        apply List.pairwise_cons.mpr
        refine ⟨?_, union_sorted left right leftTail rightTail⟩
        intro item member
        rcases (union_member left right item).mp member with l | r
        · exact leftHead item l
        · exact rightHead item r
      next unequal =>
        split
        next smaller =>
          apply List.pairwise_cons.mpr
          refine ⟨?_, union_sorted left (b :: right) leftTail rightSorted⟩
          intro item member
          rcases (union_member left (b :: right) item).mp member with l | r
          · exact leftHead item l
          · rcases List.mem_cons.mp r with equal | rest
            · subst item; exact smaller
            · exact Nat.lt_trans smaller (rightHead item rest)
        next larger =>
          have smaller : b < a := by omega
          apply List.pairwise_cons.mpr
          refine ⟨?_, union_sorted (a :: left) right leftSorted rightTail⟩
          intro item member
          rcases (union_member (a :: left) right item).mp member with l | r
          · rcases List.mem_cons.mp l with equal | rest
            · subst item; exact smaller
            · exact Nat.lt_trans smaller (leftHead item rest)
          · exact rightHead item r
termination_by left.length + right.length

theorem closure_merge_sorted (kernel predictions : List Nat)
    (kernelSorted : kernel.Pairwise (· < ·)) (templateSorted : predictions.Pairwise (· < ·)) :
    (merge kernel predictions []).Pairwise (· < ·) := by
  simpa [merge_accumulator] using union_sorted kernel predictions kernelSorted templateSorted

/-- Decode physical core IDs only after the source merge; this composes list
publication with grammar closure, independently of traversal or kernel overlap. -/
theorem decoded_closure {g : CFG} {kernel : Core → Prop} (decode : Nat → Option Core)
    (seeds predictions : List Nat)
    (seedExact : ∀ item, kernel item ↔ ∃ code ∈ seeds, decode code = some item)
    (templateExact : ∀ item, Template g kernel item ↔ ∃ code ∈ predictions, decode code = some item)
    (item : Core) :
    (∃ code ∈ merge seeds predictions [], decode code = some item) ↔ Closure g kernel item := by
  rw [closure_exact]
  constructor
  · rintro ⟨code, member, decoded⟩
    have choices := (merge_member seeds predictions [] code).mp member
    rcases choices with impossible | seed | predicted
    · cases impossible
    · exact Or.inl ((seedExact item).mpr ⟨code, seed, decoded⟩)
    · exact Or.inr ((templateExact item).mpr ⟨code, predicted, decoded⟩)
  · intro choices
    rcases choices with seed | predicted
    · obtain ⟨code, member, decoded⟩ := (seedExact item).mp seed
      exact ⟨code, (merge_member seeds predictions [] code).mpr (Or.inr (Or.inl member)), decoded⟩
    · obtain ⟨code, member, decoded⟩ := (templateExact item).mp predicted
      exact ⟨code, (merge_member seeds predictions [] code).mpr (Or.inr (Or.inr member)), decoded⟩

/-- This composition removes per-suffix FIRST/nullable soundness assumptions
from prediction. The remaining decoding premises concern encoded lookaheads. -/
theorem compiled_prediction {g : CFG} {facts : SuffixSemantics.Facts}
    (factsExact : SuffixSemantics.Exact g facts) {seed : Item → Prop}
    {parent child : Item} {name : Nat} {suffix : List Symbol}
    (known : LR1Closure g seed parent)
    (afterDot : parent.production.rhs.drop parent.dot = .nonterminal name :: suffix)
    (member : child.production ∈ g) (lhs : child.production.lhs = name) (zero : child.dot = 0)
    (bit delta : Nat) (activated : Bool)
    (terminalDecode : (SuffixSemantics.scan facts suffix).first.testBit bit = true → child.lookahead = some bit)
    (inheritedDecode : delta.testBit bit = true → child.lookahead = parent.lookahead)
    (present : (Construction.suffixEvidence (SuffixSemantics.scan facts suffix).first
      (SuffixSemantics.scan facts suffix).nullable activated delta).testBit bit = true) :
    LR1Closure g seed child := by
  exact LR1Closure.predict known ⟨name, suffix, afterDot, member, lhs, zero,
    SuffixSemantics.delivered_allowed factsExact suffix parent.lookahead child.lookahead
      bit delta activated terminalDecode inheritedDecode present⟩

/-- The complete solver-to-prediction composition consumes histories, not a
user-supplied assertion that the suffix catalogue already has paper meaning. -/
theorem completed_prediction (g : CFG) (nullable : Worklist.State Nat)
    (first : Worklist.State (Nat × Nat))
    (nullableHistory : Worklist.Trace (Solver.nullableProgram g)
      (Worklist.initial (Solver.nullableProgram g)) nullable)
    (nullableFinished : nullable.pending = [])
    (firstHistory : Worklist.Trace (Solver.firstProgram g nullable.known (Solver.terminalCatalog g))
      (Worklist.initial (Solver.firstProgram g nullable.known (Solver.terminalCatalog g))) first)
    (firstFinished : first.pending = [])
    {seed : Item → Prop} {parent child : Item} {name : Nat} {suffix : List Symbol}
    (known : LR1Closure g seed parent)
    (afterDot : parent.production.rhs.drop parent.dot = .nonterminal name :: suffix)
    (member : child.production ∈ g) (lhs : child.production.lhs = name) (zero : child.dot = 0)
    (bit delta : Nat) (activated : Bool)
    (terminalDecode : (SuffixSemantics.scan (SuffixSemantics.fromCompleted nullable.known first.known) suffix).first.testBit bit = true → child.lookahead = some bit)
    (inheritedDecode : delta.testBit bit = true → child.lookahead = parent.lookahead)
    (present : (Construction.suffixEvidence
      (SuffixSemantics.scan (SuffixSemantics.fromCompleted nullable.known first.known) suffix).first
      (SuffixSemantics.scan (SuffixSemantics.fromCompleted nullable.known first.known) suffix).nullable
      activated delta).testBit bit = true) : LR1Closure g seed child := by
  exact compiled_prediction (SuffixSemantics.from_completed_exact g nullable first
    nullableHistory nullableFinished firstHistory firstFinished)
    known afterDot member lhs zero bit delta activated terminalDecode inheritedDecode present

/-- Physical +1 agrees with paper goto and preserves the lookahead, including
EOF. The after-dot read prevents crossing a production's completed slot. -/
theorem physical_goto {g : CFG} {production dot : Nat} {rule : Production}
    {symbol : Symbol} (row : g[production]? = some rule)
    (next : rule.rhs[dot]? = some symbol) (lookahead : Option Nat) :
    ∃ before after,
      decodeCore g (denseOffset (g.map (fun p => p.rhs.length)) production + dot) = some before ∧
      decodeCore g (denseOffset (g.map (fun p => p.rhs.length)) production + dot + 1) = some after ∧
      Advances ⟨before.production, before.dot, lookahead⟩ symbol
        ⟨after.production, after.dot, lookahead⟩ := by
  obtain ⟨before, after, decodedBefore, decodedAfter, shifted, symbolRead⟩ :=
    dense_goto_refines_advance row next
  refine ⟨before, after, decodedBefore, decodedAfter, symbolRead, ?_⟩
  cases shifted
  rfl

end ParserEngine.LRStateSemantics
