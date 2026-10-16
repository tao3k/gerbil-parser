import ParserEngine.Viable
import ParserEngine.Encoding

/-! Native construction contracts: dense key interning, monotone signature
refinement, conflict-preserving packing and cell-origin publication.
Hash/vector implementation and scheduler termination remain separate owners. -/
namespace ParserEngine.Construction
open Grammar Viable

/-- A canonical key is interned once; state zero is a successful lookup. -/
def intern [BEq α] [LawfulBEq α] (keys : List α) (key : α) : List α × Nat :=
  if key ∈ keys then (keys, keys.idxOf key) else (keys ++ [key], keys.length)

theorem intern_decodes [BEq α] [LawfulBEq α] (keys : List α) (key : α) :
    (intern keys key).1[(intern keys key).2]? = some key := by
  unfold intern
  split
  next member =>
    have bound := List.idxOf_lt_length_of_mem member
    simp [List.getElem?_eq_getElem bound, List.getElem_idxOf bound]
  next absent => simp

theorem intern_retains [BEq α] [LawfulBEq α] (keys : List α) (key : α) :
    ∀ old ∈ keys, old ∈ (intern keys key).1 := by
  unfold intern
  split <;> simp_all

theorem intern_unique [BEq α] [LawfulBEq α] (keys : List α) (key : α)
    (unique : keys.Nodup) : (intern keys key).1.Nodup := by
  unfold intern
  split
  next member => exact unique
  next absent =>
    apply List.nodup_append.mpr
    refine ⟨unique, by simp, ?_⟩
    intro a member b singleton equal
    have same : b = key := List.mem_singleton.mp singleton
    subst b
    subst a
    exact absent member

theorem intern_existing_zero [BEq α] [LawfulBEq α] (key : α) (rest : List α) :
    intern (key :: rest) key = (key :: rest, 0) := by simp [intern]

/-- Indexed lists recover equal keys from equal interned identities. This is
independent of hash iteration order and does not assume hash collision freedom. -/
theorem interned_identity_injective [BEq α] [LawfulBEq α]
    {keys : List α} {left right : α} (l : left ∈ keys) (r : right ∈ keys)
    (same : keys.idxOf left = keys.idxOf right) : left = right := by
  have a := List.getElem_idxOf (List.idxOf_lt_length_of_mem l)
  have b := List.getElem_idxOf (List.idxOf_lt_length_of_mem r)
  have aligned : left = keys[keys.idxOf right]'(List.idxOf_lt_length_of_mem r) := by simpa only [same] using a.symm
  exact aligned.trans b

/-- Closure adds dot-zero items only. Positive goto kernels can therefore be
recovered from their closures, justifying the native kernel-mask key. -/
theorem positive_kernel_recovered {g : CFG} {kernel : Core → Prop}
    (positive : ∀ item, kernel item → 0 < item.dot) (item : Core) :
    kernel item ↔ Closure g kernel item ∧ 0 < item.dot := by
  exact ⟨fun member => ⟨Closure.seed member, positive item member⟩,
    fun closed => positive_closure_has_seed closed.1 closed.2⟩

theorem positive_kernel_identity {g : CFG} {left right : Core → Prop}
    (lp : ∀ item, left item → 0 < item.dot) (rp : ∀ item, right item → 0 < item.dot)
    (same : ∀ item, Closure g left item ↔ Closure g right item) :
    ∀ item, left item ↔ right item := by
  intro item
  rw [positive_kernel_recovered lp item, positive_kernel_recovered rp item, same item]

/-- Keys retain the old group in both full and affected-only refinement.
A missing signature means inactive/singleton; it never means a missing owner. -/
def refinementKey (old : Nat → Nat) (signature : Nat → σ)
    (active : Nat → Bool) (node : Nat) : Nat × Option σ :=
  (old node, if active (old node) then some (signature node) else none)

theorem refinement_never_merges {old : Nat → Nat} {signature : Nat → σ}
    {active : Nat → Bool} {left right : Nat}
    (same : refinementKey old signature active left = refinementKey old signature active right) :
    old left = old right := congrArg Prod.fst same

theorem active_signature_agrees {old : Nat → Nat} {signature : Nat → σ}
    {active : Nat → Bool} {left right : Nat}
    (affected : active (old left) = true)
    (same : refinementKey old signature active left = refinementKey old signature active right) :
    signature left = signature right := by
  have group := refinement_never_merges same
  have signatures := congrArg Prod.snd same
  simpa [refinementKey, affected, ← group] using signatures

/-- The exact native mask predicate is kept here; identifying its nonzero
pair semantics is a separate integer-primitive obligation. -/
def conflicting (mask : Nat) : Bool := (mask &&& (mask - 1)) != 0

def admitMerge (members : List Nat) (accumulated candidate : Nat) : Bool :=
  let combined := accumulated ||| candidate
  !conflicting combined || candidate == combined || members.contains combined

theorem merge_conflict_has_origin {members accumulated candidate}
    (accepted : admitMerge members accumulated candidate = true)
    (conflict : conflicting (accumulated ||| candidate) = true) :
    accumulated ||| candidate ∈ candidate :: members := by
  simp only [admitMerge, conflict, Bool.not_true, Bool.false_or, Bool.or_eq_true,
    beq_iff_eq, List.contains_iff_mem] at accepted
  rcases accepted with equal | member
  · exact List.mem_cons.mpr (Or.inl equal.symm)
  · exact List.mem_cons.mpr (Or.inr member)

inductive Packing : List Nat → Nat → Prop where
  | single (mask : Nat) : Packing [mask] mask
  | add {members accumulated candidate} (earlier : Packing members accumulated)
      (accepted : admitMerge members accumulated candidate = true) :
      Packing (candidate :: members) (accumulated ||| candidate)

theorem packed_conflict_has_origin {members mask} (history : Packing members mask)
    (conflict : conflicting mask = true) : mask ∈ members := by
  cases history with
  | single => simp
  | add earlier accepted => exact merge_conflict_has_origin accepted conflict

theorem packed_bits_exact {members mask} (history : Packing members mask) (bit : Nat) :
    mask.testBit bit = true ↔ ∃ member ∈ members, member.testBit bit = true := by
  induction history with
  | single value => simp
  | add earlier accepted ih => simp [Nat.testBit_or, ih, or_comm]

/-- Refining a previously safe group does not itself preserve the witness.
The subgroup must replay admission using its own surviving canonical members. -/
theorem splitting_can_lose_conflict_origin :
    Packing [16, 8, 56] 56 ∧ conflicting 24 = true ∧ 24 ∉ ([8, 16] : List Nat) := by
  constructor
  · have first : Packing [8, 56] 56 := Packing.add (Packing.single 56) (by decide)
    exact Packing.add first (by decide)
  · decide

/-- A resolved cell contains only selected candidate actions. Rejection has
no selected action; it cannot fabricate a shift, reduce or accept. -/
inductive Resolution where
  | chosen (action : Grammar.Action)
  | fork (actions : List Grammar.Action)
  | rejected
  | failed
  deriving DecidableEq, Repr

def selected : Resolution → List Grammar.Action
  | .chosen action => [action]
  | .fork actions => actions
  | .rejected | .failed => []

inductive Choice where
  | left | right | fork | reject | fail
  deriving DecidableEq, Repr

inductive Association where
  | left | right | none | dynamic
  deriving DecidableEq, Repr

structure Priority where
  association : Association
  rank : Int
  deriving DecidableEq, Repr

def unresolved (allowFork : Bool) : Choice := if allowFork then .fork else .fail

def shiftReduceChoice (allowFork : Bool) (shift reduce : Option Priority) : Choice :=
  match shift, reduce with
  | some s, some r =>
      if s.association = .dynamic ∨ r.association = .dynamic then unresolved allowFork
      else if r.rank > s.rank then .right
      else if r.rank < s.rank then .left
      else match r.association with
        | .left => .right
        | .right => .left
        | .none => .reject
        | .dynamic => .fail
  | _, _ => unresolved allowFork

def reduceReduceChoice (allowFork : Bool) (left right : Option Priority) : Choice :=
  if left.any (fun p => p.association == .dynamic) || right.any (fun p => p.association == .dynamic)
    then unresolved allowFork
  else if (left.map Priority.rank).getD 0 > (right.map Priority.rank).getD 0 then .left
  else if (left.map Priority.rank).getD 0 < (right.map Priority.rank).getD 0 then .right
  else unresolved allowFork

def applyChoice (choice : Choice) (left right : Grammar.Action) : Resolution :=
  match choice with
  | .left => .chosen left
  | .right => .chosen right
  | .fork => .fork [left, right]
  | .reject => .rejected
  | .fail => .failed

def installChoice (choice : Choice) (before : Resolution) (incoming : Grammar.Action) : Resolution :=
  match before with
  | .chosen old => applyChoice choice old incoming
  | .fork actions => .fork (actions ++ [incoming])
  | .rejected => .rejected
  | .failed => .failed

theorem install_choice_origin (choice : Choice) (before : Resolution) (incoming : Grammar.Action) :
    ∀ action ∈ selected (installChoice choice before incoming),
      action ∈ incoming :: selected before := by
  cases before <;> cases choice <;> simp_all [installChoice, applyChoice, selected, or_comm]

inductive RawEvent where
  | shift (item : Item) (terminal target : Nat)
  | reduce (item : Item)
  | accept (item : Item)
  deriving DecidableEq, Repr

def eventAction : RawEvent → Grammar.Action
  | .shift _ _ target => .shift target
  | .reduce item => .reduce item.production
  | .accept _ => .accept

/-- Admission checks native-origin data, not a supplied CellSupported proof.
Item decoding, production-zero separation and goto publication are explicit
inputs whose correctness must be connected to the real constructor. -/
def admitEvent (g : CFG) (start : Nat) (augmented : Production) (table : Table)
    (state : Nat) (lookahead : Option Nat) : RawEvent → Bool
  | .shift item terminal target => decide (item ∈ table.items state ∧
      item.production.rhs[item.dot]? = some (.terminal terminal) ∧
      lookahead = some terminal ∧ table.goto state (.terminal terminal) = some target)
  | .reduce item => decide (item ∈ table.items state ∧ item.dot = item.production.rhs.length ∧
      item.lookahead = lookahead ∧ item.production ∈ g)
  | .accept item => decide (item ∈ table.items state ∧ item.production = augmented ∧
      item.dot = 1 ∧ item.lookahead = none ∧ lookahead = none ∧ augmented.rhs = [.nonterminal start])

theorem admitted_event_supported {g start augmented table state lookahead event}
    (admitted : admitEvent g start augmented table state lookahead event = true) :
    CellSupported g start augmented table state lookahead (eventAction event) := by
  cases event with
  | shift item terminal target =>
    have h := of_decide_eq_true admitted
    exact ⟨terminal, h.2.2.1, h.2.2.2, item, h.1, h.2.1⟩
  | reduce item =>
    have h := of_decide_eq_true admitted
    refine ⟨h.2.2.2, ?_⟩
    have same : item = ⟨item.production, item.production.rhs.length, lookahead⟩ := by
      cases item
      simp_all
    rw [← same]
    exact h.1
  | accept item =>
    have h := of_decide_eq_true admitted
    refine ⟨h.2.2.2.2.1, h.2.2.2.2.2, ?_⟩
    have same : item = ⟨augmented, 1, none⟩ := by cases item; simp_all
    rw [← same]
    exact h.1

/-- The producer retains provenance through every pairwise update. This is
weaker than policy equivalence: associativity/rank language choices need their
own contract, while origin is required under every conflict policy. -/
inductive CellTrace (origin : Grammar.Action → Prop) : Resolution → Prop where
  | empty : CellTrace origin .failed
  | first {action} (source : origin action) : CellTrace origin (.chosen action)
  | install {before after incoming} (earlier : CellTrace origin before)
      (source : origin incoming)
      (subset : ∀ action ∈ selected after, action ∈ incoming :: selected before) :
      CellTrace origin after

theorem cell_trace_supported {origin result} (history : CellTrace origin result) :
    ∀ action ∈ selected result, origin action := by
  induction history with
  | empty => simp [selected]
  | first source => simpa [selected] using source
  | install earlier source subset ih =>
    intro action member
    rcases List.mem_cons.mp (subset action member) with rfl | previous
    · exact source
    · exact ih action previous

/-- Every concrete policy decision preserves origins by construction. Fork
append and rejection are covered without assuming a final subset certificate. -/
theorem install_preserves_cell_trace {origin before incoming}
    (earlier : CellTrace origin before) (source : origin incoming) (choice : Choice) :
    CellTrace origin (installChoice choice before incoming) :=
  CellTrace.install earlier source (install_choice_origin choice before incoming)

theorem published_table_supported (g : CFG) (start : Nat) (augmented : Production)
    (table : Table) (results : Nat → Option Nat → Resolution)
    (histories : ∀ state lookahead, CellTrace
      (CellSupported g start augmented table state lookahead) (results state lookahead))
    (publication : ∀ state lookahead action, table.action state lookahead = some action →
      action ∈ selected (results state lookahead)) : TableSupported g start augmented table := by
  intro state lookahead action member
  exact cell_trace_supported (histories state lookahead) action (publication state lookahead action member)

end ParserEngine.Construction
