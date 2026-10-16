import ParserEngine.Viable
import ParserEngine.Encoding

/-! Native construction contracts: dense key interning, monotone signature
refinement, conflict-preserving packing and cell-origin publication.
Physical identity, dirty dependency transport and event-row publication are
composed below. Host hash/vector refinement and termination remain separate owners. -/
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

/-- Physical identity precedes the semantic production projection. Equal rule
bodies can have different native precedence, aliases and reduction actions. -/
structure PhysicalItem where
  production : Nat
  dot : Nat
  lookahead : Option Nat
  deriving DecidableEq, Repr

def decodePhysical (rows : CFG) (item : PhysicalItem) : Option Item :=
  (rows[item.production]?).map (fun rule => ⟨rule, item.dot, item.lookahead⟩)

theorem physical_code_injective {widths : List Nat} {left right lw rw ld rd : Nat}
    (l : widths[left]? = some lw) (r : widths[right]? = some rw)
    (lv : ld ≤ lw) (rv : rd ≤ rw)
    (same : denseOffset widths left + ld = denseOffset widths right + rd) :
    left = right ∧ ld = rd := by
  have a := dense_coordinate_roundtrip l lv
  have b := dense_coordinate_roundtrip r rv
  rw [same] at a
  have equal := Option.some.inj (a.symm.trans b)
  exact ⟨congrArg Prod.fst equal, congrArg Prod.snd equal⟩

theorem duplicate_rules_keep_physical_identity (rule : Production) :
    decodePhysical [rule, rule] ⟨0, 0, none⟩ = decodePhysical [rule, rule] ⟨1, 0, none⟩ ∧
    denseOffset [rule.rhs.length, rule.rhs.length] 0 ≠
      denseOffset [rule.rhs.length, rule.rhs.length] 1 := by
  simp [decodePhysical, denseOffset]


inductive PhysicalEvent where
  | shift (item : PhysicalItem) (terminal target : Nat)
  | reduce (item : PhysicalItem)
  | accept (item : PhysicalItem)
  deriving DecidableEq, Repr

/-- Production zero is reserved for augmentation, before semantic projection.
Even an authored production with the same lhs/rhs cannot take its identity. -/
def decodePhysicalEvent (rows : CFG) : PhysicalEvent → Option RawEvent
  | .shift item terminal target =>
      (decodePhysical rows item).map (fun decoded => .shift decoded terminal target)
  | .reduce item =>
      if item.production = 0 then none else (decodePhysical rows item).map RawEvent.reduce
  | .accept item =>
      if item.production = 0 then (decodePhysical rows item).map RawEvent.accept else none

def admitPhysicalEvent (g : CFG) (start : Nat) (augmented : Production) (table : Table)
    (state : Nat) (lookahead : Option Nat) (event : PhysicalEvent) : Bool :=
  match decodePhysicalEvent (augmented :: g) event with
  | none => false
  | some decoded => admitEvent g start augmented table state lookahead decoded

theorem admitted_physical_event_supported {g start augmented table state lookahead event}
    (admitted : admitPhysicalEvent g start augmented table state lookahead event = true) :
    ∃ decoded, decodePhysicalEvent (augmented :: g) event = some decoded ∧
      CellSupported g start augmented table state lookahead (eventAction decoded) := by
  unfold admitPhysicalEvent at admitted
  split at admitted
  · contradiction
  · rename_i decoded equation
    exact ⟨decoded, equation, admitted_event_supported admitted⟩

theorem augmented_cannot_reduce (rows : CFG) (dot : Nat) (lookahead : Option Nat) :
    decodePhysicalEvent rows (.reduce ⟨0, dot, lookahead⟩) = none := by
  simp [decodePhysicalEvent]

theorem authored_cannot_accept (rows : CFG) (item : PhysicalItem)
    (authored : item.production ≠ 0) : decodePhysicalEvent rows (.accept item) = none := by
  simp [decodePhysicalEvent, authored]

/-- A group is split exactly when one of its peers acquires a different ID.
Numeric renumbering alone is not a split. -/
def SplitGroup (old next : Nat → Nat) (source : Nat) : Prop :=
  ∃ peer, old peer = old source ∧ next peer ≠ next source

def DirtyTarget (old next : Nat → Nat) (edge : Nat → Nat → Nat → Prop)
    (node : Nat) : Prop :=
  ∃ source target label, SplitGroup old next source ∧ edge source label target ∧
    next target = next node

def SameIncoming (partition : Nat → Nat) (edge : Nat → Nat → Nat → Prop)
    (left right : Nat) : Prop :=
  ∀ label group, (∃ source, edge source label left ∧ partition source = group) ↔
    (∃ source, edge source label right ∧ partition source = group)

theorem split_successor_dirty {old next edge source target label}
    (split : SplitGroup old next source) (successor : edge source label target) :
    DirtyTarget old next edge target := ⟨source, target, label, split, successor, rfl⟩

/-- Forward signatures are sets of predecessor groups per label. If a target
is not dirty, every incoming old group is unsplit. Equality of signatures then
survives arbitrary dense renumbering, not merely unchanged integer labels. -/
theorem clean_predecessor_transport {old next edge left right}
    (clean : ¬ DirtyTarget old next edge left)
    (earlier : SameIncoming old edge left right) : SameIncoming next edge left right := by
  intro label group
  constructor
  · rintro ⟨source, incoming, identity⟩
    obtain ⟨peer, other, previous⟩ := (earlier label (old source)).mp ⟨source, incoming, rfl⟩
    have unsplit : ¬ SplitGroup old next source :=
      fun split => clean (split_successor_dirty split incoming)
    have equal : next peer = next source := by
      by_cases h : next peer = next source
      · exact h
      · exact False.elim (unsplit ⟨peer, previous, h⟩)
    exact ⟨peer, other, equal.trans identity⟩
  · rintro ⟨source, incoming, identity⟩
    obtain ⟨peer, other, previous⟩ := (earlier label (old source)).mpr ⟨source, incoming, rfl⟩
    have unsplit : ¬ SplitGroup old next peer :=
      fun split => clean (split_successor_dirty split other)
    have equal : next source = next peer := by
      by_cases h : next source = next peer
      · exact h
      · exact False.elim (unsplit ⟨source, previous.symm, h⟩)
    exact ⟨peer, other, equal.symm.trans identity⟩

/-- None is an untouched scratch cell, distinct from a failed resolution. -/
def writeCandidate (before : Option Resolution) (choice : Choice)
    (incoming : Grammar.Action) : Option Resolution :=
  some (match before with
    | none => .chosen incoming
    | some result => installChoice choice result incoming)

def replayCandidates : List (Choice × RawEvent) → Option Resolution → Option Resolution
  | [], before => before
  | (choice, event) :: rest, before =>
      replayCandidates rest (writeCandidate before choice (eventAction event))

def OptionalTrace (origin : Grammar.Action → Prop) : Option Resolution → Prop
  | none => True
  | some result => CellTrace origin result

theorem replay_candidates_trace {origin events before}
    (sources : ∀ entry ∈ events, origin (eventAction entry.2))
    (initialHistory : OptionalTrace origin before) :
    OptionalTrace origin (replayCandidates events before) := by
  induction events generalizing before with
  | nil => exact initialHistory
  | cons entry rest ih =>
    apply ih (fun candidate member => sources candidate (List.mem_cons_of_mem entry member))
    have source := sources entry (by simp)
    cases before with
    | none => exact CellTrace.first source
    | some result => exact install_preserves_cell_trace initialHistory source entry.1

/-- Final histories are generated by concrete event replay, rather than supplied
as a premise about the completed table. Publication may choose any retained
fork alternative; rejection and failure cannot publish an action. -/
theorem replayed_table_supported (g : CFG) (start : Nat) (augmented : Production)
    (table : Table) (events : Nat → Option Nat → List (Choice × RawEvent))
    (admission : ∀ state lookahead entry, entry ∈ events state lookahead →
      admitEvent g start augmented table state lookahead entry.2 = true)
    (publication : ∀ state lookahead action, table.action state lookahead = some action →
      ∃ result, replayCandidates (events state lookahead) none = some result ∧
        action ∈ selected result) : TableSupported g start augmented table := by
  intro state lookahead action published
  obtain ⟨result, execution, member⟩ := publication state lookahead action published
  have history := replay_candidates_trace
    (fun entry present => admitted_event_supported (admission state lookahead entry present))
    (show OptionalTrace (CellSupported g start augmented table state lookahead) none from True.intro)
  rw [execution] at history
  exact cell_trace_supported history action member

/-- The touched catalog covers every live slot. Duplicate touches are harmless;
the native producer's unique first-install catalog is a stronger property. -/
def ScratchCovered (slots : Nat → Option Resolution) (touched : List Nat) : Prop :=
  ∀ key, key ∉ touched → slots key = none

def scratchWrite (slots : Nat → Option Resolution) (key : Nat) (result : Resolution) :=
  fun query => if query = key then some result else slots query

def scratchClear (slots : Nat → Option Resolution) (touched : List Nat) :=
  fun key => if key ∈ touched then none else slots key

theorem scratch_write_covered {slots touched key result}
    (covered : ScratchCovered slots touched) :
    ScratchCovered (scratchWrite slots key result) (key :: touched) := by
  intro query absent
  have different : query ≠ key := fun same => absent (by simp [same])
  have previous : query ∉ touched := fun member => absent (List.mem_cons_of_mem key member)
  simp [scratchWrite, different, covered query previous]

theorem scratch_clear_empty {slots touched} (covered : ScratchCovered slots touched) :
    ∀ key, scratchClear slots touched key = none := by
  intro key
  by_cases present : key ∈ touched
  · simp [scratchClear, present]
  · simp [scratchClear, present, covered key present]

/-- A frozen row is a value, not an alias of reusable mutable scratch storage.
This transition publishes the old value and returns clean storage for the next
state. The native vector-to-row copy remains an explicit implementation bridge. -/
def publishScratch (slots : Nat → Option Resolution) (touched : List Nat) :=
  (slots, scratchClear slots touched)

theorem publication_retains_row_and_clears {slots touched}
    (covered : ScratchCovered slots touched) :
    (publishScratch slots touched).1 = slots ∧
      ∀ key, (publishScratch slots touched).2 key = none :=
  ⟨rfl, scratch_clear_empty covered⟩

/-- Decode every physical candidate before policy replay. Invalid IDs never
become semantic actions. The admission premise additionally rules out dropping
invalid input silently in a successful native construction. -/
def decodeCandidates (rows : CFG) (events : List (Choice × PhysicalEvent)) :
    List (Choice × RawEvent) :=
  events.filterMap (fun entry =>
    (decodePhysicalEvent rows entry.2).map (fun decoded => (entry.1, decoded)))

theorem decoded_candidates_admitted {g start augmented table state lookahead events}
    (admission : ∀ entry ∈ events,
      admitPhysicalEvent g start augmented table state lookahead entry.2 = true) :
    ∀ decoded ∈ decodeCandidates (augmented :: g) events,
      admitEvent g start augmented table state lookahead decoded.2 = true := by
  intro decoded member
  obtain ⟨entry, present, equation⟩ := List.mem_filterMap.mp member
  have admitted := admission entry present
  unfold admitPhysicalEvent at admitted
  cases physical : decodePhysicalEvent (augmented :: g) entry.2 with
  | none => simp [physical] at admitted
  | some raw =>
    simp only [physical, Option.map_some, Option.some.injEq] at equation
    subst decoded
    simpa only [physical] using admitted

theorem physical_replayed_table_supported (g : CFG) (start : Nat) (augmented : Production)
    (table : Table) (events : Nat → Option Nat → List (Choice × PhysicalEvent))
    (admission : ∀ state lookahead entry, entry ∈ events state lookahead →
      admitPhysicalEvent g start augmented table state lookahead entry.2 = true)
    (publication : ∀ state lookahead action, table.action state lookahead = some action →
      ∃ result, replayCandidates (decodeCandidates (augmented :: g) (events state lookahead)) none =
        some result ∧ action ∈ selected result) : TableSupported g start augmented table :=
  replayed_table_supported g start augmented table
    (fun state lookahead => decodeCandidates (augmented :: g) (events state lookahead))
    (fun state lookahead => decoded_candidates_admitted (admission state lookahead)) publication

/-- Active groups are re-keyed by their actual predecessor signature. Inactive
old groups retain the scheduler invariant. Thus every resulting group agrees
under the old partition, even when an active old group genuinely splits. -/
theorem refined_signature_uniform {old next : Nat → Nat} {signature : Nat → σ}
    {active : Nat → Bool} {edge : Nat → Nat → Nat → Prop}
    (keyed : ∀ left right, next left = next right →
      refinementKey old signature active left = refinementKey old signature active right)
    (signatureSound : ∀ left right, signature left = signature right →
      SameIncoming old edge left right)
    (inactive : ∀ left right, old left = old right → active (old left) = false →
      SameIncoming old edge left right)
    {left right} (same : next left = next right) : SameIncoming old edge left right := by
  have keys := keyed left right same
  cases affected : active (old left) with
  | false => exact inactive left right (refinement_never_merges keys) affected
  | true => exact signatureSound left right (active_signature_agrees affected keys)

/-- New clean groups retain uniform signatures after dirty delivery, allowing
elision in the next round. Active old groups need not already be uniform. -/
theorem clean_group_uniform {old next : Nat → Nat} {signature : Nat → σ}
    {active : Nat → Bool} {edge : Nat → Nat → Nat → Prop}
    (keyed : ∀ left right, next left = next right →
      refinementKey old signature active left = refinementKey old signature active right)
    (signatureSound : ∀ left right, signature left = signature right →
      SameIncoming old edge left right)
    (inactive : ∀ left right, old left = old right → active (old left) = false →
      SameIncoming old edge left right)
    {left right} (same : next left = next right)
    (clean : ¬ DirtyTarget old next edge left) : SameIncoming next edge left right :=
  clean_predecessor_transport clean
    (refined_signature_uniform keyed signatureSound inactive same)

/-- Newest event first, matching the cons-built native provenance catalog.
Each cell resolves against its earlier value; the row records its touched key. -/
def buildEventScratch : List (Nat × Choice × RawEvent) →
    (Nat → Option Resolution) × List Nat
  | [] => (fun _ => none, [])
  | (key, choice, event) :: rest =>
      let previous := buildEventScratch rest
      let result := match previous.1 key with
        | none => Resolution.chosen (eventAction event)
        | some before => installChoice choice before (eventAction event)
      (scratchWrite previous.1 key result, key :: previous.2)

theorem event_scratch_covered (events : List (Nat × Choice × RawEvent)) :
    ScratchCovered (buildEventScratch events).1 (buildEventScratch events).2 := by
  induction events with
  | nil => intro key absent; rfl
  | cons entry rest ih => exact scratch_write_covered ih

theorem event_scratch_trace {origin : Nat → Grammar.Action → Prop} {events}
    (sources : ∀ entry ∈ events, origin entry.1 (eventAction entry.2.2)) :
    ∀ key, OptionalTrace (origin key) ((buildEventScratch events).1 key) := by
  induction events with
  | nil => intro key; trivial
  | cons entry rest ih =>
    have earlier := ih (fun candidate present => sources candidate (List.mem_cons_of_mem entry present))
    intro key
    by_cases same : key = entry.1
    · subst key
      have source := sources entry (by simp)
      have initialHistory := earlier entry.1
      simp only [buildEventScratch, scratchWrite, ite_true]
      cases previous : (buildEventScratch rest).1 entry.1 with
      | none => exact CellTrace.first source
      | some result =>
        rw [previous] at initialHistory
        exact install_preserves_cell_trace initialHistory source entry.2.1
    · simpa only [buildEventScratch, scratchWrite, ite_eq_right same] using earlier key

/-- One composition proves event origins, frozen row contents and empty storage
for the next state. It takes neither a final CellTrace nor a coverage premise. -/
theorem event_row_publication {origin : Nat → Grammar.Action → Prop} {events}
    (sources : ∀ entry ∈ events, origin entry.1 (eventAction entry.2.2)) :
    let built := buildEventScratch events
    let published := publishScratch built.1 built.2
    (∀ key result, published.1 key = some result →
      ∀ action ∈ selected result, origin key action) ∧
    (∀ key, published.2 key = none) := by
  constructor
  · intro key result equation
    have history := event_scratch_trace sources key
    have stored : (buildEventScratch events).1 key = some result := equation
    rw [stored] at history
    exact cell_trace_supported history
  · exact scratch_clear_empty (event_scratch_covered events)

end ParserEngine.Construction
