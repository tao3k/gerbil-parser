import ParserEngine.Worklist
import Init.Data.Nat.Bitwise

/-! Unbounded masks and batch notification. Nat represents nonnegative Scheme
integers, not machine words. Primitive correctness and vector access remain
host obligations. No representation theorem below is a timing claim. -/
namespace ParserEngine.Encoding

def difference (a b : Nat) : Nat := Nat.bitwise (fun x y => x && !y) a b

@[simp] theorem difference_bit (a b i : Nat) :
    (difference a b).testBit i = (a.testBit i && !b.testBit i) := by
  exact Nat.testBit_bitwise (by rfl) a b i

theorem difference_fresh (a b i : Nat) :
    (difference a b).testBit i = true ↔ a.testBit i = true ∧ b.testBit i = false := by simp

theorem delta_conservation (a b : Nat) : b ||| difference a b = b ||| a := by
  apply Nat.eq_of_testBit_eq
  intro i
  simp only [Nat.testBit_or, difference_bit]
  cases a.testBit i <;> cases b.testBit i <;> rfl

theorem delta_disjoint (a b : Nat) : b &&& difference a b = 0 := by
  apply Nat.eq_of_testBit_eq
  intro i
  simp only [Nat.testBit_and, difference_bit, Nat.zero_testBit]
  cases a.testBit i <;> cases b.testBit i <;> rfl

theorem mask_zero_iff (a : Nat) : a = 0 ↔ ∀ i, a.testBit i = false := by
  constructor
  · rintro rfl; simp
  · intro h; exact Nat.eq_of_testBit_eq (fun i => by simpa using h i)

/-- Scheme extract-bit-field width offset without a signed complement. -/
def field (a width offset : Nat) : Nat := (a >>> offset) % 2 ^ width

theorem field_member (a width offset i : Nat) :
    (field a width offset).testBit i = true ↔ i < width ∧ a.testBit (offset + i) = true := by
  simp [field, Nat.testBit_mod_two_pow, Nat.testBit_shiftRight]

/-- Every bit belongs to a field for any positive host field width. -/
theorem field_address (width bit : Nat) (positive : 0 < width) :
    bit % width < width ∧ width * (bit / width) + bit % width = bit := by
  exact ⟨Nat.mod_lt _ positive, by simpa [Nat.add_comm] using Nat.mod_add_div bit width⟩

theorem field_complete (a width bit : Nat) (positive : 0 < width) :
    (field a width (width * (bit / width))).testBit (bit % width) = true ↔
      a.testBit bit = true := by
  rw [field_member]
  have address := field_address width bit positive
  simp [address.1, address.2]

structure Cell where
  known : Nat
  pending : Nat
  deriving DecidableEq, Repr

def merge (s : Cell) (incoming : Nat) : Cell :=
  let delta := difference incoming s.known
  ⟨s.known ||| delta, s.pending ||| delta⟩

def needsEnqueue (s : Cell) (incoming : Nat) : Prop :=
  difference incoming s.known ≠ 0 ∧ s.pending = 0

@[simp] theorem merge_known (s : Cell) (incoming : Nat) :
    (merge s incoming).known = s.known ||| incoming := delta_conservation _ _

theorem merge_pending (s : Cell) (incoming i : Nat) :
    (merge s incoming).pending.testBit i = true ↔ s.pending.testBit i = true ∨
      (incoming.testBit i = true ∧ s.known.testBit i = false) := by simp [merge]

theorem coalesced (s : Cell) (incoming : Nat) (queued : s.pending ≠ 0) :
    ¬ needsEnqueue s incoming := by simp [needsEnqueue, queued]

def clear (s : Cell) : Cell := ⟨s.known, 0⟩

/-- Clear before invoking callbacks, allowing a fresh bit to requeue this owner. -/
theorem callback_requeues (s : Cell) (incoming : Nat)
    (fresh : difference incoming s.known ≠ 0) : needsEnqueue (clear s) incoming := by
  exact ⟨fresh, rfl⟩

theorem pending_conserved (s : Cell) (incoming i : Nat)
    (old : s.pending.testBit i = true → s.known.testBit i = true) :
    (merge s incoming).pending.testBit i = true → (merge s incoming).known.testBit i = true := by
  rw [merge_pending, merge_known, Nat.testBit_or, Bool.or_eq_true]
  intro h
  rcases h with h | h
  · exact Or.inl (old h)
  · exact Or.inr h.1

/-- The native nonzero-and-singleton membership test has the same meaning as
testBit. This applies beyond a fixnum width without a chosen finite bound. -/
theorem singleton_membership (a i : Nat) :
    a &&& (1 <<< i) ≠ 0 ↔ a.testBit i = true := by
  rw [Nat.shiftLeft_eq, Nat.one_mul]
  constructor
  · intro nonzero
    cases bitValue : a.testBit i with
    | true => rfl
    | false =>
      apply False.elim (nonzero ?_)
      apply Nat.eq_of_testBit_eq
      intro j
      by_cases same : i = j
      · subst j; simp [bitValue]
      · simp [same]
  · intro member zero
    have observed := congrArg (fun mask : Nat => mask.testBit i) zero
    simp [member] at observed

/-- Coalescing a fresh delta cannot erase already delivered facts. Delivery is
known minus pending; publication and notification are distinct operations. -/
theorem merge_delivered (s : Cell) (incoming : Nat) :
    difference (merge s incoming).known (merge s incoming).pending = difference s.known s.pending := by
  apply Nat.eq_of_testBit_eq
  intro i
  simp only [merge, difference_bit, Nat.testBit_or]
  cases s.known.testBit i <;> cases s.pending.testBit i <;> cases incoming.testBit i <;> rfl

structure Queue where
  cells : Nat → Cell
  work : List Nat

/-- An owner occurs once exactly when its row has undelivered bits. -/
structure QueueValid (q : Queue) : Prop where
  unique : q.work.Nodup
  support : ∀ owner, owner ∈ q.work ↔ (q.cells owner).pending ≠ 0

def publish (q : Queue) (owner incoming : Nat) : Queue :=
  let before := q.cells owner
  let delta := difference incoming before.known
  if delta = 0 then q else
    ⟨(fun index => if index = owner then merge before incoming else q.cells index),
      if before.pending = 0 then owner :: q.work else q.work⟩

theorem publish_queue_valid (q : Queue) (valid : QueueValid q) (owner incoming : Nat) :
    QueueValid (publish q owner incoming) := by
  unfold publish
  dsimp only
  split
  · exact valid
  next fresh =>
    constructor
    · split
      next empty =>
        apply List.nodup_cons.mpr
        exact ⟨fun member => (valid.support owner).mp member empty, valid.unique⟩
      next pending => exact valid.unique
    · intro index
      by_cases same : index = owner
      · subst index
        simp only [↓reduceIte, merge]
        have nonzero : ¬ ((q.cells owner).pending ||| difference incoming (q.cells owner).known) = 0 := by
          intro zero
          exact fresh (Nat.or_eq_zero_iff.mp zero).2
        split
        · simp [nonzero]
        · simpa [nonzero] using (valid.support owner).mpr (by assumption)
      · simp only [same, ↓reduceIte]
        split <;> simpa [same] using valid.support index

/-- Clearing precedes callbacks; removing the unique head loses no other row. -/
def drain (q : Queue) (owner : Nat) (rest : List Nat) : Queue :=
  ⟨(fun index => if index = owner then clear (q.cells owner) else q.cells index), rest⟩

theorem drain_queue_valid (q : Queue) (valid : QueueValid q) (owner : Nat) (rest : List Nat)
    (front : q.work = owner :: rest) : QueueValid (drain q owner rest) := by
  have unique : owner ∉ rest ∧ rest.Nodup := List.nodup_cons.mp (front ▸ valid.unique)
  constructor
  · exact unique.2
  · intro index
    by_cases same : index = owner
    · subst index; simp [drain, clear, unique.1]
    · have old := valid.support index
      rw [front] at old
      simpa [drain, same] using old

theorem queue_empty_exact (q : Queue) (valid : QueueValid q) :
    q.work = [] ↔ ∀ owner, (q.cells owner).pending = 0 := by
  constructor
  · intro empty owner
    by_cases zero : (q.cells owner).pending = 0
    · exact zero
    · have member := (valid.support owner).mpr zero
      simp [empty] at member
  · intro empty
    cases h : q.work with
    | nil => rfl
    | cons owner rest =>
      have nonempty := (valid.support owner).mp (by simp [h])
      exact False.elim (nonempty (empty owner))

/-- A finite catalog decodes dense owner/bit coordinates. Coverage of the
catalog is a producer obligation, independent of bit arithmetic. -/
def decode (masks : Nat → Nat) (catalog : List (Nat × Nat)) : List (Nat × Nat) :=
  catalog.filter (fun coordinate => (masks coordinate.1).testBit coordinate.2)

@[simp] theorem decode_member (masks : Nat → Nat) (catalog : List (Nat × Nat)) (p : Nat × Nat) :
    p ∈ decode masks catalog ↔ p ∈ catalog ∧ (masks p.1).testBit p.2 = true := by
  simp [decode]

def clearOwner (masks : Nat → Nat) (owner : Nat) : Nat → Nat :=
  fun index => if index = owner then 0 else masks index

def ownerBatch (masks : Nat → Nat) (catalog : List (Nat × Nat)) (owner : Nat) : List (Nat × Nat) :=
  (decode masks catalog).filter (fun p => p.1 == owner)

/-- Derives the abstract queue partition from actual vector row clearing.
The batch partition is not assumed by this physical decoding theorem. -/
theorem clear_owner_partition (masks : Nat → Nat) (catalog : List (Nat × Nat)) (owner : Nat)
    (p : Nat × Nat) :
    p ∈ decode masks catalog ↔ p ∈ ownerBatch masks catalog owner ∨
      p ∈ decode (clearOwner masks owner) catalog := by
  by_cases same : p.1 = owner <;> simp [ownerBatch, clearOwner, same]

end ParserEngine.Encoding

namespace ParserEngine.Worklist

/-- A coalesced owner delivers its pending bits before scanning dependents.
The partition permits arbitrary batch size and physical queue order. -/
def batchState (s : State Fact) (batch rest : List Fact) : State Fact :=
  {s with delivered := batch ++ s.delivered, pending := rest}

def batchNotify [DecidableEq Fact] (program : List (Clause Fact))
    (s : State Fact) (batch rest : List Fact) : State Fact :=
  scan (program.filter fun clause => clause.dependencies.any (fun fact => batch.contains fact))
    (batchState s batch rest)

theorem batch_accounting {s : State Fact} (valid : Accounting s) (batch rest : List Fact)
    (partition : ∀ fact, fact ∈ s.pending ↔ fact ∈ batch ∨ fact ∈ rest) :
    Accounting (batchState s batch rest) := by
  constructor
  · intro fact member
    rcases List.mem_append.mp member with current | old
    · exact valid.pending fact ((partition fact).mpr (Or.inl current))
    · exact valid.delivered fact old
  · intro fact member
    exact valid.pending fact ((partition fact).mpr (Or.inr member))
  · intro fact member
    rcases valid.conserved fact member with old | pending
    · exact Or.inl (List.mem_append_right _ old)
    · rcases (partition fact).mp pending with current | rest
      · exact Or.inl (List.mem_append_left _ current)
      · exact Or.inr rest

theorem batch_notification_valid [DecidableEq Fact] (program : List (Clause Fact))
    (s : State Fact) (valid : Valid program s) (batch rest : List Fact)
    (partition : ∀ fact, fact ∈ s.pending ↔ fact ∈ batch ∨ fact ∈ rest) :
    Valid program (batchNotify program s batch rest) := by
  let affected := program.filter fun clause => clause.dependencies.any (fun fact => batch.contains fact)
  let before := batchState s batch rest
  have accounting := batch_accounting valid.accounting batch rest partition
  constructor
  · exact scan_accounting affected before accounting
  · exact scan_sound program affected (fun clause member => (List.mem_filter.mp member).1)
      before accounting valid.sound
  · intro clause member ready
    change ∀ dependency ∈ clause.dependencies, dependency ∈ (scan affected before).delivered at ready
    rw [scan_delivered] at ready
    by_cases touches : ∃ dependency ∈ clause.dependencies, dependency ∈ batch
    · apply scan_closes affected before clause
      · apply List.mem_filter.mpr
        exact ⟨member, by simpa only [List.any_eq_true, List.contains_iff_mem] using touches⟩
      · exact ready
    · apply scan_monotone affected before
      apply valid.closed clause member
      intro dependency belongs
      rcases List.mem_append.mp (ready dependency belongs) with current | old
      · exact False.elim (touches ⟨dependency, belongs, current⟩)
      · exact old

inductive BatchTrace [DecidableEq Fact] (program : List (Clause Fact)) : State Fact → State Fact → Prop where
  | refl (s : State Fact) : BatchTrace program s s
  | next {a b batch rest} (history : BatchTrace program a b)
      (partition : ∀ fact, fact ∈ b.pending ↔ fact ∈ batch ∨ fact ∈ rest) :
      BatchTrace program a (batchNotify program b batch rest)

theorem batch_trace_valid [DecidableEq Fact] {program : List (Clause Fact)} {before after : State Fact}
    (valid : Valid program before) (history : BatchTrace program before after) : Valid program after := by
  induction history with
  | refl => exact valid
  | next history partition ih => exact batch_notification_valid program _ ih _ _ partition

/-- No final closure, final equality or successful certificate is assumed. -/
theorem batch_quiescent_exact [DecidableEq Fact] (program : List (Clause Fact)) (last : State Fact)
    (history : BatchTrace program (initial program) last) (finished : last.pending = []) :
    ∀ fact, fact ∈ last.known ↔ Entails program fact := by
  have valid := batch_trace_valid (initial_valid program) history
  intro fact
  constructor
  · exact valid.sound fact
  · intro derivation
    induction derivation with
    | rule clause member body ih =>
      apply valid.closed clause member
      intro dependency belongs
      rcases valid.accounting.conserved dependency (ih dependency belongs) with delivered | pending
      · exact delivered
      · simp [finished] at pending

/-- Concrete coalesced row delivery refines batch notification without an
externally supplied partition certificate. Any finite coordinate catalog is
allowed, including nullable/productive bit pairs and FIRST terminal IDs. -/
theorem mask_notification_valid (program : List (Clause (Nat × Nat)))
    (s : State (Nat × Nat)) (valid : Valid program s) (masks : Nat → Nat)
    (catalog : List (Nat × Nat)) (owner : Nat)
    (representation : s.pending = Encoding.decode masks catalog) :
    Valid program (batchNotify program s (Encoding.ownerBatch masks catalog owner)
      (Encoding.decode (Encoding.clearOwner masks owner) catalog)) := by
  apply batch_notification_valid program s valid
  intro p
  rw [representation]
  exact Encoding.clear_owner_partition masks catalog owner p

end ParserEngine.Worklist
