import ParserEngine.Encoding
import Init.Data.List.Sort.Lemmas

/-! Nonnegative Scheme integer traversal. Host integer operations have their
ordinary arithmetic meanings; loop order and termination are proved here. -/
namespace ParserEngine.BitTraversal

def least (mask : Nat) : Nat :=
  if mask = 0 then 0 else if mask % 2 = 1 then 0 else 1 + least (mask / 2)
termination_by mask
decreasing_by apply Nat.div_lt_self <;> omega

theorem least_properties (mask : Nat) (nonzero : mask ≠ 0) :
    mask.testBit (least mask) = true ∧ ∀ bit, mask.testBit bit = true → least mask ≤ bit := by
  rw [least, ite_eq_right nonzero]
  split
  next odd =>
    refine ⟨by simp [Nat.testBit_zero, odd], ?_⟩
    intro bit present; omega
  next even =>
    have parity : mask % 2 = 0 := by omega
    have smaller : mask / 2 ≠ 0 := by omega
    obtain ⟨present, minimal⟩ := least_properties (mask / 2) smaller
    constructor
    · simpa only [Nat.add_comm 1, Nat.testBit_succ] using present
    · intro bit selected
      cases bit with
      | zero => simp [Nat.testBit_zero, parity] at selected
      | succ bit => have bound := minimal bit (by simpa only [Nat.testBit_succ] using selected); omega
termination_by mask
decreasing_by apply Nat.div_lt_self <;> omega

def clear (mask : Nat) : Nat := mask &&& (mask - 1)

theorem clear_decreases (mask : Nat) (nonzero : mask ≠ 0) : clear mask < mask := by
  have bound : clear mask ≤ mask - 1 := Nat.and_le_right
  omega

/-- Clearing the predecessor intersection removes exactly the least bit,
including across arbitrary-precision borrow chains. -/
theorem clear_bit (mask bit : Nat) :
    (clear mask).testBit bit = (mask.testBit bit && decide (bit ≠ least mask)) := by
  by_cases zero : mask = 0
  · subst mask; simp [clear]
  · by_cases odd : mask % 2 = 1
    · have index : least mask = 0 := by rw [least]; simp [zero, odd]
      have quotient : (mask - 1) / 2 = mask / 2 := by omega
      have parity : (mask - 1) % 2 = 0 := by omega
      cases bit with
      | zero => simp [clear, Nat.testBit_zero, parity, index]
      | succ bit => simp [clear, Nat.testBit_succ, Nat.and_div_two, quotient, Nat.and_self, index]
    · have parity : mask % 2 = 0 := by omega
      have index : least mask = 1 + least (mask / 2) := by rw [least]; simp [zero, odd]
      have quotient : (mask - 1) / 2 = mask / 2 - 1 := by omega
      cases bit with
      | zero => simp [clear, Nat.testBit_zero, parity]
      | succ bit =>
        simp only [clear, Nat.testBit_succ, Nat.and_div_two, quotient]
        have tail := clear_bit (mask / 2) bit
        simpa [clear, index, Nat.add_comm 1] using tail
termination_by mask
decreasing_by apply Nat.div_lt_self <;> omega

def direct (mask : Nat) : List Nat :=
  if _zero : mask = 0 then [] else least mask :: direct (clear mask)
termination_by mask
decreasing_by exact clear_decreases mask _zero

theorem direct_member (mask bit : Nat) : bit ∈ direct mask ↔ mask.testBit bit = true := by
  rw [direct]
  split
  next zero => simp [zero]
  next nonzero =>
    rw [List.mem_cons, direct_member (clear mask) bit, clear_bit]
    have present := (least_properties mask nonzero).1
    by_cases same : bit = least mask
    · simp [same, present]
    · simp [same]
termination_by mask
decreasing_by exact clear_decreases mask (by assumption)

theorem direct_sorted (mask : Nat) : (direct mask).Pairwise (· < ·) := by
  rw [direct]
  split
  · simp
  next nonzero =>
    refine List.pairwise_cons.mpr ⟨?_, direct_sorted (clear mask)⟩
    intro bit member
    have selected := (direct_member (clear mask) bit).mp member
    rw [clear_bit, Bool.and_eq_true, decide_eq_true_eq] at selected
    have bound := (least_properties mask nonzero).2 bit selected.1
    omega
termination_by mask
decreasing_by exact clear_decreases mask (by assumption)

/-- Ascending lists with identical membership have one unique callback order. -/
theorem ordered_unique (left right : List Nat) (leftSorted : left.Pairwise (· < ·))
    (rightSorted : right.Pairwise (· < ·)) (same : ∀ bit, bit ∈ left ↔ bit ∈ right) : left = right := by
  have leftUnique : left.Nodup := leftSorted.imp (by intro a b lower; omega)
  have rightUnique : right.Nodup := rightSorted.imp (by intro a b lower; omega)
  exact List.Perm.eq_of_pairwise (by intro a b _ _ ab ba; omega)
    leftSorted rightSorted ((List.perm_ext_iff_of_nodup leftUnique rightUnique).mpr same)

def Upper (mask limit : Nat) : Prop := ∀ bit, mask.testBit bit = true → bit < limit

def fields (mask width limit : Nat) (positive : 0 < width) (offset : Nat) : List Nat :=
  if _inside : offset < limit then
    (direct (Encoding.field mask width offset)).map (offset + ·) ++ fields mask width limit positive (offset + width)
  else []
termination_by limit - offset
decreasing_by omega

theorem fields_member (mask width limit : Nat) (positive : 0 < width) (upper : Upper mask limit) (offset bit : Nat) :
    bit ∈ fields mask width limit positive offset ↔ offset ≤ bit ∧ mask.testBit bit = true := by
  rw [fields]
  split
  next inside =>
    simp only [List.mem_append, List.mem_map, direct_member, Encoding.field_member,
      fields_member mask width limit positive upper (offset + width) bit]
    constructor
    · intro selected
      rcases selected with ⟨fieldBit, ⟨bound, present⟩, rfl⟩ | ⟨bound, present⟩
      · exact ⟨by omega, present⟩
      · exact ⟨by omega, present⟩
    · rintro ⟨bound, present⟩
      by_cases own : bit < offset + width
      · refine Or.inl ⟨bit - offset, ⟨by omega, ?_⟩, by omega⟩
        simpa only [Nat.add_sub_of_le bound] using present
      · exact Or.inr ⟨by omega, present⟩
  next outside =>
    constructor
    · intro impossible; cases impossible
    · rintro ⟨bound, present⟩
      have high := upper bit present
      omega
termination_by limit - offset
decreasing_by omega

theorem fields_sorted (mask width limit : Nat) (positive : 0 < width) (upper : Upper mask limit) (offset : Nat) :
    (fields mask width limit positive offset).Pairwise (· < ·) := by
  rw [fields]
  split
  next inside =>
    apply List.pairwise_append.mpr
    refine ⟨List.pairwise_map.mpr ((direct_sorted _).imp (by intro a b lower; omega)),
      fields_sorted mask width limit positive upper (offset + width), ?_⟩
    intro left own right later
    obtain ⟨fieldBit, present, rfl⟩ := List.mem_map.mp own
    have low := (Encoding.field_member mask width offset fieldBit).mp ((direct_member _ fieldBit).mp present)
    have high := (fields_member mask width limit positive upper (offset + width) right).mp later
    omega
  next outside => simp
termination_by limit - offset
decreasing_by omega

def integerLength (mask : Nat) : Nat := if mask = 0 then 0 else mask.log2 + 1

theorem length_upper (mask : Nat) : Upper mask (integerLength mask) := by
  intro bit present
  by_cases zero : mask = 0
  · simp [zero] at present
  · have bound : mask < 2 ^ (mask.log2 + 1) := Nat.lt_log2_self
    have low := Nat.ge_two_pow_of_testBit present
    have index : bit < mask.log2 + 1 := by
      by_cases inside : bit < mask.log2 + 1
      · exact inside
      · have power := Nat.pow_le_pow_of_le (by decide : 1 < 2) (Nat.le_of_not_gt inside)
        omega
    simpa [integerLength, zero] using index

def start (mask width : Nat) : Nat := width * (least mask / width)

theorem start_before (mask width : Nat) : start mask width ≤ least mask := by
  have division := Nat.mod_add_div (least mask) width
  unfold start
  omega

theorem fields_exact (mask width : Nat) (positive : 0 < width) :
    fields mask width (integerLength mask) positive (start mask width) = direct mask := by
  apply ordered_unique _ _ (fields_sorted mask width _ positive (length_upper mask) _) (direct_sorted mask)
  intro bit
  rw [fields_member mask width _ positive (length_upper mask), direct_member]
  constructor
  · exact And.right
  · intro present
    have nonzero : mask ≠ 0 := by intro zero; simp [zero] at present
    exact ⟨Nat.le_trans (start_before mask width) ((least_properties mask nonzero).2 bit present), present⟩

/-- The heuristic can select either path without altering callbacks. Its
population-count accuracy is irrelevant to sequence correctness. -/
def dispatch (mask width population : Nat) (positive : 0 < width) (fixnum : Bool) : List Nat :=
  if fixnum then direct mask else
    let count := (integerLength mask - start mask width + (width - 1)) / width
    if population < count then direct mask else fields mask width (integerLength mask) positive (start mask width)

theorem dispatch_exact (mask width population : Nat) (positive : 0 < width) (fixnum : Bool) :
    dispatch mask width population positive fixnum = direct mask := by
  unfold dispatch
  split
  · rfl
  · dsimp only
    split
    · rfl
    · exact fields_exact mask width positive

/-- Relational source control flow: one callback precedes the recursive
cleared-mask invocation. No sorted output is assumed in a run witness. -/
inductive DirectRun : Nat → List Nat → Prop where
  | done : DirectRun 0 []
  | step {mask tail} (nonzero : mask ≠ 0) (later : DirectRun (clear mask) tail) :
      DirectRun mask (least mask :: tail)

theorem direct_run_sound {mask : Nat} {trace : List Nat} (run : DirectRun mask trace) : trace = direct mask := by
  induction run with
  | done => simp [direct]
  | step nonzero later ih => rw [direct, dite_eq_right nonzero, ih]

/-- The field invocation must finish before the next offset starts. Empty
fields execute no callbacks while still advancing the outer control loop. -/
inductive FieldRun (mask width limit : Nat) : Nat → List Nat → Prop where
  | done {offset} (finished : limit ≤ offset) : FieldRun mask width limit offset []
  | step {offset head tail} (inside : offset < limit)
      (visit : DirectRun (Encoding.field mask width offset) head)
      (later : FieldRun mask width limit (offset + width) tail) :
      FieldRun mask width limit offset (head.map (offset + ·) ++ tail)

theorem field_run_sound {mask width limit offset : Nat} {trace : List Nat}
    (positive : 0 < width) (run : FieldRun mask width limit offset trace) :
    trace = fields mask width limit positive offset := by
  induction run with
  | done finished => rw [fields]; simp [Nat.not_lt.mpr finished]
  | step inside visit later ih =>
    rw [fields, dite_eq_left inside, direct_run_sound visit, ih]

theorem source_field_callbacks {mask width : Nat} {trace : List Nat} (positive : 0 < width)
    (run : FieldRun mask width (integerLength mask) (start mask width) trace) : trace = direct mask :=
  (field_run_sound positive run).trans (fields_exact mask width positive)

theorem least_unique (mask candidate : Nat) (nonzero : mask ≠ 0)
    (present : mask.testBit candidate = true)
    (minimal : ∀ bit, mask.testBit bit = true → candidate ≤ bit) : candidate = least mask := by
  have expected := least_properties mask nonzero
  have lower := expected.2 candidate present
  have upper := minimal (least mask) expected.1
  omega

theorem start_inside (mask width : Nat) (nonzero : mask ≠ 0) : start mask width < integerLength mask :=
  Nat.lt_of_le_of_lt (start_before mask width) (length_upper mask _ (least_properties mask nonzero).1)

end ParserEngine.BitTraversal
