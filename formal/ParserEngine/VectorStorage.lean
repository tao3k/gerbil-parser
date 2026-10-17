import ParserEngine.StateInterning

/-! Execute the native counted ascending copy into fresh false slots, then
write the captured append ID. A mere prefix-retention assumption is insufficient:
capacity, initialization, copy coverage and the fresh write are derived here. -/
namespace ParserEngine.VectorStorage
variable {Value : Type}

def copy (source : List (Option Value)) (target : List (Option Value)) : Nat → List (Option Value)
  | 0 => target
  | count + 1 => (copy source target count).set count (source[count]?.join)

@[simp] theorem copy_length (source target : List (Option Value)) (count : Nat) :
    (copy source target count).length = target.length := by
  induction count with
  | zero => rfl
  | succ count ih => simp [copy, ih]

theorem copy_read (source target : List (Option Value)) (count node : Nat) (inside : node < target.length) :
    (copy source target count)[node]? = if node < count then some (source[node]?.join) else target[node]? := by
  induction count with
  | zero => simp [copy]
  | succ count ih =>
    by_cases same : count = node
    · subst node
      rw [copy, List.getElem?_set_self (by simpa using inside)]
      simp
    · rw [copy, List.getElem?_set_ne same, ih]
      by_cases before : node < count <;> simp [before] <;> omega

def grow (source : List (Option Value)) : List (Option Value) :=
  copy source (List.replicate (2 * source.length) none) source.length

theorem grow_exact (source : List (Option Value)) : grow source = StateInterning.grow source := by
  apply List.ext_getElem?
  intro node
  by_cases inside : node < 2 * source.length
  · rw [grow, copy_read _ _ _ _ (by simpa using inside)]
    by_cases old : node < source.length
    · simp only [old, ↓reduceIte]
      rw [StateInterning.grow_retains source node old, List.getElem?_eq_getElem old]
      rfl
    · simp only [old, ↓reduceIte, StateInterning.grow]
      rw [List.getElem?_append_right (by omega)]
      simp [inside, show node - source.length < source.length by omega]
  · have outside : 2 * source.length ≤ node := by omega
    rw [List.getElem?_eq_none (by simpa [grow] using outside), List.getElem?_eq_none]
    simp [StateInterning.grow]; omega

def capacity (count current : Nat) : Nat := if count = current then 2 * current else current

def append (source : List (Option Value)) (count : Nat) (value : Value) : List (Option Value) :=
  (if count = source.length then grow source else source).set count (some value)

theorem prepared_length (source : List (Option Value)) (count : Nat) :
    (if count = source.length then grow source else source).length = capacity count source.length := by
  split <;> simp_all [capacity, grow]

@[simp] theorem append_length (source : List (Option Value)) (count : Nat) (value : Value) :
    (append source count value).length = capacity count source.length := by
  simp only [append, List.length_set, prepared_length]

theorem room (count current : Nat) (bounded : count ≤ current) (positive : 0 < current) :
    count < capacity count current := by unfold capacity; split <;> omega

theorem append_read (source : List (Option Value)) (count : Nat) (value : Value)
    (bounded : count ≤ source.length) (positive : 0 < source.length) :
    (append source count value)[count]? = some (some value) := by
  unfold append
  rw [List.getElem?_set_self]
  rw [prepared_length]
  exact room count source.length bounded positive

theorem append_old (source : List (Option Value)) (count : Nat) (value : Value) (node : Nat)
    (old : node < count) (_bounded : count ≤ source.length) :
    (append source count value)[node]? = source[node]? := by
  unfold append
  rw [List.getElem?_set_ne (Nat.ne_of_gt old)]
  split
  · rw [grow_exact, StateInterning.grow_retains source node (by omega)]
  · rfl

/-- One producer step transports an existing bounded representation and adds
its newly captured ID. No final-vector representation certificate is assumed. -/
theorem append_refines (source : List (Option Value)) (cells : Nat → Option Value) (count : Nat) (value : Value)
    (bounded : count ≤ source.length) (positive : 0 < source.length)
    (represented : ∀ node, node < count → source[node]?.join = cells node) :
    ∀ node, node < count + 1 → (append source count value)[node]?.join =
      ProducerAdmission.Storage.update cells count value node := by
  intro node inside
  by_cases same : node = count
  · subst node
    simp [append_read source count value bounded positive, ProducerAdmission.Storage.update]
  · have old : node < count := by omega
    rw [append_old source count value node old bounded, represented node old]
    simp [ProducerAdmission.Storage.update, same]

end ParserEngine.VectorStorage
