namespace ParserEngine.Reuse

/-- Logical identities, not hashes: implementations must establish that their
fingerprints bind these values. Equal keys are not a hash-collision proof. -/
structure Key where
  source : Nat
  grammar : Nat
  plan : Nat
  deriving DecidableEq, Repr

structure Receipt (Value : Type) where
  basis : Key
  value : Value

/-- Admission checks identity and preserves the already computed value.
This does not establish that the value was computed correctly. -/
def admit (current : Key) (receipt : Receipt Value) : Option Value :=
  if receipt.basis = current then some receipt.value else none

theorem admitted_identity (current : Key) (receipt : Receipt Value) (value : Value)
    (accepted : admit current receipt = some value) :
    receipt.basis = current ∧ receipt.value = value := by
  unfold admit at accepted
  split at accepted
  · exact ⟨‹receipt.basis = current›, Option.some.inj accepted⟩
  · contradiction

theorem current_receipt (key : Key) (value : Value) :
    admit key ⟨key, value⟩ = some value := by simp [admit]

theorem source_change_rejects (current : Key) (receipt : Receipt Value)
    (changed : receipt.basis.source ≠ current.source) :
    admit current receipt = none := by
  have different : receipt.basis ≠ current := fun equal => changed (congrArg Key.source equal)
  simp [admit, different]

theorem grammar_change_rejects (current : Key) (receipt : Receipt Value)
    (changed : receipt.basis.grammar ≠ current.grammar) :
    admit current receipt = none := by
  have different : receipt.basis ≠ current := fun equal => changed (congrArg Key.grammar equal)
  simp [admit, different]

theorem plan_change_rejects (current : Key) (receipt : Receipt Value)
    (changed : receipt.basis.plan ≠ current.plan) :
    admit current receipt = none := by
  have different : receipt.basis ≠ current := fun equal => changed (congrArg Key.plan equal)
  simp [admit, different]

/-- Repeated static preparation versus a retained plan. Counts are abstract
operations; hashing, eligibility checks, allocation and CPU require measurements. -/
def repeatedCost (requests prepare execute : Nat) := requests * (prepare + execute)
def retainedCost (requests prepare execute : Nat) :=
  if requests = 0 then 0 else prepare + requests * execute

theorem retained_cost_not_more (requests prepare execute : Nat) :
    retainedCost requests prepare execute ≤ repeatedCost requests prepare execute := by
  cases requests with
  | zero => simp [retainedCost, repeatedCost]
  | succ n =>
    simp only [retainedCost, Nat.succ_ne_zero, ↓reduceIte, repeatedCost, Nat.mul_add]
    have h : prepare ≤ (n + 1) * prepare := by
      rw [Nat.add_mul, Nat.one_mul]
      omega
    omega

/-- Retaining a plan saves preparation exactly once per additional request.
This excludes revalidation cost and says nothing about wall-clock speed. -/
theorem retained_cost_decomposition (additional prepare execute : Nat) :
    repeatedCost (additional + 1) prepare execute =
      retainedCost (additional + 1) prepare execute + additional * prepare := by
  simp [repeatedCost, retainedCost, Nat.add_mul, Nat.mul_add]
  omega

/-- Include per-request eligibility work. Reuse is beneficial only if the
checks fit within saved preparation; identity correctness alone is insufficient. -/
def checkedRetainedCost (requests prepare execute check : Nat) :=
  retainedCost requests prepare execute + requests * check

theorem checked_reuse_break_even (additional prepare execute check : Nat)
    (budget : (additional + 1) * check ≤ additional * prepare) :
    checkedRetainedCost (additional + 1) prepare execute check ≤
      repeatedCost (additional + 1) prepare execute := by
  rw [retained_cost_decomposition]
  exact Nat.add_le_add_left budget _

example : checkedRetainedCost 64 100 20 5 < repeatedCost 64 100 20 := by decide
example : repeatedCost 1 100 20 < checkedRetainedCost 1 100 20 5 := by decide

end ParserEngine.Reuse
