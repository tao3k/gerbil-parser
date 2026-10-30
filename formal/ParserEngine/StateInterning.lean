import ParserEngine.Construction
import ParserEngine.ProducerAdmission

/-! Physical hash lookup, captured append ID, immutable publications and the
native FIFO accounting. Key equality/storage primitives are explicit; there
is no premise identifying a final registry with a semantic construction. -/
namespace ParserEngine.StateInterning
open ProducerAdmission

structure Registry (Key Template : Type) where
  keys : List Key
  index : Key → Option Nat
  states : Nat → Option (List Nat)
  plans : Nat → Option (Key × Template)
  pending : List Nat
  processed : Nat

structure Result (Key Template : Type) where
  registry : Registry Key Template
  id : Nat
  fresh : Bool

variable {Key Template : Type}

def intern [DecidableEq Key] (compute : Key → List Nat × Template)
    (before : Registry Key Template) (key : Key) : Result Key Template :=
  match before.index key with
  | some id => ⟨before, id, false⟩
  | none =>
    let id := before.keys.length
    let product := compute key
    ⟨{ keys := before.keys ++ [key]
       index := fun other => if other = key then some id else before.index other
       states := Storage.update before.states id product.1
       plans := Storage.update before.plans id (key, product.2)
       pending := before.pending ++ [id]
       processed := before.processed }, id, true⟩

structure Valid [DecidableEq Key] (compute : Key → List Nat × Template) (registry : Registry Key Template) : Prop where
  unique : registry.keys.Nodup
  index : ∀ key id, registry.index key = some id ↔ registry.keys[id]? = some key
  states : ∀ id key, registry.keys[id]? = some key → registry.states id = some (compute key).1
  plans : ∀ id key, registry.keys[id]? = some key → registry.plans id = some (key, (compute key).2)
  accounting : registry.pending.length + registry.processed = registry.keys.length
  queued : registry.pending.Nodup
  bound : ∀ id ∈ registry.pending, id < registry.keys.length

theorem missing_absent [DecidableEq Key] {compute : Key → List Nat × Template} {before : Registry Key Template}
    (valid : Valid compute before) {key : Key} (missing : before.index key = none) : key ∉ before.keys := by
  intro present
  obtain ⟨front, rest, split⟩ := List.mem_iff_append.mp present
  have read : before.keys[front.length]? = some key := by rw [split]; simp
  have found := (valid.index key front.length).mpr read
  rw [missing] at found
  cases found

theorem existing [DecidableEq Key] (compute : Key → List Nat × Template) (before : Registry Key Template)
    (key : Key) (id : Nat) (found : before.index key = some id) :
    intern compute before key = ⟨before, id, false⟩ := by simp [intern, found]

theorem novel [DecidableEq Key] (compute : Key → List Nat × Template) (before : Registry Key Template)
    (key : Key) (missing : before.index key = none) :
    (intern compute before key).id = before.keys.length ∧
      (intern compute before key).fresh = true ∧
      (intern compute before key).registry.states before.keys.length = some (compute key).1 ∧
      (intern compute before key).registry.plans before.keys.length = some (key, (compute key).2) := by
  simp [intern, missing, Storage.update]

theorem retained [DecidableEq Key] (compute : Key → List Nat × Template) (before : Registry Key Template)
    (key : Key) (id : Nat) (old : id < before.keys.length) :
    (intern compute before key).registry.keys[id]? = before.keys[id]? ∧
      (intern compute before key).registry.states id = before.states id ∧
      (intern compute before key).registry.plans id = before.plans id := by
  unfold intern
  split
  · exact ⟨rfl, rfl, rfl⟩
  · simp [Storage.update, Nat.ne_of_lt old, List.getElem?_append_left old]

theorem intern_valid [DecidableEq Key] (compute : Key → List Nat × Template) (before : Registry Key Template)
    (valid : Valid compute before) (key : Key) : Valid compute (intern compute before key).registry := by
  cases lookup : before.index key with
  | some id => simpa [intern, lookup] using valid
  | none =>
    have absent := missing_absent valid lookup
    have unique : (before.keys ++ [key]).Nodup := by
      apply List.nodup_append.mpr
      refine ⟨valid.unique, by simp, ?_⟩
      intro a member b singleton same
      have : b = key := List.mem_singleton.mp singleton
      subst b; subst a; exact absent member
    refine ⟨by simpa only [intern, lookup] using unique, ?_, ?_, ?_, ?_, ?_, ?_⟩
    · intro other id
      simp only [intern, lookup]
      by_cases same : other = key
      · subst other
        simp only [ite_true]
        constructor
        · intro sameID; cases sameID; simp
        · intro read
          by_cases old : id < before.keys.length
          · rw [List.getElem?_append_left old] at read
            exact False.elim (absent (List.mem_of_getElem? read))
          · rw [List.getElem?_append_right (by omega)] at read
            have equal : id - before.keys.length = 0 := by
              by_cases zero : id - before.keys.length = 0
              · exact zero
              · have tooFar : ([key] : List Key).length ≤ id - before.keys.length := by simp; omega
                rw [List.getElem?_eq_none tooFar] at read
                cases read
            congr; omega
      · simp only [same, ite_false]
        constructor
        · intro found
          have read := (valid.index other id).mp found
          have bound : id < before.keys.length := by
            by_cases good : id < before.keys.length
            · exact good
            · simp [List.getElem?_eq_none (by omega : before.keys.length ≤ id)] at read
          rw [List.getElem?_append_left bound]
          exact read
        · intro read
          by_cases old : id < before.keys.length
          · exact (valid.index other id).mpr (by simpa only [List.getElem?_append_left old] using read)
          · rw [List.getElem?_append_right (by omega)] at read
            have member := List.mem_of_getElem? read
            have equal : other = key := by simpa using member
            exact False.elim (same equal)
    · intro id other read
      by_cases old : id < before.keys.length
      · have kept := (retained compute before key id old).2.1
        rw [kept]
        apply valid.states id other
        simpa only [intern, lookup, List.getElem?_append_left old] using read
      · simp only [intern, lookup] at read ⊢
        rw [List.getElem?_append_right (by omega)] at read
        have member := List.mem_of_getElem? read
        have same : other = key := by simpa using member
        subst other
        have zero : id - before.keys.length = 0 := by
          by_cases zero : id - before.keys.length = 0
          · exact zero
          · have tooFar : ([key] : List Key).length ≤ id - before.keys.length := by simp; omega
            rw [List.getElem?_eq_none tooFar] at read
            cases read
        have sameID : id = before.keys.length := by omega
        simp [Storage.update, sameID]
    · intro id other read
      by_cases old : id < before.keys.length
      · have kept := (retained compute before key id old).2.2
        rw [kept]
        apply valid.plans id other
        simpa only [intern, lookup, List.getElem?_append_left old] using read
      · simp only [intern, lookup] at read ⊢
        rw [List.getElem?_append_right (by omega)] at read
        have member := List.mem_of_getElem? read
        have same : other = key := by simpa using member
        subst other
        have zero : id - before.keys.length = 0 := by
          by_cases zero : id - before.keys.length = 0
          · exact zero
          · have tooFar : ([key] : List Key).length ≤ id - before.keys.length := by simp; omega
            rw [List.getElem?_eq_none tooFar] at read
            cases read
        have sameID : id = before.keys.length := by omega
        simp [Storage.update, sameID]
    · simp only [intern, lookup, List.length_append, List.length_singleton]
      have := valid.accounting; omega
    · simp only [intern, lookup]
      apply List.nodup_append.mpr
      refine ⟨valid.queued, by simp, ?_⟩
      intro a member b singleton same
      have equal : b = before.keys.length := List.mem_singleton.mp singleton
      subst b; subst a
      have := valid.bound before.keys.length member; omega
    · intro id member
      simp only [intern, lookup, List.mem_append, List.mem_singleton] at member
      simp only [intern, lookup, List.length_append, List.length_singleton]
      rcases member with old | same
      · have := valid.bound id old; omega
      · omega

/-- Vector growth copies the existing prefix and allocates only false slots.
The theorem applies simultaneously to state rows, transitions and kernel plans. -/
def grow (values : List (Option Value)) : List (Option Value) :=
  values ++ List.replicate values.length none

theorem grow_retains (values : List (Option Value)) (id : Nat) (old : id < values.length) :
    (grow values)[id]? = values[id]? := by simp only [grow, List.getElem?_append_left old]

theorem existing_zero [DecidableEq Key] (compute : Key → List Nat × Template) (before : Registry Key Template)
    (key : Key) (found : before.index key = some 0) :
    (intern compute before key).id = 0 ∧ (intern compute before key).registry.pending = before.pending := by
  simp [intern, found]

end ParserEngine.StateInterning
