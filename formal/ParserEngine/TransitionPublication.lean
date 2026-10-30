import ParserEngine.StateInterning

/-! Source-shaped transition publication: each group interns its kernel,
records its assigned target ID, and publishes the reversed accumulator once.
Hash equality and the pointwise closure/template factory remain primitives. -/
namespace ParserEngine.TransitionPublication
open StateInterning
variable {Template : Type}

structure Published (Template : Type) where
  registry : Registry Nat Template
  rows : List (Nat × Nat)

def publish (compute : Nat → List Nat × Template) (before : Registry Nat Template) :
    List (Nat × Nat) → Published Template
  | [] => ⟨before, []⟩
  | (symbol, kernel) :: rest =>
    let installed := intern compute before kernel
    let later := publish compute installed.registry rest
    ⟨later.registry, (symbol, installed.id) :: later.rows⟩

theorem intern_lookup (compute : Nat → List Nat × Template) (before : Registry Nat Template) (kernel : Nat) :
    (intern compute before kernel).registry.index kernel = some (intern compute before kernel).id := by
  unfold intern
  split
  next id found => exact found
  next absent => simp

theorem intern_retains (compute : Nat → List Nat × Template) (before : Registry Nat Template)
    (kernel key id : Nat) (found : before.index key = some id) :
    (intern compute before kernel).registry.index key = some id := by
  unfold intern
  split
  · exact found
  next absent =>
    have distinct : key ≠ kernel := by intro same; subst key; rw [absent] at found; cases found
    simp [distinct, found]

theorem retains (compute : Nat → List Nat × Template) (before : Registry Nat Template)
    (groups : List (Nat × Nat)) (key id : Nat) (found : before.index key = some id) :
    (publish compute before groups).registry.index key = some id := by
  induction groups generalizing before with
  | nil => exact found
  | cons group rest ih => exact ih _ (intern_retains compute before group.2 key id found)

theorem valid (compute : Nat → List Nat × Template) (before : Registry Nat Template)
    (admitted : Valid compute before) (groups : List (Nat × Nat)) :
    Valid compute (publish compute before groups).registry := by
  induction groups generalizing before with
  | nil => exact admitted
  | cons group rest ih => exact ih _ (intern_valid compute before admitted group.2)

theorem symbols (compute : Nat → List Nat × Template) (before : Registry Nat Template)
    (groups : List (Nat × Nat)) : (publish compute before groups).rows.map Prod.fst = groups.map Prod.fst := by
  induction groups generalizing before with
  | nil => rfl
  | cons group rest ih => simp [publish, ih]

/-- Every target is the final registry's ID for that group's kernel. Subsequent
appends cannot invalidate IDs already recorded in the unpublished row. -/
theorem row_exact (compute : Nat → List Nat × Template) (before : Registry Nat Template)
    (groups : List (Nat × Nat)) (symbol target : Nat) :
    (symbol, target) ∈ (publish compute before groups).rows ↔
      ∃ kernel, (symbol, kernel) ∈ groups ∧ (publish compute before groups).registry.index kernel = some target := by
  induction groups generalizing before with
  | nil => simp [publish]
  | cons group rest ih =>
    rcases group with ⟨head, kernel⟩
    simp only [publish, List.mem_cons, Prod.mk.injEq, ih]
    constructor
    · intro member
      rcases member with ⟨sameSymbol, sameID⟩ | later
      · subst head; subst target
        exact ⟨kernel, Or.inl ⟨rfl, rfl⟩, retains compute _ rest kernel _ (intern_lookup compute before kernel)⟩
      · obtain ⟨key, member, indexed⟩ := later
        exact ⟨key, Or.inr member, indexed⟩
    · rintro ⟨key, member, indexed⟩
      rcases member with ⟨sameSymbol, sameKey⟩ | later
      · subst head; subst key
        have retained := retains compute (intern compute before kernel).registry rest kernel _ (intern_lookup compute before kernel)
        have sameID := Option.some.inj (indexed.symm.trans retained)
        exact Or.inl ⟨rfl, sameID⟩
      · exact Or.inr ⟨key, later, indexed⟩

/-- The Scheme accumulator is reversed exactly once at publication. -/
theorem accumulator (rows found : List (Nat × Nat)) :
    (rows.foldl (fun out row => row :: out) found).reverse = found.reverse ++ rows := by
  induction rows generalizing found with
  | nil => simp
  | cons row rest ih => simp [List.reverse_cons, List.append_assoc]

def drain (before : Registry Nat Template) (tail : List Nat) : Registry Nat Template :=
  {before with pending := tail, processed := before.processed + 1}

theorem drain_valid (compute : Nat → List Nat × Template) (before : Registry Nat Template)
    (admitted : Valid compute before) (node : Nat) (tail : List Nat) (front : before.pending = node :: tail) :
    Valid compute (drain before tail) := by
  refine ⟨admitted.unique, admitted.index, admitted.states, admitted.plans, ?_, ?_, ?_⟩
  · have accounting := admitted.accounting
    simp only [drain, front, List.length_cons] at *
    omega
  · have unique := admitted.queued
    rw [front] at unique
    exact (List.nodup_cons.mp unique).2
  · intro id member
    exact admitted.bound id (by rw [front]; exact List.mem_cons_of_mem node member)

end ParserEngine.TransitionPublication
