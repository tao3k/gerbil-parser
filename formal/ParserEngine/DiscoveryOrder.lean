import ParserEngine.SparseGroupMasks

/-! First-occurrence symbol discovery is ordered by the first physical core.
This law is about arbitrary sorted core lists and arbitrary symbol maps. -/
namespace ParserEngine.DiscoveryOrder
open SparseGroupMasks

def hide (owner : Nat) (symbol : Nat → Option Nat) (item : Nat) : Option Nat :=
  if symbol item = some owner then none else symbol item

theorem hide_match (owner : Nat) (symbol : Nat → Option Nat) (item key : Nat) :
    hide owner symbol item = some key ↔ symbol item = some key ∧ key ≠ owner := by
  unfold hide
  split
  next same =>
    constructor
    · intro impossible; cases impossible
    · rintro ⟨equal, different⟩
      exact False.elim (different (Option.some.inj (equal.symm.trans same)))
  next distinct =>
    constructor
    · intro found; exact ⟨found, by intro same; subst key; exact distinct found⟩
    · exact And.left

theorem dedup_filter (values : List Nat) (p : Nat → Bool) :
    (values.filter p).eraseDups = values.eraseDups.filter p := by
  induction values generalizing p with
  | nil => rfl
  | cons value rest ih =>
    by_cases selected : p value = true
    · simp only [List.filter_cons, selected, ↓reduceIte, List.eraseDups_cons]
      simp only [List.filter_filter, ih, List.filter_filter]
      apply congrArg (List.cons value)
      apply congrArg (fun f => rest.eraseDups.filter f)
      funext key
      exact Bool.and_comm _ _
    · have rejected : p value = false := Bool.eq_false_iff.mpr selected
      simp only [List.filter_cons, rejected, Bool.false_eq_true, ↓reduceIte, List.eraseDups_cons]
      simp only [ih, List.filter_filter]
      apply List.filter_congr
      intro key member
      by_cases same : key = value
      · subst key; simp [rejected]
      · simp [same]

theorem hidden_stream (owner : Nat) (symbol : Nat → Option Nat) (items : List Nat) :
    items.filterMap (hide owner symbol) = (items.filterMap symbol).filter (fun key => key != owner) := by
  induction items with
  | nil => rfl
  | cons item rest ih =>
    cases source : symbol item with
    | none => simp [hide, source, ih]
    | some key =>
      by_cases same : key = owner
      · subst key; simp [hide, source, ih]
      · simp [hide, source, same, ih]

theorem hidden_keys (owner : Nat) (symbol : Nat → Option Nat) (items : List Nat) :
    keys (hide owner symbol) items = (keys symbol items).filter (fun key => key != owner) := by
  simp only [keys, hidden_stream, dedup_filter]

theorem hidden_first (owner : Nat) (symbol : Nat → Option Nat) (items : List Nat) (key : Nat) (different : key ≠ owner) :
    first (hide owner symbol) items key = first symbol items key := by
  have predicates : (fun item => hide owner symbol item == some key) = (fun item => symbol item == some key) := by
    funext item
    apply Bool.eq_iff_iff.mpr
    simp only [beq_iff_eq, hide_match]
    exact and_iff_left different
  simp only [first, predicates]

theorem first_origin (symbol : Nat → Option Nat) (items : List Nat) (key : Nat) (present : key ∈ keys symbol items) :
    first symbol items key ∈ items ∧ symbol (first symbol items key) = some key := by
  obtain ⟨item, member, source⟩ := (key_present symbol items key).mp present
  cases found : items.find? (fun item => symbol item == some key) with
  | none =>
    have absent := List.find?_eq_none.mp found item member
    simp [source] at absent
  | some selected =>
    have member := List.mem_of_find?_eq_some found
    have source := List.find?_some found
    simpa [first, found] using And.intro member (by simpa using source : symbol selected = some key)

/-- Removing already-discovered symbols preserves the first physical address
of every remaining symbol, so first-occurrence rows inherit core order. -/
theorem keys_sorted (symbol : Nat → Option Nat) (items : List Nat) (sorted : items.Pairwise (· < ·)) :
    (keys symbol items).Pairwise (fun left right => first symbol items left < first symbol items right) := by
  induction items generalizing symbol with
  | nil => simp [keys]
  | cons item rest ih =>
    obtain ⟨head, tail⟩ := List.pairwise_cons.mp sorted
    cases source : symbol item with
    | none =>
      have sameKeys : keys symbol (item :: rest) = keys symbol rest := by simp [keys, source]
      have sameFirst : ∀ key, first symbol (item :: rest) key = first symbol rest key := by intro key; simp [first, source]
      simpa only [sameKeys, sameFirst] using ih symbol tail
    | some owner =>
      have sameKeys : keys symbol (item :: rest) = owner :: keys (hide owner symbol) rest := by
        simp only [keys, List.filterMap_cons, source, List.eraseDups_cons]
        simp only [hidden_stream, dedup_filter]
        congr 2
      have ownFirst : first symbol (item :: rest) owner = item := by simp [first, source]
      have others : ∀ key ∈ keys (hide owner symbol) rest,
          first symbol (item :: rest) key = first (hide owner symbol) rest key := by
        intro key member
        obtain ⟨origin, _, selected⟩ := (key_present _ _ _).mp member
        have different := (hide_match owner symbol origin key).mp selected |>.2
        rw [hidden_first owner symbol rest key different]
        simp [first, source, Ne.symm different]
      rw [sameKeys, List.pairwise_cons]
      constructor
      · intro key member
        rw [ownFirst, others key member]
        exact head _ (first_origin _ _ _ member).1
      · exact (ih (hide owner symbol) tail).imp_of_mem (by
          intro left right leftMem rightMem smaller
          simpa only [others left leftMem, others right rightMem] using smaller)

theorem shared_sorted (symbol : Nat → Option Nat) (items : List Nat) (sorted : items.Pairwise (· < ·)) :
    (sharedRows symbol items).Pairwise (fun left right => left.first < right.first) := by
  exact List.pairwise_map.mpr (keys_sorted symbol items sorted)

/-- Earliest addresses cannot tie across distinct symbols. Therefore stable
versus unstable sorting cannot change the symbol order. -/
theorem first_unique (symbol : Nat → Option Nat) (items : List Nat) (left right : Nat)
    (leftMem : left ∈ keys symbol items) (rightMem : right ∈ keys symbol items)
    (same : first symbol items left = first symbol items right) : left = right := by
  have leftRead := (first_origin symbol items left leftMem).2
  have rightRead := (first_origin symbol items right rightMem).2
  rw [same] at leftRead
  exact Option.some.inj (leftRead.symm.trans rightRead)

end ParserEngine.DiscoveryOrder
