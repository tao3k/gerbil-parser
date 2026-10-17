import ParserEngine.CachedConstruction

/-! Extract scalar layout parameters from named lexical bindings, then admit
complete independent source schemas. Recipes execute the proved copy/write
recurrence. This is a restricted parser protocol, not a Scheme interpreter. -/
namespace ParserEngine.ConstructionExtraction
open WalkSource

mutual
  def bindings (name : String) : Datum → List Datum
    | .list [.symbol "quote", _] => []
    | .list (.symbol command :: .list entries :: body) =>
      if command = "let" ∨ command = "let*" then
        entries.filterMap (fun entry => match entry with
          | .list [.symbol key, value] => if key = name then some value else none
          | _ => none) ++ bindingList name body
      else bindingList name (.symbol command :: .list entries :: body)
    | .list values => bindingList name values
    | _ => []
  termination_by datum => sizeOf datum
  def bindingList (name : String) : List Datum → List Datum
    | [] => []
    | value :: rest => bindings name value ++ bindingList name rest
  termination_by values => sizeOf values
end

def binding (name : String) (source : Datum) : Option Datum :=
  match bindings name source with | [value] => some value | _ => none

def literal (value : Datum) : Option Nat := match value with
  | .list [.symbol "make-vector", .integer capacity, _] => some capacity
  | _ => none

def multiplier (value : Datum) : Option Nat := match value with
  | .list [.symbol "make-vector", .list [.symbol "*", .integer factor, .symbol "state-count"], _] => some factor
  | _ => none

def sizes (read : Datum → Option Nat) (a b c : String) (source : Datum) : Option Nat := do
  let first ← (binding a source).bind read
  let second ← (binding b source).bind read
  let third ← (binding c source).bind read
  if first = second ∧ second = third then some first else none

structure Recipe where
  initial : Nat
  positive : 0 < initial
  factor : Nat
  doubled : factor = 2

def extract (builder growth append template : Datum) : Option Recipe := do
  let initial ← sizes literal "states" "state-transitions" "kernel-plans" builder
  let factor ← sizes multiplier "next-states" "next-transitions" "next-plans" growth
  if positive : 0 < initial then
    if doubled : factor = 2 then
      if builder = LRTransitionSource.builderWith initial ∧ growth = LRConstructionSource.growWith factor ∧
          append = LRConstructionSource.appendState ∧ template = LRTemplateSource.template then
        some ⟨initial, positive, factor, doubled⟩
      else none
    else none
  else none

theorem extracted_sources (builder growth append template : Datum) (recipe : Recipe)
    (admitted : extract builder growth append template = some recipe) :
    builder = LRTransitionSource.builderWith recipe.initial ∧ growth = LRConstructionSource.growWith recipe.factor ∧
      append = LRConstructionSource.appendState ∧ template = LRTemplateSource.template := by
  unfold extract at admitted
  obtain ⟨initial, _, rest⟩ := Option.bind_eq_some_iff.mp admitted
  obtain ⟨factor, _, result⟩ := Option.bind_eq_some_iff.mp rest
  split at result
  next positive =>
    split at result
    next doubled =>
      split at result
      next matching =>
        cases Option.some.inj result
        exact matching
      next mismatch => cases result
    next different => cases result
  next zero => cases result

/-- Altering any admitted growth instruction, including a copy destination or
loop bound, is rejected even if all allocation literals remain unchanged. -/
theorem changed_growth_rejected (builder growth append template : Datum)
    (changed : growth ≠ LRConstructionSource.growWith 2) : extract builder growth append template = none := by
  cases read : extract builder growth append template with
  | none => rfl
  | some recipe =>
    have exactSource := (extracted_sources builder growth append template recipe read).2.1
    rw [recipe.doubled] at exactSource
    exact False.elim (changed exactSource)

theorem changed_template_rejected (builder growth append template : Datum)
    (changed : template ≠ LRTemplateSource.template) : extract builder growth append template = none := by
  cases read : extract builder growth append template with
  | none => rfl
  | some recipe => exact False.elim (changed (extracted_sources builder growth append template recipe read).2.2.2)

def Recipe.start (recipe : Recipe) : List (Option Value) := List.replicate recipe.initial none

def Recipe.install (_recipe : Recipe) (slots : List (Option Value)) (count : Nat) (value : Value) :=
  VectorStorage.append slots count value

theorem start_size (recipe : Recipe) : (recipe.start (Value := Value)).length = recipe.initial := by simp [Recipe.start]

theorem recipe_write (recipe : Recipe) (slots : List (Option Value)) (count : Nat) (value : Value)
    (bounded : count ≤ slots.length) (positive : 0 < slots.length) :
    (recipe.install slots count value)[count]? = some (some value) := VectorStorage.append_read _ _ _ bounded positive

theorem recipe_copy (recipe : Recipe) (slots : List (Option Value)) (count : Nat) (value : Value) (node : Nat)
    (inside : node < count) (bounded : count ≤ slots.length) :
    (recipe.install slots count value)[node]? = slots[node]? := VectorStorage.append_old _ _ _ _ inside bounded

set_option maxRecDepth 8192 in
/-- Current scalar values are reader-derived from all three allocations; the
complete-schema checks also bind the copy loop and publication order. -/
theorem source_current :
    (extract LRTransitionWitness.builderSource LRConstructionWitness.growSource LRConstructionWitness.appendSource
      LRTemplateWitness.templateSource).map (fun recipe => (recipe.initial, recipe.factor)) = some (128, 2) := by
  have initial : sizes literal "states" "state-transitions" "kernel-plans" LRTransitionWitness.builderSource = some 128 := by
    simp [sizes, binding, bindings, bindingList, literal, LRTransitionWitness.builderSource]
  have factor : sizes multiplier "next-states" "next-transitions" "next-plans" LRConstructionWitness.growSource = some 2 := by
    simp [sizes, binding, bindings, bindingList, multiplier, LRConstructionWitness.growSource]
  simp only [extract, initial, factor]
  simp [LRTransitionWitness.source_checked, LRConstructionWitness.source_checked.1,
    LRConstructionWitness.source_checked.2.1, LRTemplateWitness.source_checked.1,
    LRTransitionSource.builder, LRConstructionSource.grow]

end ParserEngine.ConstructionExtraction
