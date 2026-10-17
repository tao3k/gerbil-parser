import ParserEngine.PredictionMaterialization

/-! One builder owns one empty per-mask cache. The fixed factory captures that
builder's grammar/layout; no result is reused across a different factory. -/
namespace ParserEngine.TemplateCache
variable {Product : Type}
abbrev Store (Product : Type) := Nat → Option Product

def empty : Store Product := fun _ => none
def Valid (compute : Nat → Product) (store : Store Product) : Prop :=
  ∀ mask value, store mask = some value → value = compute mask

def fetch (compute : Nat → Product) (store : Store Product) (mask : Nat) : Product × Store Product :=
  match store mask with
  | some value => (value, store)
  | none => (compute mask, fun key => if key = mask then some (compute mask) else store key)

theorem value_exact (compute : Nat → Product) (store : Store Product) (valid : Valid compute store) (mask : Nat) :
    (fetch compute store mask).1 = compute mask := by
  cases read : store mask with
  | none => simp [fetch, read]
  | some value => simpa [fetch, read] using valid mask value read

theorem preserves (compute : Nat → Product) (store : Store Product) (valid : Valid compute store) (mask : Nat) :
    Valid compute (fetch compute store mask).2 := by
  cases read : store mask with
  | some value => simpa [fetch, read] using valid
  | none =>
    intro key value found
    by_cases same : key = mask
    · subst key; simpa [fetch, read] using found.symm
    · exact valid key value (by simpa [fetch, read, same] using found)

/-- First installation and later hits retain this exact key. Other populated
slots survive a miss, so sharing does not depend on adjacent queue visits. -/
theorem fetch_stored (compute : Nat → Product) (store : Store Product)
    (valid : Valid compute store) (mask : Nat) :
    (fetch compute store mask).2 mask = some (compute mask) := by
  cases read : store mask with
  | none => simp [fetch, read]
  | some value => simp [fetch, read, valid mask value read]

theorem fetch_retained (compute : Nat → Product) (store : Store Product)
    (mask key : Nat) (value : Product) (found : store key = some value) :
    (fetch compute store mask).2 key = some value := by
  cases read : store mask with
  | some existing => simpa [fetch, read] using found
  | none =>
    have different : key ≠ mask := by intro same; subst key; rw [read] at found; cases found
    simpa [fetch, read, different] using found

inductive Trace (compute : Nat → Product) : Store Product → Prop where
  | start : Trace compute empty
  | next {before} (earlier : Trace compute before) (mask : Nat) : Trace compute (fetch compute before mask).2

theorem trace_valid {compute : Nat → Product} {store : Store Product} (history : Trace compute store) : Valid compute store := by
  induction history with
  | start => intro mask value found; cases found
  | next earlier mask ih => exact preserves _ _ ih mask

theorem fetched_from_history {compute : Nat → Product} {store : Store Product}
    (history : Trace compute store) (mask : Nat) : (fetch compute store mask).1 = compute mask :=
  value_exact compute store (trace_valid history) mask

end ParserEngine.TemplateCache
