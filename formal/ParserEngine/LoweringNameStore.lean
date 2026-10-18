import ParserEngine.LoweringNames

/-! Persistent name meanings across mutable claims, stack entry and return.
No global owner-to-name injection is imposed: equal expression owners may have
distinct physical helper names. -/
namespace ParserEngine.LoweringNameStore
open Lowering

def decode (state : Names.State Nat) (name : Nat) : Owner :=
  (state.bindings.lookup name).getD (.named name)

theorem trans {a b c : Names.State Name} (first : Names.Trace a b) (last : Names.Trace b c) :
    Names.Trace a c := by
  induction last with
  | refl => exact first
  | retry _ occupied ih => exact .retry ih occupied
  | claim _ fresh ih => exact .claim ih fresh

theorem stable {before after : Names.State Nat} (history : Names.Trace before after)
    {name} (used : name ∈ before.used) : decode after name = decode before name := by
  induction history with
  | refl => rfl
  | retry _ _ ih => exact ih
  | @claim current freshName owner history fresh ih =>
      have present := Names.trace_used history name used
      have different : freshName ≠ name := by intro same; exact fresh (same ▸ present)
      have miss : (name == freshName) = false := by simp [Ne.symm different]
      simpa only [decode, List.lookup_cons, miss] using ih

structure Store (reserved : List Nat) where
  names : Names.State Nat
  history : Names.Trace (Names.initial reserved) names

def initial (reserved : List Nat) : Store reserved := ⟨Names.initial reserved, .refl _⟩

structure Handle {reserved} (store : Store reserved) (owner : Owner) where
  name : Nat
  used : name ∈ store.names.used
  meaning : decode store.names name = owner

def carry {reserved} {before after : Store reserved} (history : Names.Trace before.names after.names)
    (handle : Handle before owner) : Handle after owner :=
  ⟨handle.name, Names.trace_used history _ handle.used, (stable history handle.used).trans handle.meaning⟩

def authored {reserved} (store : Store reserved) (name : Nat) (member : name ∈ reserved) :
    Handle store (.named name) :=
  ⟨name, Names.trace_used store.history name member, by
    have same := stable store.history member
    simpa [decode, Names.initial] using same⟩

structure Allocation {reserved} (before : Store reserved) (owner : Owner) where
  after : Store reserved
  history : Names.Trace before.names after.names
  handle : Handle after owner
  index : Nat
  counter : after.names.attempt = index + 1

def allocate {reserved} (candidate : Nat → Nat)
    (injective : ∀ a b, candidate a = candidate b → a = b)
    (before : Store reserved) (owner : Owner) : Allocation before owner :=
  let result := LoweringNames.allocate candidate injective before.names owner
  let after : Store reserved := ⟨result.val.2, trans before.history result.property.2.2.2⟩
  { after := after
    history := result.property.2.2.2
    index := result.val.1
    counter := by change result.val.2.attempt = _; rw [result.property.2.2.1]; rfl
    handle := ⟨candidate result.val.1, by
      change candidate result.val.1 ∈ result.val.2.used
      rw [result.property.2.2.1]; simp [LoweringNames.claimed],
      by change decode result.val.2 _ = owner
         rw [result.property.2.2.1]
         simp [decode, LoweringNames.claimed]⟩ }

end ParserEngine.LoweringNameStore
