import Std

/-! Ordered source inventory with opaque ownership boundaries. The reference
enumerates every file, then filters it. Filesystem enumeration and stable
source bytes are external premises; no operating-system behavior is proved. -/
namespace ParserEngine.SourceInventory

inductive Forest (Path : Type) where
  | empty
  | file (path : Path) (rest : Forest Path)
  | directory (path : Path) (children rest : Forest Path)

def files : Forest Path → List Path
  | .empty => []
  | .file path rest => path :: files rest
  | .directory _ children rest => files children ++ files rest

def selected (admit : Path → Bool) : Forest Path → List Path
  | .empty => []
  | .file path rest => (if admit path then [path] else []) ++ selected admit rest
  | .directory path children rest =>
    (if admit path then selected admit children else []) ++ selected admit rest

def owned (admit : Path → Bool) : Forest Path → Prop
  | .empty => True
  | .file _ rest => owned admit rest
  | .directory path children rest =>
    owned admit children ∧ owned admit rest ∧
      (admit path = false → ∀ child ∈ files children, admit child = false)

/-- For arbitrary directory forests, pruning retains precisely the reference
file sequence when a rejected ownership root owns all its descendants. -/
theorem selection_exact (admit : Path → Bool) (tree : Forest Path)
    (closed : owned admit tree) : selected admit tree = (files tree).filter admit := by
  induction tree with
  | empty => rfl
  | file path rest ih =>
    cases h : admit path <;> simp [selected, files, h, ih closed]
  | directory path children rest ihChildren ihRest =>
    obtain ⟨childrenClosed, restClosed, excluded⟩ := closed
    have childrenExact := ihChildren childrenClosed
    have restExact := ihRest restClosed
    cases h : admit path
    · have none : (files children).filter admit = [] :=
        List.filter_eq_nil_iff.mpr (by
          intro child member
          simp [excluded h child member])
      simp [selected, files, h, restExact, none]
    · simp [selected, files, h, childrenExact, restExact]

def visits : Forest Path → Nat
  | .empty => 0
  | .file _ rest => 1 + visits rest
  | .directory _ children rest => 1 + visits children + visits rest

def selectedVisits (admit : Path → Bool) : Forest Path → Nat
  | .empty => 0
  | .file _ rest => 1 + selectedVisits admit rest
  | .directory path children rest =>
    1 + (if admit path then selectedVisits admit children else 0) + selectedVisits admit rest

/-- Pruning never adds filesystem visits; this is a work bound, not a CPU or
wall-clock theorem. Callback cost and filesystem latency are not hidden. -/
theorem visits_bound (admit : Path → Bool) (tree : Forest Path) :
    selectedVisits admit tree ≤ visits tree := by
  induction tree with
  | empty => exact Nat.le_refl _
  | file _ _ ih => simpa [selectedVisits, visits] using Nat.add_le_add_left ih 1
  | directory path children rest ihChildren ihRest =>
    cases h : admit path <;> simp [selectedVisits, visits, h] <;> omega

/-- Compiler output files may be inserted anywhere in an existing inventory.
Every inserted file must be rejected by the input selector; source files and
directory ownership remain unchanged. -/
inductive AddsDerived (admit : Path → Bool) : Forest Path → Forest Path → Prop where
  | empty : AddsDerived admit .empty .empty
  | keepFile {path before after} : AddsDerived admit before after →
      AddsDerived admit (.file path before) (.file path after)
  | keepDirectory {path children expanded before after} :
      AddsDerived admit children expanded → AddsDerived admit before after →
      AddsDerived admit (.directory path children before) (.directory path expanded after)
  | addOutput {path before after} : admit path = false → AddsDerived admit before after →
      AddsDerived admit before (.file path after)

theorem derived_inputs_exact (change : AddsDerived admit source expanded) :
    selected admit expanded = selected admit source := by
  induction change with
  | empty => rfl
  | keepFile _ ih => simp only [selected]; rw [ih]
  | keepDirectory _ _ childrenExact restExact =>
      simp only [selected]; rw [childrenExact, restExact]
  | addOutput hidden _ ih => simpa [selected, hidden] using ih

/-- Arbitrary replacement of derived output inventories preserves every source
observation, including digests and modes, provided retained observations stay
stable. Compiler and filesystem correctness are not inferred from this law. -/
theorem derived_snapshot_exact (observeBefore observeAfter : Path → Record)
    (left : AddsDerived admit source before) (right : AddsDerived admit source after)
    (stable : ∀ path ∈ selected admit source, observeBefore path = observeAfter path) :
    (selected admit before).map observeBefore = (selected admit after).map observeAfter := by
  rw [derived_inputs_exact left, derived_inputs_exact right]
  exact List.map_congr_left stable

end ParserEngine.SourceInventory
