import LeanPoo.Functional.Requirements

/-! The host supplies stable directory lookup during admission. Only repeated
leading entries are removed. Compiler bytes, flags, unset PATH, relative
entries and the remaining search order retain their existing contracts. -/
namespace ParserEngine.BuildEnvironment
open LeanPoo.Functional
universe u v w x
variable {Path : Type u} [DecidableEq Path] {Value : Type v}

def search (lookup : Path → Option Value) : List Path → Option Value
  | [] => none
  | path :: rest => match lookup path with
    | some value => some value
    | none => search lookup rest

def dropLeading (first : Path) : List Path → List Path
  | [] => []
  | path :: rest => if path = first then dropLeading first rest else path :: rest

def normalize : List Path → List Path
  | [] => []
  | first :: rest => first :: dropLeading first rest

theorem dropLeading_miss (lookup : Path → Option Value) (first : Path)
    (missing : lookup first = none) (paths : List Path) :
    search lookup (dropLeading first paths) = search lookup paths := by
  induction paths with
  | nil => rfl
  | cons path rest ih =>
    by_cases same : path = first
    · subst path
      simp [dropLeading, search, missing, ih]
    · simp [dropLeading, same]

/-- Arbitrary lookup and arbitrary paths, including absent tools and empty
entries. A changing filesystem remains an explicit external premise. -/
theorem normalize_preserves (lookup : Path → Option Value) (paths : List Path) :
    search lookup (normalize paths) = search lookup paths := by
  cases paths with
  | nil => rfl
  | cons first rest =>
    cases missing : lookup first with
    | none => simpa [normalize, search, missing] using dropLeading_miss lookup first missing rest
    | some value => simp [normalize, search, missing]

/-- Directory lookup selects existing LeanPoo factories; no factory is invoked
or replaced by normalization, and dependent contexts and keys are retained. -/
def orderedProvider {Context : Type v} {Key : Type w} {Payload : Context → Key → Type x}
    (lookup : Path → Provider Context Key Payload) (paths : List Path) :
    Provider Context Key Payload := fun key => search (fun path => lookup path key) paths

theorem provider_preserved {Context : Type v} {Key : Type w}
    {Payload : Context → Key → Type x}
    (lookup : Path → Provider Context Key Payload) (paths : List Path) :
    orderedProvider lookup (normalize paths) = orderedProvider lookup paths := by
  funext key
  exact normalize_preserves (fun path => lookup path key) paths

theorem prepared_capabilities_preserved {Context : Type v} {Key : Type w}
    {Payload : Context → Key → Type x}
    (lookup : Path → Provider Context Key Payload) (paths : List Path) (keys : List Key) :
    Requirements.prepare (orderedProvider lookup (normalize paths)) keys =
      Requirements.prepare (orderedProvider lookup paths) keys := by
  rw [provider_preserved]

omit [DecidableEq Path] in
/-- Reordering distinct successful entries must still invalidate admission. -/
theorem reordering_changes (lookup : Path → Option Value) (first second : Path)
    (left : lookup first = some a) (right : lookup second = some b) (different : a ≠ b) :
    search lookup [first, second] ≠ search lookup [second, first] := by
  simpa [search, left, right] using different

end ParserEngine.BuildEnvironment
