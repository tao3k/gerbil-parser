import ParserEngine.SourceEnvironment

/-! Project-owned dependency recording and admission over arbitrary expanded
visitor trees. The SDK owns expansion, binding classification and core dispatch.
Unknown core nodes must fail dispatch; the project does not scan raw symbols. -/
namespace ParserEngine.SourceDependencies
open LeanPoo.Functional
universe u v
variable {Key : Type u}

structure Reference (Key : Type u) where
  name : Key
  resolved : Bool

/-- Compiler resolution and the stable host primitive slot are separate.
The kernel spelling is necessary but does not supply a missing primitive. -/
def resolvedReference (sdkBound kernelName primitiveSlot : Bool) : Bool :=
  sdkBound || (kernelName && primitiveSlot)

theorem kernel_prefix_is_insufficient : resolvedReference false true false = false := rfl

theorem host_primitive_is_resolved : resolvedReference false true true = true := rfl

/-- SDK traversal structure, including deferred closure bodies and assignments.
Literal/quoted data has no executable references. This is not a Scheme reader. -/
inductive Tree (Key : Type u) where
  | data
  | reference (ref : Reference Key)
  | assignment (target : Reference Key) (rhs : Tree Key)
  | sequence (left right : Tree Key)
  | closure (body : Tree Key)
  | unsupported

def leaf (ref : Reference Key) : List Key := if ref.resolved then [] else [ref.name]

theorem absent_capability_is_recorded (name : Key) :
    leaf ⟨name, resolvedReference false true false⟩ = [name] := rfl

def dependencies : Tree Key → Option (List Key)
  | .data => some []
  | .reference ref => some (leaf ref)
  | .assignment target rhs => (dependencies rhs).map (leaf target ++ ·)
  | .sequence left right => do
      let a ← dependencies left
      let b ← dependencies right
      pure (a ++ b)
  | .closure body => dependencies body
  | .unsupported => none

/-- An independent occurrence specification, without collection order or any
list accumulator. Deferred bodies and write targets remain obligations. -/
def Requires (name : Key) : Tree Key → Prop
  | .data | .unsupported => False
  | .reference ref => ref.resolved = false ∧ name = ref.name
  | .assignment target rhs =>
      (target.resolved = false ∧ name = target.name) ∨ Requires name rhs
  | .sequence left right => Requires name left ∨ Requires name right
  | .closure body => Requires name body

@[simp] theorem leaf_membership (ref : Reference Key) :
    name ∈ leaf ref ↔ ref.resolved = false ∧ name = ref.name := by
  cases ref with | mk key resolved => cases resolved <;> simp [leaf]

/-- No finite inventory premise: every accepted recursive source visit has
exactly all unresolved dependencies, including repeated and nested references. -/
theorem complete (tree : Tree Key) (accepted : dependencies tree = some names) :
    name ∈ names ↔ Requires name tree := by
  induction tree generalizing names with
  | data => cases accepted; simp [Requires]
  | reference ref => cases accepted; exact leaf_membership ref
  | assignment target rhs ih =>
    cases step : dependencies rhs with
    | none => simp [dependencies, step] at accepted
    | some rest =>
      simp only [dependencies, step, Option.map_some, Option.some.injEq] at accepted
      subst names
      simp only [List.mem_append, leaf_membership, Requires, ih step]
  | sequence left right ihLeft ihRight =>
    cases a : dependencies left with
    | none => simp [dependencies, a] at accepted
    | some leading =>
      cases b : dependencies right with
      | none => simp [dependencies, a, b] at accepted
      | some trailing =>
        simp [dependencies, a, b] at accepted
        subst names
        simp only [List.mem_append, Requires, ihLeft a, ihRight b]
  | closure body ih => exact ih accepted
  | unsupported => simp [dependencies] at accepted

/-- Actual callback storage is cons followed by reverse, not an unordered set. -/
def record (names : List Key) (reversed : List Key) : List Key :=
  names.foldl (fun found name => name :: found) reversed

theorem record_exact (names reversed : List Key) :
    (record names reversed).reverse = reversed.reverse ++ names := by
  induction names generalizing reversed with
  | nil => simp [record]
  | cons head rest ih =>
    change (record rest (head :: reversed)).reverse = _
    rw [ih]
    simp [List.reverse_cons, List.append_assoc]

theorem recorded_complete (tree : Tree Key) (accepted : dependencies tree = some names) :
    name ∈ (record names []).reverse ↔ Requires name tree := by
  rw [record_exact]
  simpa using complete tree accepted

def bindReference [DecidableEq Key] (supplied : List Key) (ref : Reference Key) : Reference Key :=
  ⟨ref.name, ref.resolved || decide (ref.name ∈ supplied)⟩

def supply [DecidableEq Key] (supplied : List Key) : Tree Key → Tree Key
  | .data => .data
  | .reference ref => .reference (bindReference supplied ref)
  | .assignment ref rhs => .assignment (bindReference supplied ref) (supply supplied rhs)
  | .sequence left right => .sequence (supply supplied left) (supply supplied right)
  | .closure body => .closure (supply supplied body)
  | .unsupported => .unsupported

/-- Supplying lexical parameters removes exactly their obligations. Real
hygienic parameter classification is supplied by the same checked SDK expansion. -/
theorem supplied_requires [DecidableEq Key] (tree : Tree Key) (supplied : List Key) :
    Requires name (supply supplied tree) ↔ Requires name tree ∧ name ∉ supplied := by
  induction tree with
  | data => simp [supply, Requires]
  | reference ref =>
    simp only [supply, Requires, bindReference, Bool.or_eq_false_iff, decide_eq_false_iff_not]
    grind
  | assignment ref rhs ih =>
    simp only [supply, Requires, bindReference, Bool.or_eq_false_iff, decide_eq_false_iff_not, ih]
    grind
  | sequence left right ihLeft ihRight =>
    simp [supply, Requires, ihLeft, ihRight, or_and_right]
  | closure body ih => exact ih
  | unsupported => simp [supply, Requires]

/-- Parameter coverage is equivalent to an empty residual list, not merely
agreement on a manually chosen subset. Unknown core dispatch still rejects. -/
theorem supplied_complete [DecidableEq Key] (tree : Tree Key) (supplied : List Key)
    (accepted : dependencies (supply supplied tree) = some remaining) :
    remaining = [] ↔ ∀ name, Requires name tree → name ∈ supplied := by
  constructor
  · intro empty name required
    by_cases present : name ∈ supplied
    · exact present
    · have member := (complete _ accepted).mpr ((supplied_requires tree supplied).mpr ⟨required, present⟩)
      simp [empty] at member
  · intro covered
    apply List.eq_nil_iff_forall_not_mem.mpr
    intro name member
    have obligation := (supplied_requires tree supplied).mp ((complete _ accepted).mp member)
    exact obligation.2 (covered name obligation.1)

/-- Reuse LeanPoo's selected typed factories: an exact source request plus
successful preparation supplies every required capability, not merely a subset. -/
theorem prepared_covers_source {Context : Type v} {Value : Context → Key → Type}
    (provider : Provider Context Key Value) (tree : Tree Key)
    (accepted : dependencies tree = some names)
    {factories : Requirements.Factories Context Value names}
    (ready : Requirements.prepare provider names = .ok factories)
    (required : Requires name tree) : ∃ factory, provider name = some factory := by
  exact Requirements.Selected.available provider names factories
    ((Requirements.prepare_ok_iff _ _ _).mp ready) name ((complete tree accepted).mpr required)

/-- Dependency-only admission retains the checked tree. Complete procedure
admission additionally checks the literal and evaluator result in
SourceProcedureAdmission; closed data alone is not a procedure capability. -/
def admit (tree : Tree Key) : Option (Tree Key) := do
  let names ← dependencies tree
  if names.isEmpty then some tree else none

theorem admitted_closed (tree retained : Tree Key) (accepted : admit tree = some retained) :
    retained = tree ∧ ∀ name, ¬ Requires name tree := by
  unfold admit at accepted
  cases visit : dependencies tree with
  | none => simp [visit] at accepted
  | some names =>
    cases names with
    | nil =>
      simp [visit] at accepted
      subst retained
      exact ⟨rfl, fun name required => by simpa using (complete tree visit).mpr required⟩
    | cons head rest => simp [visit] at accepted

/-- A deferred callback cannot be silently treated like quoted data. -/
theorem deferred_omission (name : Key) :
    dependencies (.closure (.reference ⟨name, false⟩)) = some [name] ∧
    admit (.closure (.reference ⟨name, false⟩)) = none := by
  simp [dependencies, leaf, admit]

/-- Visiting only the assignment target misses a distinct dependency in RHS,
including when that target is already lexically bound. -/
theorem rhs_omission (target name : Key) :
    dependencies (.assignment ⟨target, true⟩ (.reference ⟨name, false⟩)) = some [name] := by
  simp [dependencies, leaf]

end ParserEngine.SourceDependencies
