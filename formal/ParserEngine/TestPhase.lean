import LeanPoo.Functional.Requirements

/-! Test declaration capabilities are selected before runtime factories run.
Gerbil expansion/import correctness and stable module identity are external
premises. This model does not assert a filesystem or dyld latency bound. -/
namespace ParserEngine.TestPhase
open LeanPoo.Functional
universe u v

inductive Profile where | engine | fixture | source | loader | development deriving DecidableEq
inductive Capability where | assertions | language | development deriving DecidableEq
variable {Context : Type u} {Value : Context → Capability → Type v}

def demand : Profile → List Capability
  | .engine | .fixture => [.assertions]
  | .source | .loader => [.assertions, .language]
  | .development => [.assertions, .language, .development]

/-- Arbitrary batches preserve declaration order, without deduplicating cases.
This is distinct from the SDK's cache of imported modules. -/
inductive Declaration where
  | single (profile : Profile)
  | batch (left right : Declaration)

def requests : Declaration → List Capability
  | .single profile => demand profile
  | .batch left right => requests left ++ requests right

def generic : Declaration → Prop
  | .single .loader | .single .source | .single .development => False
  | .single _ => True
  | .batch left right => generic left ∧ generic right

theorem generic_requests (declaration : Declaration) (admitted : generic declaration) :
    ∀ key ∈ requests declaration, key = .assertions := by
  induction declaration with
  | single profile =>
    cases profile <;> simp_all [generic, requests, demand]
  | batch left right ihLeft ihRight =>
    intro key member
    rcases List.mem_append.mp member with member | member
    · exact ihLeft admitted.1 key member
    · exact ihRight admitted.2 key member

theorem batch_independent
    (declaration : Declaration) (admitted : generic declaration)
    (provider other : Provider Context Capability Value)
    (same : provider .assertions = other .assertions) :
    Requirements.prepare provider (requests declaration) =
      Requirements.prepare other (requests declaration) := by
  apply Requirements.prepare_congr
  intro key member
  rw [generic_requests declaration admitted key member]
  exact same

/-- Replacing any unrequested provider cannot change preparation, including
its first error. Thus missing language execution cannot break generic tests. -/
theorem generic_independent
    (provider other : Provider Context Capability Value)
    (same : provider .assertions = other .assertions) :
    Requirements.prepare provider (demand .engine) =
      Requirements.prepare other (demand .engine) := by
  apply Requirements.prepare_congr
  intro key member
  simp only [demand, List.mem_singleton] at member
  subst key
  exact same

/-- Basic parsing clauses do not request development/build services. -/
theorem source_independent
    (provider other : Provider Context Capability Value)
    (sameAssertions : provider .assertions = other .assertions)
    (sameLanguage : provider .language = other .language) :
    Requirements.prepare provider (demand .source) =
      Requirements.prepare other (demand .source) := by
  apply Requirements.prepare_congr
  intro key member
  simp [demand] at member
  rcases member with rfl | rfl
  · exact sameAssertions
  · exact sameLanguage

/-- The production POO loader needs parsing assertions, not the development
catalog. This corresponds to the real loader-controls module snapshot. -/
theorem loader_independent
    (provider other : Provider Context Capability Value)
    (sameAssertions : provider .assertions = other .assertions)
    (sameLanguage : provider .language = other .language) :
    Requirements.prepare provider (demand .loader) =
      Requirements.prepare other (demand .loader) :=
  source_independent provider other sameAssertions sameLanguage

/-- The admitted loader branch must preserve the language capability failure;
it cannot silently fall back to an empty or generic test suite. -/
theorem loader_missing
    (provider : Provider Context Capability Value)
    (assertions : provider .assertions = some factory)
    (missing : provider .language = none) :
    Requirements.prepare provider (demand .loader) = .error .language := by
  simp [demand, Requirements.prepare, assertions, missing]

/-- Capability application preserves the caller's context and order. This
is the operational boundary reused by the constrained assertion consumer. -/
theorem build_order
    (assertions : Factory Context (fun context => Value context Capability.assertions))
    (language : Factory Context (fun context => Value context Capability.language))
    (context : Context) :
    Requirements.build (keys := demand .source)
      (assertions, language, PUnit.unit) context =
        (assertions context, language context, PUnit.unit) := rfl

/-- Provider selection is not a module-effect proof: every retained factory
has its own transitive import closure. Source extraction must supply it. -/
def importTrace (closure : Capability → List Module) (keys : List Capability) : List Module :=
  keys.flatMap closure

theorem import_absence
    (closure : Capability → List Module) (keys : List Capability) (target : Module)
    (closed : ∀ key ∈ keys, target ∉ closure key) : target ∉ importTrace closure keys := by
  simp only [importTrace, List.mem_flatMap]
  rintro ⟨key, member, found⟩
  exact closed key member found

/-- A caller's explicit parser import is still present after narrowing the
framework demand. Isolation cannot erase dependencies requested by the caller. -/
theorem caller_import_survives
    (caller framework : List Module) (target : Module) (requested : target ∈ caller) :
    target ∈ caller ++ framework := List.mem_append_left framework requested

/-- A generic capability can itself import development services. Therefore
provider independence alone must never be presented as transitive isolation. -/
theorem generic_effect_counterexample :
    importTrace (fun (_ : Capability) => [Capability.development]) (demand .engine) =
      [Capability.development] := rfl

/-- SDK import records project to an effective phase, an export phase and an
owned physical runtime. Source overlays or mismatched interface/wrapper roots
supply none; extracting those fields from the SDK remains an explicit premise. -/
structure ResolvedImport (Module : Type) where
  phase : Int
  sourcePhase : Int
  runtime : Option Module

def runtimeRequest (entry : ResolvedImport Module) : Option Module :=
  if entry.phase = 0 ∧ entry.sourcePhase = 0 then entry.runtime else none

def selectedImports (entries : List (ResolvedImport Module)) : List Module :=
  entries.filterMap runtimeRequest

theorem requested_phases (entry : ResolvedImport Module)
    (found : runtimeRequest entry = some name) :
    entry.phase = 0 ∧ entry.sourcePhase = 0 ∧ entry.runtime = some name := by
  by_cases phase : entry.phase = 0 ∧ entry.sourcePhase = 0
  · exact ⟨phase.1, phase.2, by simpa [runtimeRequest, phase] using found⟩
  · simp [runtimeRequest, phase] at found

theorem selected_import_owned (entries : List (ResolvedImport Module))
    (found : name ∈ selectedImports entries) :
    ∃ entry ∈ entries, entry.phase = 0 ∧ entry.sourcePhase = 0 ∧ entry.runtime = some name := by
  obtain ⟨entry, member, requested⟩ := List.mem_filterMap.mp found
  exact ⟨entry, member, requested_phases entry requested⟩

theorem selectedImports_append (left right : List (ResolvedImport Module)) :
    selectedImports (left ++ right) = selectedImports left ++ selectedImports right := by
  simp [selectedImports]

/-- The SDK accumulates each declaration into its reverse-order import field.
Projecting the expanded field restores source order for arbitrary batches. -/
theorem stored_import_order (left right : List (ResolvedImport Module)) :
    selectedImports ((right.reverse ++ left.reverse).reverse) =
      selectedImports left ++ selectedImports right := by
  rw [List.reverse_append, List.reverse_reverse, List.reverse_reverse]
  exact selectedImports_append left right

theorem source_overlay_excluded (phase sourcePhase : Int) :
    runtimeRequest (⟨phase, sourcePhase, none⟩ : ResolvedImport Module) = none := by
  simp [runtimeRequest]

theorem nonzero_phase_excluded (entry : ResolvedImport Module) (phase : entry.phase ≠ 0) :
    runtimeRequest entry = none := by
  simp [runtimeRequest, phase]

end ParserEngine.TestPhase
