import ParserEngine.TestPhase
import ParserEngine.LanguagePhase
import LeanPoo.Functional.Access
import ParserEngine.ModuleWrapper

/-! Direct imports alone cannot bound runtime admission. SDK phase extraction
and completeness of the actual dependency graph remain external premises. -/
namespace ParserEngine.PhaseDemand
open LeanPoo.Functional
universe v w x
variable {Module : Type} {Context : Type v} {Value : Context → Module → Type w}

/-- Keep template, expansion and runtime phases distinct. -/
abbrev Imports (Module : Type) := Module → List (TestPhase.ResolvedImport Module)

inductive Reach (roots : List Module) (imports : Imports Module) : Module → Prop where
  | root : name ∈ roots → Reach roots imports name
  | dependency : Reach roots imports owner →
      name ∈ TestPhase.selectedImports (imports owner) → Reach roots imports name

/-- Close the capability boundary over every admitted owner. Cycles and
repeated requests require no acyclicity or deduplication assumption. -/
structure Cut (roots : List Module) (imports : Imports Module) (allowed : Module → Prop) : Prop where
  roots : ∀ name ∈ roots, allowed name
  dependencies : ∀ owner, allowed owner →
    ∀ name ∈ TestPhase.selectedImports (imports owner), allowed name

theorem reachable_allowed (cut : Cut roots imports allowed)
    (reachable : Reach roots imports name) : allowed name := by
  induction reachable with
  | root member => exact cut.roots _ member
  | dependency _ member ih => exact cut.dependencies _ ih _ member

/-- Finite actual requests may include retries, failures and repeated keys.
Source correspondence is explicit, never inferred from a directory listing. -/
def Sound (roots : List Module) (imports : Imports Module) (requests : List Module) : Prop :=
  ∀ name ∈ requests, Reach roots imports name

/-- Recording a real owner's selected dependencies preserves correspondence.
The event list is append-only: source order and duplicate requests are retained. -/
theorem record_dependencies (sound : Sound roots imports before)
    (recorded : owner ∈ before) :
    Sound roots imports (before ++ TestPhase.selectedImports (imports owner)) := by
  intro name member
  rcases List.mem_append.mp member with old | dependency
  · exact sound name old
  · exact Reach.dependency (sound owner recorded) dependency

/-- A stopped request is a prefix of the admitted transcript, including a
first-error stop. Nothing here executes or admits an unrecorded suffix. -/
theorem stopped_requests (sound : Sound roots imports (before ++ after)) :
    Sound roots imports before := by
  intro name member
  exact sound name (List.mem_append_left after member)

/-- Actual discovery starts at the roots and records only a requested owner's
selected dependency list. A failed evaluation may stop at any recorded prefix. -/
inductive Recording (roots : List Module) (imports : Imports Module) : List Module → Prop where
  | begin : Recording roots imports roots
  | dependencies : Recording roots imports before → owner ∈ before →
      Recording roots imports (before ++ TestPhase.selectedImports (imports owner))
  | stopped : Recording roots imports (before ++ after) → Recording roots imports before

theorem recorded_sound (recording : Recording roots imports requests) :
    Sound roots imports requests := by
  induction recording with
  | begin => exact fun _ member => Reach.root member
  | dependencies _ member ih => exact record_dependencies ih member
  | stopped _ ih => exact stopped_requests ih

/-- Closed generated wrappers have only phase-zero SDK runtime requests. -/
def wrapperImports (names : List Module) : List (TestPhase.ResolvedImport Module) :=
  names.map (fun name => ⟨0, 0, some name⟩)

theorem wrapper_requests (names : List Module) :
    TestPhase.selectedImports (wrapperImports names) = names := by
  induction names with
  | nil => rfl
  | cons name rest ih =>
    simpa [wrapperImports, TestPhase.selectedImports, TestPhase.runtimeRequest] using
      congrArg (List.cons name) ih

/-- Compose arbitrary recursive reader-level wrapper extraction with phase
selection. This uses the shared closed-wrapper theorem, not fixture enumeration.
The host reader, selected filename and complete owner graph remain premises. -/
theorem extracted_wrapper_requests (native : Bool)
    (execution : ModuleWrapper.Executes form names) :
    (ModuleWrapper.rawExtract native (ModuleWrapper.encode form)).map
      (fun dependencies => TestPhase.selectedImports (wrapperImports dependencies)) = some names := by
  rw [ModuleWrapper.raw_execution native execution]
  simp only [Option.map_some, wrapper_requests]

theorem requests_allowed (cut : Cut roots imports allowed)
    (sound : Sound roots imports requests) : ∀ name ∈ requests, allowed name := by
  intro name member
  exact reachable_allowed cut (sound name member)

theorem prepare_preserved (cut : Cut roots imports allowed)
    (sound : Sound roots imports requests)
    (left right : Provider Context Module Value)
    (same : ∀ name, allowed name → left name = right name) :
    Requirements.prepare left requests = Requirements.prepare right requests := by
  apply Requirements.prepare_congr
  intro name member
  exact same name (requests_allowed cut sound name member)

/-- Preserve arbitrary prepared consumers and missing-key errors with the
existing LeanPoo-backed language phase contract. -/
theorem consumer_preserved (cut : Cut roots imports allowed)
    (sound : Sound roots imports requests)
    (left right : Provider Context Module Value)
    (same : ∀ name, allowed name → left name = right name)
    {Output : Context → Type x}
    (consume : ∀ context, Requirements.Results Value context requests → Output context) :
    Requirements.prepareWith left requests consume =
      Requirements.prepareWith right requests consume := by
  apply LanguagePhase.consumer_preserved
  intro name member
  exact same name (requests_allowed cut sound name member)

/-- Absence of a constructor follows from the transitive cut, not root names. -/
theorem constructor_absent (cut : Cut roots imports allowed)
    (sound : Sound roots imports requests) (outside : ¬ allowed constructor) :
    constructor ∉ requests := by
  intro member
  exact outside (requests_allowed cut sound constructor member)

theorem recorded_constructor_absent (cut : Cut roots imports allowed)
    (recording : Recording roots imports requests) (outside : ¬ allowed constructor) :
    constructor ∉ requests :=
  constructor_absent cut (recorded_sound recording) outside

/-- A successful capability tuple fixes its selected providers. A physical
object published afterward cannot be substituted under a retained factory.
This is the failed premise behind linking a pinned object while SDK admission
selects another generation; SDK filename selection remains external. -/
theorem prepared_provider_identity [DecidableEq Module]
    (left right : Provider Context Module Value)
    (readyLeft : Requirements.prepare left requests = .ok factories)
    (readyRight : Requirements.prepare right requests = .ok factories)
    (requested : name ∈ requests) : left name = right name := by
  rw [Requirements.factoryAt_selected left requests factories
        ((Requirements.prepare_ok_iff _ _ _).mp readyLeft) name requested,
      Requirements.factoryAt_selected right requests factories
        ((Requirements.prepare_ok_iff _ _ _).mp readyRight) name requested]

/-- A direct-import-only audit misses a selected runtime constructor edge. -/
theorem direct_import_counterexample :
    true ∉ ([false] : List Bool) ∧
      Reach [false] (fun owner => if owner then [] else [⟨0, 0, some true⟩]) true := by
  constructor
  · decide
  · apply Reach.dependency (owner := false) (Reach.root (by simp))
    simp [TestPhase.selectedImports, TestPhase.runtimeRequest]

/-- The same template-only edge does not invalidate the runtime cut. Expansion
has its own graph; this theorem does not suppress author service execution. -/
theorem template_runtime_cut :
    Cut ([false] : List Bool)
      (fun owner => if owner then [] else [⟨-1, 0, some true⟩]) (fun name => name = false) := by
  constructor
  · intro name member
    simpa using member
  · intro owner permitted name member
    subst owner
    simp [TestPhase.selectedImports, TestPhase.runtimeRequest] at member

end ParserEngine.PhaseDemand
