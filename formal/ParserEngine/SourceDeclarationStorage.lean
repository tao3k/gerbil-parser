import ParserEngine.SourceDeclarationTransformation
import ParserEngine.SchemeStorage

/-! Declaration publication uses the existing Scheme heap and allocation model.
The host codec supplies fresh graph storage. This module proves the project
frame/consumer consequences for arbitrary later mutation histories. -/
namespace ParserEngine.SourceDeclarationStorage
open WalkSource SchemeTableEvaluation LeanPoo.Functional

inductive Mutation where
  | index (address key : Nat) (value : SchemeTableEvaluation.Value)
  | field (address : Nat) (key : String) (value : SchemeTableEvaluation.Value)

def Mutation.address : Mutation → Nat
  | .index address _ _ | .field address _ _ => address

def applyMutation : Mutation → Heap → Heap
  | .index address key value, heap => write heap address key value
  | .field address key value, heap => writeSymbol heap address key value

def replay (mutations : List Mutation) (heap : Heap) : Heap :=
  mutations.foldl (fun heap mutation => applyMutation mutation heap) heap

/-- Mutation events use the actual primitive semantics, with no second store. -/
theorem index_primitive (heap : Heap) (address key : Nat) (value : SchemeTableEvaluation.Value) :
    primitive "table-set!" [.table address, .natural key, value] heap =
      some (.unit, applyMutation (.index address key value) heap) := rfl

theorem field_primitive (heap : Heap) (address : Nat) (key : String) (value : SchemeTableEvaluation.Value) :
    primitive "table-set!" [.table address, .symbol key, value] heap =
      some (.unit, applyMutation (.field address key value) heap) := rfl

theorem assignment_event (env : Environment) (before middle : Heap) (address : Nat)
    (value : SchemeTableEvaluation.Value) (target : env name = some (.cell address))
    (rhs : Eval env before expression value middle) :
    Eval env before (app "set!" [s name, expression]) .unit
      (applyMutation (.index address 0 value) middle) := .assignment target rhs

theorem mutation_frame (mutation : Mutation) (heap : Heap) (snapshot : Nat) (key : Key)
    (separate : snapshot ≠ mutation.address) :
    applyMutation mutation heap snapshot key = heap snapshot key := by
  cases mutation <;> simp_all [applyMutation, Mutation.address, write, writeSymbol]

/-- Every finite callback mutation history is covered, including repeated
writes and interleaved numeric/symbolic fields. No history length is fixed. -/
theorem replay_frame (mutations : List Mutation) (heap : Heap) (snapshot : Nat) (key : Key)
    (owned : ∀ mutation ∈ mutations, snapshot ≠ mutation.address) :
    replay mutations heap snapshot key = heap snapshot key := by
  induction mutations generalizing heap with
  | nil => rfl
  | cons first rest ih =>
    change replay rest (applyMutation first heap) snapshot key = _
    rw [ih _ (fun mutation included => owned mutation (List.mem_cons_of_mem _ included))]
    exact mutation_frame first heap snapshot key (owned first (by simp))

/-- Reuse the existing logical allocator. Physical graph allocation and host
round-trip identity remain external; no physical GC or ABI theorem is inferred. -/
theorem published_slot (store : SchemeStorage.Store) (value : SchemeTableEvaluation.Value) :
    (SchemeStorage.cell store value).heap store.frontier 0 = some value := by
  simp [SchemeStorage.cell, write]

theorem callback_slot_separate (store : SchemeStorage.Store) (callback : Nat)
    (older : callback < store.frontier) : store.frontier ≠ callback := by omega

/-- LeanPoo's retained selected capability reads the detached declaration at
its invocation heap after the complete callback history, rather than freezing
or accidentally following the callback's old output slot. -/
theorem published_factory_reads (env : Environment) (store : SchemeStorage.Store)
    (value : SchemeTableEvaluation.Value) (mutations : List Mutation) (keys : List String)
    {factories : Requirements.Factories Heap (fun _ (_ : String) => Option SchemeTableEvaluation.Value) keys}
    (ready : Requirements.prepare (SourceEnvironment.provider env) keys = .ok factories)
    (member : name ∈ keys) (bound : env name = some (.cell store.frontier))
    (owned : ∀ mutation ∈ mutations, store.frontier ≠ mutation.address) :
    Requirements.factoryAt factories name member
      (replay mutations (SchemeStorage.cell store value).heap) = some value := by
  rw [SourceEnvironment.selected_resolves env keys ready member]
  simp only [resolve, bound]
  rw [replay_frame mutations _ store.frontier 0 owned]
  exact published_slot store value

/-- The exact named factory is consumed through the real Scheme variable rule. -/
theorem published_variable_evaluates (env : Environment) (store : SchemeStorage.Store)
    (value : SchemeTableEvaluation.Value) (mutations : List Mutation) (keys : List String)
    {factories : Requirements.Factories Heap (fun _ (_ : String) => Option SchemeTableEvaluation.Value) keys}
    (ready : Requirements.prepare (SourceEnvironment.provider env) keys = .ok factories)
    (member : name ∈ keys) (bound : env name = some (.cell store.frontier))
    (owned : ∀ mutation ∈ mutations, store.frontier ≠ mutation.address) :
    Eval env (replay mutations (SchemeStorage.cell store value).heap) (s name) value
      (replay mutations (SchemeStorage.cell store value).heap) :=
  SourceEnvironment.variable_evaluates env keys ready member
    (published_factory_reads env store value mutations keys ready member bound owned)

/-- Without the fresh publication boundary, the very same selected factory
observes a callback write. Name admission cannot repair this aliasing defect. -/
theorem aliased_factory_changes (env : Environment) (heap : Heap) (keys : List String)
    (address : Nat) (changed : SchemeTableEvaluation.Value)
    {factories : Requirements.Factories Heap (fun _ (_ : String) => Option SchemeTableEvaluation.Value) keys}
    (ready : Requirements.prepare (SourceEnvironment.provider env) keys = .ok factories)
    (member : name ∈ keys) (bound : env name = some (.cell address)) :
    Requirements.factoryAt factories name member
      (applyMutation (.index address 0 changed) heap) = some changed :=
  SourceEnvironment.retained_cell_reads_write env keys ready member bound

/-- Source-reference names and the declaration slot occupy separate capability
spaces, matching the helper frame outside the constructed source lambda. -/
inductive Capability where
  | reference (name : String)
  | declaration
  deriving DecidableEq

def requestKeys (names : List String) : List Capability :=
  names.map Capability.reference ++ [.declaration]

def requestProvider (source publication : Environment) (publishedName : String) :
    Provider Heap Capability (fun _ _ => Option SchemeTableEvaluation.Value)
  | .reference name => SourceEnvironment.provider source name
  | .declaration => SourceEnvironment.provider publication publishedName

/-- The post-call heap, fresh allocation, native guards and one typed
checklist are connected. A declaration capability cannot satisfy a missing
source reference, even when the textual names happen to be equal. -/
theorem request_after_mutations (env source : Environment) (frontier : Nat)
    (sourceName publishedName : String) (dependencies : List String) (forms : List Datum) (original transformed : Datum)
    (selected : SourceDeclarationReading.readDeclaration sourceName forms = some original)
    (before after : Heap) (fresh : SchemeStorage.Fresh ⟨after, frontier⟩)
    (called : SourceDeclarationTransformation.GuardedCall env before after original transformed)
    (expand : Environment → Datum → Option (SourceDependencies.Tree String)) (tree : SourceDependencies.Tree String)
    (expanded : expand source (SourceDependencyAdapter.lambdaData transformed dependencies) = some tree)
    (collected : SourceDependencies.dependencies tree = some names)
    {factories : Requirements.Factories Heap (fun _ (_ : Capability) => Option SchemeTableEvaluation.Value)
      (requestKeys names)}
    (ready : Requirements.prepare
      (requestProvider source (extend env publishedName (.cell frontier)) publishedName) (requestKeys names) = .ok factories)
    (mutations : List Mutation) (owned : ∀ mutation ∈ mutations, frontier ≠ mutation.address) :
    original ∈ forms ∧ SourceDeclarationTransformation.guardsAccept original transformed = true ∧
    Eval env before SourceDeclarationTransformation.invocation called.transformedValue after ∧
    SchemeStorage.Fresh (SchemeStorage.cell ⟨after, frontier⟩ (datumValue transformed)) ∧
    (Requirements.factoryAt factories Capability.declaration (by simp [requestKeys])
      (replay mutations (SchemeStorage.cell ⟨after, frontier⟩ (datumValue transformed)).heap) = some (datumValue transformed)) ∧
    Eval (extend env publishedName (.cell frontier))
      (replay mutations (SchemeStorage.cell ⟨after, frontier⟩ (datumValue transformed)).heap)
      (s publishedName) (datumValue transformed)
      (replay mutations (SchemeStorage.cell ⟨after, frontier⟩ (datumValue transformed)).heap) ∧
    expand source (SourceDependencyAdapter.lambdaData transformed dependencies) = some tree ∧
    (∀ key, SourceDependencies.Requires key tree →
      ∃ factory, SourceEnvironment.provider source key = some factory) := by
  let publication := extend env publishedName (.cell frontier)
  let future := replay mutations (SchemeStorage.cell ⟨after, frontier⟩ (datumValue transformed)).heap
  have resolved : resolve publication future publishedName = some (datumValue transformed) := by
    have publishedBinding : publication publishedName = some (.cell frontier) := by simp [publication, extend]
    simp only [resolve, publishedBinding]
    change replay mutations (SchemeStorage.cell ⟨after, frontier⟩ (datumValue transformed)).heap frontier 0 = _
    rw [replay_frame mutations _ frontier 0 owned]
    exact published_slot ⟨after, frontier⟩ (datumValue transformed)
  have chosen := Requirements.factoryAt_selected
    (requestProvider source publication publishedName) (requestKeys names) factories
    ((Requirements.prepare_ok_iff _ _ _).mp ready) Capability.declaration (by simp [requestKeys])
  have exactFactory : (fun heap => resolve publication heap publishedName) =
      Requirements.factoryAt factories Capability.declaration (by simp [requestKeys]) := by
    apply Option.some.inj
    simpa [requestProvider, SourceEnvironment.provider, publication, extend] using chosen
  refine ⟨(SourceDeclarationReading.selected_occurs selected).1,
    (SourceDeclarationTransformation.guards_accept_iff original transformed).mpr called.accepted,
    called.execution, SchemeStorage.cell_fresh fresh, ?_, .variable resolved, expanded, ?_⟩
  · exact (congrFun exactFactory future).symm.trans resolved
  · intro key required
    have available := Requirements.Selected.available
      (requestProvider source publication publishedName) (requestKeys names) factories
      ((Requirements.prepare_ok_iff _ _ _).mp ready) (Capability.reference key)
      (List.mem_append_left _ (List.mem_map.mpr ⟨key, (SourceDependencies.complete tree collected).mpr required, rfl⟩))
    exact available

/-- The declaration slot never masks a missing lexical source capability. -/
theorem declaration_does_not_supply_reference (source publication : Environment)
    (publishedName name : String) (absent : source name = none) :
    requestProvider source publication publishedName (.reference name) = none := by
  simp [requestProvider, SourceEnvironment.provider, absent]

end ParserEngine.SourceDeclarationStorage
