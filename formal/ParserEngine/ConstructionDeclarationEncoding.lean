import ParserEngine.ConstructionEncoding

/-! Declaration authority comes from raw source names, not the reserved-name
index or a second metadata object's declaration field. This contract feeds the
same existing retained constructor; semantic recipe extraction remains separate. -/
namespace ParserEngine.ConstructionDeclarationEncoding
open ConstructionEncoding

/-- A side-channel declaration list cannot alter the raw reader result. -/
theorem metadata_independent (raw : WalkSource.Datum) (metadata : ConstructionGrammar.Input)
    (replacement : List Nat) :
    read raw {metadata with declarations := replacement} = read raw metadata := rfl

/-- The declaration stream is decoded from the same concrete raw frame.
No declaration list is reconstructed from rows or supplied origin metadata. -/
theorem reader_declarations (directory rows declarations : List WalkSource.Datum)
    (augmented root : String) (valid : Bool) (metadata : ConstructionGrammar.Input) (input : Input)
    (accepted : read (WalkSource.l [WalkSource.l directory, WalkSource.l rows,
      WalkSource.s augmented, WalkSource.s root, WalkSource.l declarations, WalkSource.b valid]) metadata = some input) :
    declarations.mapM readDeclaration = some input.declarations := by
  unfold ConstructionEncoding.read at accepted
  obtain ⟨names, _, rest⟩ := Option.bind_eq_some_iff.mp accepted
  obtain ⟨records, _, rest⟩ := Option.bind_eq_some_iff.mp rest
  obtain ⟨declared, parsed, equal⟩ := Option.bind_eq_some_iff.mp rest
  cases Option.some.inj equal
  exact parsed

/-- Native declarations are individually checked against this frame's directory.
An absent declaration name cannot acquire the default code of another name. -/
theorem normalized_declarations (input : Input) (normalized : ConstructionGrammar.Input)
    (accepted : normalize input = some normalized) :
    input.declarations.mapM (lookup input.names) = some normalized.declarations := by
  unfold normalize at accepted
  obtain ⟨augmented, _, rest⟩ := Option.bind_eq_some_iff.mp accepted
  obtain ⟨root, _, rest⟩ := Option.bind_eq_some_iff.mp rest
  obtain ⟨declarations, checked, rest⟩ := Option.bind_eq_some_iff.mp rest
  obtain ⟨records, _, equal⟩ := Option.map_eq_some_iff.mp rest
  subst normalized
  exact checked

/-- An independent membership consequence of a complete partial traversal. -/
theorem mapM_members (convert : α → Option β) (raw : List α) (encoded : List β)
    (accepted : raw.mapM convert = some encoded) :
    result ∈ encoded ↔ ∃ value ∈ raw, convert value = some result := by
  induction raw generalizing encoded with
  | nil => cases accepted; simp
  | cons first rest ih =>
    rw [List.mapM_cons] at accepted
    cases headRead : convert first with
    | none => simp [headRead] at accepted
    | some head =>
      cases tailRead : rest.mapM convert with
      | none => simp [headRead, tailRead] at accepted
      | some tail =>
        simp [headRead, tailRead] at accepted
        subst encoded
        constructor
        · intro member
          rcases List.mem_cons.mp member with same | member
          · subst result; exact ⟨first, by simp, headRead⟩
          · obtain ⟨value, present, converted⟩ := (ih tail tailRead).mp member
            exact ⟨value, List.mem_cons_of_mem _ present, converted⟩
        · rintro ⟨value, present, converted⟩
          rcases List.mem_cons.mp present with same | present
          · subst value
            have sameResult := Option.some.inj (headRead.symm.trans converted)
            exact List.mem_cons.mpr (Or.inl sameResult.symm)
          · exact List.mem_cons_of_mem _ ((ih tail tailRead).mpr ⟨value, present, converted⟩)

theorem declaration_positions (input : Input) (normalized : ConstructionGrammar.Input)
    (accepted : normalize input = some normalized) :
    input.declarations.length = normalized.declarations.length ∧
      ∀ (index : Nat) (name : String), input.declarations[index]? = some name →
        ∃ (code : Nat), normalized.declarations[index]? = some code ∧ input.names[code]? = some name := by
  obtain ⟨length, positions⟩ := mapM_trace (lookup input.names) input.declarations
    normalized.declarations (normalized_declarations input normalized accepted)
  refine ⟨length, ?_⟩
  intro index name present
  obtain ⟨code, read, found⟩ := positions index name present
  exact ⟨code, read, lookup_sound input.names name code found⟩

/-- Reserved references and declarations remain distinct even when both have
valid directory indices. Zero-row declarations and duplicates are retained. -/
theorem declared_iff (input : Input) (normalized : ConstructionGrammar.Input)
    (accepted : normalize input = some normalized) (name : String) (code : Nat)
    (found : lookup input.names name = some code) :
    code ∈ normalized.declarations ↔ name ∈ input.declarations := by
  rw [mapM_members (lookup input.names) input.declarations normalized.declarations
    (normalized_declarations input normalized accepted)]
  constructor
  · rintro ⟨other, present, read⟩
    have same := lookup_injective input.names other name code read found
    exact same ▸ present
  · intro present; exact ⟨name, present, found⟩

theorem missing_name_rejected (input : Input) (name : String)
    (present : name ∈ input.declarations) (absent : lookup input.names name = none) :
    normalize input = none := by
  cases checked : normalize input with
  | none => rfl
  | some normalized =>
    have converted := normalized_declarations input normalized checked
    obtain ⟨index, position⟩ := List.mem_iff_getElem?.mp present
    obtain ⟨code, _, found⟩ := (mapM_trace (lookup input.names) input.declarations normalized.declarations converted).2
      index name position
    rw [absent] at found
    cases found

theorem undeclared_operand (input : Input) (normalized : ConstructionGrammar.Input)
    (accepted : normalize input = some normalized) (name : String) (code : Nat)
    (found : lookup input.names name = some code) (absent : name ∉ input.declarations)
    (operand : ProducerAdmission.Operand) (base : operand.base = .nonterminal code) :
    ConstructionGrammar.operandAllowed normalized operand = false := by
  have missing : code ∉ normalized.declarations := fun present =>
    absent ((declared_iff input normalized accepted name code found).mp present)
  simp [ConstructionGrammar.operandAllowed, base, missing]

/-- The existing LeanPoo product cannot authorize a physical lhs solely from
metadata: admitted owners are traced back to the raw declaration stream. -/
theorem factory_owner_declared (sources : ConstructionConsumer.Sources) (input : Input)
    (traversal : CachedConstruction.Traversal) (output : Output sources input traversal)
    (record : ConstructionGrammar.Record) (member : record ∈ output.normalized.records)
    (name : String) (found : lookup input.names name = some record.value.lhs) :
    name ∈ input.declarations :=
  (declared_iff input output.normalized output.encoded name record.value.lhs found).mp
    (ConstructionGrammar.lhs_declared output.normalized output.result.receipt record member)

end ParserEngine.ConstructionDeclarationEncoding
