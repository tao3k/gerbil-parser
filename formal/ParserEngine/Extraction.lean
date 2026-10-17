import ParserEngine.IteratorEncoding

/-! Executable witness generation over a finite ranked record graph. Certificate
admission checks references and strict dependency ranks; successful extraction
constructs Extract, rather than taking Extract as an assumed native fact. -/
namespace ParserEngine.Extraction
open IteratorEncoding

def lookup : List (Ref × Program) → Ref → Program
  | [], _ => .empty
  | (key, value) :: rest, address => if key = address then value else lookup rest address

def collect (visit : Ref → Option Program) : List Ref → Option (List (Ref × Program))
  | [] => some []
  | address :: rest => do
      let value ← visit address
      let tail ← collect visit rest
      pure ((address, value) :: tail)

theorem collect_lookup (visit : Ref → Option Program) (addresses : List Ref)
    (rows : List (Ref × Program)) (done : collect visit addresses = some rows) :
    ∀ address, address ∈ addresses → visit address = some (lookup rows address) := by
  induction addresses generalizing rows with
  | nil => simp
  | cons head rest ih =>
    cases first : visit head with
    | none => simp [collect, first] at done
    | some value =>
      cases tail : collect visit rest with
      | none => simp [collect, first, tail] at done
      | some more =>
        simp [collect, first, tail] at done
        subst rows
        intro address member
        by_cases equal : head = address
        · subst address; simpa [lookup] using first
        · have inRest : address ∈ rest := (List.mem_cons.mp member).resolve_left (Ne.symm equal)
          simpa [lookup, equal] using ih more tail address inRest

theorem collect_complete (visit : Ref → Option Program) (addresses : List Ref)
    (available : ∀ address ∈ addresses, ∃ value, visit address = some value) :
    ∃ rows, collect visit addresses = some rows := by
  induction addresses with
  | nil => exact ⟨[], rfl⟩
  | cons head rest ih =>
    obtain ⟨value, first⟩ := available head (by simp)
    obtain ⟨tail, done⟩ := ih (fun address member => available address (by simp [member]))
    exact ⟨(head, value) :: tail, by simp [collect, first, done]⟩

def admitted (address : Ref) : Record → Bool
  | .empty => address.isNone
  | .chunk items _ _ => address.isSome && decide (2 ≤ items.length)
  | _ => address.isSome

theorem admitted_correct (address : Ref) (record : Record) :
    admitted address record = true ↔ recordAdmitted address record := by
  cases record <;> cases address <;> simp [admitted, recordAdmitted]

def generate (heap : Nat → Option (List Cell)) : Nat → Ref → Option Program
  | 0, _ => none
  | fuel + 1, address => do
      let record ← readRecord heap address
      if admitted address record then
        let rows ← collect (generate heap fuel) (recordChildren record)
        pure (lowerRecord (lookup rows) record)
      else none

theorem generate_sound (heap : Nat → Option (List Cell)) (fuel : Nat) (address : Ref)
    (program : Program) (generated : generate heap fuel address = some program) :
    Extract heap address program := by
  induction fuel generalizing address program with
  | zero => simp [generate] at generated
  | succ fuel ih =>
    cases read : readRecord heap address with
    | none => simp [generate, read] at generated
    | some record =>
      cases valid : admitted address record with
      | false => simp [generate, read, valid] at generated
      | true =>
        cases gathered : collect (generate heap fuel) (recordChildren record) with
        | none => simp [generate, read, valid, gathered] at generated
        | some rows =>
          simp [generate, read, valid, gathered] at generated
          subst program
          exact .record address record (lookup rows) read ((admitted_correct address record).mp valid)
            (fun ref member => ih ref (lookup rows ref)
              (collect_lookup (generate heap fuel) (recordChildren record) rows gathered ref member))

/-- A finite checked domain closes all record references. False is represented
by none; rank zero is permitted for the empty record. Other edges decrease. -/
structure Certificate (heap : Nat → Option (List Cell)) where
  domain : List Ref
  rank : Ref → Nat
  valid : ∀ address ∈ domain, ∃ record,
    readRecord heap address = some record ∧ recordAdmitted address record ∧
    ∀ child ∈ recordChildren record, child ∈ domain ∧ rank child < rank address

theorem generate_complete (certificate : Certificate heap) (fuel : Nat) (address : Ref)
    (member : address ∈ certificate.domain) (enough : certificate.rank address < fuel) :
    ∃ program, generate heap fuel address = some program := by
  induction fuel generalizing address with
  | zero => omega
  | succ fuel ih =>
    obtain ⟨record, read, valid, children⟩ := certificate.valid address member
    have available : ∀ child ∈ recordChildren record, ∃ program, generate heap fuel child = some program := by
      intro child inRecord
      obtain ⟨inside, smaller⟩ := children child inRecord
      exact ih child inside (by omega)
    obtain ⟨rows, gathered⟩ := collect_complete (generate heap fuel) (recordChildren record) available
    exact ⟨lowerRecord (lookup rows) record, by
      simp [generate, read, (admitted_correct address record).mpr valid, gathered]⟩

theorem generate_witness (certificate : Certificate heap) (address : Ref)
    (member : address ∈ certificate.domain) :
    ∃ program, generate heap (certificate.rank address + 1) address = some program ∧
      Extract heap address program := by
  obtain ⟨program, generated⟩ := generate_complete certificate (certificate.rank address + 1) address member (by omega)
  exact ⟨program, generated, generate_sound heap _ address program generated⟩

/-- Certificate validation is executable and covers every referenced edge, not
just a fixed family of example grammars. Ranks are data, never trusted proofs. -/
def verify (heap : Nat → Option (List Cell)) (domain : List Ref) (rank : Ref → Nat) : Bool :=
  domain.all fun address =>
    match readRecord heap address with
    | none => false
    | some record => admitted address record &&
        (recordChildren record).all (fun child => domain.contains child && decide (rank child < rank address))

theorem verify_certificate (heap : Nat → Option (List Cell)) (domain : List Ref) (rank : Ref → Nat)
    (checked : verify heap domain rank = true) :
    ∃ certificate : Certificate heap, certificate.domain = domain ∧ certificate.rank = rank := by
  refine ⟨⟨domain, rank, ?_⟩, rfl, rfl⟩
  intro address member
  have valid := List.all_eq_true.mp checked address member
  cases read : readRecord heap address with
  | none => simp [read] at valid
  | some record =>
    simp only [read, Bool.and_eq_true] at valid
    refine ⟨record, rfl, (admitted_correct address record).mp valid.1, ?_⟩
    intro child inRecord
    have edge := List.all_eq_true.mp valid.2 child inRecord
    simpa using edge

theorem checked_generate (heap : Nat → Option (List Cell)) (domain : List Ref) (rank : Ref → Nat)
    (checked : verify heap domain rank = true) (address : Ref) (member : address ∈ domain) :
    ∃ program, generate heap (rank address + 1) address = some program ∧ Extract heap address program := by
  obtain ⟨certificate, sameDomain, sameRank⟩ := verify_certificate heap domain rank checked
  simpa [sameRank] using generate_witness certificate address (by simpa [sameDomain] using member)

theorem lower_children_congr (record : Record) (left right : Ref → Program)
    (agree : ∀ ref ∈ recordChildren record, left ref = right ref) :
    lowerRecord left record = lowerRecord right record := by
  cases record with
  | empty | token => rfl
  | branch a b start finish height =>
    simp only [lowerRecord]
    rw [agree a (by simp [recordChildren]), agree b (by simp [recordChildren])]
  | view body delta => simp only [lowerRecord]; rw [agree body (by simp [recordChildren])]
  | value kind start finish body =>
    cases kind <;> simp only [lowerRecord] <;> rw [agree body (by simp [recordChildren])]
  | field name start finish body => simp only [lowerRecord]; rw [agree body (by simp [recordChildren])]
  | append leading blocks tail start finish height =>
    have mapped : blocks.map left = blocks.map right := List.map_congr_left
      (fun ref member => agree ref (by simp [recordChildren, member]))
    simp only [lowerRecord]
    rw [mapped, agree leading (by simp [recordChildren]), agree tail (by simp [recordChildren])]
  | chunk items start finish =>
    have mapped : items.map left = items.map right := List.map_congr_left
      (fun ref member => agree ref (by simpa [recordChildren] using member))
    change items.foldr (fun item rest => EventIterator.Rope.branch (left item) rest) .empty =
      items.foldr (fun item rest => EventIterator.Rope.branch (right item) rest) .empty
    rw [← List.foldr_map, mapped, List.foldr_map]

theorem extraction_unique (first : Extract heap address left) (second : Extract heap address right) :
    left = right := by
  induction first generalizing right with
  | record address record child read admitted children ih =>
    cases second with
    | record _ other otherChild otherRead otherAdmitted otherChildren =>
      have same : record = other := Option.some.inj (read.symm.trans otherRead)
      subst other
      exact lower_children_congr record child otherChild
        (fun ref member => ih ref member (otherChildren ref member))

def environment (certificate : Certificate heap) (address : Ref) : Program :=
  (generate heap (certificate.rank address + 1) address).getD .empty

theorem environment_extracts (certificate : Certificate heap) (address : Ref)
    (member : address ∈ certificate.domain) : Extract heap address (environment certificate address) := by
  obtain ⟨program, generated, witness⟩ := generate_witness certificate address member
  simpa [environment, generated] using witness

theorem environment_equation (certificate : Certificate heap) (address : Ref)
    (member : address ∈ certificate.domain) (record : Record)
    (read : readRecord heap address = some record) :
    environment certificate address = lowerRecord (environment certificate) record := by
  obtain ⟨actual, actualRead, admitted, children⟩ := certificate.valid address member
  have same : actual = record := Option.some.inj (actualRead.symm.trans read)
  subst actual
  exact extraction_unique (environment_extracts certificate address member)
    (.record address record (environment certificate) read admitted
      (fun ref inRecord => environment_extracts certificate ref (children ref inRecord).1))

end ParserEngine.Extraction
