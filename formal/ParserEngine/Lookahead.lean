import LeanPoo.Prototype.DictionaryBridge

namespace ParserEngine.Lookahead

/-- Logical request-local scope, not implemented hashes or pointer equality.
The contract binds grammar, prepared plan and lexical policy. Extent changes
at input admission, including EOF; revision changes on metadata mutation. -/
structure Key where
  owner : Nat
  source : Nat
  contract : Nat
  extent : Nat
  revision : Nat
  deriving DecidableEq, Repr

structure Snapshot (Value : Type) where
  basis : Key
  value : Value

def read (current : Key) (snapshot : Snapshot Value) : Option Value :=
  if snapshot.basis = current then some snapshot.value else none

/-- Key equality is sufficient only with a producer premise binding the stored
value to a pure projection. It does not prove mutable Scheme tokens satisfy it. -/
theorem admitted_projection (project : Key → Value) (current : Key)
    (snapshot : Snapshot Value) (value : Value)
    (producer : snapshot.value = project snapshot.basis)
    (accepted : read current snapshot = some value) : value = project current := by
  unfold read at accepted
  split at accepted
  · have keys : snapshot.basis = current := ‹snapshot.basis = current›
    have values : snapshot.value = value := Option.some.inj accepted
    rw [← values, producer, keys]
  · contradiction

theorem changed_key_rejects (current : Key) (snapshot : Snapshot Value)
    (changed : snapshot.basis ≠ current) : read current snapshot = none := by
  simp [read, changed]

theorem mutation_rejects (current : Key) (snapshot : Snapshot Value)
    (changed : snapshot.basis.revision ≠ current.revision) : read current snapshot = none := by
  apply changed_key_rejects
  exact fun equal => changed (congrArg Key.revision equal)

theorem supersession_rejects (current : Key) (snapshot : Snapshot Value)
    (changed : snapshot.basis.owner ≠ current.owner) : read current snapshot = none := by
  apply changed_key_rejects
  exact fun equal => changed (congrArg Key.owner equal)

/-- Every pure consumer gets the same ordered metadata observations within
one stable extent. This does not preserve mutable pair identity or effects. -/
theorem extent_observations_equal (project : Key → Value) (current : Key)
    (snapshot : Snapshot Value) (reductions : Nat)
    (producer : snapshot.value = project snapshot.basis)
    (keys : snapshot.basis = current) :
    List.replicate reductions snapshot.value = List.replicate reductions (project current) := by
  rw [producer, keys]

-- Equal identities alone cannot establish value equality: omitting the producer
-- premise admits stale metadata even when the read guard succeeds.
example (key : Key) : read key (⟨key, 0⟩ : Snapshot Nat) = some 0 ∧ (0 : Nat) ≠ 1 := by
  simp [read]

/-- Number of capture reductions while one admitted metadata extent is stable.
An extent is not an LR state or a source offset. These are weighted abstract
operations, not CPU instructions; a no-capture or unused extent has count zero. -/
def repeatedMetadata (reductions materialize : Nat) := reductions * materialize
def scopedMetadata (reductions materialize check setup : Nat) :=
  if reductions = 0 then 0 else materialize + reductions * check + setup

theorem unused_extent_free (materialize check setup : Nat) :
    scopedMetadata 0 materialize check setup = 0 := rfl

/-- Exact break-even, charging all checks and scope construction. In particular,
one reduction cannot benefit when either check or setup has positive cost. -/
theorem extent_break_even (additional materialize check setup : Nat) :
    scopedMetadata (additional + 1) materialize check setup ≤
      repeatedMetadata (additional + 1) materialize ↔
    (additional + 1) * check + setup ≤ additional * materialize := by
  simp only [scopedMetadata, Nat.add_one_ne_zero, ↓reduceIte, repeatedMetadata,
             Nat.add_mul, Nat.one_mul]
  omega

theorem singleton_overhead_regresses (materialize check setup : Nat) :
    repeatedMetadata 1 materialize < scopedMetadata 1 materialize check setup ↔
    0 < check + setup := by
  simp [repeatedMetadata, scopedMetadata]
  omega

/-- Costs paid by both complete requests. Equality is an obligation, not an
inference from metadata parity; FFI and scheduling are explicit separate terms. -/
structure SharedWork where
  preparation : Nat
  scan : Nat
  lr : Nat
  captureOther : Nat
  validation : Nat
  publication : Nat
  ffi : Nat
  scheduling : Nat
  deriving Repr

def shared (work : SharedWork) : Nat :=
  work.preparation + work.scan + work.lr + work.captureOther +
  work.validation + work.publication + work.ffi + work.scheduling

/-- Failed snapshot attempts must be charged even if no capture later uses
this extent. Each failed attempt materializes, validates and discards its work.
These costs are logical weights, not measurements or a bounded retry policy. -/
def retryWork (failed materialize validate discard : Nat) :=
  failed * (materialize + validate + discard)

theorem one_failed_attempt_charged (materialize validate discard : Nat) :
    retryWork 1 materialize validate discard = materialize + validate + discard := by
  simp [retryWork]

theorem extent_with_retries_break_even (additional materialize check setup failed : Nat) :
    scopedMetadata (additional + 1) materialize check setup + failed ≤
      repeatedMetadata (additional + 1) materialize ↔
    (additional + 1) * check + setup + failed ≤ additional * materialize := by
  simp only [scopedMetadata, Nat.add_one_ne_zero, ↓reduceIte, repeatedMetadata,
             Nat.add_mul, Nat.one_mul]
  omega

/-- Each extent has its own weights: EOF construction may be zero, and a
mutation/snapshot check may depend on lexeme size. Uniform weights are not
assumed when admitting the complete request. -/
structure ExtentWork where
  reductions : Nat
  materialize : Nat
  check : Nat
  setup : Nat
  failedAdmission : Nat := 0
  deriving Repr

def repeatedTotal (extents : List ExtentWork) : Nat :=
  (extents.map (fun e => repeatedMetadata e.reductions e.materialize)).sum

def scopedTotal (extents : List ExtentWork) : Nat :=
  (extents.map (fun e => scopedMetadata e.reductions e.materialize e.check e.setup +
    e.failedAdmission)).sum

def baseline (work : SharedWork) (extents : List ExtentWork) : Nat :=
  shared work + repeatedTotal extents

def candidate (work : SharedWork) (extents : List ExtentWork) : Nat :=
  shared work + scopedTotal extents

/-- Amortize over the whole request, not a chosen favorable extent. A costly
singleton extent may be offset by others; every extent remains in the sum. -/
theorem complete_request_break_even (work : SharedWork) (extents : List ExtentWork) :
    candidate work extents ≤ baseline work extents ↔
    scopedTotal extents ≤ repeatedTotal extents := by
  simp [candidate, baseline]

/-- Common work cannot be dropped if the proposal changes it. -/
theorem complete_request_with_extra_work (work : SharedWork) (extents : List ExtentWork)
    (extra : Nat) :
    candidate work extents + extra ≤ baseline work extents ↔
    scopedTotal extents + extra ≤ repeatedTotal extents := by
  unfold candidate baseline
  omega

theorem unused_extent_can_pay_failed_admission (e : ExtentWork)
    (unused : e.reductions = 0) : scopedTotal [e] = e.failedAdmission := by
  simp [scopedTotal, unused, scopedMetadata]

-- A candidate beneficial without retries can lose after failed admission work.
example : scopedTotal [⟨8, 4, 1, 1, retryWork 5 4 1 1⟩] >
    repeatedTotal [⟨8, 4, 1, 1, retryWork 5 4 1 1⟩] := by decide

-- Retain singleton and EOF losses in a heterogeneous complete-request sum.
-- These are illustrative weights, not measured parser or CPU observations.
example : scopedTotal [⟨1, 4, 1, 1, 0⟩, ⟨8, 4, 1, 1, 0⟩, ⟨5, 0, 1, 0, 0⟩] <
    repeatedTotal [⟨1, 4, 1, 1, 0⟩, ⟨8, 4, 1, 1, 0⟩, ⟨5, 0, 1, 0, 0⟩] := by decide

example : repeatedTotal [⟨1, 4, 1, 1, 0⟩, ⟨1, 4, 1, 1, 0⟩, ⟨5, 0, 1, 0, 0⟩] <
    scopedTotal [⟨1, 4, 1, 1, 0⟩, ⟨1, 4, 1, 1, 0⟩, ⟨5, 0, 1, 0, 0⟩] := by decide

-- Both positive and negative regimes are part of the model.
example : scopedMetadata 8 4 1 1 < repeatedMetadata 8 4 := by decide
example : repeatedMetadata 1 4 < scopedMetadata 1 4 1 1 := by decide
example : repeatedMetadata 8 0 < scopedMetadata 8 0 1 0 := by decide
example : scopedMetadata 0 4 1 1 < 4 + 1 := by decide

/-- A pending scanner admission owns its mode, even when reductions change
LR state before consumption. This is input transport, not metadata reuse. -/
structure PendingInput (Token Mode : Type) where
  token : Token
  mode : Mode
  deriving DecidableEq

structure InputFrontier (Token Mode : Type) where
  state : Nat
  pending : Option (PendingInput Token Mode)
  emitted : List (PendingInput Token Mode)

def reduceFrontier (nextState : Nat) (frontier : InputFrontier Token Mode) :=
  { frontier with state := nextState }

def shiftFrontier (frontier : InputFrontier Token Mode) :=
  { frontier with pending := none, emitted := frontier.emitted ++ frontier.pending.toList }

/-- Capture the admitted token/mode, not the lexical mode of the reduced state. -/
theorem reductions_preserve_pending (states : List Nat) (frontier : InputFrontier Token Mode) :
    (states.foldl (fun current next => reduceFrontier next current) frontier).pending =
      frontier.pending := by
  induction states generalizing frontier with
  | nil => rfl
  | cons head tail ih => exact ih (reduceFrontier head frontier)

theorem reduced_shift_preserves_admission (states : List Nat)
    (frontier : InputFrontier Token Mode) :
    (shiftFrontier (states.foldl (fun current next => reduceFrontier next current) frontier)).emitted =
      frontier.emitted ++ frontier.pending.toList := by
  have unchanged : (states.foldl (fun current next => reduceFrontier next current) frontier).emitted =
      frontier.emitted := by
    induction states generalizing frontier with
    | nil => rfl
    | cons head tail ih => exact ih (reduceFrontier head frontier)
  simp only [shiftFrontier, unchanged, reductions_preserve_pending]

/-- List-cell transport counts only. A scalar input saves the admission cell
only if action queries accept it directly. Converting back on each query pays
again for reductions and escapes; neither term may be omitted. -/
def listInputCells (admissions : Nat) := admissions
def scalarInputCells (convertedQueries escapes : Nat) := convertedQueries + escapes

theorem scalar_input_break_even (admissions queries escapes : Nat) :
    scalarInputCells queries escapes ≤ listInputCells admissions ↔
      queries + escapes ≤ admissions := by rfl

theorem per_query_conversion_regresses (admissions extraQueries escapes : Nat)
    (more : 0 < extraQueries + escapes) :
    listInputCells admissions < scalarInputCells (admissions + extraQueries) escapes := by
  unfold listInputCells scalarInputCells
  omega

/-- Lift transport regression to complete logical work only when every other
request term is identical. This is not a physical CPU-cost assumption. -/
theorem request_transport_regresses (work : SharedWork) (admissions extraQueries escapes : Nat)
    (more : 0 < extraQueries + escapes) :
    shared work + listInputCells admissions <
      shared work + scalarInputCells (admissions + extraQueries) escapes := by
  exact Nat.add_lt_add_left
    (per_query_conversion_regresses admissions extraQueries escapes more) (shared work)

-- Counterexample with nonempty queries: not a measured action trace.
example : listInputCells 192 < scalarInputCells (192 + 322) 0 := by decide

/-- The streaming domain is empty or one admitted token. Borrowed multi-token
suffixes keep their separate identity-preserving list representation. -/
abbrev SingleInput (Token : Type) := { xs : List Token // xs.length ≤ 1 }

def encodeInput (input : Option Token) : SingleInput Token :=
  ⟨input.toList, by cases input <;> simp⟩

def decodeInput (input : SingleInput Token) : Option Token := input.val.head?

def inputBridge (Token : Type) :
    LeanPoo.Prototype.DictionaryBridge (Option Token) (SingleInput Token) where
  toFields := encodeInput
  fromFields := decodeInput
  from_to := by intro input; cases input <;> rfl
  to_from := by
    intro input
    apply Subtype.ext
    rcases input with ⟨values, bounded⟩
    cases values with
    | nil => rfl
    | cons token tail =>
      have empty : tail = [] := by
        have lengthZero : tail.length = 0 := by simp only [List.length_cons] at bounded; omega
        cases tail with
        | nil => rfl
        | cons _ _ => simp at lengthZero
      subst tail
      rfl

/-- Reuse the upstream inverse-bearing representation contract for every
state-specific selector, including EOF and effect-free literal precedence. -/
theorem selector_transport_exact (selector : Nat → SingleInput Token → Row)
    (state : Nat) (input : SingleInput Token) :
    selector state ((inputBridge Token).toFields ((inputBridge Token).fromFields input)) =
      selector state input := by rw [(inputBridge Token).to_from]

/-- Rejected flat-selector experiment: token-fallback queries transfer through
three procedures; EOF and literal hits have separate costs. Flattening those
queries would remove two procedure transfers each. Private-row branches and body work are independent charged costs. -/
def nestedSelectorCalls (queries : Nat) := 3 * queries
def directSelectorCalls (queries : Nat) := queries

theorem direct_dispatch_transfers (queries : Nat) :
    nestedSelectorCalls queries = directSelectorCalls queries + 2 * queries := by
  unfold nestedSelectorCalls directSelectorCalls
  omega

end ParserEngine.Lookahead
