import ParserEngine.Fold

namespace ParserEngine

/-- Whole-region execution: State includes all consumer slots, the ordered
event owner and rejection state. Fact is recognition over one immutable source
extent, not a cache of a mutable consumer or a request result. -/
def repeatedRecognition {Source Fact State : Type}
    (recognize : Source → Fact) (source : Source)
    (consumers : List (Fact → State → State)) (state : State) : State :=
  consumers.foldl (fun current consume => consume (recognize source) current) state

def sharedRecognition {Fact State : Type} (fact : Fact)
    (consumers : List (Fact → State → State)) (state : State) : State :=
  consumers.foldl (fun current consume => consume fact current) state

/-- Consumers may depend on preceding consumer writes; their original order
is retained. Only recognition is shared. Scheme must establish immutable extent
and absence of recognition effects; this theorem does not prove that analysis. -/
theorem shared_recognition_ordered {Source Fact State : Type}
    (recognize : Source → Fact) (source : Source)
    (consumers : List (Fact → State → State)) (state : State) :
    repeatedRecognition recognize source consumers state =
      sharedRecognition (recognize source) consumers state := rfl

/-- An invariant can survive writes (literal zero resets), rather than require
all state untouched. This applies to arbitrary ordered engine transitions. -/
theorem transition_list_preserves {State : Type} (safe : State → Prop)
    (transitions : List (State → State))
    (preserves : ∀ transition ∈ transitions, ∀ state, safe state → safe (transition state))
    (state : State) (entry : safe state) :
    safe (transitions.foldl (fun current transition => transition current) state) := by
  induction transitions generalizing state with
  | nil => exact entry
  | cons transition rest ih =>
    simp only [List.foldl_cons]
    apply ih
    · intro action member current valid
      exact preserves action (List.mem_cons_of_mem transition member) current valid
    · exact preserves transition (List.mem_cons_self ..) state entry

/-- Logical repeated source work, not a CPU/GC/wall-time estimate. Real AOT
counters must bind consumers and scanned bytes before applying this count. -/
def repeatedSourceVisits (consumers bytes : Nat) : Nat := consumers * bytes
def sharedSourceVisits (consumers bytes : Nat) : Nat := if consumers = 0 then 0 else bytes

theorem shared_source_visits_not_more (consumers bytes : Nat) :
    sharedSourceVisits consumers bytes ≤ repeatedSourceVisits consumers bytes := by
  cases consumers with
  | zero => simp [sharedSourceVisits, repeatedSourceVisits]
  | succ n =>
    simp [sharedSourceVisits, repeatedSourceVisits, Nat.succ_mul]

/-- A generated call boundary executes an ordered region on the same owner.
Splicing it into the caller must retain every prior/dependent effect. State
includes rejection and tape state; lexical alpha conversion is a separate
compiler obligation checked by directed Scheme controls, not this theorem. -/
theorem splice_call_preserves_order {State : Type}
    (before region after : List (State → State)) (state : State) :
    (before ++ [fun owner => region.foldl (fun s action => action s) owner] ++ after).foldl
      (fun s action => action s) state =
    (before ++ region ++ after).foldl (fun s action => action s) state := by
  simp [List.foldl_append]

/-- Copying the used prefix and transferring a buffer with its logical length
reduced to used have identical observable byte sequences. This does not prove
the primitive's memory reclamation or the implementation's exclusive owner. -/
theorem owned_publication_prefix {Byte : Type} (buffer : List Byte) (used : Nat) :
    (buffer.take used).take used = buffer.take used := by
  rw [List.take_take, Nat.min_self]

/-- Carry is encoded by equality of suffix length and consumed extent, not by
passing the unbounded initial Scheme counter through a fixed-width ABI. Reuse
Fold's counter semantics; do not duplicate its refinement model here. -/
def countSummarySized (summary : Bool × Nat) (consumed : Nat) : Prop :=
  (summary.1 = true ↔ summary.2 = consumed) ∧ summary.2 ≤ consumed

theorem count_summary_extent_step (summary : Bool × Nat) (consumed : Nat)
    (sized : countSummarySized summary consumed) (member : Bool) :
    countSummarySized (countSummaryStep summary member) (consumed + 1) := by
  rcases summary with ⟨carry, suffix⟩
  cases member <;> cases carry <;>
    simp_all [countSummarySized, countSummaryStep] <;> omega

theorem count_summary_extent (members : List Bool) (summary : Bool × Nat)
    (consumed : Nat) (sized : countSummarySized summary consumed) :
    countSummarySized (members.foldl countSummaryStep summary) (consumed + members.length) := by
  induction members generalizing summary consumed with
  | nil => simpa using sized
  | cons member rest ih =>
    simp only [List.foldl_cons]
    have result := ih (countSummaryStep summary member) (consumed + 1)
      (count_summary_extent_step summary consumed sized member)
    simpa [Nat.add_assoc, Nat.add_comm, Nat.add_left_comm] using result

/-- This is the representation boundary used by the withdrawn span leaf. It
does not establish C storage, source eligibility or physical performance. -/
theorem count_summary_length_encoding (members : List Bool) (initial : Nat) :
    let summary := members.foldl countSummaryStep (true, 0)
    applyCountSummary summary initial =
      if summary.2 = members.length then initial + summary.2 else summary.2 := by
  have extent := count_summary_extent members (true, 0) 0
    (by simp [countSummarySized])
  simp only [Nat.zero_add, countSummarySized] at extent
  cases carry : (members.foldl countSummaryStep (true, 0)).1 with
  | false =>
    have mismatch : (members.foldl countSummaryStep (true, 0)).2 ≠ members.length := by
      intro equal
      have contradiction := extent.1.mpr equal
      simp [carry] at contradiction
    simp [applyCountSummary, carry, mismatch]
  | true =>
    have equal := extent.1.mp carry
    simp [applyCountSummary, carry, equal]

end ParserEngine

#print axioms ParserEngine.shared_recognition_ordered
#print axioms ParserEngine.transition_list_preserves
#print axioms ParserEngine.shared_source_visits_not_more
#print axioms ParserEngine.splice_call_preserves_order
#print axioms ParserEngine.owned_publication_prefix
#print axioms ParserEngine.count_summary_extent_step
#print axioms ParserEngine.count_summary_extent
#print axioms ParserEngine.count_summary_length_encoding
