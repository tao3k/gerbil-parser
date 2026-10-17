import ParserEngine.Completion

/-! Operational publication and dependent-notification proof for arbitrary
finite Horn programs. Quiescence, not fuel exhaustion, implies completeness.
The native occurrence counters implement the enabled test using Completion's
multiplicity invariant; bit encodings and batch decoding are separate adapters. -/
namespace ParserEngine.Worklist

structure Clause (Fact : Type) where
  dependencies : List Fact
  head : Fact

inductive Entails (program : List (Clause Fact)) : Fact → Prop where
  | rule (clause : Clause Fact) (member : clause ∈ program)
      (body : ∀ fact ∈ clause.dependencies, Entails program fact) : Entails program clause.head

structure State (Fact : Type) where
  known : List Fact
  delivered : List Fact
  pending : List Fact

def publish [DecidableEq Fact] (s : State Fact) (fact : Fact) : State Fact :=
  if fact ∈ s.known then s else {s with known := fact :: s.known, pending := fact :: s.pending}

def enabled [DecidableEq Fact] (s : State Fact) (clause : Clause Fact) : Bool :=
  clause.dependencies.all (fun fact => s.delivered.contains fact)

def scan [DecidableEq Fact] : List (Clause Fact) → State Fact → State Fact
  | [], s => s
  | clause :: tail, s => scan tail (if enabled s clause then publish s clause.head else s)

structure Accounting (s : State Fact) : Prop where
  delivered : ∀ fact ∈ s.delivered, fact ∈ s.known
  pending : ∀ fact ∈ s.pending, fact ∈ s.known
  conserved : ∀ fact ∈ s.known, fact ∈ s.delivered ∨ fact ∈ s.pending

def Sound (program : List (Clause Fact)) (s : State Fact) : Prop :=
  ∀ fact ∈ s.known, Entails program fact

def ClosedDelivered (program : List (Clause Fact)) (s : State Fact) : Prop :=
  ∀ clause ∈ program, (∀ fact ∈ clause.dependencies, fact ∈ s.delivered) → clause.head ∈ s.known

structure Valid (program : List (Clause Fact)) (s : State Fact) : Prop where
  accounting : Accounting s
  sound : Sound program s
  closed : ClosedDelivered program s

@[simp] theorem publish_delivered [DecidableEq Fact] (s : State Fact) (fact : Fact) :
    (publish s fact).delivered = s.delivered := by
  unfold publish
  split <;> rfl

theorem publish_monotone [DecidableEq Fact] (s : State Fact) (fact : Fact) :
    ∀ old ∈ s.known, old ∈ (publish s fact).known := by
  intro old member
  unfold publish
  split <;> simp_all

theorem publish_accounting [DecidableEq Fact] {s : State Fact} (valid : Accounting s) (fact : Fact) :
    Accounting (publish s fact) := by
  unfold publish
  split
  · exact valid
  · constructor
    · intro old member; exact List.mem_cons_of_mem _ (valid.delivered old member)
    · intro old member
      rcases List.mem_cons.mp member with rfl | member
      · simp
      · exact List.mem_cons_of_mem _ (valid.pending old member)
    · intro old member
      rcases List.mem_cons.mp member with rfl | member
      · exact Or.inr (by simp)
      · rcases valid.conserved old member with delivered | pending
        · exact Or.inl delivered
        · exact Or.inr (List.mem_cons_of_mem _ pending)

theorem scan_delivered [DecidableEq Fact] (clauses : List (Clause Fact)) (s : State Fact) :
    (scan clauses s).delivered = s.delivered := by
  induction clauses generalizing s with
  | nil => rfl
  | cons clause tail ih =>
    rw [scan, ih]
    split
    · simp [publish]; split <;> rfl
    · rfl

theorem scan_monotone [DecidableEq Fact] (clauses : List (Clause Fact)) (s : State Fact) :
    ∀ fact ∈ s.known, fact ∈ (scan clauses s).known := by
  induction clauses generalizing s with
  | nil => exact fun _ member => member
  | cons clause tail ih =>
    intro fact member
    apply ih
    split
    · exact publish_monotone s clause.head fact member
    · exact member

theorem scan_accounting [DecidableEq Fact] (clauses : List (Clause Fact)) (s : State Fact)
    (valid : Accounting s) : Accounting (scan clauses s) := by
  induction clauses generalizing s with
  | nil => exact valid
  | cons clause tail ih =>
    apply ih
    split
    · exact publish_accounting valid clause.head
    · exact valid

theorem scan_sound [DecidableEq Fact] (program clauses : List (Clause Fact))
    (included : ∀ clause ∈ clauses, clause ∈ program) (s : State Fact)
    (accounting : Accounting s) (sound : Sound program s) : Sound program (scan clauses s) := by
  induction clauses generalizing s with
  | nil => exact sound
  | cons clause tail ih =>
    apply ih (fun other member => included other (by simp [member]))
    · split
      · exact publish_accounting accounting clause.head
      · exact accounting
    · split
      next ready =>
        intro fact member
        unfold publish at member
        split at member
        · exact sound fact member
        · rcases List.mem_cons.mp member with rfl | member
          · apply Entails.rule clause (included clause (by simp))
            intro dependency member
            have delivered := List.all_eq_true.mp ready dependency member
            exact sound dependency (accounting.delivered dependency (by simpa using delivered))
          · exact sound fact member
      next absent => exact sound

theorem scan_closes [DecidableEq Fact] (clauses : List (Clause Fact)) (s : State Fact) :
    ∀ clause ∈ clauses, (∀ fact ∈ clause.dependencies, fact ∈ s.delivered) →
      clause.head ∈ (scan clauses s).known := by
  induction clauses generalizing s with
  | nil => simp
  | cons first tail ih =>
    intro clause member ready
    rcases List.mem_cons.mp member with rfl | member
    · have test : enabled s clause = true := by simpa [enabled, List.all_eq_true] using ready
      simp only [scan, test, ↓reduceIte]
      apply scan_monotone
      simp [publish]; split <;> simp_all
    · apply ih _ clause member
      split <;> simpa only [publish_delivered] using ready

/-- Removing one pending fact and registering its delivery preserves exact
publication accounting before dependency notifications start. -/
def deliveredState (s : State Fact) (fact : Fact) (tail : List Fact) : State Fact :=
  {s with delivered := fact :: s.delivered, pending := tail}

theorem delivery_accounting {s : State Fact} (valid : Accounting s) {fact : Fact} {tail : List Fact}
    (queue : s.pending = fact :: tail) : Accounting (deliveredState s fact tail) := by
  constructor
  · intro other member
    rcases List.mem_cons.mp member with rfl | member
    · exact valid.pending other (by simp [queue])
    · exact valid.delivered other member
  · intro other member
    change other ∈ tail at member
    exact valid.pending other (by simp [queue, member])
  · intro other member
    rcases valid.conserved other member with delivered | pending
    · exact Or.inl (List.mem_cons_of_mem _ delivered)
    · rw [queue] at pending
      rcases List.mem_cons.mp pending with rfl | pending
      · exact Or.inl (by simp [deliveredState])
      · exact Or.inr pending

def notify [DecidableEq Fact] (program : List (Clause Fact)) (fact : Fact) (s : State Fact) : State Fact :=
  scan (program.filter fun clause => clause.dependencies.contains fact) s

theorem notification_valid [DecidableEq Fact] (program : List (Clause Fact)) (s : State Fact)
    (valid : Valid program s) (fact : Fact) (tail : List Fact) (queue : s.pending = fact :: tail) :
    Valid program (notify program fact (deliveredState s fact tail)) := by
  let before := deliveredState s fact tail
  let affected := program.filter fun clause => clause.dependencies.contains fact
  have accounting := delivery_accounting valid.accounting queue
  constructor
  · exact scan_accounting affected before accounting
  · exact scan_sound program affected (fun clause member => (List.mem_filter.mp member).1)
      before accounting valid.sound
  · intro clause member ready
    change ∀ dependency ∈ clause.dependencies, dependency ∈ (scan affected before).delivered at ready
    rw [scan_delivered] at ready
    by_cases dependent : fact ∈ clause.dependencies
    · apply scan_closes affected before clause
      · exact List.mem_filter.mpr ⟨member, by simpa using dependent⟩
      · exact ready
    · apply scan_monotone affected before
      apply valid.closed clause member
      intro dependency belongs
      have delivered := ready dependency belongs
      rcases List.mem_cons.mp delivered with equal | earlier
      · subst dependency; exact False.elim (dependent belongs)
      · exact earlier

def initial [DecidableEq Fact] (program : List (Clause Fact)) : State Fact :=
  scan program ⟨[], [], []⟩

theorem initial_valid [DecidableEq Fact] (program : List (Clause Fact)) : Valid program (initial program) := by
  have accounting : Accounting (⟨[], [], []⟩ : State Fact) := by constructor <;> simp
  constructor
  · exact scan_accounting program _ accounting
  · exact scan_sound program program (fun _ member => member) _ accounting (by simp [Sound])
  · intro clause member ready
    change ∀ dependency ∈ clause.dependencies, dependency ∈ (scan program (⟨[], [], []⟩ : State Fact)).delivered at ready
    rw [scan_delivered] at ready
    exact scan_closes program _ clause member ready

inductive Trace [DecidableEq Fact] (program : List (Clause Fact)) : State Fact → State Fact → Prop where
  | refl (s : State Fact) : Trace program s s
  | next {a b fact tail} (history : Trace program a b) (queue : b.pending = fact :: tail) :
      Trace program a (notify program fact (deliveredState b fact tail))

theorem trace_valid [DecidableEq Fact] {program : List (Clause Fact)} {before after : State Fact}
    (valid : Valid program before) (history : Trace program before after) : Valid program after := by
  induction history with
  | refl => exact valid
  | next history queue ih => exact notification_valid program _ ih _ _ queue

/-- Universal correctness at a genuinely quiescent return. No final closure,
soundness, fact equality or successful certificate is assumed by the caller. -/
theorem quiescent_exact [DecidableEq Fact] (program : List (Clause Fact)) (last : State Fact)
    (history : Trace program (initial program) last) (finished : last.pending = []) :
    ∀ fact, fact ∈ last.known ↔ Entails program fact := by
  have valid := trace_valid (initial_valid program) history
  intro fact
  constructor
  · exact valid.sound fact
  · intro derivation
    induction derivation with
    | rule clause member body ih =>
      apply valid.closed clause member
      intro dependency belongs
      rcases valid.accounting.conserved dependency (ih dependency belongs) with delivered | pending
      · exact delivered
      · simp [finished] at pending

end ParserEngine.Worklist
