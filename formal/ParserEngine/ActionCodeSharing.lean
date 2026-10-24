import ParserEngine.Components

/-! Code sharing preserves ordered, effectful predicate execution. Handlers
remain arbitrary; this is a code-generation law, not a host compiler theorem. -/
namespace ParserEngine.ActionCodeSharing

def choose (test : K → S → Bool × S) : List (K × A) → S → Option A × S
  | [], state => (none, state)
  | (key, action) :: rest, state =>
    let observed := test key state
    if observed.1 then (some action, observed.2) else choose test rest observed.2

theorem choose_map (test : K → S → Bool × S) (encode : A → B)
    (rows : List (K × A)) (state : S) :
    choose test (rows.map fun row => (row.1, encode row.2)) state =
      ((choose test rows state).1.map encode, (choose test rows state).2) := by
  induction rows generalizing state with
  | nil => rfl
  | cons row rows ih =>
    simp only [List.map_cons, choose]
    split <;> simp_all

theorem chosen_member (test : K → S → Bool × S) (rows : List (K × A))
    (state : S) (selected : (choose test rows state).1 = some action) :
    action ∈ rows.map Prod.snd := by
  induction rows generalizing state with
  | nil => simp [choose] at selected
  | cons row rows ih =>
    simp only [choose] at selected
    split at selected
    · simp_all
    · exact List.mem_cons_of_mem _ (ih _ selected)

def catalog [BEq A] (rows : List (K × A)) : List A := (rows.map Prod.snd).eraseDups

theorem decode_encode [BEq A] [LawfulBEq A] (rows : List (K × A))
    (member : action ∈ rows.map Prod.snd) :
    (catalog rows)[(catalog rows).idxOf action]? = some action := by
  have present : action ∈ catalog rows := by simpa [catalog] using member
  have bound : (catalog rows).idxOf action < (catalog rows).length :=
    List.idxOf_lt_length_iff.mpr present
  rw [List.getElem?_eq_getElem bound, List.getElem_idxOf bound]

def execute (handle : A → S → R) (fallback : S → R) (result : Option A × S) : R :=
  match result.1 with
  | none => fallback result.2
  | some action => handle action result.2

/-- Predicate effects and order, first-match priority, fallback and the complete
handler result are retained for arbitrary rows, states and handler effects. -/
theorem execution_exact [BEq A] [LawfulBEq A] (test : K → S → Bool × S)
    (rows : List (K × A)) (state : S) (handle : A → S → R) (fallback : S → R) :
    let library := catalog rows
    let selected := choose test (rows.map fun row => (row.1, library.idxOf row.2)) state
    execute handle fallback (selected.1.bind (library[·]?), selected.2) =
      execute handle fallback (choose test rows state) := by
  dsimp
  rw [choose_map test (fun action => (catalog rows).idxOf action) rows state]
  cases selected : (choose test rows state).1 with
  | none => simp [execute, selected]
  | some action =>
    simp [execute, selected, decode_encode rows (chosen_member test rows state selected)]

end ParserEngine.ActionCodeSharing
