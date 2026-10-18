import ParserEngine.SchemeTableEvaluation

/-! Allocation semantics for the source evaluateInitializerr fragment. The frontier is
logical allocation ownership, not a physical GC address or capacity bound.
Every binding is evaluateInitializerd from syntax before its location becomes visible. -/
namespace ParserEngine.SchemeStorage
open WalkSource SchemeTableEvaluation

structure Store where
  heap : Heap
  frontier : Nat

def Fresh (store : Store) : Prop :=
  ∀ address, store.frontier ≤ address → ∀ key, store.heap address key = none

def cell (store : Store) (value : Value) : Store :=
  ⟨write store.heap store.frontier 0 value, store.frontier + 1⟩

def table (store : Store) : Store := ⟨store.heap, store.frontier + 1⟩

theorem cell_fresh (fresh : Fresh store) : Fresh (cell store value) := by
  intro address beyond key
  have different : address ≠ store.frontier := by dsimp [cell] at beyond; omega
  simpa [cell, write, different] using fresh address (by dsimp [cell] at beyond; omega) key

theorem table_fresh (fresh : Fresh store) : Fresh (table store) := by
  intro address beyond key
  exact fresh address (by dsimp [table] at beyond; omega) key

theorem cell_frame (allocated : address < store.frontier) :
    (cell store value).heap address key = store.heap address key := by
  have different : address ≠ store.frontier := by omega
  simp [cell, write, different]

/-- No source-schema dispatch: these are the actual evaluateInitializerr forms. The
empty table consumes a fresh identity even though all of its entries are absent. -/
def evaluateInitializer (expression : Datum) (store : Store) : Option (Value × Store) :=
  match expression with
  | .integer value => some (.natural value, store)
  | .boolean value => some (.boolean value, store)
  | .text value => some (.text value, store)
  | .list [.symbol "quote", value] => (literal value).map (fun value => (value, store))
  | .list [.symbol "make-table", .keyword "test", .symbol "eq?"] =>
      some (.table store.frontier, table store)
  | _ => none

def putBinding (mutable : Bool) (name : String) (value : Value) (env : Environment) (store : Store) :
    Environment × Store :=
  if mutable then (extend env name (.cell store.frontier), cell store value)
  else (extend env name value, store)

def readBinding : Datum → Option (String × Datum)
  | .list [.symbol name, expression] => some (name, expression)
  | _ => none

/-- Parallel-let initializers in this supported fragment are closed. Earlier
bindings cannot accidentally supply a value to a later initializer. -/
def bindings (mutable : String → Bool) : List Datum → Environment → Store → Option (Environment × Store)
  | [], env, store => some (env, store)
  | binding :: rest, env, store => do
      let (name, expression) ← readBinding binding
      let (value, prepared) ← evaluateInitializer expression store
      let (entered, next) := putBinding (mutable name) name value env prepared
      bindings mutable rest entered next

theorem initializer_fresh (fresh : Fresh before) (ran : evaluateInitializer expression before = some (value, after)) :
    Fresh after := by
  unfold evaluateInitializer at ran
  split at ran <;> (try simp only [Option.map_eq_some_iff, Option.some.injEq, Prod.mk.injEq] at ran)
  all_goals grind [table_fresh]

theorem bindings_fresh (fresh : Fresh before)
    (ran : bindings mutable source env before = some (entered, after)) : Fresh after := by
  induction source generalizing env before with
  | nil => simp only [bindings, Option.some.injEq, Prod.mk.injEq] at ran; rcases ran with ⟨rfl, rfl⟩; exact fresh
  | cons head rest ih =>
    simp only [bindings] at ran
    obtain ⟨binding, _, body⟩ := Option.bind_eq_some_iff.mp ran
    obtain ⟨pair, initialized, rest⟩ := Option.bind_eq_some_iff.mp body
    have valid := initializer_fresh fresh initialized
    unfold putBinding at rest
    split at rest
    · exact ih (cell_fresh valid) rest
    · exact ih valid rest

end ParserEngine.SchemeStorage
