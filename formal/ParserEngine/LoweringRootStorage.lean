import ParserEngine.SchemeStorage
import ParserEngine.LoweringBranchSource

/-! The actual root let generates its owned counter cells and table identities.
Mutation analysis is conservative across nested procedures and ignores quoted
data. No source-schema check substitutes for executing the initializer forms. -/
namespace ParserEngine.LoweringRootStorage
open WalkSource SchemeTableEvaluation SchemeStorage

mutual
  def assigned : Datum → List String
    | .list [.symbol "quote", _] => []
    | .list [.symbol "set!", .symbol name, value] => name :: assigned value
    | .list values => assignedList values
    | _ => []
  def assignedList : List Datum → List String
    | [] => []
    | first :: rest => assigned first ++ assignedList rest
end

def rootParts : Datum → Option (List Datum × List Datum)
  | .list [.symbol "def", .list _, .list (.symbol "let" :: .list initializers :: body)] =>
      some (initializers, body)
  | _ => none

def enter (source : Datum) (env : Environment) (before : Store) : Option (Environment × Store) := do
  let (initializers, body) ← rootParts source
  bindings (fun name => (assignedList body).contains name) initializers env before

def environment (env : Environment) (base : Nat) : Environment :=
  extend (extend (extend (extend (extend env
    "next-id" (.cell base)) "next-synthetic" (.cell (base + 1)))
    "used-names" (.table (base + 2))) "rule-counts" (.table (base + 3)))
    "productions" (.cell (base + 4))

def heap (before : Heap) (base : Nat) : Heap :=
  write (write (write before base 0 (.natural 1)) (base + 1) 0 (.natural 0))
    (base + 4) 0 (.list [])

set_option maxRecDepth 8192 in
theorem source_entry : enter LoweringBranchSource.rootSource env before =
    some (environment env before.frontier, ⟨heap before.heap before.frontier, before.frontier + 5⟩) := by
  rfl

theorem root_fresh (fresh : Fresh before) : Fresh (⟨heap before.heap before.frontier, before.frontier + 5⟩ : Store) := by
  have entered := source_entry (env := fun _ => none) (before := before)
  unfold enter at entered
  obtain ⟨parts, _, initialized⟩ := Option.bind_eq_some_iff.mp entered
  exact bindings_fresh fresh initialized

theorem roots (fresh : Fresh before) :
    let env' := environment env before.frontier
    let heap' := heap before.heap before.frontier
    resolve env' heap' "next-id" = some (.natural 1) ∧
    resolve env' heap' "next-synthetic" = some (.natural 0) ∧
    resolve env' heap' "used-names" = some (.table (before.frontier + 2)) ∧
    resolve env' heap' "rule-counts" = some (.table (before.frontier + 3)) ∧
    resolve env' heap' "productions" = some (.list []) ∧
    (∀ key, heap' (before.frontier + 2) key = none) ∧
    (∀ key, heap' (before.frontier + 3) key = none) := by
  dsimp only
  simp only [environment, extend, resolve]
  simp only [heap, write]
  have used := fresh (before.frontier + 2) (by omega)
  have counts := fresh (before.frontier + 3) (by omega)
  simp_all

theorem root_frame (before : Store) (owned : address < before.frontier) :
    heap before.heap before.frontier address key = before.heap address key := by
  have first : address ≠ before.frontier := by omega
  have second : address ≠ before.frontier + 1 := by omega
  have last : address ≠ before.frontier + 4 := by omega
  simp [heap, write, first, second, last]

end ParserEngine.LoweringRootStorage
