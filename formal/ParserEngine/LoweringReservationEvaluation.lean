import ParserEngine.LoweringRootScope

/-! The reservation walk executes the real recursive Scheme body. Its trace
allows arbitrary depth, repeated names, and nonempty initial heaps. Reservation
is occupancy for collision avoidance; it does not grant declaration authority. -/
namespace ParserEngine.LoweringReservationEvaluation
open WalkSource SchemeTableEvaluation

def body : Datum := LoweringBranchSource.reserveReferences
  |> LoweringClaimEvaluation.procedure
  |> fun entry => ((entry.getD ("", ⟨[], []⟩)).2.body.headD (.list []))

def prelude : List String := ["pair?", "eq?", "car", "cadr", "table-set!", "for-each"]

structure Scope where
  group : List (String × Procedure)
  captured : Environment
  address : Nat
  definition : group.lookup "reserve-references" = some ⟨[s "expression"], [body]⟩
  table : tie group captured "used-names" = some (.table address)
  globals : ∀ name ∈ prelude, tie group captured name = none

def environment (scope : Scope) (value : Value) :=
  extend (tie scope.group scope.captured) "expression" value

mutual
inductive Trace (address : Nat) : Value → Heap → Heap → Prop where
  | atom (data : ∀ address, value ≠ .cell address) (nonpair : primitive "pair?" [value] before = some (.boolean false, before)) :
      Trace address value before before
  | ordinary (different : primitive "eq?" [head, .symbol "reference"] before = some (.boolean false, before))
      (children : Traces address (head :: tail) before after) :
      Trace address (.list (head :: tail)) before after
  | reference (children : Traces address (.symbol "reference" :: .symbol name :: tail)
        (writeSymbol before address name (.boolean true)) after) :
      Trace address (.list (.symbol "reference" :: .symbol name :: tail)) before after
inductive Traces (address : Nat) : List Value → Heap → Heap → Prop where
  | nil : Traces address [] heap heap
  | cons (head : Trace address value before middle) (tail : Traces address values middle after) :
      Traces address (value :: values) before after
end

private theorem read (scope : Scope) :
    Eval (environment scope (.list values)) heap (s "expression") (.list values) heap :=
  .variable (by simp [environment, resolve, extend])

private theorem absent (scope : Scope) (member : operation ∈ prelude) :
    environment scope value operation = none := by
  have different : operation ≠ "expression" := by
    intro equal; subst operation; simp [prelude] at member
  simpa [environment, extend, different] using scope.globals operation member

private theorem each_call (scope : Scope)
    (children : Each (tie scope.group scope.captured) "expression" [body] before values after) :
    Eval (environment scope (.list values)) before
      (app "for-each" [s "reserve-references", s "expression"]) .unit after := by
  apply Eval.forEachRecursive (read scope) ?_ scope.definition children (absent scope (by simp [prelude]))
  simp only [environment, extend, resolve]
  simp [tie, scope.definition]

private theorem atom_eval (scope : Scope)
    (data : ∀ address, value ≠ .cell address)
    (nonpair : primitive "pair?" [value] before = some (.boolean false, before)) :
    Eval (environment scope value) before body .unit before := by
  have argument : Eval (environment scope value) before (s "expression") value before := by
    apply Eval.variable
    cases value <;> simp_all [environment, resolve, extend]
  exact .whenFalse (.apply (.cons argument .nil) nonpair (absent scope (by simp [prelude]))) rfl

private theorem ordinary_eval (scope : Scope)
    (different : primitive "eq?" [head, .symbol "reference"] before = some (.boolean false, before))
    (children : Each (tie scope.group scope.captured) "expression" [body] before (head :: tail) after) :
    Eval (environment scope (.list (head :: tail))) before body .unit after := by
  have pairTest := Eval.apply (.cons (read (heap := before) (values := head :: tail) scope) .nil)
    (by rfl) (absent scope (by simp [prelude] : "pair?" ∈ prelude))
  have carRead := Eval.apply (.cons (read (heap := before) (values := head :: tail) scope) .nil)
    (by rfl) (absent scope (by simp [prelude] : "car" ∈ prelude))
  have equality := Eval.apply (.cons carRead (.cons (.quoteSymbol (name := "reference")) .nil))
    different (absent scope (by simp [prelude] : "eq?" ∈ prelude))
  exact .whenTrue pairTest rfl (.cons (.whenFalse equality rfl) (.single (each_call scope children)))

private theorem reference_eval (scope : Scope)
    (children : Each (tie scope.group scope.captured) "expression" [body]
      (writeSymbol before scope.address name (.boolean true)) (.symbol "reference" :: .symbol name :: tail) after) :
    Eval (environment scope (.list (.symbol "reference" :: .symbol name :: tail))) before body .unit after := by
  have expression := read (heap := before) (values := .symbol "reference" :: .symbol name :: tail) scope
  have pairTest := Eval.apply (.cons expression .nil) (by rfl)
    (absent scope (by simp [prelude] : "pair?" ∈ prelude))
  have carRead := Eval.apply (.cons expression .nil) (by rfl)
    (absent scope (by simp [prelude] : "car" ∈ prelude))
  have equality : Eval (environment scope (.list (.symbol "reference" :: .symbol name :: tail))) before
      (app "eq?" [app "car" [s "expression"], app "quote" [s "reference"]]) (.boolean true) before :=
    Eval.apply (.cons carRead (.cons (.quoteSymbol (name := "reference")) .nil))
    rfl (absent scope (by simp [prelude] : "eq?" ∈ prelude))
  have nameRead := Eval.apply (.cons expression .nil) (by rfl)
    (absent scope (by simp [prelude] : "cadr" ∈ prelude))
  have tableRead : Eval (environment scope (.list (.symbol "reference" :: .symbol name :: tail))) before
      (s "used-names") (.table scope.address) before :=
    .variable (by simp [environment, resolve, extend, scope.table])
  have mark := Eval.apply (.cons tableRead (.cons nameRead (.cons (.boolean (value := true)) .nil))) rfl
    (absent scope (by simp [prelude] : "table-set!" ∈ prelude))
  exact .whenTrue pairTest rfl (.cons (.whenTrue equality rfl (.single mark)) (.single (each_call scope children)))

/-- The actual extracted root supplies the table identity and recursive group.
Only unshadowed host primitives remain external to this parser-owned scope. -/
def rootScope (env : Environment) (base : Nat)
    (globals : ∀ name ∈ prelude, env name = none) : Scope where
  group := LoweringRootScope.group
  captured := LoweringRootStorage.environment env base
  address := base + 2
  definition := by rfl
  table := by
    have absent : LoweringRootScope.group.lookup "used-names" = none := rfl
    simp [tie, absent, LoweringRootStorage.environment, extend]
  globals := by
    intro name member
    have absent : LoweringRootScope.group.lookup name = none := by
      simp only [prelude, List.mem_cons, List.not_mem_nil, or_false] at member
      rcases member with rfl | rfl | rfl | rfl | rfl | rfl <;> rfl
    have unbound := globals name member
    simp only [prelude, List.mem_cons, List.not_mem_nil, or_false] at member
    rcases member with rfl | rfl | rfl | rfl | rfl | rfl <;>
      simpa [tie, absent, LoweringRootStorage.environment, extend] using unbound

/-- Every finite recursive reservation trace, not only a fixture corpus,
has an evaluation of the extracted source body in its complete lexical group. -/
theorem source_execution (scope : Scope) (trace : Trace scope.address value before after) :
    Eval (environment scope value) before body .unit after := by
  induction trace using Trace.rec
      (motive_2 := fun values before after _ =>
        Each (tie scope.group scope.captured) "expression" [body] before values after) with
  | atom data nonpair => exact atom_eval scope data nonpair
  | ordinary different _ children => exact ordinary_eval scope different children
  | reference _ children => exact reference_eval scope children
  | nil => exact .nil
  | cons _ _ head tail => exact .cons (.single head) tail

/-- Any returning execution of this source walk has the same heap result. -/
theorem every_return (scope : Scope) (trace : Trace scope.address value before after)
    (execution : Eval (environment scope value) before body result last) : result = .unit ∧ last = after :=
  execution.deterministic (source_execution scope trace)

/-- Reservation cannot mutate the declaration table, counters, or emitter
storage when those are distinct from the collision-avoidance table. -/
theorem frame (trace : Trace address value before after) (different : other ≠ address) :
    after other key = before other key := by
  induction trace using Trace.rec
      (motive_2 := fun _ before after _ => after other key = before other key) with
  | atom _ _ => rfl
  | ordinary _ _ ih => exact ih
  | reference _ ih => simpa [writeSymbol, different] using ih
  | nil => rfl
  | cons _ _ first last => exact last.trans first

end ParserEngine.LoweringReservationEvaluation
