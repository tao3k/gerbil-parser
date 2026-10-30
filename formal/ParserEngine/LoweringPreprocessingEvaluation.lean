import ParserEngine.LoweringReservationEvaluation

/-! Execute the real root reservation statements and the complete authored-rule
loop. Heap transitions retain owner writes, mutable occurrence counts, recursive
reference walks and source order. Procedure declarations use the same extracted
root lexical group; evaluating that declaration group remains a separate edge. -/
namespace ParserEngine.LoweringPreprocessingEvaluation
open WalkSource SchemeTableEvaluation
namespace Reservation
export LoweringReservationEvaluation (Scope prelude Trace frame rootScope source_execution)
end Reservation

structure Rule where
  owner : String
  expression : Value

def Rule.value (row : Rule) : Value := .list [.symbol row.owner, row.expression]

structure Scope (rules : List Rule) (root : String) where
  reservation : Reservation.Scope
  counts : Nat
  separate : counts ≠ reservation.address
  countBinding : tie reservation.group reservation.captured "rule-counts" = some (.table counts)
  ruleBinding : tie reservation.group reservation.captured "rules" = some (.list (rules.map Rule.value))
  rootBinding : tie reservation.group reservation.captured "root" = some (.symbol root)
  lookup : tie reservation.group reservation.captured "table-ref" = none
  add : tie reservation.group reservation.captured "+" = none

def environment (scope : Scope rules root) := tie scope.reservation.group scope.reservation.captured

def rowEnvironment (scope : Scope rules root) (row : Rule) := extend (environment scope) "row" row.value

def markOwner : Datum := app "table-set!" [s "used-names", app "car" [s "row"], b true]
def countOwner : Datum := app "table-set!" [s "rule-counts", app "car" [s "row"],
  app "+" [n 1, app "table-ref" [s "rule-counts", app "car" [s "row"], n 0]]]
def reserveExpression : Datum := app "reserve-references" [app "cadr" [s "row"]]
def rowBody : List Datum := [markOwner, countOwner, reserveExpression]

def marked (scope : Scope rules root) (heap : Heap) (row : Rule) : Heap :=
  writeSymbol heap scope.reservation.address row.owner (.boolean true)

def counted (scope : Scope rules root) (heap : Heap) (row : Rule) (count : Nat) : Heap :=
  writeSymbol (marked scope heap row) scope.counts row.owner (.natural (1 + count))

inductive Trace (scope : Scope rules root) : List Rule → Heap → Heap → Prop where
  | nil : Trace scope [] heap heap
  | cons (countRead : (before scope.counts (.symbol row.owner)).getD (.natural 0) = .natural count)
      (references : Reservation.Trace scope.reservation.address row.expression (counted scope before row count) middle)
      (tail : Trace scope rows middle after) : Trace scope (row :: rows) before after

private theorem absent (scope : Scope rules root) (row : Rule) (operation : String)
    (name : operation ≠ "row") (unbound : environment scope operation = none) :
    rowEnvironment scope row operation = none := by simpa [rowEnvironment, extend, name] using unbound

private theorem global (scope : Scope rules root) (row : Rule) (member : operation ∈ Reservation.prelude) :
    rowEnvironment scope row operation = none := by
  have name : operation ≠ "row" := by intro same; subst operation; simp [Reservation.prelude] at member
  exact absent scope row operation name (scope.reservation.globals operation member)

private theorem rowRead (scope : Scope rules root) (row : Rule) :
    Eval (rowEnvironment scope row) heap (s "row") row.value heap :=
  .variable (by simp [rowEnvironment, Rule.value, resolve, extend])

private theorem ownerRead (scope : Scope rules root) (row : Rule) :
    Eval (rowEnvironment scope row) heap (app "car" [s "row"]) (.symbol row.owner) heap :=
  .apply (.cons (rowRead scope row) .nil) rfl (global scope row (by simp [Reservation.prelude]))

private theorem tableRead (scope : Scope rules root) (row : Rule)
    (binding : environment scope name = some (.table address)) (different : name ≠ "row") :
    Eval (rowEnvironment scope row) heap (s name) (.table address) heap :=
  .variable (by simp [rowEnvironment, resolve, extend, different, binding])

private theorem row_execution (scope : Scope rules root) (row : Rule)
    (countRead : (before scope.counts (.symbol row.owner)).getD (.natural 0) = .natural count)
    (references : Reservation.Trace scope.reservation.address row.expression (counted scope before row count) after) :
    SchemeTableEvaluation.Sequence (rowEnvironment scope row) before rowBody .unit after := by
  have used := tableRead (heap := before) scope row scope.reservation.table (by decide : "used-names" ≠ "row")
  have mark : Eval (rowEnvironment scope row) before markOwner .unit (marked scope before row) :=
    .apply (.cons used (.cons (ownerRead scope row) (.cons (.boolean (value := true)) .nil))) rfl
      (global scope row (by simp [Reservation.prelude]))
  have counts := tableRead (heap := marked scope before row) scope row scope.countBinding (by decide : "rule-counts" ≠ "row")
  have lookup : Eval (rowEnvironment scope row) (marked scope before row)
      (app "table-ref" [s "rule-counts", app "car" [s "row"], n 0]) (.natural count) (marked scope before row) := by
    apply Eval.apply (.cons counts (.cons (ownerRead scope row) (.cons .natural .nil))) ?_
      (absent scope row "table-ref" (by decide) scope.lookup)
    simpa [primitive, marked, writeSymbol, scope.separate] using
      congrArg (fun value => some (value, marked scope before row)) countRead
  have addition : Eval (rowEnvironment scope row) (marked scope before row)
      (app "+" [n 1, app "table-ref" [s "rule-counts", app "car" [s "row"], n 0]])
      (.natural (1 + count)) (marked scope before row) :=
    .apply (.cons .natural (.cons lookup .nil)) rfl (absent scope row "+" (by decide) scope.add)
  have store : Eval (rowEnvironment scope row) (marked scope before row) countOwner .unit (counted scope before row count) :=
    .apply (.cons counts (.cons (ownerRead scope row) (.cons addition .nil))) rfl
      (global scope row (by simp [Reservation.prelude]))
  have argument : Eval (rowEnvironment scope row) (counted scope before row count)
      (app "cadr" [s "row"]) row.expression (counted scope before row count) :=
    .apply (.cons (rowRead scope row) .nil) rfl (global scope row (by simp [Reservation.prelude]))
  have callee : Eval (rowEnvironment scope row) (counted scope before row count) (s "reserve-references")
      (.recursive scope.reservation.group "reserve-references" scope.reservation.captured) (counted scope before row count) :=
    .variable (by simp [rowEnvironment, environment, resolve, extend, tie, scope.reservation.definition])
  have traversal : Eval (rowEnvironment scope row) (counted scope before row count) reserveExpression .unit after :=
    .call rfl callee (.cons argument .nil)
      (.recursive scope.reservation.definition rfl (.single (Reservation.source_execution scope.reservation references)))
  exact .cons mark (.cons store (.single traversal))

private theorem each_execution (scope : Scope rules root) (trace : Trace scope rows before after) :
    Each (environment scope) "row" rowBody before (rows.map Rule.value) after := by
  induction trace with
  | nil => exact .nil
  | cons countRead references tail ih => exact .cons (row_execution scope _ countRead references) ih

/-- Arbitrary duplicate-owner lists execute the actual native for-each body;
counts and reference reservation are not separate caller-supplied projections. -/
theorem source_execution (scope : Scope rules root) (trace : Trace scope rules before after) :
    Eval (environment scope) before LoweringBranchSource.reserveRules .unit after := by
  exact .forEach (.variable (by simp [environment, resolve, scope.ruleBinding])) (each_execution scope trace)
    (scope.reservation.globals "for-each" (by simp [Reservation.prelude]))

def prelude : List String := Reservation.prelude ++ ["table-ref", "+"]

def arguments (env : Environment) (rules : List Rule) (root : String) : Environment :=
  extend (extend env "rules" (.list (rules.map Rule.value))) "root" (.symbol root)

private theorem argument_global (unbound : ∀ name ∈ prelude, env name = none) (member : name ∈ prelude) :
    arguments env rules root name = none := by
  have absent := unbound name member
  have notRules : name ≠ "rules" := by intro same; subst name; simp [prelude, Reservation.prelude] at member
  have notRoot : name ≠ "root" := by intro same; subst name; simp [prelude, Reservation.prelude] at member
  simpa [arguments, extend, notRules, notRoot] using absent

/-- Actual root initialization supplies both table identities and the original
rule/root arguments. No replacement procedure group is supplied by a caller. -/
def rootScope (rules : List Rule) (root : String) (env : Environment) (base : Nat)
    (unbound : ∀ name ∈ prelude, env name = none) : Scope rules root where
  reservation := Reservation.rootScope (arguments env rules root) base
    (fun name member => argument_global unbound (by simp only [prelude, List.mem_append]; exact Or.inl member))
  counts := base + 3
  separate := by dsimp [Reservation.rootScope]; omega
  countBinding := by
    have absent : LoweringRootScope.group.lookup "rule-counts" = none := rfl
    simp [Reservation.rootScope, tie, absent, LoweringRootStorage.environment, extend]
  ruleBinding := by
    have absent : LoweringRootScope.group.lookup "rules" = none := rfl
    simp [Reservation.rootScope, tie, absent, LoweringRootStorage.environment, arguments, extend]
  rootBinding := by
    have absent : LoweringRootScope.group.lookup "root" = none := rfl
    simp [Reservation.rootScope, tie, absent, LoweringRootStorage.environment, arguments, extend]
  lookup := by
    have absent : LoweringRootScope.group.lookup "table-ref" = none := rfl
    have inherited := argument_global (rules := rules) (root := root) unbound
      (by simp [prelude, Reservation.prelude] : "table-ref" ∈ prelude)
    simpa [Reservation.rootScope, tie, absent, LoweringRootStorage.environment, extend] using inherited
  add := by
    have absent : LoweringRootScope.group.lookup "+" = none := rfl
    have inherited := argument_global (rules := rules) (root := root) unbound
      (by simp [prelude, Reservation.prelude] : "+" ∈ prelude)
    simpa [Reservation.rootScope, tie, absent, LoweringRootStorage.environment, extend] using inherited

def reserveRoot : Datum := app "table-set!" [s "used-names", s "root", b true]

/-- Root reservation precedes every authored declaration and recursive walk. -/
theorem prefix_execution (scope : Scope rules root)
    (trace : Trace scope rules (writeSymbol before scope.reservation.address root (.boolean true)) after) :
    SchemeTableEvaluation.Sequence (environment scope) before [reserveRoot, LoweringBranchSource.reserveRules] .unit after := by
  have used : Eval (environment scope) before (s "used-names") (.table scope.reservation.address) before :=
    .variable (by simp [environment, resolve, scope.reservation.table])
  have rootRead : Eval (environment scope) before (s "root") (.symbol root) before :=
    .variable (by simp [environment, resolve, scope.rootBinding])
  have mark : Eval (environment scope) before reserveRoot .unit
      (writeSymbol before scope.reservation.address root (.boolean true)) :=
    .apply (.cons used (.cons rootRead (.cons (.boolean (value := true)) .nil))) rfl
      (scope.reservation.globals "table-set!" (by simp [Reservation.prelude]))
  exact .cons mark (.single (source_execution scope trace))

/-- The joint loop preserves emitter storage and the fresh-name counter. -/
theorem frame (trace : Trace scope rows before after)
    (unused : address ≠ scope.reservation.address) (uncounted : address ≠ scope.counts) :
    after address key = before address key := by
  induction trace with
  | nil => rfl
  | cons countRead references tail ih =>
    have child := Reservation.frame (key := key) references unused
    simpa [counted, marked, writeSymbol, unused, uncounted] using ih.trans child

end ParserEngine.LoweringPreprocessingEvaluation
