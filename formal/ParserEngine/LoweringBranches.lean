import ParserEngine.LoweringPublication

/-! Origin-preserving recursive branch contexts. The traversal builds witnesses
with these constructors; physical-name realization is checked separately. -/
namespace ParserEngine.LoweringBranches

open Lowering Names

structure Witness (rules : Rules) (before after : List Lowering.Symbol) where
  recipe : Recipe
  produced : Produced rules recipe before after

structure Context (rules : Rules) (expression : Expr) where
  owner : Owner
  wrap : Recipe → Recipe
  sound : ∀ {recipe output}, Produced rules recipe [body expression] output →
    Produced rules (wrap recipe) [.nonterminal owner] output

def identityContext (rules : Rules) (expression : Expr) : Context rules expression :=
  ⟨.expression expression, id, fun h => h⟩

def rebase {rules first second} (equal : first = second) (context : Context rules first) :
    Context rules second where
  owner := context.owner
  wrap := context.wrap
  sound h := context.sound (by simpa only [equal] using h)

def authoredContext {rules name expression} (member : (name, expression) ∈ rules) :
    Context rules expression where
  owner := .named name
  wrap recipe := .expand (.named name) recipe (.identity [])
  sound h := by
    have row : [body expression] ∈ schemaRows rules (.named name) := by
      simp only [schemaRows, List.mem_filterMap]
      exact ⟨(name, expression), member, by simp⟩
    simpa [body] using Produced.expand row h (Produced.identity [])

def namedContext {rules name expression} (unique : UniqueBody rules name expression) :
    Context rules expression := authoredContext unique.1

def descend {rules parent child} (context : Context rules parent)
    (row : [body child] ∈ schemaRows rules (.expression parent)) : Context rules child where
  owner := context.owner
  wrap recipe := context.wrap (.expand (.expression parent) recipe (.identity []))
  sound h := context.sound (by simpa [body] using Produced.expand row h (Produced.identity []))

structure Emission (rules : Rules) where
  owner : Owner
  rhs : List Lowering.Symbol
  recipe : Recipe
  produced : Produced rules recipe [.nonterminal owner] rhs

def emit {rules expression} (context : Context rules expression)
    (witness : Witness rules [body expression] rhs) : Emission rules :=
  ⟨context.owner, rhs, context.wrap witness.recipe, context.sound witness.produced⟩

def step {rules expression rhs output}
    (row : rhs ∈ schemaRows rules (.expression expression))
    (children : Witness rules rhs output) : Witness rules [body expression] output :=
  ⟨.expand (.expression expression) children.recipe (.identity []),
    by simpa [body] using Produced.expand row children.produced (Produced.identity [])⟩

def identity (rules : Rules) (symbols : List Lowering.Symbol) : Witness rules symbols symbols :=
  ⟨.identity symbols, .identity symbols⟩

def append {rules a b c d} (first : Witness rules a b) (last : Witness rules c d) :
    Witness rules (a ++ c) (b ++ d) :=
  ⟨.append first.recipe last.recipe, .append first.produced last.produced⟩

structure Operand (rules : Rules) (expression : Expr) where
  helpers : List (Emission rules)
  symbol : Lowering.Symbol
  witness : Witness rules [body expression] [symbol]

theorem every_emission_checked {rules} (row : Emission rules) :
    ∃ checked, verify rules row.recipe = some checked ∧
      checked.val = ([.nonterminal row.owner], row.rhs) :=
  produced_recipe_checked row.produced

/-- Per-occurrence realization permits different fresh names to decode to the
same expression. It does not require one global physical name per owner. -/
def Realizes {rules} (decode : Nat → Owner) (row : Emission rules)
    (native : Grammar.Production) : Prop :=
  decode native.lhs = row.owner ∧ native.rhs.map (nativeSymbol decode) = row.rhs

theorem realized_checked {rules decode} (row : Emission rules) (native : Grammar.Production)
    (aligned : Realizes decode row native) : admitOrigin rules decode native row.recipe = true := by
  obtain ⟨checked, equal, endpoints⟩ := every_emission_checked row
  simp only [admitOrigin, equal, endpoints, aligned.1, aligned.2]
  rfl

/-- No zip truncation: the physical list and generated recursive emission list
must align completely, including duplicate productions and helper occurrences. -/
inductive RealizedRows {rules} (decode : Nat → Owner) :
    List (Emission rules) → Grammar.CFG → Prop where
  | nil : RealizedRows decode [] []
  | cons {row native rows rest} (first : Realizes decode row native)
      (later : RealizedRows decode rows rest) : RealizedRows decode (row :: rows) (native :: rest)

theorem realized_native {rules decode} {rows : List (Emission rules)} {native : Grammar.CFG}
    (aligned : RealizedRows decode rows native) :
    admitNative rules decode native (rows.map Emission.recipe) = true := by
  induction aligned with
  | nil => rfl
  | @cons row native rows rest first later ih =>
      simp only [List.map_cons, admitNative, realized_checked row native first, ih, Bool.true_and]

end ParserEngine.LoweringBranches
