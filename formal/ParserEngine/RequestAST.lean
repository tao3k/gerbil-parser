import ParserEngine.SemanticValues

/-! One ID-bearing request trace drives both certified recognition and AST
semantics. Native trace extraction and source-coordinate decoding remain inputs. -/
namespace ParserEngine.RequestAST

structure Driver (Raw : Type) where
  engine : RawRequest.Engine Raw
  rules : List SemanticValues.Rule
  leaf : Raw → Tree
  inputEnd : Nat
  choose : Nat → Option Raw → Option (Option Nat × TableBridge.RuntimeAction)
  decoded : ∀ state input, engine.select state input =
    (choose state input).bind (TableBridge.decodeEntry (SemanticValues.grammarView rules))

/-- Nullable marked values anchor at the next nonempty semantic value, then
at the invocation boundary. Nonempty values retain their own tree bounds. -/
def followingStart : List Children → Nat → Nat
  | [], offset => offset
  | [] :: rest, offset => followingStart rest offset
  | ((_, tree) :: _) :: _, _ => tree.span.start

def operands : List (List Action) → List Children → Nat → List ParserEngine.Operand
  | [], _, _ => []
  | _, [], _ => []
  | actions :: rest, value :: values, offset =>
    ⟨actions, value, if value.isEmpty then followingStart values offset else offset⟩ ::
      operands rest values offset

def evaluate (rule : SemanticValues.Rule) (values : List Children) (offset : Nat) : Children :=
  reduceSource (operands rule.actions values offset)

inductive Instruction (Raw : Type) where
  | shift (raw : Raw)
  | reduce (id offset : Nat)

def execute (rules : List SemanticValues.Rule) (leaf : Raw → Tree)
    (instruction : Instruction Raw) (values : List Children) : Option (List Children) :=
  match instruction with
  | .shift raw => some ([(none, leaf raw)] :: values)
  | .reduce id offset => (rules[id]?).bind fun rule =>
    if rule.production.rhs.length ≤ values.length then
      some (evaluate rule (values.take rule.production.rhs.length).reverse offset ::
        values.drop rule.production.rhs.length)
    else none

def run (rules : List SemanticValues.Rule) (leaf : Raw → Tree)
    (instructions : List (Instruction Raw)) (values : List Children) : Option (List Children) :=
  match instructions with
  | [] => some values
  | first :: rest => (execute rules leaf first values).bind (run rules leaf rest)

theorem run_append (rules : List SemanticValues.Rule) (leaf : Raw → Tree)
    (first rest : List (Instruction Raw)) (values : List Children) :
    run rules leaf (first ++ rest) values = (run rules leaf first values).bind (run rules leaf rest) := by
  induction first generalizing values with
  | nil => rfl
  | cons instruction first ih =>
    simp only [List.cons_append, run]
    cases executed : execute rules leaf instruction values with
    | none => simp
    | some next => simpa using ih next

structure State (Raw : Type) where
  raw : RawRequest.Config Raw
  values : List Children

def initial (driver : Driver Raw) (input : List Raw) : State Raw :=
  ⟨RawRequest.initial driver.engine input, []⟩

inductive Step (driver : Driver Raw) : State Raw → Instruction Raw → State Raw → Prop where
  | shift (c : State Raw) (target terminal : Nat) (raw : Raw) (tail : List Raw)
      (pending : c.raw.rest = raw :: tail)
      (selected : driver.choose (c.raw.states.headD 0) (some raw) =
        some (some terminal, .shift target)) :
      Step driver c (.shift raw)
        ⟨RawRequest.shift c.raw target terminal raw tail, [(none, driver.leaf raw)] :: c.values⟩
  | reduce (c : State Raw) (key : Option Nat) (id offset target : Nat) (rule : SemanticValues.Rule)
      (found : driver.rules[id]? = some rule)
      (selected : driver.choose (c.raw.states.headD 0) c.raw.rest.head? = some (key, .reduce id))
      (edge : driver.engine.table.goto ((c.raw.states.drop rule.production.rhs.length).headD 0)
        (.nonterminal rule.production.lhs) = some target)
      (fits : rule.production.rhs.length ≤ c.values.length)
      (boundary : offset = (c.raw.rest.head?.map (fun raw => (driver.leaf raw).span.start)).getD driver.inputEnd) :
      Step driver c (.reduce id offset)
        ⟨RawRequest.reduce c.raw rule.production target,
          evaluate rule (c.values.take rule.production.rhs.length).reverse offset ::
            c.values.drop rule.production.rhs.length⟩

theorem step_erases {driver : Driver Raw} {before after : State Raw} {instruction}
    (step : Step driver before instruction after) :
    RawRequest.Step driver.engine before.raw after.raw := by
  cases step with
  | shift target terminal raw tail pending selected =>
    apply RawRequest.Step.shift before.raw target terminal raw tail pending
    rw [driver.decoded, selected]
    rfl
  | reduce key id offset target rule found selected edge fits _boundary =>
    apply RawRequest.Step.reduce before.raw key rule.production target _ edge
    rw [driver.decoded, selected]
    simp [TableBridge.decodeEntry, (SemanticValues.same_id_views driver.rules id rule found).1]

theorem step_executes {driver : Driver Raw} {before after : State Raw} {instruction}
    (step : Step driver before instruction after) :
    execute driver.rules driver.leaf instruction before.values = some after.values := by
  cases step with
  | shift => rfl
  | reduce key id offset target rule found selected edge fits _boundary => simp [execute, found, fits]

inductive Trace (driver : Driver Raw) : State Raw → List (Instruction Raw) → State Raw → Prop where
  | refl (state : State Raw) : Trace driver state [] state
  | next {a b c instructions instruction} (history : Trace driver a instructions b)
      (step : Step driver b instruction c) : Trace driver a (instructions ++ [instruction]) c

theorem trace_erases {driver : Driver Raw} {before after : State Raw} {instructions}
    (trace : Trace driver before instructions after) :
    RawRequest.Trace driver.engine before.raw after.raw := by
  induction trace with
  | refl => exact .refl _
  | next history step ih => exact .next ih (step_erases step)

theorem trace_executes {driver : Driver Raw} {before after : State Raw} {instructions}
    (trace : Trace driver before instructions after) :
    run driver.rules driver.leaf instructions before.values = some after.values := by
  induction trace with
  | refl => rfl
  | next history step ih => rw [run_append, ih]; simpa [run] using step_executes step

/-- Both conclusions come from the same ID-bearing trace. Output shape is the
native root admission, not a consequence of grammar recognition alone. -/
theorem complete_request (driver : Driver Raw) (input : List Raw) (last : State Raw)
    (instructions : List (Instruction Raw)) (root : Tree)
    (trace : Trace driver (initial driver input) instructions last)
    (complete : last.raw.rest = [])
    (accepted : driver.engine.select (last.raw.states.headD 0) none = some (none, .accept))
    (rootShape : last.values = [[(none, root)]]) :
    (∃ terminals, Grammar.Derives driver.engine.grammar [.nonterminal driver.engine.start] terminals ∧
      RawRequest.Relates driver.engine.realizesToken terminals input) ∧
    run driver.rules driver.leaf instructions [] = some [[(none, root)]] := by
  constructor
  · exact RawRequest.complete_request driver.engine input last.raw (trace_erases trace) complete accepted
  · simpa [initial, rootShape] using trace_executes trace

end ParserEngine.RequestAST
