import ParserEngine.SchemeTableEvaluation

/-! Determinism of the supported source fragment, independent of rule indexing. -/
namespace ParserEngine.SchemeTableEvaluation
open WalkSource

def EvalClaim (env : Environment) (before : Heap) (expression : Datum) (value : Value) (after : Heap) : Prop :=
  ∀ code other last, expression = code → Eval env before code other last → value = other ∧ after = last

def InvokeClaim (before : Heap) (procedure : Value) (values : List Value) (value : Value) (after : Heap) : Prop :=
  ∀ other last, Invoke before procedure values other last → value = other ∧ after = last

def ArgsClaim (env : Environment) (before : Heap) (expressions : List Datum) (values : List Value) (after : Heap) : Prop :=
  ∀ others last, Arguments env before expressions others last → values = others ∧ after = last

def SeqClaim (env : Environment) (before : Heap) (expressions : List Datum) (value : Value) (after : Heap) : Prop :=
  ∀ other last, Sequence env before expressions other last → value = other ∧ after = last

def EachClaim (env : Environment) (name : String) (expressions : List Datum) (before : Heap)
    (values : List Value) (after : Heap) : Prop :=
  ∀ last, Each env name expressions before values last → after = last

def MapClaim (env : Environment) (name : String) (expressions : List Datum) (before : Heap)
    (values results : List Value) (after : Heap) : Prop :=
  ∀ others last, Mapping env name expressions before values others last → results = others ∧ after = last

theorem Sequence.nonempty (evaluated : Sequence env before expressions value after) : expressions ≠ [] := by
  cases evaluated <;> simp

theorem Eval.variable_result (evaluated : Eval env before (.symbol name) value after) :
    resolve env before name = some value ∧ after = before := by
  cases evaluated with
  | «variable» found => exact ⟨found, rfl⟩

theorem Eval.no_unbound_call (absent : env name = none)
    (evaluated : Eval env before callee value after)
    (shape : Datum.list (callee :: args) = app name expressions) : False := by
  simp only [app, Datum.list.injEq, List.cons.injEq] at shape
  rcases shape with ⟨rfl, _⟩
  have found := evaluated.variable_result.1
  simp [resolve, absent] at found

theorem primitive_callable (result : primitive operation values heap = some outcome) :
    callable (s operation) = true := by
  unfold primitive at result
  split at result <;> first | rfl | cases result

theorem primitive_map : primitive "map" values heap = none := rfl

theorem primitive_forEach : primitive "for-each" values heap = none := rfl

theorem callable_quote : callable (s "quote") = false := rfl
theorem callable_lambda : callable (s "lambda") = false := rfl
theorem callable_assignment : callable (s "set!") = false := rfl
theorem callable_conjunction : callable (s "and") = false := rfl
theorem callable_disjunction : callable (s "or") = false := rfl
theorem callable_begin : callable (s "begin") = false := rfl
theorem callable_let : callable (s "let") = false := rfl
theorem callable_conditional : callable (s "if") = false := rfl
theorem callable_when : callable (s "when") = false := rfl

theorem callable_def : callable (s "def") = false := rfl

theorem callable_unless : callable (s "unless") = false := rfl

theorem callable_case : callable (s "case") = false := rfl

theorem callable_cond : callable (s "cond") = false := rfl

/-- Procedure declarations are sequence bindings, never value expressions. -/
theorem Eval.no_definition
    (actual : Eval env before (.list (.symbol "def" :: definition)) result after) : False := by
  generalize codeEq : Datum.list (.symbol "def" :: definition) = code at actual
  cases actual <;> grind [app, callable, primitive_callable]

macro "source_determinism_cases" : tactic => `(tactic|
  (dsimp only [EvalClaim, InvokeClaim, ArgsClaim, SeqClaim, EachClaim, MapClaim] at *
   intros
   rename_i second
   cases second <;> grind [app, callable_quote, callable_lambda, callable_assignment,
     callable_conjunction, callable_disjunction, callable_begin, callable_let, callable_conditional, callable_cond, callable_when, callable_case, callable_unless, callable_def,
     Sequence.nonempty, Eval.no_unbound_call, Eval.no_definition, primitive_callable, primitive_forEach, primitive_map, loopBindings]))

theorem conditional_true_unique
    (_test : Eval env before condition value middle) (chosen : truthy value = true)
    (_branch : Eval env middle yes result after)
    (testUnique : EvalClaim env before condition value middle)
    (branchUnique : EvalClaim env middle yes result after) :
    EvalClaim env before (app "if" [condition, yes, no]) result after := by
  source_determinism_cases

theorem conditional_false_unique
    (_test : Eval env before condition value middle) (chosen : truthy value = false)
    (_branch : Eval env middle no result after)
    (testUnique : EvalClaim env before condition value middle)
    (branchUnique : EvalClaim env middle no result after) :
    EvalClaim env before (app "if" [condition, yes, no]) result after := by
  source_determinism_cases

theorem cond_unique (expanded : expandCond clauses = some expression)
    (_execution : Eval env before expression result after)
    (unique : EvalClaim env before expression result after) :
    EvalClaim env before (app "cond" clauses) result after := by
  source_determinism_cases

theorem case_symbol_unique
    (_test : Eval env before selectorExpression (.symbol key) middle)
    (selected : selectCase key clauses = some expressions)
    (_body : Sequence env middle expressions result after)
    (testUnique : EvalClaim env before selectorExpression (.symbol key) middle)
    (bodyUnique : SeqClaim env middle expressions result after) :
    EvalClaim env before (app "case" (selectorExpression :: clauses)) result after := by
  source_determinism_cases

theorem parallel_let_unique
    (shape : loopBindings bindings = some (names, initial))
    (_distinct : names.Nodup)
    (_arguments : Arguments env before initial values middle)
    (bound : bind (names.map Datum.symbol) values env = some entered)
    (_execution : Sequence entered middle body result after)
    (argumentsUnique : ArgsClaim env before initial values middle)
    (bodyUnique : SeqClaim entered middle body result after) :
    EvalClaim env before (.list (.symbol "let" :: .list bindings :: body)) result after := by
  source_determinism_cases

theorem variable_unique
    (found : resolve env heap name = some value) : EvalClaim env heap (.symbol name) value heap := by
  source_determinism_cases

theorem negative_unique : EvalClaim env heap (.negative magnitude) (.negative magnitude) heap := by
  source_determinism_cases

theorem natural_unique
    : EvalClaim env heap (.integer value) (.natural value) heap := by
  source_determinism_cases

theorem boolean_unique
    : EvalClaim env heap (.boolean value) (.boolean value) heap := by
  source_determinism_cases

theorem empty_unique
    : EvalClaim env heap (app "quote" [.list []]) (.list []) heap := by
  source_determinism_cases

theorem text_unique
    : EvalClaim env heap (.text value) (.text value) heap := by
  source_determinism_cases

theorem lambda_unique
    : EvalClaim env heap (.list (.symbol "lambda" :: .list parameters :: body)) (.closure parameters body env) heap := by
  source_determinism_cases

theorem assignment_unique
    (target : env name = some (.cell address)) (unique : EvalClaim env before expression result middle) : EvalClaim env before (app "set!" [.symbol name, expression]) .unit (write middle address 0 result) := by
  source_determinism_cases

theorem conjunction_false_unique
    (stopped : truthy value = false) (unique : EvalClaim env before first value after) : EvalClaim env before (app "and" [first, second]) value after := by
  source_determinism_cases

theorem conjunction_true_unique
    (continued : truthy value = true) (head : EvalClaim env before first value middle) (tail : EvalClaim env middle second result after) : EvalClaim env before (app "and" [first, second]) result after := by
  source_determinism_cases

theorem disjunction_true_unique
    (stopped : truthy value = true) (unique : EvalClaim env before first value after) : EvalClaim env before (app "or" [first, second]) value after := by
  source_determinism_cases

theorem disjunction_false_unique
    (continued : truthy value = false) (head : EvalClaim env before first value middle) (tail : EvalClaim env middle second result after) : EvalClaim env before (app "or" [first, second]) result after := by
  source_determinism_cases

theorem begin_unique
    (unique : SeqClaim env before expressions result after) : EvalClaim env before (app "begin" expressions) result after := by
  source_determinism_cases

theorem call_unique
    (called : Eval env before callee function middle)
    (allowed : callable callee = true) (procedure : EvalClaim env before callee function middle) (arguments : ArgsClaim env middle expressions values ready) (result : InvokeClaim ready function values value after) : EvalClaim env before (.list (callee :: expressions)) value after := by
  source_determinism_cases

theorem named_loop_unique
    (shape : loopBindings bindings = some (names, initial)) (bound : bind (names.map Datum.symbol) values (tie [(name, ⟨names.map Datum.symbol, body⟩)] env) = some entered) (arguments : ArgsClaim env before initial values middle) (execution : SeqClaim entered middle body result after) : EvalClaim env before (.list (.symbol "let" :: .symbol name :: .list bindings :: body)) result after := by
  source_determinism_cases

theorem primitive_unique
    (result : primitive operation values middle = some (value, after)) (unshadowed : env operation = none) (arguments : ArgsClaim env before expressions values middle) : EvalClaim env before (app operation expressions) value after := by
  source_determinism_cases

theorem let_unique
    (bound : EvalClaim env before expression value middle) (body : SeqClaim (extend env name value) middle expressions result after) : EvalClaim env before (.list (.symbol "let" :: .list [.symbol name, expression] :: expressions)) result after := by
  source_determinism_cases

theorem for_each_unique
    (argument : EvalClaim env before input (.list values) middle) (body : EachClaim env name expressions middle values after) (unshadowed : env "for-each" = none) : EvalClaim env before (app "for-each" [.list (.symbol "lambda" :: .list [.symbol name] :: expressions), input]) .unit after := by
  source_determinism_cases

theorem closure_unique
    (bound : bind parameters values captured = some entered) (body : SeqClaim entered before expressions result after) : InvokeClaim before (.closure parameters expressions captured) values result after := by
  source_determinism_cases

theorem recursive_unique
    (found : group.lookup name = some definition) (bound : bind definition.parameters values (tie group captured) = some entered) (body : SeqClaim entered before definition.body result after) : InvokeClaim before (.recursive group name captured) values result after := by
  source_determinism_cases

theorem arguments_nil_unique
    : ArgsClaim env heap [] [] heap := by
  source_determinism_cases

theorem arguments_cons_unique
    (head : EvalClaim env before expression value middle) (tail : ArgsClaim env middle expressions values after) : ArgsClaim env before (expression :: expressions) (value :: values) after := by
  source_determinism_cases

theorem sequence_single_unique
    (body : EvalClaim env before expression value after) : SeqClaim env before [expression] value after := by
  source_determinism_cases

theorem sequence_cons_unique
    (_started : Eval env before expression value middle)
    (continued : Sequence env middle expressions result after)
    (head : EvalClaim env before expression value middle) (tail : SeqClaim env middle expressions result after) : SeqClaim env before (expression :: expressions) result after := by
  source_determinism_cases

theorem each_nil_unique
    : EachClaim env name expressions heap [] heap := by
  source_determinism_cases

theorem each_cons_unique
    (head : SeqClaim (extend env name value) before expressions result middle) (tail : EachClaim env name expressions middle values after) : EachClaim env name expressions before (value :: values) after := by
  source_determinism_cases

theorem quote_symbol_unique : EvalClaim env heap (app "quote" [.symbol name]) (.symbol name) heap := by
  source_determinism_cases

theorem when_true_unique (_test : Eval env before condition value middle) (chosen : truthy value = true)
    (_body : Sequence env middle expressions result after)
    (testUnique : EvalClaim env before condition value middle)
    (bodyUnique : SeqClaim env middle expressions result after) :
    EvalClaim env before (app "when" (condition :: expressions)) result after := by
  source_determinism_cases

theorem when_false_unique (_test : Eval env before condition value after) (chosen : truthy value = false)
    (testUnique : EvalClaim env before condition value after) :
    EvalClaim env before (app "when" (condition :: expressions)) .unit after := by
  source_determinism_cases

theorem for_each_recursive_unique
    (_argument : Eval env before input (.list values) middle)
    (found : resolve env middle procedure = some (.recursive group procedure captured))
    (definition : group.lookup procedure = some ⟨[.symbol name], expressions⟩)
    (_body : Each (tie group captured) name expressions middle values after)
    (unshadowed : env "for-each" = none)
    (argumentUnique : EvalClaim env before input (.list values) middle)
    (bodyUnique : EachClaim (tie group captured) name expressions middle values after) :
    EvalClaim env before (app "for-each" [.symbol procedure, input]) .unit after := by
  source_determinism_cases

theorem unless_unique
    (_execution : Eval env before (app "if" [condition, app "void" [], app "begin" expressions]) result after)
    (unique : EvalClaim env before (app "if" [condition, app "void" [], app "begin" expressions]) result after) :
    EvalClaim env before (app "unless" (condition :: expressions)) result after := by
  source_determinism_cases

theorem map_lambda_unique (_argument : Eval env before input (.list values) middle)
    (_body : Mapping env name expressions middle values results after) (unshadowed : env "map" = none)
    (argumentUnique : EvalClaim env before input (.list values) middle)
    (bodyUnique : MapClaim env name expressions middle values results after) :
    EvalClaim env before (app "map" [
      .list (.symbol "lambda" :: .list [.symbol name] :: expressions), input]) (.list results) after := by
  source_determinism_cases

theorem mapping_nil_unique : MapClaim env name expressions heap [] [] heap := by
  source_determinism_cases

theorem mapping_cons_unique
    (head : SeqClaim (extend env name value) before expressions result middle)
    (tail : MapClaim env name expressions middle values results after) :
    MapClaim env name expressions before (value :: values) (result :: results) after := by
  source_determinism_cases

theorem sequence_local_unique
    (_execution : Sequence (tie [(name, ⟨parameters, expressions⟩)] env) before rest result after)
    (unique : SeqClaim (tie [(name, ⟨parameters, expressions⟩)] env) before rest result after) :
    SeqClaim env before
      (.list (.symbol "def" :: .list (.symbol name :: parameters) :: expressions) :: rest) result after := by
  source_determinism_cases

theorem Eval.unique (first : Eval env before expression value after) : EvalClaim env before expression value after := by
  induction first using Eval.rec
      (motive_2 := fun before procedure values value after _ => InvokeClaim before procedure values value after)
      (motive_3 := fun env before expressions values after _ => ArgsClaim env before expressions values after)
      (motive_4 := fun env before expressions value after _ => SeqClaim env before expressions value after)
      (motive_5 := fun env name expressions before values after _ => EachClaim env name expressions before values after)
      (motive_6 := fun env name expressions before values results after _ => MapClaim env name expressions before values results after) <;>
    first
    | apply quote_symbol_unique <;> assumption
    | apply when_true_unique <;> assumption
    | apply when_false_unique <;> assumption
    | apply for_each_recursive_unique <;> assumption
    | apply conditional_true_unique <;> assumption
    | apply conditional_false_unique <;> assumption
    | apply cond_unique <;> assumption
    | apply case_symbol_unique <;> assumption
    | apply unless_unique <;> assumption
    | apply map_lambda_unique <;> assumption
    | apply mapping_nil_unique <;> assumption
    | apply mapping_cons_unique <;> assumption
    | apply sequence_local_unique <;> assumption
    | apply parallel_let_unique <;> assumption
    | apply variable_unique <;> assumption
    | apply natural_unique <;> assumption
    | apply negative_unique <;> assumption
    | apply boolean_unique <;> assumption
    | apply empty_unique <;> assumption
    | apply text_unique <;> assumption
    | apply lambda_unique <;> assumption
    | apply assignment_unique <;> assumption
    | apply conjunction_false_unique <;> assumption
    | apply conjunction_true_unique <;> assumption
    | apply disjunction_true_unique <;> assumption
    | apply disjunction_false_unique <;> assumption
    | apply begin_unique <;> assumption
    | apply call_unique <;> assumption
    | apply named_loop_unique <;> assumption
    | apply primitive_unique <;> assumption
    | apply let_unique <;> assumption
    | apply for_each_unique <;> assumption
    | apply closure_unique <;> assumption
    | apply recursive_unique <;> assumption
    | apply arguments_nil_unique <;> assumption
    | apply arguments_cons_unique <;> assumption
    | apply sequence_single_unique <;> assumption
    | apply sequence_cons_unique <;> assumption
    | apply each_nil_unique <;> assumption
    | apply each_cons_unique <;> assumption

theorem Eval.deterministic (first : Eval env before expression value after)
    (second : Eval env before expression other last) : value = other ∧ after = last :=
  first.unique expression other last rfl second
/-- Reuse the same body determinism for arbitrary direct closure invocations,
including source calls whose arguments are not reader-serializable values. -/
theorem Invoke.deterministic (first : Invoke before function values value after)
    (second : Invoke before function values other last) : value = other ∧ after = last := by
  cases first with
  | closure bound body =>
      cases second with
      | closure otherBound otherBody =>
          have same := Option.some.inj (bound.symm.trans otherBound)
          cases same
          exact (Eval.begin body).deterministic (.begin otherBody)
  | recursive found bound body =>
      cases second with
      | recursive otherFound otherBound otherBody =>
          have sameDefinition := Option.some.inj (found.symm.trans otherFound)
          cases sameDefinition
          have same := Option.some.inj (bound.symm.trans otherBound)
          cases same
          exact (Eval.begin body).deterministic (.begin otherBody)

end ParserEngine.SchemeTableEvaluation
