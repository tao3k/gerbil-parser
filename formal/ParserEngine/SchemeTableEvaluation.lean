import ParserEngine.WalkSource

/-! Operational semantics for the source table-building fragment. Rules evaluate
actual Datum expressions and thread a heap; they do not dispatch on source
schema equality to run a separately written algorithm. Lexical closures,
recursive procedure groups and named-let calls preserve captured cell addresses.
This fragment excludes continuations and the complete lowering dispatcher. -/
namespace ParserEngine.SchemeTableEvaluation
open WalkSource

structure Procedure where
  parameters : List Datum
  body : List Datum

inductive Key where
  | index (value : Nat)
  | symbol (value : String)
  deriving DecidableEq
instance : Coe Nat Key := ⟨Key.index⟩
instance (n : Nat) : OfNat Key n := ⟨.index n⟩
@[simp] theorem key_numeral (n : Nat) : (OfNat.ofNat n : Key) = .index n := rfl

inductive Value where
  | natural (value : Nat)
  | boolean (value : Bool)
  | list (values : List Value)
  | table (address : Nat)
  | unit
  | text (value : String)
  | keyword (name : String)
  | symbol (name : String)
  | cell (address : Nat)
  | closure (parameters : List Datum) (body : List Datum) (captured : String → Option Value)
  | recursive (group : List (String × Procedure)) (name : String) (captured : String → Option Value)

abbrev Environment := String → Option Value
abbrev Heap := Nat → Key → Option Value

def extend (env : Environment) (name : String) (value : Value) : Environment :=
  fun other => if other = name then some value else env other

def write (heap : Heap) (address key : Nat) (value : Value) : Heap :=
  fun other index => if other = address ∧ index = key then some value else heap other index

def writeSymbol (heap : Heap) (address : Nat) (key : String) (value : Value) : Heap :=
  fun other index => if other = address ∧ index = .symbol key then some value else heap other index

def resolve (env : Environment) (heap : Heap) (name : String) : Option Value :=
  match env name with
  | some (.cell address) => heap address 0
  | other => other

def truthy : Value → Bool
  | .boolean false => false
  | _ => true

mutual
  def literal : Datum → Option Value
    | .integer value => some (.natural value)
    | .boolean value => some (.boolean value)
    | .text value => some (.text value)
    | .symbol name => some (.symbol name)
    | .list values => (literals values).map Value.list
    | .keyword name => some (.keyword name)
  def literals : List Datum → Option (List Value)
    | [] => some []
    | head :: tail => do return (← literal head) :: (← literals tail)
end

/- Reader declarations are data even when their bodies contain keyword
arguments. Every representable datum has a value; keywords remain distinct
from symbols instead of making the complete declaration unrepresentable. -/
mutual
  def datumValue : Datum → Value
    | .integer value => .natural value
    | .boolean value => .boolean value
    | .text value => .text value
    | .keyword name => .keyword name
    | .symbol name => .symbol name
    | .list values => .list (datumValues values)
  def datumValues : List Datum → List Value
    | [] => []
    | head :: tail => datumValue head :: datumValues tail
end

mutual
  theorem literal_total (datum : Datum) : literal datum = some (datumValue datum) := by
    cases datum <;> simp only [literal, datumValue]
    rename_i values
    rw [literals_total values]; rfl
  theorem literals_total (values : List Datum) : literals values = some (datumValues values) := by
    cases values with
    | nil => rfl
    | cons head tail => simp [literals, datumValues, literal_total head, literals_total tail]
end

def bind : List Datum → List Value → Environment → Option Environment
  | [], [], env => some env
  | .symbol name :: rest, value :: values, env => bind rest values (extend env name value)
  | .list [.symbol name, _] :: rest, value :: values, env => bind rest values (extend env name value)
  | .list [.symbol name, fallback] :: rest, [], env => do
      bind rest [] (extend env name (← literal fallback))
  | _, _, _ => none

def tie (group : List (String × Procedure)) (captured : Environment) : Environment :=
  fun name => match group.lookup name with
    | some _ => some (.recursive group name captured)
    | none => captured name

def loopBindings : List Datum → Option (List String × List Datum)
  | [] => some ([], [])
  | .list [.symbol name, expression] :: rest => do
      let (names, expressions) ← loopBindings rest
      return (name :: names, expression :: expressions)
  | _ => none

def callable : Datum → Bool
  | .symbol "quote" | .symbol "lambda" | .symbol "set!" | .symbol "and"
  | .symbol "or" | .symbol "begin" | .symbol "let" | .symbol "if" | .symbol "cond" | .symbol "when"
  | .symbol "case" | .symbol "unless" | .symbol "def" => false
  | _ => true

/-- Primitive globals are unshadowed in this fragment. Local procedure calls
resolve their captured environments separately; cell reads use the current heap. -/
def primitive (operation : String) (args : List Value) (heap : Heap) : Option (Value × Heap) :=
  match operation, args with
  | "car", [.list (head :: _)] => some (head, heap)
  | "cadr", [.list (_ :: head :: _)] => some (head, heap)
  | "cdr", [.list (_ :: rest)] => some (.list rest, heap)
  | "cddr", [.list (_ :: _ :: rest)] => some (.list rest, heap)
  | "append", [.list left, .list right] => some (.list (left ++ right), heap)
  | "pair?", [value] => some (.boolean (match value with | .list (_ :: _) => true | _ => false), heap)
  | "void", [] => some (.unit, heap)
  | "null?", [.list values] => some (.boolean values.isEmpty, heap)
  | "eq?", [value, .symbol right] => some (.boolean (match value with | .symbol left => left == right | _ => false), heap)
  | "cons", [head, .list tail] => some (.list (head :: tail), heap)
  | "list", values => some (.list values, heap)
  | "reverse", [.list values] => some (.list values.reverse, heap)
  | "table-ref", [.table address, .natural key, fallback] => some ((heap address key).getD fallback, heap)
  | "table-set!", [.table address, .natural key, value] => some (.unit, write heap address key value)
  | "table-ref", [.table address, .symbol key, fallback] => some ((heap address (.symbol key)).getD fallback, heap)
  | "table-set!", [.table address, .symbol key, value] => some (.unit, writeSymbol heap address key value)
  | "+", [.natural a, .natural b] => some (.natural (a + b), heap)
  | "not", [value] => some (.boolean (!truthy value), heap)
  | "symbol->string", [.symbol name] => some (.text name, heap)
  | "number->string", [.natural value] => some (.text (toString value), heap)
  | "string-append", [.text a, .text b, .text c, .text d] => some (.text (a ++ b ++ c ++ d), heap)
  | "string->symbol", [.text name] => some (.symbol name, heap)
  | _, _ => none

/-- The supported cond syntax has a final else clause. Expansion preserves
test order and each clause's sequence; unsupported envelopes are rejected. -/
def expandCond : List Datum → Option Datum
  | [.list (.symbol "else" :: first :: rest)] => some (app "begin" (first :: rest))
  | .list (test :: first :: body) :: rest => do
      let fallback ← expandCond rest
      return app "if" [test, app "begin" (first :: body), fallback]
  | _ => none

/-- Lowering dispatches on interned symbolic tags. The selector is evaluated
once by Eval; clause selection neither reads nor mutates the runtime heap.
Unsupported clause envelopes fail closed. -/
def selectCase (key : String) : List Datum → Option (List Datum)
  | [.list (.symbol "else" :: first :: body)] => some (first :: body)
  | .list (.list keys :: first :: body) :: rest =>
      if keys.any (fun value => match value with | .symbol name => name == key | _ => false) then some (first :: body)
      else selectCase key rest
  | _ => none

mutual
inductive Eval : Environment → Heap → Datum → Value → Heap → Prop where
  | variable (found : resolve env heap name = some value) : Eval env heap (.symbol name) value heap
  | natural : Eval env heap (.integer value) (.natural value) heap
  | boolean : Eval env heap (.boolean value) (.boolean value) heap
  | empty : Eval env heap (app "quote" [.list []]) (.list []) heap
  | quoteSymbol : Eval env heap (app "quote" [.symbol name]) (.symbol name) heap
  | text : Eval env heap (.text value) (.text value) heap
  | lambda : Eval env heap (.list (.symbol "lambda" :: .list parameters :: body)) (.closure parameters body env) heap
  | assignment (target : env name = some (.cell address))
      (value : Eval env before expression result middle) :
      Eval env before (app "set!" [.symbol name, expression]) .unit (write middle address 0 result)
  | conjunctionFalse (head : Eval env before first value after) (stopped : truthy value = false) :
      Eval env before (app "and" [first, second]) value after
  | conjunctionTrue (head : Eval env before first value middle) (continued : truthy value = true)
      (tail : Eval env middle second result after) : Eval env before (app "and" [first, second]) result after
  | disjunctionTrue (head : Eval env before first value after) (stopped : truthy value = true) :
      Eval env before (app "or" [first, second]) value after
  | disjunctionFalse (head : Eval env before first value middle) (continued : truthy value = false)
      (tail : Eval env middle second result after) : Eval env before (app "or" [first, second]) result after
  | begin (body : Sequence env before expressions result after) : Eval env before (app "begin" expressions) result after
  | conditionalTrue (test : Eval env before condition value middle) (chosen : truthy value = true)
      (branch : Eval env middle yes result after) : Eval env before (app "if" [condition, yes, no]) result after
  | conditionalFalse (test : Eval env before condition value middle) (chosen : truthy value = false)
      (branch : Eval env middle no result after) : Eval env before (app "if" [condition, yes, no]) result after
  | cond (expanded : expandCond clauses = some expression) (execution : Eval env before expression result after) :
      Eval env before (app "cond" clauses) result after
  | caseSymbol (test : Eval env before selectorExpression (.symbol key) middle)
      (selected : selectCase key clauses = some expressions)
      (body : Sequence env middle expressions result after) :
      Eval env before (app "case" (selectorExpression :: clauses)) result after
  | unless (execution : Eval env before
      (app "if" [condition, app "void" [], app "begin" expressions]) result after) :
      Eval env before (app "unless" (condition :: expressions)) result after
  | whenTrue (test : Eval env before condition value middle) (chosen : truthy value = true)
      (body : Sequence env middle expressions result after) :
      Eval env before (app "when" (condition :: expressions)) result after
  | whenFalse (test : Eval env before condition value after) (chosen : truthy value = false) :
      Eval env before (app "when" (condition :: expressions)) .unit after
  | call (allowed : callable callee = true) (procedure : Eval env before callee function middle)
      (arguments : Arguments env middle expressions values ready)
      (result : Invoke ready function values value after) :
      Eval env before (.list (callee :: expressions)) value after
  | namedLoop (shape : loopBindings bindings = some (names, initial))
      (arguments : Arguments env before initial values middle)
      (bound : bind (names.map Datum.symbol) values (tie [(name, ⟨names.map Datum.symbol, body⟩)] env) = some entered)
      (execution : Sequence entered middle body result after) :
      Eval env before (.list (.symbol "let" :: .symbol name :: .list bindings :: body)) result after
  | apply (arguments : Arguments env before expressions values middle)
      (result : primitive operation values middle = some (value, after))
      (unshadowed : env operation = none) :
      Eval env before (app operation expressions) value after
  | letBody (bound : Eval env before expression value middle)
      (body : Sequence (extend env name value) middle expressions result after) :
      Eval env before (.list (.symbol "let" :: .list [.symbol name, expression] :: expressions)) result after
  | mapLambda (argument : Eval env before input (.list values) middle)
      (body : Mapping env name expressions middle values results after) (unshadowed : env "map" = none) :
      Eval env before (app "map" [
        .list (.symbol "lambda" :: .list [.symbol name] :: expressions), input]) (.list results) after
  | forEach (argument : Eval env before input (.list values) middle)
      (body : Each env name expressions middle values after) (unshadowed : env "for-each" = none) :
      Eval env before (app "for-each" [
        .list (.symbol "lambda" :: .list [.symbol name] :: expressions), input]) .unit after
  | forEachRecursive (argument : Eval env before input (.list values) middle)
      (found : resolve env middle procedure = some (.recursive group procedure captured))
      (definition : group.lookup procedure = some ⟨[.symbol name], expressions⟩)
      (body : Each (tie group captured) name expressions middle values after)
      (unshadowed : env "for-each" = none) :
      Eval env before (app "for-each" [.symbol procedure, input]) .unit after
inductive Invoke : Heap → Value → List Value → Value → Heap → Prop where
  | closure (bound : bind parameters values captured = some entered)
      (body : Sequence entered before expressions result after) :
      Invoke before (.closure parameters expressions captured) values result after
  | recursive (found : group.lookup name = some definition)
      (bound : bind definition.parameters values (tie group captured) = some entered)
      (body : Sequence entered before definition.body result after) :
      Invoke before (.recursive group name captured) values result after
inductive Arguments : Environment → Heap → List Datum → List Value → Heap → Prop where
  | nil : Arguments env heap [] [] heap
  | cons (head : Eval env before expression value middle)
      (tail : Arguments env middle expressions values after) :
      Arguments env before (expression :: expressions) (value :: values) after
inductive Sequence : Environment → Heap → List Datum → Value → Heap → Prop where
  | single (body : Eval env before expression value after) : Sequence env before [expression] value after
  | cons (head : Eval env before expression value middle)
      (tail : Sequence env middle expressions result after) :
      Sequence env before (expression :: expressions) result after
  | localProcedure (execution : Sequence (tie [(name, ⟨parameters, expressions⟩)] env) before rest result after) :
      Sequence env before
        (.list (.symbol "def" :: .list (.symbol name :: parameters) :: expressions) :: rest) result after
inductive Each : Environment → String → List Datum → Heap → List Value → Heap → Prop where
  | nil : Each env name expressions heap [] heap
  | cons (head : Sequence (extend env name value) before expressions result middle)
      (tail : Each env name expressions middle values after) :
      Each env name expressions before (value :: values) after
/-- The installed map primitive's ordered callback contract. Lambda creation
is pure; every application consumes its predecessor's resulting heap. -/
inductive Mapping : Environment → String → List Datum → Heap → List Value → List Value → Heap → Prop where
  | nil : Mapping env name expressions heap [] [] heap
  | cons (head : Sequence (extend env name value) before expressions result middle)
      (tail : Mapping env name expressions middle values results after) :
      Mapping env name expressions before (value :: values) (result :: results) after

end

end ParserEngine.SchemeTableEvaluation
