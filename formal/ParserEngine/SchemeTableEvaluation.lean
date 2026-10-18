import ParserEngine.WalkSource

/-! Operational semantics for the source table-building fragment. Rules evaluate
actual Datum expressions and thread a heap; they do not dispatch on source
schema equality to run a separately written algorithm. This fragment excludes
continuations, arbitrary closures and the recursive lowering dispatcher. -/
namespace ParserEngine.SchemeTableEvaluation
open WalkSource

inductive Value where
  | natural (value : Nat)
  | boolean (value : Bool)
  | list (values : List Value)
  | table (address : Nat)
  | unit
  deriving Repr

abbrev Environment := String → Option Value
abbrev Heap := Nat → Nat → Option Value

def extend (env : Environment) (name : String) (value : Value) : Environment :=
  fun other => if other = name then some value else env other

def write (heap : Heap) (address key : Nat) (value : Value) : Heap :=
  fun other index => if other = address ∧ index = key then some value else heap other index

/-- Allocation and table identity obey an external primitive contract. Reads,
writes, list projection and evaluation order are explicit inside this fragment. -/
def primitive (operation : String) (args : List Value) (heap : Heap) : Option (Value × Heap) :=
  match operation, args with
  | "car", [.list (head :: _)] => some (head, heap)
  | "cadr", [.list (_ :: head :: _)] => some (head, heap)
  | "cons", [head, .list tail] => some (.list (head :: tail), heap)
  | "reverse", [.list values] => some (.list values.reverse, heap)
  | "table-ref", [.table address, .natural key, fallback] => some ((heap address key).getD fallback, heap)
  | "table-set!", [.table address, .natural key, value] => some (.unit, write heap address key value)
  | _, _ => none

mutual
inductive Eval : Environment → Heap → Datum → Value → Heap → Prop where
  | variable (found : env name = some value) : Eval env heap (.symbol name) value heap
  | natural : Eval env heap (.integer value) (.natural value) heap
  | boolean : Eval env heap (.boolean value) (.boolean value) heap
  | empty : Eval env heap (app "quote" [.list []]) (.list []) heap
  | apply (arguments : Arguments env before expressions values middle)
      (result : primitive operation values middle = some (value, after)) :
      Eval env before (app operation expressions) value after
  | letBody (bound : Eval env before expression value middle)
      (body : Sequence (extend env name value) middle expressions result after) :
      Eval env before (.list (.symbol "let" :: .list [.symbol name, expression] :: expressions)) result after
  | forEach (argument : Eval env before input (.list values) middle)
      (body : Each env name expressions middle values after) :
      Eval env before (app "for-each" [
        .list (.symbol "lambda" :: .list [.symbol name] :: expressions), input]) .unit after
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
inductive Each : Environment → String → List Datum → Heap → List Value → Heap → Prop where
  | nil : Each env name expressions heap [] heap
  | cons (head : Sequence (extend env name value) before expressions result middle)
      (tail : Each env name expressions middle values after) :
      Each env name expressions before (value :: values) after
end

end ParserEngine.SchemeTableEvaluation
