import ParserEngine.LRStateSemantics
import ParserEngine.NativeActions
import ParserEngine.SourceData

/-! Independent reader schemas for the complete producer and closure bodies.
Equality admits the restricted control-flow interpretation, not arbitrary
Scheme/compiler correctness. Primitive storage and host behavior are external. -/
namespace ParserEngine.CompilerSemanticsSource
open WalkSource NativeActions.Source

def binding (name : String) (value : Datum) : Datum := l [s name, value]
def emptyList : Datum := app "quote" [l []]

def terminalMask : Datum := defn "terminal-list->mask" ["terminals", "terminal-index"] [
  app "foldl" [app "lambda" [names ["terminal", "mask"],
    app "compiler-index-set-add" [s "mask", call "table-ref" ["terminal-index", "terminal"]]],
    n 0, s "terminals"]]

def maskReader : Datum := defn "first-mask" ["name"] [
  app "let" [binding "known" (app "table-ref" [s "nonterminal-masks", s "name", b false]),
    app "or" [s "known", app "let" [binding "mask"
      (app "terminal-list->mask" [app "table-ref" [s "first", s "name", emptyList], s "terminal-index"]),
      app "table-set!" [s "nonterminal-masks", s "name", s "mask"], s "mask"]]]]

def itemLoop : Datum := app "let" [s "item-loop", l [
  binding "dot" (app "-" [s "width", n 1]), binding "tail-mask" (n 0), binding "tail-nullable?" (b true)],
  app "when" [app ">=" [s "dot", n 0], app "let*" [l [
    binding "item" (call "make-core-item" ["production-id", "dot", "layout"]),
    binding "symbol" (call "vector-ref" ["core-symbols", "item"]),
    binding "nonterminal?" (call "nonterminal-symbol?" ["symbol"]),
    binding "head-nullable?" (app "and" [s "nonterminal?",
      app "table-ref" [s "nullable", call "nonterminal-name" ["symbol"], b false]]),
    binding "head-mask" (app "if" [s "nonterminal?",
      app "first-mask" [call "nonterminal-name" ["symbol"]],
      app "compiler-index-set-singleton" [call "table-ref" ["terminal-index", "symbol"]]]),
    binding "mask" (app "if" [s "head-nullable?",
      call "compiler-index-set-union" ["head-mask", "tail-mask"], s "head-mask"]),
    binding "nullable?" (call "and" ["head-nullable?", "tail-nullable?"])],
    call "vector-set!" ["first-masks", "item", "mask"],
    call "vector-set!" ["nullable-suffixes", "item", "nullable?"],
    app "item-loop" [app "-" [s "dot", n 1], s "mask", s "nullable?"]]]]

def suffixCatalogue : Datum := defn "make-core-suffix-catalog"
    ["table", "layout", "core-symbols", "first", "nullable", "terminal-index"] [
  app "let*" [l [binding "size" (call "core-item-count" ["layout"]),
    binding "first-masks" (app "make-vector" [s "size", n 0]),
    binding "nullable-suffixes" (app "make-vector" [s "size", b false]),
    binding "nonterminal-masks" (app "make-table" [.keyword "test", s "eq?"])], maskReader,
    app "let" [s "production-loop", l [binding "production-id" (n 0)],
      app "when" [app "<" [s "production-id", call "vector-length" ["table"]],
        app "let" [binding "width" (app "length" [app "production-rhs" [call "vector-ref" ["table", "production-id"]]]),
          app "vector-set!" [s "nullable-suffixes", app "make-core-item" [s "production-id", s "width", s "layout"], b true],
          itemLoop], app "production-loop" [app "+" [s "production-id", n 1]]]],
    call "values" ["first-masks", "nullable-suffixes"]]]

def closureMerge : Datum := defn "lr0-closure" ["kernel-mask", "template"] [
  app "let" [binding "seed" emptyList,
    app "compiler-index-set-for-each" [s "kernel-mask", app "lambda" [names ["item"],
      app "set!" [s "seed", call "cons" ["item", "seed"]]]],
    app "let" [s "merge", l [binding "left" (call "reverse" ["seed"]),
      binding "right" (app "vector-ref" [s "template", n 0]), binding "found" emptyList],
      app "cond" [
        l [call "null?" ["left"], call "foldl" ["cons", "right", "found"]],
        l [call "null?" ["right"], call "foldl" ["cons", "left", "found"]],
        l [app "=" [call "car" ["left"], call "car" ["right"]],
          app "merge" [call "cdr" ["left"], call "cdr" ["right"], app "cons" [call "car" ["left"], s "found"]]],
        l [app "<" [call "car" ["left"], call "car" ["right"]],
          app "merge" [call "cdr" ["left"], s "right", app "cons" [call "car" ["left"], s "found"]]],
        l [s "else", app "merge" [s "left", call "cdr" ["right"], app "cons" [call "car" ["right"], s "found"]]]]]]]

/-- Fresh source equality is an admission gate. It does not infer that a cache
hit, source digest, or successful finite test proves the grammar semantics. -/
def checkedSuffix (source maskSource : Datum) (facts : SuffixSemantics.Facts)
    (symbols : List Grammar.Symbol) : Option (List SuffixSemantics.Summary) :=
  if source = suffixCatalogue ∧ maskSource = terminalMask then
    some (SuffixSemantics.catalogue facts symbols) else none

theorem checked_suffix_semantic (source maskSource : Datum) {g : Grammar.CFG}
    {facts : SuffixSemantics.Facts} (admitted : SuffixSemantics.Exact g facts)
    (symbols : List Grammar.Symbol) (dot : Nat) (valid : dot ≤ symbols.length)
    (rows : List SuffixSemantics.Summary)
    (checked : checkedSuffix source maskSource facts symbols = some rows) :
    ∃ result, rows[dot]? = some result ∧
      (∀ token, result.first.testBit token = true ↔ Grammar.First g (symbols.drop dot) token) ∧
      (result.nullable = true ↔ Grammar.Nullable g (symbols.drop dot)) := by
  unfold checkedSuffix at checked
  split at checked
  next matched =>
    cases checked
    exact SuffixSemantics.catalogue_semantic admitted symbols dot valid
  next mismatch => cases checked

def checkedClosure (source : Datum) (kernel predictions : List Nat) : Option (List Nat) :=
  if source = closureMerge then some (LRStateSemantics.merge kernel predictions []) else none

theorem checked_closure_sorted (source : Datum) (kernel predictions rows : List Nat)
    (kernelSorted : kernel.Pairwise (· < ·)) (templateSorted : predictions.Pairwise (· < ·))
    (checked : checkedClosure source kernel predictions = some rows) :
    rows.Pairwise (· < ·) := by
  unfold checkedClosure at checked
  split at checked
  next matched => cases checked; exact LRStateSemantics.closure_merge_sorted _ _ kernelSorted templateSorted
  next mismatch => cases checked

theorem checked_closure_exact (source : Datum) {g : Grammar.CFG}
    {kernel : Viable.Core → Prop} (decode : Nat → Option Viable.Core)
    (seeds predictions rows : List Nat)
    (seedExact : ∀ item, kernel item ↔ ∃ code ∈ seeds, decode code = some item)
    (templateExact : ∀ item, LRStateSemantics.Template g kernel item ↔
      ∃ code ∈ predictions, decode code = some item)
    (checked : checkedClosure source seeds predictions = some rows) (item : Viable.Core) :
    (∃ code ∈ rows, decode code = some item) ↔ Viable.Closure g kernel item := by
  unfold checkedClosure at checked
  split at checked
  next matched =>
    cases checked
    exact LRStateSemantics.decoded_closure decode seeds predictions seedExact templateExact item
  next mismatch => cases checked

theorem changed_suffix_rejected (source maskSource : Datum) (facts : SuffixSemantics.Facts)
    (symbols : List Grammar.Symbol) (changed : source ≠ suffixCatalogue ∨ maskSource ≠ terminalMask) :
    checkedSuffix source maskSource facts symbols = none := by
  rcases changed with mismatch | mismatch <;> simp [checkedSuffix, mismatch]

end ParserEngine.CompilerSemanticsSource

namespace ParserEngine.CompilerSemanticsWitness
open CompilerSemanticsSource

theorem source_checked : suffixSource = suffixCatalogue ∧ maskSource = terminalMask ∧
    closureSource = closureMerge := by
  exact ⟨rfl, rfl, rfl⟩

end ParserEngine.CompilerSemanticsWitness
