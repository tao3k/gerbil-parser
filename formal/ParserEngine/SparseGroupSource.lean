import ParserEngine.KernelConstruction
import ParserEngine.PhysicalOperandExtraction

/-! Complete independent schemas for physical symbol interning and sparse
state grouping. Meaning is derived from constructed masks, not output labels. -/
namespace ParserEngine.SparseGroupSource
open WalkSource NativeActions.Source CompilerSemanticsSource

def symbolCatalogue : Datum := defn "make-lr0-symbol-catalog" ["core-symbols"] [
  app "let" [l [binding "index" (app "make-table" [.keyword "test", s "equal?"]),
    binding "ids" (app "make-vector" [app "vector-length" [s "core-symbols"], b false]), binding "count" (n 0)],
    app "let" [s "loop", l [binding "item" (n 0)], app "when" [app "<" [s "item", app "vector-length" [s "core-symbols"]],
      app "let" [binding "symbol" (call "vector-ref" ["core-symbols", "item"]), app "when" [s "symbol",
        app "let" [binding "id" (app "table-ref" [s "index", s "symbol", b false]),
          app "unless" [s "id", call "set!" ["id", "count"], app "set!" [s "count", app "+" [s "count", n 1]],
            call "table-set!" ["index", "symbol", "id"]], call "vector-set!" ["ids", "item", "id"]]]],
      app "loop" [app "+" [s "item", n 1]]]],
    app "let" [binding "symbols" (app "make-vector" [s "count", b false]),
      app "hash-for-each" [app "lambda" [names ["symbol", "id"], call "vector-set!" ["symbols", "id", "symbol"]], s "index"],
      call "values" ["symbols", "ids"]]]]

def ownFill : Datum := app "compiler-index-set-for-each" [s "kernel-mask", app "lambda" [names ["item"],
  app "let" [binding "id" (call "vector-ref" ["symbol-ids", "item"]), app "when" [s "id",
    app "when" [app "zero?" [call "vector-ref" ["masks", "id"]],
      app "set!" [s "order", call "cons" ["id", "order"]], call "vector-set!" ["first-items", "id", "item"]],
    app "vector-set!" [s "masks", s "id", app "compiler-index-set-add" [call "vector-ref" ["masks", "id"], app "+" [s "item", n 1]]]]]]]

def ownRows : Datum := app "list-sort" [app "lambda" [names ["left", "right"], app "<" [
  app "vector-ref" [s "left", n 0], app "vector-ref" [s "right", n 0]]], app "map" [app "lambda" [names ["id"],
    app "let" [l [binding "shared" (app "table-ref" [s "groups", s "id", b false]),
      binding "first" (call "vector-ref" ["first-items", "id"]), binding "mask" (call "vector-ref" ["masks", "id"])],
      app "if" [s "shared", app "vector" [app "min" [s "first", app "vector-ref" [s "shared", n 0]], s "id",
        app "compiler-index-set-union" [s "mask", app "vector-ref" [s "shared", n 2]]],
        call "vector" ["first", "id", "mask"]]]], s "order"]]

def mergeRows : Datum := app "let" [s "merge", l [binding "left" (s "own"),
  binding "right" (app "vector-ref" [s "template", n 1]), binding "found" emptyList], app "cond" [
    l [app "and" [app "pair?" [s "right"], app "not" [app "zero?" [app "vector-ref" [s "masks",
      app "vector-ref" [app "car" [s "right"], n 1]]]]], app "merge" [s "left", app "cdr" [s "right"], s "found"]],
    l [app "null?" [s "right"], call "foldl" ["cons", "left", "found"]],
    l [app "null?" [s "left"], app "merge" [s "left", app "cdr" [s "right"], app "cons" [app "car" [s "right"], s "found"]]],
    l [app "<" [app "vector-ref" [app "car" [s "left"], n 0], app "vector-ref" [app "car" [s "right"], n 0]],
      app "merge" [app "cdr" [s "left"], s "right", app "cons" [app "car" [s "left"], s "found"]]],
    l [s "else", app "merge" [s "left", app "cdr" [s "right"], app "cons" [app "car" [s "right"], s "found"]]]]]

def groupConstructor : Datum := defn "lr0-state-kernels" ["kernel-mask", "template", "symbol-ids", "masks", "first-items"] [
  app "let" [l [binding "order" emptyList, binding "groups" (app "vector-ref" [s "template", n 2])], ownFill,
    app "let" [binding "own" ownRows, app "let" [binding "rows" mergeRows,
      app "for-each" [app "lambda" [names ["id"], app "vector-set!" [s "masks", s "id", n 0]], s "order"], s "rows"]]]]

def checkedRows (source : Datum) (g : Grammar.CFG) (kernel predictions : List Nat) : Option (List SparseGroupMasks.Row) :=
  if source = groupConstructor then some (SparseGoto.physicalRows g kernel predictions) else none

theorem checked_bound (source : Datum) (g : Grammar.CFG) (kernel predictions : List Nat)
    (rows : List SparseGroupMasks.Row) (checked : checkedRows source g kernel predictions = some rows)
    (row : SparseGroupMasks.Row) (member : row ∈ rows) : PublishedClosure.KernelBound g row.mask := by
  unfold checkedRows at checked
  split at checked
  next matched => cases checked; exact SparseGoto.kernel_bound g kernel predictions row member
  next mismatch => cases checked

end ParserEngine.SparseGroupSource
namespace ParserEngine.SparseGroupWitness
open SparseGroupSource
set_option autoImplicit false in
theorem source_checked : symbolSource = symbolCatalogue ∧ groupSource = groupConstructor := ⟨rfl, rfl⟩
end ParserEngine.SparseGroupWitness
