import ParserEngine.LRConstructionSource
import ParserEngine.TemplateCache
import ParserEngine.TransitionPublication

/-! Independent complete declaration schemas for template materialization and
its per-mask cache. Bit enumeration and primitive storage remain external. -/
namespace ParserEngine.LRTemplateSource
open WalkSource NativeActions.Source CompilerSemanticsSource

def template : Datum := defn "make-prediction-template" ["mask", "layout", "symbol-ids"] [
  app "let" [l [binding "items" emptyList, binding "order" emptyList,
    binding "groups" (app "make-table" [.keyword "test", s "eq?"])],
    app "compiler-index-set-for-each" [s "mask", app "lambda" [names ["production-id"],
      app "let*" [l [binding "item" (app "make-core-item" [s "production-id", n 0, s "layout"]),
        binding "id" (call "vector-ref" ["symbol-ids", "item"])],
        app "set!" [s "items", call "cons" ["item", "items"]],
        app "when" [s "id", app "let" [binding "row" (app "table-ref" [s "groups", s "id", b false]),
          app "unless" [s "row", app "set!" [s "row", app "vector" [s "item", s "id", n 0]],
            app "set!" [s "order", call "cons" ["id", "order"]], call "table-set!" ["groups", "id", "row"]],
          app "vector-set!" [s "row", n 2, app "compiler-index-set-add" [
            app "vector-ref" [s "row", n 2], app "+" [s "item", n 1]]]]]]]],
    app "vector" [app "reverse" [s "items"],
      app "map" [app "lambda" [names ["id"], call "table-ref" ["groups", "id"]], app "reverse" [s "order"]], s "groups"]]]

def templateFor : Datum := defn "template-for" ["kernel"] [app "let" [binding "mask" (n 0),
  app "compiler-index-set-for-each" [s "kernel", app "lambda" [names ["item"],
    app "set!" [s "mask", app "compiler-index-set-union" [s "mask", call "vector-ref" ["prediction-catalog", "item"]]]]],
  app "or" [app "table-ref" [s "templates", s "mask", b false], app "let" [
    binding "template" (call "make-prediction-template" ["mask", "layout", "symbol-ids"]),
    call "table-set!" ["templates", "mask", "template"], s "template"]]]]

def checkedTemplate (source : Datum) (g : Grammar.CFG) (mask : Nat) : Option (List Nat) :=
  if source = template then some (PredictionMaterialization.materialize g mask) else none

theorem checked_closure (source : Datum) (keys : List Nat) (index : Nat → Option Nat)
    (valid : PredictionExtraction.NameIndex keys index) (g : Grammar.CFG)
    (covered : PredictionMaterialization.Covers keys g) (seeds result : List Nat)
    (checked : checkedTemplate source g
      (PredictionMaterialization.aggregate g index (PredictionMaterialization.completed keys index valid g) seeds) = some result)
    (item : Viable.Core) :
    (∃ code ∈ LRStateSemantics.merge seeds result [], Viable.decodeCore g code = some item) ↔
      Viable.Closure g (PredictionMaterialization.Kernel g seeds) item := by
  unfold checkedTemplate at checked
  split at checked
  next matched => cases checked; exact PredictionMaterialization.closure_exact keys index valid g covered seeds item
  next mismatch => cases checked

end ParserEngine.LRTemplateSource
namespace ParserEngine.LRTemplateWitness
open LRTemplateSource

theorem source_checked : templateSource = template ∧ cacheSource = templateFor := ⟨rfl, rfl⟩
end ParserEngine.LRTemplateWitness
