import ParserEngine.CompilerSemanticsSource
import ParserEngine.PredictionExtraction
import ParserEngine.NodeCoordinates

/-! Independent source schemas admit the parser-owned construction idioms.
Storage, hash equality and host execution remain explicit external premises. -/
namespace ParserEngine.LRConstructionSource
open WalkSource NativeActions.Source CompilerSemanticsSource

def growWith (factor : Nat) : Datum := defn "grow!" [] [app "when" [
  app "=" [s "state-count", app "vector-length" [s "states"]],
  app "let" [l [binding "next-states" (app "make-vector" [app "*" [n factor, s "state-count"], b false]),
    binding "next-transitions" (app "make-vector" [app "*" [n factor, s "state-count"], emptyList]),
    binding "next-plans" (app "make-vector" [app "*" [n factor, s "state-count"], b false])],
    app "let" [s "copy", l [binding "index" (n 0)], app "when" [call "<" ["index", "state-count"],
      app "vector-set!" [s "next-states", s "index", call "vector-ref" ["states", "index"]],
      app "vector-set!" [s "next-transitions", s "index", call "vector-ref" ["state-transitions", "index"]],
      app "vector-set!" [s "next-plans", s "index", call "vector-ref" ["kernel-plans", "index"]],
      app "copy" [app "+" [s "index", n 1]]]],
    call "set!" ["states", "next-states"], call "set!" ["state-transitions", "next-transitions"],
    call "set!" ["kernel-plans", "next-plans"]]]]

def grow : Datum := growWith 2

def appendState : Datum := defn "append-state!" ["kernel"] [app "grow!" [],
  app "let" [l [binding "index" (s "state-count"), binding "template" (app "template-for" [s "kernel"])],
    app "vector-set!" [s "states", s "index", call "lr0-closure" ["kernel", "template"]],
    app "vector-set!" [s "kernel-plans", s "index", call "cons" ["kernel", "template"]],
    call "table-set!" ["state-index", "kernel", "index"],
    app "set!" [s "state-count", app "+" [s "state-count", n 1]], s "index"]]

def indexNames : Datum := app "let" [s "index-loop", l [binding "item" (n 0)],
  app "when" [app "<" [s "item", app "vector-length" [s "core-symbols"]],
    app "let" [binding "symbol" (call "vector-ref" ["core-symbols", "item"]),
      app "when" [app "and" [s "symbol", app "nonterminal-symbol?" [s "symbol"]],
        app "let*" [l [binding "name" (app "nonterminal-name" [s "symbol"]),
          binding "known" (app "table-ref" [s "name-index", s "name", b false]),
          binding "node" (call "or" ["known", "node-count"])],
          app "unless" [s "known", call "table-set!" ["name-index", "name", "node"],
            app "set!" [s "names", app "cons" [call "cons" ["name", "node"], s "names"]],
            app "set!" [s "node-count", app "+" [s "node-count", n 1]]],
          call "vector-set!" ["item-nodes", "item", "node"]]]],
    app "index-loop" [app "+" [s "item", n 1]]]]

def ownSeeds : Datum := app "for-each" [app "lambda" [names ["entry"],
  app "let" [l [binding "node" (app "cdr" [s "entry"]), binding "own" (n 0), binding "children" (n 0)],
    app "for-each" [app "lambda" [names ["production"],
      app "let*" [l [binding "id" (app "production-id" [s "production"]),
        binding "child" (app "vector-ref" [s "item-nodes", app "make-core-item" [s "id", n 0, s "layout"]])],
        app "set!" [s "own", call "compiler-index-set-add" ["own", "id"]],
        app "when" [s "child", app "set!" [s "children", call "compiler-index-set-add" ["children", "child"]]]]],
      app "table-ref" [s "productions-by-lhs", app "car" [s "entry"], emptyList]],
    call "vector-set!" ["masks", "node", "own"], call "vector-set!" ["pending", "node", "own"],
    app "unless" [app "zero?" [s "own"], app "vector-set!" [s "queued", s "node", b true],
      call "enqueue!" ["queue", "node"]],
    app "compiler-index-set-for-each" [s "children", app "lambda" [names ["child"],
      app "vector-set!" [s "parents", s "child", app "cons" [s "node", call "vector-ref" ["parents", "child"]]]]]]], s "names"]

def propagate : Datum := app "let" [s "propagate", l [],
  app "unless" [app "queue-empty?" [s "queue"],
    app "let*" [l [binding "node" (app "dequeue!" [s "queue"]),
      binding "delta" (call "vector-ref" ["pending", "node"])],
      app "vector-set!" [s "pending", s "node", n 0],
      app "vector-set!" [s "queued", s "node", b false],
      app "for-each" [app "lambda" [names ["parent"], app "let" [
        binding "novel" (app "compiler-index-set-difference" [s "delta", call "vector-ref" ["masks", "parent"]]),
        app "unless" [app "zero?" [s "novel"],
          app "vector-set!" [s "masks", s "parent", app "compiler-index-set-union" [call "vector-ref" ["masks", "parent"], s "novel"]],
          app "vector-set!" [s "pending", s "parent", app "compiler-index-set-union" [call "vector-ref" ["pending", "parent"], s "novel"]],
          app "unless" [call "vector-ref" ["queued", "parent"],
            app "vector-set!" [s "queued", s "parent", b true], call "enqueue!" ["queue", "parent"]]]]],
        call "vector-ref" ["parents", "node"]]], app "propagate" []]]

def materialize : Datum := app "let" [binding "catalog" (app "make-vector" [app "vector-length" [s "core-symbols"], n 0]),
  app "let" [s "materialize", l [binding "item" (n 0)],
    app "when" [app "<" [s "item", app "vector-length" [s "item-nodes"]],
      app "let" [binding "node" (call "vector-ref" ["item-nodes", "item"]),
        app "when" [s "node", app "vector-set!" [s "catalog", s "item", call "vector-ref" ["masks", "node"]]]],
      app "materialize" [app "+" [s "item", n 1]]]], s "catalog"]

def prediction : Datum := defn "make-core-prediction-catalog" ["core-symbols", "productions-by-lhs", "layout"] [
  app "let" [l [binding "name-index" (app "make-table" [.keyword "test", s "eq?"]),
    binding "names" emptyList, binding "node-count" (n 0),
    binding "item-nodes" (app "make-vector" [app "vector-length" [s "core-symbols"], b false])],
    indexNames, app "let" [l [binding "masks" (app "make-vector" [s "node-count", n 0]),
      binding "pending" (app "make-vector" [s "node-count", n 0]),
      binding "parents" (app "make-vector" [s "node-count", emptyList]),
      binding "queued" (app "make-vector" [s "node-count", b false]), binding "queue" (app "make-Queue" [])],
      ownSeeds, propagate, materialize]]]

end ParserEngine.LRConstructionSource
namespace ParserEngine.LRConstructionWitness
open LRConstructionSource

theorem source_checked : growSource = grow ∧ appendSource = appendState ∧ predictionSource = prediction :=
  ⟨rfl, rfl, rfl⟩
end ParserEngine.LRConstructionWitness
