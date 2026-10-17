import ParserEngine.LRTemplateSource

/-! Complete builder envelope binds interning and transition publication to
one actual FIFO owner. Source admission is not a proof of arbitrary Scheme. -/
namespace ParserEngine.LRTransitionSource
open WalkSource NativeActions.Source CompilerSemanticsSource LRConstructionSource LRTemplateSource

def targets : Datum := app "for-each" [app "lambda" [names ["kernel"],
  app "let*" [l [binding "symbol" (app "vector-ref" [s "symbols", app "vector-ref" [s "kernel", n 1]]),
    binding "mask" (app "vector-ref" [s "kernel", n 2]),
    binding "existing" (app "table-ref" [s "state-index", s "mask", b false]),
    binding "target-index" (app "or" [s "existing", app "append-state!" [s "mask"]])],
    app "set!" [s "rows", app "cons" [call "cons" ["symbol", "target-index"], s "rows"]],
    app "unless" [s "existing", call "enqueue!" ["queue", "target-index"]]]],
  app "lr0-state-kernels" [app "car" [s "plan"], app "cdr" [s "plan"], s "symbol-ids", s "masks", s "first-items"]]

def traceProgress : Datum := app "when" [app "and" [s "trace?", app "zero?" [app "modulo" [s "processed-count", n 100]]],
  app "display" [.text "[gerbil-parser-lr] lr0-states="], app "display" [s "state-count"],
  app "display" [.text " processed="], app "displayln" [s "processed-count"], app "force-output" []]

def workQueue : Datum := app "let" [binding "queue" (app "make-Queue" []),
  app "enqueue!" [s "queue", n 0], app "let" [s "loop", l [],
    app "if" [app "queue-empty?" [s "queue"],
      call "values" ["states", "state-transitions", "state-count", "productions-by-lhs", "processed-count"],
      app "let" [binding "index" (app "dequeue!" [s "queue"]),
        app "set!" [s "processed-count", app "+" [s "processed-count", n 1]], traceProgress,
        app "let" [l [binding "rows" emptyList, binding "plan" (call "vector-ref" ["kernel-plans", "index"])],
          targets, app "vector-set!" [s "state-transitions", s "index", app "reverse" [s "rows"]]],
        app "loop" []]]]]

def builder : Datum := defn "build-lr0-automaton" ["productions", "table", "layout", "core-symbols"] [
  app "let*" [l [binding "productions-by-lhs" (app "production-index-by-lhs" [s "productions"]),
    binding "prediction-catalog" (call "make-core-prediction-catalog" ["core-symbols", "productions-by-lhs", "layout"]),
    binding "initial-kernel" (app "compiler-index-set-add" [n 0, app "make-core-item" [n 0, n 0, s "layout"]]),
    binding "templates" (app "make-table" [.keyword "test", s "equal?"]),
    binding "states" (app "make-vector" [n 128, b false]),
    binding "state-transitions" (app "make-vector" [n 128, emptyList]),
    binding "kernel-plans" (app "make-vector" [n 128, b false]),
    binding "state-index" (app "make-table" [.keyword "test", s "equal?"]),
    binding "state-count" (n 1), binding "processed-count" (n 0),
    binding "trace?" (app "equal?" [app "getenv" [.text "GERBIL_PARSER_LR_TRACE", b false], .text "1"])],
    app "let-values" [l [l [names ["symbols", "symbol-ids"], app "make-lr0-symbol-catalog" [s "core-symbols"]]],
      app "let" [l [binding "masks" (app "make-vector" [app "vector-length" [s "symbols"], n 0]),
        binding "first-items" (app "make-vector" [app "vector-length" [s "symbols"], n 0])],
        templateFor,
        app "let" [binding "template" (app "template-for" [s "initial-kernel"]),
          app "vector-set!" [s "states", n 0, call "lr0-closure" ["initial-kernel", "template"]],
          app "vector-set!" [s "kernel-plans", n 0, call "cons" ["initial-kernel", "template"]]],
        app "table-set!" [s "state-index", s "initial-kernel", n 0], grow, appendState, workQueue]]]]

def checkedPublish {Template : Type} (source : Datum) (compute : Nat → List Nat × Template)
    (before : StateInterning.Registry Nat Template) (groups : List (Nat × Nat)) :
    Option (TransitionPublication.Published Template) :=
  if source = builder then some (TransitionPublication.publish compute before groups) else none

theorem published_rows {Template : Type} (source : Datum) (compute : Nat → List Nat × Template)
    (before : StateInterning.Registry Nat Template) (groups : List (Nat × Nat))
    (after : TransitionPublication.Published Template) (checked : checkedPublish source compute before groups = some after)
    (symbol target : Nat) :
    (symbol, target) ∈ after.rows ↔ ∃ kernel, (symbol, kernel) ∈ groups ∧ after.registry.index kernel = some target := by
  unfold checkedPublish at checked
  split at checked
  next matched => cases checked; exact TransitionPublication.row_exact compute before groups symbol target
  next mismatch => cases checked

end ParserEngine.LRTransitionSource
namespace ParserEngine.LRTransitionWitness
open LRTransitionSource

theorem source_checked : builderSource = builder := rfl
end ParserEngine.LRTransitionWitness
