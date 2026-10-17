-- Generated from the current Scheme reader tree; regenerate with t/generate-walk-source-witness.ss.
import ParserEngine.WalkSource
namespace ParserEngine.WalkSourceWitness
open WalkSource IteratorEncoding IteratorMachine HybridIterator
def source : Datum :=
(l [(s "def"), (l [(s "walk"), (s "code"), (s "delta"), (s "moved?"), (s "depth")]), (l [(s "cond"), (l [(l [(s "not"), (s "code")]), (l [(s "void")])]), (l [(l [(s ">="), (s "depth"), (n 64)]), (l [(s "iterative"), (s "code"), (s "delta"), (s "moved?")])]), (l [(l [(s "event-program-value?"), (s "code")]), (l [(s "let*"), (l [(l [(s "kind"), (l [(s "event-program-value-kind"), (s "code")])]), (l [(s "node-state"), (l [(s "and"), (s "kind"), (l [(l [(s "lambda"), (l [(s "op"), (s "name"), (s "offset"), (s "shift"), (s "moved"), (s "context")]), (s "effect"), (s "...")]), (l [(s "quote"), (s "open-node")]), (s "kind"), (l [(s "event-program-value-start"), (s "code")]), (s "delta"), (s "moved?"), (b false)])])])]), (l [(s "walk"), (l [(s "event-program-value-body"), (s "code")]), (s "delta"), (s "moved?"), (l [(s "+"), (s "depth"), (n 1)])]), (l [(s "if"), (s "kind"), (l [(l [(s "lambda"), (l [(s "op"), (s "name"), (s "offset"), (s "shift"), (s "moved"), (s "context")]), (s "effect"), (s "...")]), (l [(s "quote"), (s "close-node")]), (s "kind"), (l [(s "event-program-value-end"), (s "code")]), (s "delta"), (s "moved?"), (s "node-state")]), (l [(l [(s "lambda"), (l [(s "op"), (s "name"), (s "offset"), (s "shift"), (s "moved"), (s "context")]), (s "effect"), (s "...")]), (l [(s "quote"), (s "boundary")]), (b false), (l [(s "event-program-value-end"), (s "code")]), (s "delta"), (s "moved?"), (b false)])])])]), (l [(l [(s "event-field-frame?"), (s "code")]), (l [(l [(s "lambda"), (l [(s "op"), (s "name"), (s "offset"), (s "shift"), (s "moved"), (s "context")]), (s "effect"), (s "...")]), (l [(s "quote"), (s "open-field")]), (l [(s "event-field-frame-name"), (s "code")]), (l [(s "event-field-frame-start"), (s "code")]), (s "delta"), (s "moved?"), (b false)]), (l [(s "walk"), (l [(s "event-field-frame-body"), (s "code")]), (s "delta"), (s "moved?"), (l [(s "+"), (s "depth"), (n 1)])]), (l [(l [(s "lambda"), (l [(s "op"), (s "name"), (s "offset"), (s "shift"), (s "moved"), (s "context")]), (s "effect"), (s "...")]), (l [(s "quote"), (s "close-field")]), (l [(s "event-field-frame-name"), (s "code")]), (l [(s "event-field-frame-end"), (s "code")]), (s "delta"), (s "moved?"), (b false)])]), (l [(l [(s "token?"), (s "code")]), (l [(l [(s "lambda"), (l [(s "op"), (s "name"), (s "offset"), (s "shift"), (s "moved"), (s "context")]), (s "effect"), (s "...")]), (l [(s "quote"), (s "token")]), (s "code"), (l [(s "token-start"), (s "code")]), (s "delta"), (s "moved?"), (b false)])]), (l [(l [(s "event-code-view?"), (s "code")]), (l [(s "walk"), (l [(s "event-code-view-code"), (s "code")]), (l [(s "+"), (s "delta"), (l [(s "event-code-view-delta"), (s "code")])]), (b true), (l [(s "+"), (s "depth"), (n 1)])])]), (l [(l [(s "event-code-chunk?"), (s "code")]), (l [(s "let*"), (l [(l [(s "items"), (l [(s "event-code-chunk-items"), (s "code")])]), (l [(s "n"), (l [(s "vector-length"), (s "items")])])]), (l [(s "let"), (s "loop"), (l [(l [(s "i"), (n 0)])]), (l [(s "when"), (l [(s "<"), (s "i"), (s "n")]), (l [(s "walk"), (l [(s "vector-ref"), (s "items"), (s "i")]), (s "delta"), (s "moved?"), (l [(s "+"), (s "depth"), (n 1)])]), (l [(s "loop"), (l [(s "+"), (s "i"), (n 1)])])])])])]), (l [(l [(s "event-code-append?"), (s "code")]), (l [(s "walk"), (l [(s "event-code-append-prefix"), (s "code")]), (s "delta"), (s "moved?"), (l [(s "+"), (s "depth"), (n 1)])]), (l [(s "for-each"), (l [(s "lambda"), (l [(s "block")]), (l [(s "walk"), (s "block"), (s "delta"), (s "moved?"), (l [(s "+"), (s "depth"), (n 1)])])]), (l [(s "reverse"), (l [(s "event-code-append-blocks"), (s "code")])])]), (l [(s "walk"), (l [(s "event-code-append-tail"), (s "code")]), (s "delta"), (s "moved?"), (l [(s "+"), (s "depth"), (n 1)])])]), (l [(l [(s "event-code-branch?"), (s "code")]), (l [(s "walk"), (l [(s "event-code-branch-left"), (s "code")]), (s "delta"), (s "moved?"), (l [(s "+"), (s "depth"), (n 1)])]), (l [(s "walk"), (l [(s "event-code-branch-right"), (s "code")]), (s "delta"), (s "moved?"), (l [(s "+"), (s "depth"), (n 1)])])]), (l [(s "else"), (l [(s "error"), (.text "invalid event instruction")])])])])
theorem syntax_checked : (decode source).map admitted = some true := by decide +kernel
theorem cutoff_checked : (decode source).map Walker.cutoff = some 64 := by decide +kernel
example : rejects (replace (app "+" [s "depth", n 1]) (app "+" [s "depth", n 2]) source) = true := by decide +kernel
example : rejects (replace (b true) (s "moved?") source) = true := by decide +kernel
example : rejects (replace (get "event-code-branch-left") (get "event-code-branch-right") source) = true := by decide +kernel
example : rejects (replace (app "reverse" [get "event-code-append-blocks"]) (get "event-code-append-blocks") source) = true := by decide +kernel
example : rejects (replace (callback "close-node" (s "kind") (get "event-program-value-end") (s "node-state")) (callback "close-node" (s "kind") (get "event-program-value-end") (b false)) source) = true := by decide +kernel
example : rejects (replace (app "+" [s "i", n 1]) (app "+" [s "i", n 2]) source) = true := by decide +kernel
example : rejects (replace (app "void" []) (app "begin" [app "void" [], app "unexpected-effect" []]) source) = true := by decide +kernel
example : (decode (replace (n 64) (n 63) source)).map admitted = some true := by decide +kernel
example : rejects (swapPriority source) = true := by decide +kernel
theorem extracted_plan (address : Ref) (record : Record) (delta : Int) (moved : Bool) (depth : Nat) (rest : List Activation) (out : List Observation) (counter : Nat) :
    ∃ program, decode source = some program ∧ planSource program address record delta moved depth rest out counter = HybridIterator.plan address record delta moved depth rest out counter := by
  have checked := syntax_checked
  cases parsed : decode source with
  | none => simp [parsed] at checked
  | some program =>
    exact ⟨program, rfl, admitted_plan program (by simpa [parsed] using checked)⟩
theorem extracted_execution (heap : Nat → Option (List Cell)) (domain : List Ref) (rank : Ref → Nat)
    (graphChecked : Extraction.verify heap domain rank = true) (address : Ref) (inside : address ∈ domain) (depth : Nat) :
    ∃ program meaning budget fuel last, decode source = some program ∧
      Extraction.generate heap (rank address + 1) address = some meaning ∧ Extract heap address meaning ∧
      runSource program heap budget fuel (HybridIterator.initial address depth) = some last ∧ last.pending = [] ∧
      last.emitted.map Observation.event = EventIterator.meaning meaning 0 false ∧
      runControl last.emitted ⟨0, []⟩ = some ⟨last.nextNode, []⟩ :=
  checked_syntax_execution source syntax_checked heap domain rank graphChecked address inside depth
#print axioms extracted_execution
#print axioms extracted_plan
#print axioms syntax_checked
#print axioms cutoff_checked
end ParserEngine.WalkSourceWitness
