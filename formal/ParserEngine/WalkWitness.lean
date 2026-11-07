import ParserEngine.SourceData
import ParserEngine.RowSource
import ParserEngine.IteratorSource
-- Source admissions and proofs use native Lean syntax; only reader data is generated.
namespace ParserEngine.WalkSourceWitness
open WalkSource IteratorEncoding IteratorMachine HybridIterator
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
namespace ParserEngine.EffectSourceWitness
open WalkSource SourceEffects IteratorEncoding IteratorMachine
theorem iterative_sites : (sites iterativeSource).length = 2 := by decide +kernel
theorem iterative_effects : (effects iterativeSource).isSome = true := by decide +kernel
theorem resume_checked : (decodeResume iterativeSource).map resumeAdmitted = some true := by decide +kernel
theorem append_checked : appendChecked iterativeSource = true := by decide +kernel
theorem rows_checked : (RowSource.decode rowSource).map RowSource.admitted = some true := by decide +kernel
example : (sites rowSource).length = 2 := by decide +kernel
example : (effects rowSource).isSome = true := by decide +kernel
example : (decodeResume (replace (app "vector-set!" [s "frame", n 1, app "+" [s "i", n 1]]) (app "vector-set!" [s "frame", n 0, app "+" [s "i", n 1]]) iterativeSource)).map resumeAdmitted ≠ some true := by decide +kernel
example : (decodeResume (replace (app "+" [s "i", n 1]) (app "+" [s "i", n 2]) iterativeSource)).map resumeAdmitted ≠ some true := by decide +kernel
example : (decodeResume (replace (app "vector-ref" [s "frame", n 2]) (app "vector-ref" [s "frame", n 1]) iterativeSource)).map resumeAdmitted ≠ some true := by decide +kernel
example : (decodeResume (replace (n 5) (n 4) iterativeSource)).map resumeAdmitted ≠ some true := by decide +kernel
example : appendChecked (replace (get "event-code-append-blocks") (app "reverse" [get "event-code-append-blocks"]) iterativeSource) = false := by decide +kernel
example : effects (replace (app "set!" [s "frames", app "cons" [nextPacket (s "block"), s "frames"]]) (app "set!" [s "frames", s "frames", s "extra"]) iterativeSource) = none := by decide +kernel
example : sites (app "quote" [app "set!" [s "frames", s "ignored"]]) = [] := by decide +kernel
example : (RowSource.decode (replace (app "reverse" [app "vector-ref" [s "indexed", s "state"]]) (app "vector-ref" [s "indexed", s "state"]) rowSource)).map RowSource.admitted ≠ some true := by decide +kernel
example : (RowSource.decode (replace (app "+" [s "state", n 1]) (app "+" [s "state", n 2]) rowSource)).map RowSource.admitted ≠ some true := by decide +kernel
example : (RowSource.decode (replace (l [l [s "state", n 0]]) (l [l [s "state", n 1]]) rowSource)).map RowSource.admitted ≠ some true := by decide +kernel
example : RowSource.decode (replace (app "key-procedure" [s "row"]) (app "value-procedure" [s "row"]) rowSource) = none := by decide +kernel
example : RowSource.decode (replace (s "indexed") (s "wrong-vector") rowSource) = none := by decide +kernel
theorem frame_write (code : Ref) (index delta : Int) (moved : Bool) :
    ∃ program, decodeResume iterativeSource = some program ∧
      decodeFrame (executeResumeWrite program code index delta moved) = some (.chunk code (index + 1) delta moved) :=
  source_chunk_write iterativeSource resume_checked code index delta moved
theorem frame_resume (heap : Nat → Option (List Cell)) (code : Ref) (items : List Ref) (start finish : Int)
    (read : readRecord heap code = some (.chunk items start finish))
    (index : Nat) (head : Ref) (tail : List Ref) (remaining : items.drop index = head :: tail)
    (delta : Int) (moved : Bool) (rest : List Frame) (out : List Observation) (counter : Nat) :
    ∃ program, decodeResume iterativeSource = some program ∧
      IteratorExecutor.advance heap ⟨.chunk code (index : Int) delta moved :: rest, out, counter⟩ =
        some ⟨.next head delta moved :: resumePending program code index delta moved items rest, out, counter⟩ :=
  source_resume_step iterativeSource resume_checked heap code items start finish read index head tail remaining delta moved rest out counter
#print axioms frame_resume
theorem blocks_assign (blocks : List Ref) (delta : Int) (moved : Bool) (environment : String → List Frame) :
    appendSite iterativeSource = some appendSchema ∧
    executeAssignments blocks delta moved environment "frames" =
      blocks.reverse.map (fun block => Frame.next block delta moved) ++ environment "frames" :=
  source_append_assignment iterativeSource append_checked blocks delta moved environment
theorem lr_rows (state : Row → Nat) (payload : Row → Entry) (rows : List Row) (count index : Nat) (inside : index < count)
    (bounded : ∀ row ∈ rows, state row < count) :
    ∃ program result, RowSource.decode rowSource = some program ∧ RowSource.execute program state payload rows count = some result ∧
      result index = (rows.filter (fun row => index == state row)).map payload :=
  RowSource.checked_rows rowSource rows_checked state payload rows count index inside bounded
#print axioms frame_write
#print axioms blocks_assign
#print axioms lr_rows
end ParserEngine.EffectSourceWitness

namespace ParserEngine.IterativeSourceWitness
open WalkSource EffectSourceWitness
/-- Admission consumes all ten clauses of the actual reader-extracted loop. -/
theorem syntax_checked :
    (IteratorSource.decode iterativeSource).map IteratorSource.admitted = some true := by
  decide +kernel
example : (IteratorSource.decode
    (replace (n 1) (n 2) iterativeSource)).map IteratorSource.admitted ≠ some true := by
  decide +kernel
example : (IteratorSource.decode
    (replace (get "event-code-branch-left") (get "event-code-branch-right") iterativeSource)).map IteratorSource.admitted ≠ some true := by
  decide +kernel
example : (IteratorSource.decode
    (replace (app "error" [.text "invalid event instruction"]) (app "void" []) iterativeSource)).map IteratorSource.admitted ≠ some true := by
  decide +kernel
end ParserEngine.IterativeSourceWitness
