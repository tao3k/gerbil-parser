import ParserEngine.BitTraversal
import ParserEngine.CompilerSemanticsSource

/-! Independent complete schemas for the three callback loops and dispatch.
Reader data carries syntax only; arithmetic and callback meaning are separate. -/
namespace ParserEngine.BitTraversalSource
open WalkSource NativeActions.Source CompilerSemanticsSource

def callbackLoop (input : String) (offset : Bool) : Datum :=
  app "let" [s "loop", l [binding "rest" (s input)],
    app "unless" [app "zero?" [s "rest"],
      app "let" [binding "index" (app "first-set-bit" [s "rest"]),
        app "procedure" [if offset then app "+" [s "offset", s "index"] else s "index"],
        app "loop" [app "bitwise-and" [s "rest", app "-" [s "rest", n 1]]]]]]

def fieldLoop : Datum := app "let" [s "loop", l [binding "offset" (s "start")],
  app "when" [app "<" [s "offset", s "limit"],
    app "visit" [call "extract-bit-field" ["width", "offset", "set"], s "offset"],
    app "loop" [app "+" [s "offset", s "width"]]]]

def declaration : Datum := defn "compiler-index-set-for-each" ["set", "procedure"] [
  defn "direct" [] [callbackLoop "set" false],
  defn "visit" ["field", "offset"] [callbackLoop "field" true],
  app "if" [app "fixnum?" [s "set"], callbackLoop "set" false,
    app "let*" [l [binding "limit" (app "integer-length" [s "set"]),
      binding "width" (s "compiler-index-field-width"),
      binding "start" (app "*" [s "width", app "quotient" [app "first-set-bit" [s "set"], s "width"]]),
      binding "fields" (app "quotient" [app "+" [app "-" [s "limit", s "start"], app "-" [s "width", n 1]], s "width"])],
      app "if" [app "<" [app "bit-count" [s "set"], s "fields"], app "direct" [], fieldLoop]]]]

def fieldWidth : Datum := app "def" [s "compiler-index-field-width",
  app "integer-length" [app "##greatest-fixnum" []]]

def decode (source widthSource : Datum) : Option Unit :=
  if source = declaration ∧ widthSource = fieldWidth then some () else none

theorem decoded (source widthSource : Datum) (admitted : decode source widthSource = some ()) :
    source = declaration ∧ widthSource = fieldWidth := by
  unfold decode at admitted
  split at admitted
  · assumption
  · cases admitted

theorem changed_source_rejected (source widthSource : Datum) (changed : source ≠ declaration) :
    decode source widthSource = none := by simp [decode, changed]

theorem changed_width_rejected (source widthSource : Datum) (changed : widthSource ≠ fieldWidth) :
    decode source widthSource = none := by simp [decode, changed]

/-- The matching declaration's chosen branch has the proved callback list.
This restricted decoder does not establish arbitrary Scheme interpretation. -/
def execute (source widthSource : Datum) (mask width population : Nat) (positive : 0 < width) (fixnum : Bool) (_zeroClass : mask = 0 → fixnum = true) : Option (List Nat) :=
  (decode source widthSource).map fun _ => BitTraversal.dispatch mask width population positive fixnum

theorem execute_exact (source widthSource : Datum) (admitted : decode source widthSource = some ())
    (mask width population : Nat) (positive : 0 < width) (fixnum : Bool) (zeroClass : mask = 0 → fixnum = true) :
    execute source widthSource mask width population positive fixnum zeroClass = some (BitTraversal.direct mask) := by
  simp only [execute, admitted, Option.map_some, BitTraversal.dispatch_exact]

end ParserEngine.BitTraversalSource
namespace ParserEngine.BitTraversalWitness
set_option autoImplicit false in
theorem source_checked : source = BitTraversalSource.declaration ∧ widthSource = BitTraversalSource.fieldWidth := ⟨rfl, rfl⟩
end ParserEngine.BitTraversalWitness
