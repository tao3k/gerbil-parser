import ParserEngine.LoweringBranchSource
import ParserEngine.SchemeCaseCorrespondence

/-! Concrete source envelopes for recursive protocol syntax. Recognition
projection alone cannot recover literal/token/layout tags or annotation data.
They remain explicit encodings, rather than being guessed from terminal IDs. -/
namespace ParserEngine.LoweringDispatchSyntax
open WalkSource SchemeTableEvaluation

inductive TerminalTag where
  | literal | token | layoutStart | layoutNext
  deriving DecidableEq

def TerminalTag.name : TerminalTag → String
  | .literal => "literal"
  | .token => "token"
  | .layoutStart => "layout-start"
  | .layoutNext => "layout-next"

structure Encoding where
  terminalTag : Nat → TerminalTag
  terminalPayload : Nat → Datum
  name : Nat → String
  label : Nat → Datum
  direction : Nat → Datum
  rank : Nat → Datum
  layoutOptions : List Datum

def tag (encoding : Encoding) : LoweringTraversal.Syntax → String
  | .empty => "empty"
  | .layoutEnd => "layout-end"
  | .terminal key => (encoding.terminalTag key).name
  | .reference _ => "reference"
  | .sequence _ => "sequence"
  | .choice _ => "choice"
  | .optional _ => "optional"
  | .repeat _ => "repeat"
  | .repeat1 _ => "repeat1"
  | .field _ _ => "field"
  | .alias _ _ => "alias"
  | .precedence _ _ _ => "precedence"

def encode (encoding : Encoding) : LoweringTraversal.Syntax → Datum
  | .empty => l [s "empty"]
  | .layoutEnd => l (s "layout-end" :: encoding.layoutOptions)
  | .terminal key => l [s (encoding.terminalTag key).name, encoding.terminalPayload key]
  | .reference name => l [s "reference", s (encoding.name name)]
  | .sequence children => l (s "sequence" :: children.map (encode encoding))
  | .choice children => l (s "choice" :: children.map (encode encoding))
  | .optional child => l [s "optional", encode encoding child]
  | .repeat child => l [s "repeat", encode encoding child]
  | .repeat1 child => l [s "repeat1", encode encoding child]
  | .field label child => l [s "field", encoding.label label, encode encoding child]
  | .alias label child => l [s "alias", encoding.label label, encode encoding child]
  | .precedence direction rank child =>
      l [s "precedence", encoding.direction direction, encoding.rank rank, encode encoding child]

/-- Covers arbitrary depth and n-ary children without a finite fixture bound. -/
theorem tagged (encoding : Encoding) (expression : LoweringTraversal.Syntax) :
    ∃ arguments, encode encoding expression = l (s (tag encoding expression) :: arguments) := by
  cases expression <;> simp [encode, tag, l, s]

/-- Evaluate the actual selector expression from the current heap. The proof
never substitutes a precomputed branch ID for a Scheme evaluation. -/
theorem selector_eval (encoding : Encoding) (expression : LoweringTraversal.Syntax)
    (read : resolve env heap "expression" = some (datumValue (encode encoding expression)))
    (unshadowed : env "car" = none) :
    Eval env heap (app "car" [s "expression"]) (.symbol (tag encoding expression)) heap := by
  obtain ⟨arguments, shape⟩ := tagged encoding expression
  apply Eval.apply (.cons (.variable read) .nil) _ unshadowed
  rw [shape]
  rfl

/-- Retain actual case source, including local definitions preceding lower-to's
case. This is extraction only; source execution is proved separately. -/
def caseSource : Datum → Datum
  | .list (_ :: _ :: body) => body.getLast?.getD (.boolean false)
  | _ => .boolean false

def clauses (source : Datum) : List Datum :=
  match caseSource source with
  | .list (.symbol "case" :: _ :: alternatives) => alternatives
  | _ => []

def body : Datum → List Datum
  | .list (_ :: statements) => statements
  | _ => []

/-- A recognition-only terminal ID cannot recover its concrete source tag.
Any complete source refinement must carry the Encoding contract explicitly. -/
theorem tag_not_recoverable (encoding : Encoding) :
    ¬ ∃ recover : LoweringTraversal.Syntax → String,
      ∀ replacement : Encoding, ∀ expression, recover expression = tag replacement expression := by
  rintro ⟨recover, correct⟩
  have literal := correct {encoding with terminalTag := fun _ => .literal} (.terminal 0)
  have token := correct {encoding with terminalTag := fun _ => .token} (.terminal 0)
  have impossible : "literal" = "token" := literal.symm.trans token
  exact (by decide : ("literal" : String) ≠ "token") impossible

end ParserEngine.LoweringDispatchSyntax
