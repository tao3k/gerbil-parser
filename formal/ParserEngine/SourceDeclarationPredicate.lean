import ParserEngine.SourceDeclarationReading

/-! Denotation of the reader's first-order predicate. Only the installed
Scheme primitives and short-circuit forms are trusted here; the project's
choice and composition of projections and tests are checked from real source. -/
namespace ParserEngine.SourceDeclarationPredicate
open WalkSource SourceDeclarationReading

inductive View where
  | form | header | owner

def view : View → Datum → Option Datum
  | .form, datum => some datum
  | .header, .list (_ :: header :: _) => some header
  | .owner, .list (_ :: .list (owner :: _) :: _) => some owner
  | _, _ => none

def emitView : View → Datum
  | .form => s "form"
  | .header => app "cadr" [s "form"]
  | .owner => app "caadr" [s "form"]

inductive Test where
  | proper
  | lengthAtLeast (count : Nat)
  | definition
  | named (projection : View)
  | pair (projection : View)
  | all (tests : List Test)
  | any (left right : Test)

def isSymbol (name : String) : Option Datum → Bool
  | some (.symbol value) => value == name
  | _ => false

mutual
  def evaluate (name : String) (datum : Datum) : Test → Bool
    | .proper => match datum with | .list _ => true | _ => false
    | .lengthAtLeast count => match datum with | .list items => count ≤ items.length | _ => false
    | .definition => match datum with | .list (.symbol "def" :: _) => true | _ => false
    | .named projection => isSymbol name (view projection datum)
    | .pair projection => match view projection datum with | some (.list (_ :: _)) => true | _ => false
    | .all tests => evaluateAll name datum tests
    | .any left right => evaluate name datum left || evaluate name datum right
  def evaluateAll (name : String) (datum : Datum) : List Test → Bool
    | [] => true
    | test :: rest => evaluate name datum test && evaluateAll name datum rest
end

mutual
  def emit : Test → Datum
    | .proper => app "list?" [s "form"]
    | .lengthAtLeast count => app ">=" [app "length" [s "form"], n count]
    | .definition => app "eq?" [app "car" [s "form"], quoted "def"]
    | .named projection => app "eq?" [emitView projection, s "name"]
    | .pair projection => app "pair?" [emitView projection]
    | .all tests => app "and" (emitAll tests)
    | .any left right => app "or" [emit left, emit right]
  def emitAll : List Test → List Datum
    | [] => []
    | test :: rest => emit test :: emitAll rest
end

def matcher : Test := .all [.proper, .lengthAtLeast 3, .definition,
  .any (.named .header) (.all [.pair .header, .named .owner])]

/-- No input predicate is assumed: this emitted protocol is the current
source declaration body, including its complete short-circuit ordering. -/
theorem emitted_source : SourceDependencyWitness.matcherSource =
    app "def" [l [s "source-declaration-matches?", s "form", s "name"], emit matcher] := rfl

/-- All representable reader forms, headers, names and body lengths, including
quoted declarations and nonmatching or malformed outer envelopes. -/
theorem predicate_corresponds (name : String) (datum : Datum) :
    evaluate name datum matcher = selects name datum := by
  cases datum with
  | symbol _ | keyword _ | text _ | integer _ | boolean _ => rfl
  | list items =>
    cases items with
    | nil => rfl
    | cons tag rest =>
      cases rest with
      | nil => cases tag <;> simp [evaluate, evaluateAll, matcher, selects]
      | cons header body =>
        cases body with
        | nil => cases tag <;> simp [evaluate, evaluateAll, matcher, selects]
        | cons first trailing =>
          cases tag with
          | symbol tag =>
            by_cases definition : tag = "def"
            · subst tag
              cases header with
              | symbol owner => simp [evaluate, evaluateAll, matcher, selects, view, isSymbol]
              | list parts => cases parts with
                | nil => simp [evaluate, evaluateAll, matcher, selects, view, isSymbol]
                | cons owner rest => cases owner <;> simp [evaluate, evaluateAll, matcher, selects, view, isSymbol]
              | keyword _ | text _ | integer _ | boolean _ => rfl
            · simp [evaluate, evaluateAll, matcher, selects, definition]
          | keyword _ | text _ | integer _ | boolean _ | list _ => rfl

/-- The actual compiled predicate's arbitrary read trace admits precisely one
complete declaration; its meaning is no longer supplied as an input premise. -/
theorem predicate_read_iff (name : String) (forms : List Datum) (declaration : Datum) :
    unique (forms.filter (fun datum => evaluate name datum matcher)) = some declaration ↔
      ∃ found, Reads name forms [] found ∧ found = [declaration] := by
  simpa only [predicate_corresponds, readDeclaration] using read_iff name forms declaration

end ParserEngine.SourceDeclarationPredicate
