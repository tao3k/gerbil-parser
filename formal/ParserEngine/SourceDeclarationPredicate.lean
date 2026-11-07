import ParserEngine.SourceDeclarationReading

/-! Denotation of the reader's first-order predicate. Only the installed
Scheme primitives and short-circuit forms are trusted here; the project's
choice and composition of projections and tests are checked from real source. -/
namespace ParserEngine.SourceDeclarationPredicate
open WalkSource SourceDeclarationReading

inductive View where
  | form | header | owner | parameters

def view : View → Datum → Option Datum
  | .form, datum => some datum
  | .header, .list (_ :: header :: _) => some header
  | .owner, .list (_ :: .list (owner :: _) :: _) => some owner
  | .parameters, .list (_ :: .list (_ :: parameters) :: _) => some (.list parameters)
  | _, _ => none

def emitView (subject : String) : View → Datum
  | .form => s subject
  | .header => app "cadr" [s subject]
  | .owner => app "caadr" [s subject]
  | .parameters => app "cdadr" [s subject]

inductive Test where
  | proper
  | lengthAtLeast (count : Nat)
  | definition
  | named (projection : View)
  | pair (projection : View)
  | symbol (projection : View)
  | properView (projection : View)
  | sameHeader (expected : Datum)
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
    | .symbol projection => match view projection datum with | some (.symbol _) => true | _ => false
    | .properView projection => match view projection datum with | some (.list _) => true | _ => false
    | .sameHeader expected => decide (view .header datum = some expected)
    | .all tests => evaluateAll name datum tests
    | .any left right => evaluate name datum left || evaluate name datum right
  def evaluateAll (name : String) (datum : Datum) : List Test → Bool
    | [] => true
    | test :: rest => evaluate name datum test && evaluateAll name datum rest
end

mutual
  def emit (subject : String) : Test → Datum
    | .proper => app "list?" [s subject]
    | .lengthAtLeast count => app ">=" [app "length" [s subject], n count]
    | .definition => app "eq?" [app "car" [s subject], quoted "def"]
    | .named projection => app "eq?" [emitView subject projection, s "name"]
    | .pair projection => app "pair?" [emitView subject projection]
    | .symbol projection => app "symbol?" [emitView subject projection]
    | .properView projection => app "list?" [emitView subject projection]
    | .sameHeader _ => app "equal?" [emitView subject .header, s "header"]
    | .all tests => app "and" (emitAll subject tests)
    | .any left right => app "or" [emit subject left, emit subject right]
  def emitAll (subject : String) : List Test → List Datum
    | [] => []
    | test :: rest => emit subject test :: emitAll subject rest
end

def matcher : Test := .all [.proper, .lengthAtLeast 3, .definition,
  .any (.named .header) (.all [.pair .header, .named .owner])]

/-- No input predicate is assumed: this emitted protocol is the current
source declaration body, including its complete short-circuit ordering. -/
theorem emitted_source : SourceDependencyWitness.matcherSource =
    app "def" [l [s "source-declaration-matches?", s "form", s "name"], emit "form" matcher] := rfl

/-- All representable reader forms, headers, names and body lengths, including
quoted declarations and nonmatching or malformed outer envelopes. -/
theorem predicate_corresponds (name : String) (datum : Datum) :
    evaluate name datum matcher = selects name datum := by
  cases datum with
  | symbol _ | keyword _ | text _ | integer _ | negative _ | boolean _ => rfl
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
              | keyword _ | text _ | integer _ | negative _ | boolean _ => rfl
            · simp [evaluate, evaluateAll, matcher, selects, definition]
          | keyword _ | text _ | integer _ | negative _ | boolean _ | list _ => rfl

/-- The actual compiled predicate's arbitrary read trace admits precisely one
complete declaration; its meaning is no longer supplied as an input premise. -/
theorem predicate_read_iff (name : String) (forms : List Datum) (declaration : Datum) :
    unique (forms.filter (fun datum => evaluate name datum matcher)) = some declaration ↔
      ∃ found, Reads name forms [] found ∧ found = [declaration] := by
  simpa only [predicate_corresponds, readDeclaration] using read_iff name forms declaration

end ParserEngine.SourceDeclarationPredicate
