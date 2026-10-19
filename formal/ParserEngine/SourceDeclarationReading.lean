import ParserEngine.SourceDependencyWitness
import ParserEngine.SourceDependencies

/-! The project reader's complete read/cons/reverse/unique control protocol.
The installed reader supplies successive syntax values or raises; the project
performs selection. No chosen declaration or matching predicate is a premise. -/
namespace ParserEngine.SourceDeclarationReading
open WalkSource

/-- Complete forms, including variable declarations, are selected exactly as in
source-declaration-matches?. Function-only admission belongs to transformation. -/
def selects (name : String) : Datum → Bool
  | .list (.symbol "def" :: header :: _ :: _) =>
    match header with
    | .symbol owner => owner == name
    | .list (.symbol owner :: _) => owner == name
    | _ => false
  | _ => false

def matcherBody : Datum := app "and" [app "list?" [s "form"],
  app ">=" [app "length" [s "form"], n 3],
  app "eq?" [app "car" [s "form"], quoted "def"],
  app "or" [app "eq?" [app "cadr" [s "form"], s "name"],
    app "and" [app "pair?" [app "cadr" [s "form"]],
      app "eq?" [app "caadr" [s "form"], s "name"]]]]

theorem extracted_matcher : SourceDependencyWitness.matcherSource =
    app "def" [l [s "source-declaration-matches?", s "form", s "name"], matcherBody] := rfl

def readerLoop : Datum := app "let" [s "loop", l [l [s "found", app "quote" [l []]]],
  app "let" [l [s "form", app "read" [s "port"]],
    app "cond" [l [app "eof-object?" [s "form"], app "reverse" [s "found"]],
      l [app "source-declaration-matches?" [s "form", s "name"],
        app "loop" [app "cons" [s "form", s "found"]]],
      l [s "else", app "loop" [s "found"]]]]]

theorem extracted_reader : SourceDependencyWitness.readerSource =
    app "def" [l [s "read-source-definition-port", s "port", s "name"],
      app "let" [l [s "found", readerLoop],
        app "unless" [app "=" [app "length" [s "found"], n 1],
          app "error" [.text "expected one source declaration", s "name"]],
        app "car" [s "found"]]] := rfl

/-- Operational transcript: each successful read consumes precisely one form.
EOF is separate from a reader exception; exceptions have no successful trace. -/
inductive Reads (name : String) : List Datum → List Datum → List Datum → Prop where
  | eof : Reads name [] reversed reversed.reverse
  | hit (matched : selects name form = true)
      (tail : Reads name rest (form :: reversed) result) :
      Reads name (form :: rest) reversed result
  | skip (unmatched : selects name form = false)
      (tail : Reads name rest reversed result) :
      Reads name (form :: rest) reversed result

/-- All accumulator prefixes and arbitrary source lengths; every complete form
is retained unchanged, including all trailing body expressions. -/
theorem reads_exact (trace : Reads name forms reversed result) :
    result = reversed.reverse ++ forms.filter (selects name) := by
  induction trace with
  | eof => simp
  | @hit form rest reversed result matched tail ih =>
    simpa [List.filter_cons, matched, List.reverse_cons, List.append_assoc] using ih
  | @skip form rest reversed result unmatched tail ih =>
    simpa [List.filter_cons, unmatched] using ih

theorem reads_total (name : String) (forms reversed : List Datum) :
    ∃ result, Reads name forms reversed result := by
  induction forms generalizing reversed with
  | nil => exact ⟨_, .eof⟩
  | cons form rest ih =>
    cases matched : selects name form with
    | false => obtain ⟨result, trace⟩ := ih reversed; exact ⟨result, .skip matched trace⟩
    | true => obtain ⟨result, trace⟩ := ih (form :: reversed); exact ⟨result, .hit matched trace⟩

def unique : List Datum → Option Datum
  | [declaration] => some declaration
  | _ => none

def readDeclaration (name : String) (forms : List Datum) : Option Datum :=
  unique (forms.filter (selects name))

/-- Both directions connect the native EOF/uniqueness path to independent
whole-file selection. Reaching the first match is never enough for success. -/
theorem read_iff (name : String) (forms : List Datum) (declaration : Datum) :
    readDeclaration name forms = some declaration ↔
      ∃ found, Reads name forms [] found ∧ found = [declaration] := by
  have one (values : List Datum) : unique values = some declaration ↔ values = [declaration] := by
    cases values with
    | nil => simp [unique]
    | cons head tail => cases tail <;> simp [unique]
  constructor
  · intro success
    obtain ⟨found, trace⟩ := reads_total name forms []
    exact ⟨found, trace, (reads_exact trace).trans ((one _).mp success)⟩
  · rintro ⟨found, trace, rfl⟩
    apply (one _).mpr
    simpa using (reads_exact trace).symm

theorem selected_occurs (success : readDeclaration name forms = some declaration) :
    declaration ∈ forms ∧ selects name declaration = true := by
  obtain ⟨found, trace, rfl⟩ := (read_iff name forms declaration).mp success
  have h : declaration ∈ forms.filter (selects name) := by
    have exactForms : [declaration] = forms.filter (selects name) := by simpa using reads_exact trace
    rw [← exactForms]; simp
  exact List.mem_filter.mp h

/-- Two occurrences of the same selected declaration remain two occurrences;
even byte-identical duplicate definitions must reject rather than deduplicate. -/
theorem duplicate_rejects (matched : selects name declaration = true) :
    readDeclaration name [declaration, declaration] = none := by
  simp [readDeclaration, matched, unique]

theorem quoted_definition_is_not_selected (name : String) (declaration : Datum) :
    selects name (app "quote" [declaration]) = false := rfl

/-- A read exception cannot be treated as a successful EOF after a valid prefix. -/
inductive Outcome where
  | eof (forms : List Datum)
  | error (leading : List Datum)

def finish (name : String) : Outcome → Option Datum
  | .eof forms => readDeclaration name forms
  | .error _ => none

theorem read_error_rejects (name : String) (leading : List Datum) :
    finish name (.error leading) = none := rfl

end ParserEngine.SourceDeclarationReading
