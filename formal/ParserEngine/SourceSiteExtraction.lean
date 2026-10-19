import ParserEngine.SourceDeclarationReading
import LeanPoo.Functional.Access

/-! Shared ordered source-site extraction. Literal syntax is a barrier. This
structural protocol is not hygienic binding resolution or a Scheme interpreter.
The native declaration and mutation adapters use the same engine traversal. -/
namespace ParserEngine.SourceSiteExtraction
open WalkSource LeanPoo.Functional

def quoted : Datum → Bool
  | .list (.symbol tag :: _) => tag == "quote" || tag == "quote-syntax" || tag == "syntax"
  | _ => false

mutual
  def collect (selected : Datum → Bool) : Datum → List Datum
    | .list items =>
      if items.isEmpty || quoted (.list items) then [] else
        (if selected (.list items) then [.list items] else []) ++ children selected items
    | _ => []
  def children (selected : Datum → Bool) : List Datum → List Datum
    | [] => []
    | first :: rest => collect selected first ++ children selected rest
end

/- An independently selected-free preorder specification retains duplicates.
Filtering it specifies the complete sequence, not only set membership. -/
mutual
  def preorder : Datum → List Datum
    | .list items => if items.isEmpty || quoted (.list items) then [] else
        .list items :: preorderChildren items
    | _ => []
  def preorderChildren : List Datum → List Datum
    | [] => []
    | first :: rest => preorder first ++ preorderChildren rest
end

theorem ordered (selected : Datum → Bool) (form : Datum) :
    collect selected form = (preorder form).filter selected := by
  induction form using Datum.rec
      (motive_2 := fun forms => children selected forms = (preorderChildren forms).filter selected) with
  | symbol | keyword | text | integer | boolean => simp [collect, preorder]
  | list items ih =>
    cases empty : items.isEmpty <;> cases blocked : quoted (.list items) <;>
      cases chosen : selected (.list items) <;> simp_all [collect, preorder]
  | nil => simp [children, preorderChildren]
  | cons first rest firstExact restExact =>
    simp [children, preorderChildren, List.filter_append, firstExact, restExact]

/- Independent ancestry predicate, without collection order or accumulators. -/
mutual
  def Visible (selected : Datum → Bool) (target : Datum) : Datum → Prop
    | .list items =>
      if items.isEmpty || quoted (.list items) then False else
        (selected (.list items) = true ∧ target = .list items) ∨ Below selected target items
    | _ => False
  def Below (selected : Datum → Bool) (target : Datum) : List Datum → Prop
    | [] => False
    | first :: rest => Visible selected target first ∨ Below selected target rest
end

/-- Every predicate, tree depth and branching factor; no finite helper list. -/
theorem complete (selected : Datum → Bool) (target form : Datum) :
    target ∈ collect selected form ↔ Visible selected target form := by
  induction form using Datum.rec
      (motive_2 := fun forms => target ∈ children selected forms ↔ Below selected target forms) with
  | symbol | keyword | text | integer | boolean => simp [collect, Visible]
  | list items ih =>
    cases empty : items.isEmpty <;> cases blocked : quoted (.list items) <;>
      cases matched : selected (.list items) <;> simp_all [collect, Visible]
  | nil => simp [children, Below]
  | cons first rest firstExact restExact => simp [children, Below, firstExact, restExact]

/-- Reuse the existing source-matcher denotation, then apply the native
procedure-header restriction used by the nested adapter. -/
def named (name : String) (form : Datum) : Bool :=
  SourceDeclarationReading.selects name form &&
    match form with
    | .list (_ :: .list (_ :: _) :: _) => true
    | _ => false


def select (name : String) (form : Datum) : Option Datum :=
  SourceDeclarationReading.unique (collect (named name) form)

theorem selected_iff (name : String) (form target : Datum) :
    select name form = some target ↔ collect (named name) form = [target] := by
  unfold select
  cases result : collect (named name) form with
  | nil => simp [SourceDeclarationReading.unique]
  | cons first rest => cases rest <;> simp [SourceDeclarationReading.unique]

theorem selected_visible (selected : select name form = some target) : Visible (named name) target form := by
  apply (complete (named name) target form).mp
  rw [(selected_iff name form target).mp selected]
  simp

theorem duplicate_rejected (duplicated : collect (named name) form = [target, target]) :
    select name form = none := by simp [select, duplicated, SourceDeclarationReading.unique]

theorem quoted_absent (blocked : quoted form = true) : collect selected form = [] := by
  cases form <;> simp_all [quoted, collect]

/-- Use the same complete outer-source reader before selecting a nested site. -/
def readNested (outer name : String) (forms : List Datum) : Option Datum :=
  (SourceDeclarationReading.readDeclaration outer forms).bind (select name)

theorem read_nested_sound (selected : readNested outer name forms = some target) :
    ∃ owner, SourceDeclarationReading.readDeclaration outer forms = some owner ∧
      owner ∈ forms ∧ collect (named name) owner = [target] ∧ Visible (named name) target owner := by
  unfold readNested at selected
  cases read : SourceDeclarationReading.readDeclaration outer forms with
  | none => simp [read] at selected
  | some owner =>
    have found : select name owner = some target := by simpa [read] using selected
    exact ⟨owner, rfl, (SourceDeclarationReading.selected_occurs read).1,
      (selected_iff name owner target).mp found, selected_visible found⟩

/-- LeanPoo retains the exact producer of the full read/select request.
Neither a caller-chosen outer tree nor same-named literal data supplies it. -/
def provider (outer : String) (forms : List Datum) : Provider Unit String (fun _ _ => Datum) :=
  fun name => (readNested outer name forms).map (fun declaration _ => declaration)

theorem factory_visible (outer : String) (forms : List Datum) (keys : List String)
    {factories : Requirements.Factories Unit (fun _ (_ : String) => Datum) keys}
    (ready : Requirements.prepare (provider outer forms) keys = .ok factories) (member : name ∈ keys) :
    ∃ owner, SourceDeclarationReading.readDeclaration outer forms = some owner ∧
      owner ∈ forms ∧ collect (named name) owner = [Requirements.factoryAt factories name member ()] ∧
      Visible (named name) (Requirements.factoryAt factories name member ()) owner := by
  have selected := Requirements.factoryAt_selected (provider outer forms) keys factories
    ((Requirements.prepare_ok_iff _ _ _).mp ready) name member
  cases found : readNested outer name forms with
  | none => simp [provider, found] at selected
  | some declaration =>
    have exactFactory : (fun _ : Unit => declaration) = Requirements.factoryAt factories name member := by
      simpa [provider, found] using selected
    rw [← exactFactory]
    exact read_nested_sound found

/-- Current native traversal includes complete recursion and ordered append.
Primitive list/map/append and stable predicate behavior remain host contracts. -/
theorem extracted_sites : SourceDependencyWitness.sitesSource =
    app "def" [l [s "parser-source-sites", s "form", s "selected?"], app "cond" [
      l [app "not" [app "pair?" [s "form"]], app "quote" [l []]],
      l [app "memq" [app "car" [s "form"], app "quote" [l [s "quote", s "quote-syntax", s "syntax"]]],
        app "quote" [l []]],
      l [s "else", app "append" [app "if" [l [s "selected?", s "form"],
        app "list" [s "form"], app "quote" [l []]],
        app "apply" [s "append", app "map" [app "lambda" [l [s "child"],
          app "parser-source-sites" [s "child", s "selected?"]], s "form"]]]]]] := rfl

theorem extracted_nested : SourceDependencyWitness.nestedSource =
    app "def" [l [s "read-source-subdefinition", s "path", s "outer", s "name"],
      app "let" [l [s "found", app "parser-source-sites" [app "read-source-definition" [s "path", s "outer"],
        app "lambda" [l [s "form"], app "and" [app "source-declaration-matches?" [s "form", s "name"],
          app "pair?" [app "cadr" [s "form"]]]]]],
        app "unless" [app "=" [app "length" [s "found"], n 1],
          app "error" [.text "expected one nested source declaration", s "outer", s "name"]],
        app "car" [s "found"]]] := rfl

theorem extracted_mutations : SourceDependencyWitness.mutationSource =
    app "def" [l [s "source-mutation-sites", s "form"], app "parser-source-sites" [s "form",
      app "lambda" [l [s "form"], app "memq" [app "car" [s "form"],
        app "quote" [l [s "set!", s "vector-set!"]]]]]] := rfl

end ParserEngine.SourceSiteExtraction
