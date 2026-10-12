namespace ParserEngine

/-- Byte coordinates are abstract naturals; source admission is a separate obligation. -/
structure Span where
  start : Nat
  finish : Nat
  deriving DecidableEq, Repr

/-- Logical recognition trees, independent of their physical child-sequence storage. -/
inductive Tree where
  | token (kind : String) (span : Span)
  | node (kind : String) (span : Span) (children : List (Option String × Tree))
  | fragment (span : Span) (children : List (Option String × Tree))
  deriving Repr

def Tree.span : Tree → Span
  | .token _ span | .node _ span _ | .fragment span _ => span

abbrev Child := Option String × Tree
abbrev Children := List Child

def bounds (children : Children) (offset : Nat) : Span :=
  match children with
  | [] => ⟨offset, offset⟩
  | first :: rest =>
    ⟨first.2.span.start, ((first :: rest).getLast (by simp)).2.span.finish⟩

/-- Preserve existing field labels by placing them inside a fragment. -/
def field (name : String) (children : Children) (offset : Nat) : Children :=
  match children with
  | [] => []
  | [(none, value)] => [(some name, value)]
  | _ => [(some name, .fragment (bounds children offset) children)]

def alias (kind : String) (children : Children) (offset : Nat) : Children :=
  [(none, .node kind (bounds children offset) children)]

@[simp] theorem field_empty (name : String) (offset : Nat) : field name [] offset = [] := rfl

theorem field_unmarked_singleton (name : String) (value : Tree) (offset : Nat) :
    field name [(none, value)] offset = [(some name, value)] := rfl

theorem field_preserves_nested_label (name previous : String) (value : Tree) (offset : Nat) :
    field name [(some previous, value)] offset =
      [(some name, .fragment (bounds [(some previous, value)] offset) [(some previous, value)])] := rfl

theorem empty_alias_anchor (kind : String) (offset : Nat) :
    alias kind [] offset = [(none, .node kind ⟨offset, offset⟩ [])] := rfl

theorem alias_children (kind : String) (children : Children) (offset : Nat) :
    alias kind children offset = [(none, .node kind (bounds children offset) children)] := rfl

end ParserEngine
