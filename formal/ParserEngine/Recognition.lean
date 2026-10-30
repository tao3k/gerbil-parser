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


/-- The constructor continuation classifies a closed logical value and receives
    scalar endpoints directly. This model does not describe native allocation. -/
def withTreeBounds (tree : Tree) (consume : Nat → Nat → α) : α :=
  match tree with
  | .token _ span | .node _ span _ | .fragment span _ => consume span.start span.finish

theorem tree_bounds_continuation_equivalent (tree : Tree) (consume : Nat → Nat → α) :
    withTreeBounds tree consume = consume tree.span.start tree.span.finish := by
  cases tree <;> rfl

/-- Empty and singleton inputs keep their original constructor boundary; wider
    children retain first/last source endpoints and exact ordered child data. -/
def withChildBounds (children : Children) (offset : Nat) (consume : Nat → Nat → α) : α :=
  match children with
  | [] => consume offset offset
  | [child] => withTreeBounds child.2 consume
  | first :: second :: rest =>
    consume first.2.span.start
      ((first :: second :: rest).getLast (by simp)).2.span.finish

theorem child_bounds_continuation_equivalent (children : Children) (offset : Nat)
    (consume : Nat → Nat → α) :
    withChildBounds children offset consume =
      consume (bounds children offset).start (bounds children offset).finish := by
  cases children with
  | nil => rfl
  | cons first rest =>
    cases rest with
    | nil => simp [withChildBounds, bounds, tree_bounds_continuation_equivalent]
    | cons second rest => rfl


/-- Physical child storage separates a bare value from explicit field metadata.
    The model concerns logical labels and tree identity, not native heap layout. -/
inductive ChildStorage where
  | unfielded (value : Tree)
  | labelled (name : String) (value : Tree)

def storeChild : Child → ChildStorage
  | (none, value) => .unfielded value
  | (some name, value) => .labelled name value

def restoreChild : ChildStorage → Child
  | .unfielded value => (none, value)
  | .labelled name value => (some name, value)

theorem compact_child_roundtrip (child : Child) :
    restoreChild (storeChild child) = child := by
  rcases child with ⟨name, value⟩
  cases name <;> rfl

/-- Request-owned semantic slots can retain a shifted terminal without its
otherwise immediately discarded unfielded wrapper. Public boundaries restore
that wrapper; reduced nonterminal values remain canonical child sequences. -/
inductive OwnedSemantic where
  | terminal (value : Tree)
  | canonical (children : Children)

def restoreOwnedSemantic : OwnedSemantic → Children
  | .terminal token => [(none, token)]
  | .canonical children => children

def fieldOwnedTerminal (name : String) (token : Tree) : Children := [(some name, token)]

theorem deferred_terminal_field_exact (name : String) (token : Tree) (offset : Nat) :
    fieldOwnedTerminal name token = field name (restoreOwnedSemantic (.terminal token)) offset := by
  rfl

theorem restored_owned_snapshot (slots : List OwnedSemantic) :
    slots.map restoreOwnedSemantic = slots.map (fun slot => match slot with
      | .terminal token => [(none,token)]
      | .canonical children => children) := rfl

/-- A following alias/field chain observes exactly the same first-field value;
no idempotence, shared mutable wrapper or changed action order is assumed. -/
theorem deferred_terminal_chain_exact (consume : Children → α) (name : String)
    (token : Tree) (offset : Nat) :
    consume (fieldOwnedTerminal name token) =
      consume (field name (restoreOwnedSemantic (.terminal token)) offset) := rfl

end ParserEngine
