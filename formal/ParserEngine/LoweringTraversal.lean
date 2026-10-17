import ParserEngine.LoweringBranches

/-! Native-shaped recursive lowering: n-ary children remain lists, transparent
operand annotations remain visible to dispatch, and precedence remains a helper
barrier in operand position. This is recognition/origin evidence, not AST-action
or dynamic-precedence policy equivalence. -/
namespace ParserEngine.LoweringTraversal
open Lowering Names LoweringBranches

inductive Syntax where
  | empty
  | layoutEnd
  | terminal (key : Nat)
  | reference (name : Nat)
  | sequence (children : List Syntax)
  | choice (children : List Syntax)
  | optional (child : Syntax)
  | repeat (child : Syntax)
  | repeat1 (child : Syntax)
  | field (label : Nat) (child : Syntax)
  | alias (label : Nat) (child : Syntax)
  | precedence (direction rank : Nat) (child : Syntax)
  deriving Repr

def meaning (expression : Syntax) : Expr := match expression with
  | .empty | .layoutEnd => .empty
  | .terminal key => .terminal key
  | .reference name => .reference name
  | .sequence children => sequenceList (children.map (fun child => meaning child))
  | .choice children => choiceList (children.map (fun child => meaning child))
  | .optional child => .optional (meaning child)
  | .repeat child => .repeat (meaning child)
  | .repeat1 child => .repeat1 (meaning child)
  | .field _ child | .alias _ child | .precedence _ _ child => meaning child

structure Operands (rules : Rules) (children : List Syntax) where
  helpers : List (Emission rules)
  symbols : List Lowering.Symbol
  witness : Witness rules [body (sequenceList (children.map meaning))] symbols

mutual
def lower (rules : Rules) (expression : Syntax) (isolated : Bool)
    (context : LoweringBranches.Context rules (meaning expression))
    (recurrence : isolated = true →
      Witness rules [body (meaning expression)] [.nonterminal context.owner]) : List (Emission rules) :=
  match expression with
  | .empty | .layoutEnd =>
      [emit context (step (by simp [meaning, schemaRows]) (identity rules []))]
  | .terminal key =>
      [emit context ⟨operandRecipe (.terminal key), by simpa only [meaning] using operand_recipe_produced rules (.terminal key)⟩]
  | .reference name =>
      [emit context ⟨operandRecipe (.reference name), by simpa only [meaning] using operand_recipe_produced rules (.reference name)⟩]
  | .sequence children => by
      let projected : LoweringBranches.Context rules (sequenceList (children.map meaning)) :=
        rebase (by rw [meaning]) context
      let items := operands rules children
      exact items.helpers ++ [emit projected items.witness]
  | .choice children => by
      let projected : LoweringBranches.Context rules (choiceList (children.map meaning)) :=
        rebase (by rw [meaning]) context
      exact choices rules children projected
  | .optional child =>
      let item := operand rules child
      [emit context (step (by simp [meaning, schemaRows]) (identity rules []))] ++ item.helpers ++
        [emit context (step (by simp [meaning, schemaRows]) item.witness)]
  | .repeat child =>
      if isolatedProof : isolated = true then
        let item := operand rules child
        [emit context (step (by simp [meaning, schemaRows]) (identity rules []))] ++ item.helpers ++
          [emit context (step (by simp [meaning, schemaRows]) (append (recurrence isolatedProof) item.witness))]
      else
        let item := operand rules (.repeat child)
        item.helpers ++ [emit context item.witness]
  | .repeat1 child =>
      if isolatedProof : isolated = true then
        let item := operand rules child
        item.helpers ++ [emit context (step (by simp [meaning, schemaRows]) item.witness),
          emit context (step (by simp [meaning, schemaRows]) (append (recurrence isolatedProof) item.witness))]
      else
        let item := operand rules (.repeat1 child)
        item.helpers ++ [emit context item.witness]
  | .field _ child | .alias _ child => by
      let projected : LoweringBranches.Context rules (meaning child) :=
        rebase (by rw [meaning]) context
      let item := operand rules child
      exact item.helpers ++ [emit projected item.witness]
  | .precedence _ _ child => by
      let childContext := rebase (by rw [meaning]) context
      exact lower rules child isolated childContext
        (fun h => ⟨(recurrence h).recipe,
          by simpa only [childContext, rebase, meaning] using (recurrence h).produced⟩)
termination_by (sizeOf expression, if isolated then 0 else 2)
decreasing_by
  all_goals simp_wf
  all_goals first | (apply Prod.Lex.left; omega) | (apply Prod.Lex.right; simp_all)

def operand (rules : Rules) (expression : Syntax) : LoweringBranches.Operand rules (meaning expression) :=
  match expression with
  | .terminal key => ⟨[], .terminal key, ⟨operandRecipe (.terminal key),
      by simpa only [meaning, operandSymbol] using operand_recipe_produced rules (.terminal key)⟩⟩
  | .reference name => ⟨[], named name, ⟨operandRecipe (.reference name),
      by simpa only [meaning, operandSymbol] using operand_recipe_produced rules (.reference name)⟩⟩
  | .field _ child | .alias _ child =>
      let item := operand rules child
      ⟨item.helpers, item.symbol, ⟨item.witness.recipe, by simpa only [meaning] using item.witness.produced⟩⟩
  | expression =>
      ⟨lower rules expression true (identityContext rules (meaning expression))
        (fun _ => identity rules [body (meaning expression)]),
        body (meaning expression), identity rules [body (meaning expression)]⟩
termination_by (sizeOf expression, 1)
decreasing_by
  all_goals simp_wf
  all_goals first | (apply Prod.Lex.left; omega) | (apply Prod.Lex.right; omega)

def operands (rules : Rules) (children : List Syntax) : Operands rules children :=
  match children with
  | [] => ⟨[], [], step (by simp [sequenceList, schemaRows]) (identity rules [])⟩
  | head :: tail =>
      let first := operand rules head
      let rest := operands rules tail
      ⟨first.helpers ++ rest.helpers, first.symbol :: rest.symbols,
        step (by simp [sequenceList, schemaRows]) (append first.witness rest.witness)⟩
termination_by (sizeOf children, 3)
decreasing_by
  all_goals simp_wf
  all_goals apply Prod.Lex.left; omega

def choices (rules : Rules) (children : List Syntax)
    (context : LoweringBranches.Context rules (choiceList (children.map meaning))) : List (Emission rules) :=
  match children with
  | [] => []
  | head :: tail =>
      lower rules head false (descend context (by simp [choiceList, schemaRows]))
        (by intro h; cases h) ++
      choices rules tail (descend context (by simp [choiceList, schemaRows]))
termination_by (sizeOf children, 3)
decreasing_by
  all_goals simp_wf
  all_goals apply Prod.Lex.left; omega
end

def declaration {rules name expression}
    (unique : UniqueBody rules name (meaning expression)) : List (Emission rules) :=
  lower rules expression true (namedContext unique)
    (fun _ => ⟨.foldNamed name (meaning expression), .foldNamed unique⟩)

def sharedDeclaration {rules : Rules} {name : Nat} {expression : Syntax}
    (member : (name, meaning expression) ∈ rules) : List (Emission rules) :=
  lower rules expression false (authoredContext member) (by intro h; cases h)

def normalized (rules : List (Nat × Syntax)) : Rules :=
  rules.map (fun row => (row.1, meaning row.2))

/-- The actual authored-row count, rather than uniqueness of normalized bodies,
determines loop isolation. Duplicate identical declarations still share a lhs. -/
theorem single_declaration {rules : List (Nat × Syntax)} {row : Nat × Syntax}
    (member : row ∈ rules)
    (count : (rules.filter (fun other => other.1 == row.1)).length = 1) :
    UniqueBody (normalized rules) row.1 (meaning row.2) := by
  obtain ⟨only, one⟩ := List.length_eq_one_iff.mp count
  have rowIn : row ∈ rules.filter (fun other => other.1 == row.1) := by simp [member]
  have onlyRow : row = only := by simpa only [one, List.mem_singleton] using rowIn
  rw [← onlyRow] at one
  refine ⟨List.mem_map.mpr ⟨row, member, rfl⟩, ?_⟩
  intro other belongs
  obtain ⟨candidate, present, equal⟩ := List.mem_map.mp belongs
  have nameEqual : candidate.1 = row.1 := congrArg (fun pair : Nat × Expr => pair.1) equal
  have candidateIn : candidate ∈ rules.filter (fun other => other.1 == row.1) := by
    simp [present, nameEqual]
  have same : candidate = row := by simpa only [one, List.mem_singleton] using candidateIn
  subst candidate
  exact (congrArg (fun pair : Nat × Expr => pair.2) equal).symm

def declarations (rules : List (Nat × Syntax)) : List (Emission (normalized rules)) :=
  rules.attach.flatMap fun entry =>
    if single : (rules.filter (fun other => other.1 == entry.val.1)).length = 1 then
      declaration (single_declaration entry.property single)
    else sharedDeclaration (List.mem_map.mpr ⟨entry.val, entry.property, rfl⟩)

/-- Arbitrary recursive depth and n-ary width; neither caller-supplied recipes
nor a per-emission verifier-success premise occurs in this statement. -/
theorem declaration_checked {rules name expression}
    (unique : UniqueBody rules name (meaning expression))
    (row : Emission rules) (_member : row ∈ declaration unique) :
    ∃ checked, verify rules row.recipe = some checked ∧
      checked.val = ([.nonterminal row.owner], row.rhs) := every_emission_checked row

theorem physical_declaration_supported {rules name expression}
    (unique : UniqueBody rules name (meaning expression))
    (encode : Owner → Nat) (decode : Nat → Owner)
    (roundtrip : ∀ owner, decode (encode owner) = owner) :
    ∀ row ∈ declaration unique, admitOrigin rules decode (physical encode row) row.recipe = true := by
  intro row _
  exact physical_supported encode decode roundtrip row

theorem declarations_admitted (rules : List (Nat × Syntax)) (decode : Nat → Owner)
    (native : Grammar.CFG)
    (aligned : RealizedRows decode (declarations rules) native) :
    admitNative (normalized rules) decode native ((declarations rules).map Emission.recipe) = true :=
  realized_native aligned

theorem declarations_language_sound (rules : List (Nat × Syntax)) (decode : Nat → Owner)
    (native : Grammar.CFG) (root source : Nat) (word : List Nat)
    (aligned : RealizedRows decode (declarations rules) native)
    (rootIdentity : decode root = .named source)
    (parsed : Grammar.Derives native [.nonterminal root] word) :
    Eval (normalized rules) (.reference source) word :=
  admitted_native_root_sound (declarations_admitted rules decode native aligned)
    root source word rootIdentity parsed

end ParserEngine.LoweringTraversal
