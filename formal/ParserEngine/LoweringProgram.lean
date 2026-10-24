import ParserEngine.LoweringProgramBuilder

/-! Source-shaped recursive compilation to lexical call trees. The continuation
captures actual operand occurrences, including helpers emitting no productions.
Erasure must equal the recursive logical producer at every constructor. -/
namespace ParserEngine.LoweringProgram
open Lowering LoweringBranches LoweringScope LoweringProgramBuilder
namespace Spec
export LoweringTraversal (Syntax meaning)
end Spec

abbrev Allowed (reserved : List Nat) (expression : LoweringTraversal.Syntax) :=
  ∀ name ∈ Names.references (LoweringTraversal.meaning expression), name ∈ reserved

theorem sequence_references (expressions : List Expr) :
    Names.references (sequenceList expressions) = expressions.flatMap Names.references := by
  induction expressions with
  | nil => rfl
  | cons head tail ih => simp [sequenceList, Names.references, ih]

theorem choice_references (expressions : List Expr) :
    Names.references (choiceList expressions) = expressions.flatMap Names.references := by
  induction expressions with
  | nil => rfl
  | cons head tail ih => simp [choiceList, Names.references, ih]

theorem childAllowed {reserved children} (allowed : Allowed reserved (.sequence children)) :
    ∀ child ∈ children, Allowed reserved child := by
  intro child member name present
  apply allowed name
  simpa [LoweringTraversal.meaning, sequence_references] using
    (List.mem_flatMap.mpr ⟨LoweringTraversal.meaning child, List.mem_map.mpr ⟨child, member, rfl⟩, present⟩)

theorem choiceAllowed {reserved children} (allowed : Allowed reserved (.choice children)) :
    ∀ child ∈ children, Allowed reserved child := by
  intro child member name present
  apply allowed name
  simpa [LoweringTraversal.meaning, choice_references] using
    (List.mem_flatMap.mpr ⟨LoweringTraversal.meaning child, List.mem_map.mpr ⟨child, member, rfl⟩, present⟩)

def put (context : LoweringBranches.Context rules expression)
    (witness : Witness rules [body expression] rhs) (lhs : Reference reserved scope context.owner)
    (references : OperandRefs reserved scope rhs) (next : Build reserved rules rest scope) :=
  LoweringProgramBuilder.emit (LoweringBranches.emit context witness) lhs references next

def isComplex : LoweringTraversal.Syntax → Bool
  | .terminal _ | .reference _ | .field _ _ | .alias _ _ => false
  | _ => true

theorem complex_operand_eq (rules : Rules) (expression : LoweringTraversal.Syntax)
    (admitted : isComplex expression = true) :
    LoweringTraversal.operand rules expression =
      ⟨LoweringTraversal.lower rules expression true (identityContext rules (LoweringTraversal.meaning expression))
        (fun _ => identity rules [body (LoweringTraversal.meaning expression)]),
        body (LoweringTraversal.meaning expression), identity rules [body (LoweringTraversal.meaning expression)]⟩ := by
  cases expression <;> simp_all only [isComplex, Bool.false_eq_true, LoweringTraversal.operand] <;> rfl

mutual
def lower (rules : Rules) (expression : LoweringTraversal.Syntax) (isolated : Bool)
    (context : LoweringBranches.Context rules (LoweringTraversal.meaning expression))
    (recurrence : isolated = true → Witness rules [body (LoweringTraversal.meaning expression)] [.nonterminal context.owner])
    (authoredOwner : Nat) (allowed : Allowed reserved expression)
    (lhs : Reference reserved scope context.owner) :
    Build reserved rules (LoweringTraversal.lower rules expression isolated context recurrence) scope :=
  (LoweringProgramBuilder.call lhs
    (lowerBody rules expression isolated context recurrence authoredOwner allowed lhs) done).retarget
      (by simp only [List.append_nil])
termination_by (sizeOf expression, if isolated then 1 else 5)
decreasing_by
  all_goals simp_wf
  all_goals apply Prod.Lex.right; split <;> omega

def lowerBody (rules : Rules) (expression : LoweringTraversal.Syntax) (isolated : Bool)
    (context : LoweringBranches.Context rules (LoweringTraversal.meaning expression))
    (recurrence : isolated = true → Witness rules [body (LoweringTraversal.meaning expression)] [.nonterminal context.owner])
    (authoredOwner : Nat) (allowed : Allowed reserved expression)
    (lhs : Reference reserved scope context.owner) :
    Build reserved rules (LoweringTraversal.lower rules expression isolated context recurrence) scope :=
  match expression with
  | .empty | .layoutEnd =>
      (put context (step (by simp [LoweringTraversal.meaning, Names.schemaRows]) (identity rules [])) lhs .nil done).retarget
        (by simp only [LoweringTraversal.lower] <;> rfl)
  | .terminal key =>
      (put context ⟨Names.operandRecipe (.terminal key), by simpa only [LoweringTraversal.meaning, Names.operandSymbol, named] using Names.operand_recipe_produced rules (.terminal key)⟩
        lhs (.terminal key .nil) done).retarget (by simp only [LoweringTraversal.lower] <;> rfl)
  | .reference name =>
      (put context ⟨Names.operandRecipe (.reference name), by simpa only [LoweringTraversal.meaning, Names.operandSymbol, named] using Names.operand_recipe_produced rules (.reference name)⟩
        lhs (.nonterminal (.authored name (allowed name (by simp [LoweringTraversal.meaning, Names.references]))) .nil) done).retarget
          (by simp only [LoweringTraversal.lower] <;> rfl)
  | .sequence children =>
      let projected : LoweringBranches.Context rules (sequenceList (children.map LoweringTraversal.meaning)) := rebase (by rw [LoweringTraversal.meaning]) context
      let items := LoweringTraversal.operands rules children
      (operands rules children authoredOwner (childAllowed allowed)
        (fun _ embedding references => put projected items.witness (embedding lhs) references done)).retarget
          (by simp only [LoweringTraversal.lower] <;> rfl)
  | .choice children =>
      let projected : LoweringBranches.Context rules (choiceList (children.map LoweringTraversal.meaning)) := rebase (by rw [LoweringTraversal.meaning]) context
      (choices rules children projected authoredOwner (choiceAllowed allowed) lhs).retarget
        (by simp only [LoweringTraversal.lower] <;> rfl)
  | .optional child =>
      let item := LoweringTraversal.operand rules child
      let empty := step (expression := LoweringTraversal.meaning (.optional child)) (by simp [LoweringTraversal.meaning, Names.schemaRows]) (identity rules [])
      let some := step (expression := LoweringTraversal.meaning (.optional child)) (by simp [LoweringTraversal.meaning, Names.schemaRows]) item.witness
      (put context empty lhs .nil
        (operand rules child authoredOwner (by simpa only [Allowed, LoweringTraversal.meaning, Names.references] using allowed)
          (fun _ embedding references => put context some (embedding lhs) references done))).retarget
            (by simp only [LoweringTraversal.lower, List.singleton_append] <;> rfl)
  | .repeat child =>
      if isolatedProof : isolated = true then
        let item := LoweringTraversal.operand rules child
        let empty := step (expression := LoweringTraversal.meaning (.repeat child)) (by simp [LoweringTraversal.meaning, Names.schemaRows]) (identity rules [])
        let loop := step (expression := LoweringTraversal.meaning (.repeat child)) (by simp [LoweringTraversal.meaning, Names.schemaRows]) (LoweringBranches.append (recurrence isolatedProof) item.witness)
        (put context empty lhs .nil
          (operand rules child authoredOwner (by simpa only [Allowed, LoweringTraversal.meaning, Names.references] using allowed)
            (fun _ embedding references => put context loop (embedding lhs) (.nonterminal (embedding lhs) references) done))).retarget
              (by simp only [LoweringTraversal.lower, isolatedProof, List.singleton_append] <;> rfl)
      else
        let item := LoweringTraversal.operand rules (.repeat child)
        (operand rules (.repeat child) authoredOwner allowed
          (fun _ embedding references => put context item.witness (embedding lhs) references done)).retarget
            (by simp only [LoweringTraversal.lower, isolatedProof] <;> rfl)
  | .repeat1 child =>
      if isolatedProof : isolated = true then
        let item := LoweringTraversal.operand rules child
        let base := step (expression := LoweringTraversal.meaning (.repeat1 child)) (by simp [LoweringTraversal.meaning, Names.schemaRows]) item.witness
        let loop := step (expression := LoweringTraversal.meaning (.repeat1 child)) (by simp [LoweringTraversal.meaning, Names.schemaRows]) (LoweringBranches.append (recurrence isolatedProof) item.witness)
        (operand rules child authoredOwner (by simpa only [Allowed, LoweringTraversal.meaning, Names.references] using allowed)
          (fun _ embedding references => put context base (embedding lhs) references
            (put context loop (embedding lhs) (.nonterminal (embedding lhs) references) done))).retarget
              (by simp only [LoweringTraversal.lower, isolatedProof] <;> rfl)
      else
        let item := LoweringTraversal.operand rules (.repeat1 child)
        (operand rules (.repeat1 child) authoredOwner allowed
          (fun _ embedding references => put context item.witness (embedding lhs) references done)).retarget
            (by simp only [LoweringTraversal.lower, isolatedProof] <;> rfl)
  | .field _ child | .alias _ child =>
      let projected : LoweringBranches.Context rules (LoweringTraversal.meaning child) := rebase (by rw [LoweringTraversal.meaning]) context
      let item := LoweringTraversal.operand rules child
      (operand rules child authoredOwner (by simpa only [Allowed, LoweringTraversal.meaning] using allowed)
        (fun _ embedding references => put projected item.witness (embedding lhs) references done)).retarget
          (by simp only [LoweringTraversal.lower] <;> rfl)
  | .precedence _ _ child =>
      let projected := rebase (by rw [LoweringTraversal.meaning]) context
      let recursive : isolated = true → Witness rules [body (LoweringTraversal.meaning child)] [.nonterminal projected.owner] := fun h => ⟨(recurrence h).recipe,
        by simpa only [projected, rebase, LoweringTraversal.meaning] using (recurrence h).produced⟩
      (lower rules child isolated projected recursive authoredOwner
        (by simpa only [Allowed, LoweringTraversal.meaning] using allowed) lhs).retarget
          (by simp only [LoweringTraversal.lower] <;> rfl)
termination_by (sizeOf expression, if isolated then 0 else 4)
decreasing_by
  all_goals simp_wf
  all_goals first | (apply Prod.Lex.left; omega) | (apply Prod.Lex.right; simp_all)

def operand {reserved : List Nat} {scope : List Owner} (rules : Rules) (expression : LoweringTraversal.Syntax) (authoredOwner : Nat)
    (allowed : Allowed reserved expression)
    {suffix : List (LoweringBranches.Emission rules)}
    (next : ∀ target, Embedding reserved scope target →
      OperandRefs reserved target [(LoweringTraversal.operand rules expression).symbol] → Build reserved rules suffix target) :
    Build reserved rules ((LoweringTraversal.operand rules expression).helpers ++ suffix) scope :=
  match expression with
  | .terminal key =>
      (next scope (fun reference => reference) (by simpa only [LoweringTraversal.operand] using OperandRefs.terminal key OperandRefs.nil)).retarget
        (by simp only [LoweringTraversal.operand, List.nil_append] <;> rfl)
  | .reference name =>
      (next scope (fun reference => reference)
        (by simpa only [LoweringTraversal.operand, named] using (OperandRefs.nonterminal
          (Reference.authored name (allowed name (by simp [LoweringTraversal.meaning, Names.references]))) OperandRefs.nil))).retarget
            (by simp only [LoweringTraversal.operand, List.nil_append] <;> rfl)
  | .field _ child | .alias _ child =>
      (operand rules child authoredOwner (by simpa only [Allowed, LoweringTraversal.meaning] using allowed)
        (fun target embedding references => next target embedding
          (by simpa only [LoweringTraversal.operand] using references))).retarget
            (by simp only [LoweringTraversal.operand] <;> rfl)
  | .empty => complexOperand rules .empty rfl authoredOwner allowed next
  | .layoutEnd => complexOperand rules .layoutEnd rfl authoredOwner allowed next
  | .sequence children => complexOperand rules (.sequence children) rfl authoredOwner allowed next
  | .choice children => complexOperand rules (.choice children) rfl authoredOwner allowed next
  | .optional child => complexOperand rules (.optional child) rfl authoredOwner allowed next
  | .repeat child => complexOperand rules (.repeat child) rfl authoredOwner allowed next
  | .repeat1 child => complexOperand rules (.repeat1 child) rfl authoredOwner allowed next
  | .precedence direction rank child => complexOperand rules (.precedence direction rank child) rfl authoredOwner allowed next
termination_by (sizeOf expression, 3)
decreasing_by
  all_goals simp_wf
  all_goals first | (apply Prod.Lex.left; omega) | (apply Prod.Lex.right; omega)

def complexOperand {reserved : List Nat} {scope : List Owner} (rules : Rules) (expression : LoweringTraversal.Syntax)
    (admitted : isComplex expression = true) (authoredOwner : Nat) (allowed : Allowed reserved expression)
    {suffix : List (LoweringBranches.Emission rules)}
    (next : ∀ target, Embedding reserved scope target →
      OperandRefs reserved target [(LoweringTraversal.operand rules expression).symbol] → Build reserved rules suffix target) :
    Build reserved rules ((LoweringTraversal.operand rules expression).helpers ++ suffix) scope :=
  let owner := Owner.expression (LoweringTraversal.meaning expression)
  (bind authoredOwner owner
    (seq (lower rules expression true (identityContext rules (LoweringTraversal.meaning expression))
        (fun _ => identity rules [body (LoweringTraversal.meaning expression)]) authoredOwner allowed Reference.here)
      (next (owner :: scope) (fun reference => .there reference)
        (by rw [complex_operand_eq rules expression admitted]; exact .nonterminal .here .nil)))).retarget
          (by rw [complex_operand_eq rules expression admitted])
termination_by (sizeOf expression, 2)
decreasing_by
  all_goals simp_wf
  all_goals apply Prod.Lex.right; omega

def operands {reserved : List Nat} {scope : List Owner} (rules : Rules) (children : List LoweringTraversal.Syntax) (authoredOwner : Nat)
    (allowed : ∀ child ∈ children, Allowed reserved child)
    {suffix : List (LoweringBranches.Emission rules)}
    (next : ∀ target, Embedding reserved scope target →
      OperandRefs reserved target (LoweringTraversal.operands rules children).symbols → Build reserved rules suffix target) :
    Build reserved rules ((LoweringTraversal.operands rules children).helpers ++ suffix) scope :=
  match children with
  | [] => (next scope (fun reference => reference) (by simpa only [LoweringTraversal.operands] using (OperandRefs.nil (reserved := reserved) (scope := scope)))).retarget (by simp only [LoweringTraversal.operands, List.nil_append] <;> rfl)
  | head :: tail =>
      (operand rules head authoredOwner (allowed head (by simp))
        (fun intermediate firstEmbedding first =>
          operands rules tail authoredOwner (fun child member => allowed child (by simp [member]))
            (fun target lastEmbedding last => next target (fun reference => lastEmbedding (firstEmbedding reference))
              (by simpa only [LoweringTraversal.operands, List.singleton_append] using appendRefs (mapRefs lastEmbedding first) last)))).retarget
                (by simp only [LoweringTraversal.operands, List.append_assoc] <;> rfl)
termination_by (sizeOf children, 3)
decreasing_by
  all_goals simp_wf
  all_goals apply Prod.Lex.left; omega

def choices (rules : Rules) (children : List LoweringTraversal.Syntax)
    (context : LoweringBranches.Context rules (choiceList (children.map LoweringTraversal.meaning)))
    (authoredOwner : Nat) (allowed : ∀ child ∈ children, Allowed reserved child)
    (lhs : Reference reserved scope context.owner) :
    Build reserved rules (LoweringTraversal.choices rules children context) scope :=
  match children with
  | [] => done.retarget (by simp only [LoweringTraversal.choices] <;> rfl)
  | head :: tail =>
      (seq (lower rules head false (descend context (by simp [choiceList, Names.schemaRows]))
          (by intro h; cases h) authoredOwner (allowed head (by simp)) lhs)
        (choices rules tail (descend context (by simp [choiceList, Names.schemaRows]))
          authoredOwner (fun child member => allowed child (by simp [member])) lhs)).retarget
            (by simp only [LoweringTraversal.choices] <;> rfl)
termination_by (sizeOf children, 3)
decreasing_by
  all_goals simp_wf
  all_goals apply Prod.Lex.left; omega
end

abbrev reserved (rules : List (Nat × LoweringTraversal.Syntax)) (root : Nat) :=
  Names.reservedNames root (LoweringTraversal.normalized rules)

def compileEntry (rules : List (Nat × LoweringTraversal.Syntax)) (root : Nat)
    (entry : { row // row ∈ rules }) :
    Build (reserved rules root) (LoweringTraversal.normalized rules) (LoweringTraversal.entryTrace rules entry) [] :=
  let member : (entry.val.1, LoweringTraversal.meaning entry.val.2) ∈ LoweringTraversal.normalized rules :=
    List.mem_map.mpr ⟨entry.val, entry.property, rfl⟩
  let included := Names.source_names_reserved root (LoweringTraversal.normalized rules) member
  if single : (rules.filter (fun other => other.1 == entry.val.1)).length = 1 then
    let unique := LoweringTraversal.single_declaration entry.property single
    (lower (LoweringTraversal.normalized rules) entry.val.2 true (namedContext unique)
      (fun _ => ⟨.foldNamed entry.val.1 (LoweringTraversal.meaning entry.val.2), .foldNamed unique⟩)
      entry.val.1 included.2 (.authored entry.val.1 included.1)).retarget
        (by simp only [LoweringTraversal.entryTrace, single, ↓reduceDIte, LoweringTraversal.declaration] <;> rfl)
  else
    (lower (LoweringTraversal.normalized rules) entry.val.2 false (authoredContext member)
      (by intro h; cases h) entry.val.1 included.2 (.authored entry.val.1 included.1)).retarget
        (by simp only [LoweringTraversal.entryTrace, single, ↓reduceDIte, LoweringTraversal.sharedDeclaration] <;> rfl)

def compile (rules : List (Nat × LoweringTraversal.Syntax)) (root : Nat) :
    Build (reserved rules root) (LoweringTraversal.normalized rules) (LoweringTraversal.declarations rules) [] :=
  concatenate (LoweringTraversal.entryTrace rules) (compileEntry rules root) rules.attach

/-- Arbitrary grammar and recursion: physical realization is generated by
compilation and execution. No RealizedRows or successful origin-check input. -/
theorem physical_realized (rules : List (Nat × LoweringTraversal.Syntax)) (root : Nat)
    (candidate : Nat → Nat → Nat)
    (injective : ∀ owner a b, candidate owner a = candidate owner b → a = b) :
    let build := compile rules root
    let execution := LoweringCallStack.execute candidate injective build.code
      (LoweringNameStore.initial (reserved rules root)) .nil
    LoweringBranches.RealizedRows (LoweringNameStore.decode execution.after.names)
      (LoweringTraversal.declarations rules) (LoweringCallStack.emissions execution.events) := by
  dsimp only
  have aligned := LoweringCallStack.realized (compile rules root).code
    (LoweringNameStore.initial (reserved rules root))
    (LoweringCallStack.execute candidate injective (compile rules root).code
      (LoweringNameStore.initial (reserved rules root)) .nil)
  rw [(compile rules root).erased] at aligned
  rw [(LoweringCallStack.execute candidate injective (compile rules root).code
    (LoweringNameStore.initial (reserved rules root)) .nil).trajectory]
  exact aligned

end ParserEngine.LoweringProgram
