import ParserEngine.LoweringCallStack
import ParserEngine.LoweringTraversal

/-! Composition for generated physical call trees. Scope morphisms weaken
captured references when an operand allocates a private helper. Erasure laws
are construction results, not a caller-supplied trace-alignment assertion. -/
namespace ParserEngine.LoweringProgramBuilder
open Lowering LoweringScope LoweringCallStack

abbrev Embedding (reserved : List Nat) (scope target : List Owner) :=
  ∀ {owner}, Reference reserved scope owner → Reference reserved target owner

def mapRefs (embedding : Embedding reserved scope target) :
    OperandRefs reserved scope rhs → OperandRefs reserved target rhs
  | .nil => .nil
  | .terminal token rest => .terminal token (mapRefs embedding rest)
  | .nonterminal reference rest => .nonterminal (embedding reference) (mapRefs embedding rest)

def appendRefs : OperandRefs reserved scope first → OperandRefs reserved scope rest →
    OperandRefs reserved scope (first ++ rest)
  | .nil, last => last
  | .terminal token tail, last => .terminal token (appendRefs tail last)
  | .nonterminal reference tail, last => .nonterminal reference (appendRefs tail last)

def append : Code reserved rules scope → Code reserved rules scope → Code reserved rules scope
  | .done, last => last
  | .emit row lhs rhs next, last => .emit row lhs rhs (append next last)
  | .bind name owner body next, last => .bind name owner body (append next last)
  | .call callee body next, last => .call callee body (append next last)

theorem erase_append (first last : Code reserved rules scope) :
    erase (append first last) = erase first ++ erase last := by
  induction first with
  | done => rfl
  | emit row lhs rhs next ih => exact congrArg (List.cons row) (ih last)
  | bind name owner body next _ ih => simpa [append, erase, List.append_assoc] using congrArg (List.append (erase body)) (ih last)
  | call callee body next _ ih => simpa [append, erase, List.append_assoc] using congrArg (List.append (erase body)) (ih last)

structure Build (reserved : List Nat) (rules : Rules)
    (logical : List (LoweringBranches.Emission rules)) (scope : List Owner) where
  code : Code reserved rules scope
  erased : erase code = logical

def Build.retarget {reserved : List Nat} {rules : Rules} {scope : List Owner}
    {first last : List (LoweringBranches.Emission rules)} (equal : first = last) (build : Build reserved rules first scope) :
    Build reserved rules last scope := ⟨build.code, build.erased.trans equal⟩

def done : Build reserved rules [] scope := ⟨.done, rfl⟩

def emit (row : LoweringBranches.Emission rules) (lhs : Reference reserved scope row.owner)
    (rhs : OperandRefs reserved scope row.rhs) (next : Build reserved rules rest scope) :
    Build reserved rules (row :: rest) scope := ⟨.emit row lhs rhs next.code, congrArg (List.cons row) next.erased⟩

def seq (first : Build reserved rules a scope) (last : Build reserved rules b scope) :
    Build reserved rules (a ++ b) scope :=
  ⟨append first.code last.code, (erase_append first.code last.code).trans (by rw [first.erased, last.erased])⟩

def bind (name : Nat) (owner : Owner) (body : Build reserved rules logical (owner :: scope)) :
    Build reserved rules logical scope :=
  ⟨.bind name owner body.code .done, by simpa [erase] using body.erased⟩

def call (callee : Reference reserved scope owner) (body : Build reserved rules first scope)
    (next : Build reserved rules last scope) : Build reserved rules (first ++ last) scope :=
  ⟨.call callee body.code next.code, by simp only [erase, body.erased, next.erased]⟩

def concatenate (trace : α → List (LoweringBranches.Emission rules))
    (compile : ∀ entry, Build reserved rules (trace entry) scope) :
    (entries : List α) → Build reserved rules (entries.flatMap trace) scope
  | [] => done
  | head :: tail => seq (compile head) (concatenate trace compile tail)

end ParserEngine.LoweringProgramBuilder
