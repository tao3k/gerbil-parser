import ParserEngine.LoweringProtocol

/-! Controls exercise the generated call tree, not a manually supplied trace.
The candidate function deliberately collides with authored name zero. -/
namespace ParserEngine.LoweringProgramControls
open LoweringCallStack LoweringScope

def emptyHelperRules : List (Nat × LoweringTraversal.Syntax) :=
  [(0, .sequence [.choice [], .optional (.terminal 1)])]

def duplicateRules : List (Nat × LoweringTraversal.Syntax) :=
  [(0, .repeat (.terminal 1)), (0, .repeat (.terminal 1))]

def execution (rules : List (Nat × LoweringTraversal.Syntax)) :=
  execute (fun _ index => index) (fun _ _ _ equal => equal) (LoweringProgram.compile rules 0).code
    (LoweringProtocol.Start rules 0) .nil

/- Executable regression controls, not kernel proofs. The generic origin
 theorem below is checked independently by the kernel. -/
#eval show IO Unit from do
  unless emissions (execution emptyHelperRules).events ==
      [⟨2, []⟩, ⟨2, [.terminal 1]⟩, ⟨0, [.nonterminal 1, .nonterminal 2]⟩] do
    throw (IO.userError "empty helper allocation trajectory changed")
  unless emissions (execution duplicateRules).events ==
      [⟨1, []⟩, ⟨1, [.nonterminal 1, .terminal 1]⟩, ⟨0, [.nonterminal 1]⟩,
       ⟨2, []⟩, ⟨2, [.nonterminal 2, .terminal 1]⟩, ⟨0, [.nonterminal 2]⟩] do
    throw (IO.userError "duplicate declaration helper isolation changed")

theorem generated_origin : Names.admitNative (LoweringTraversal.normalized duplicateRules)
    (LoweringNameStore.decode (execution duplicateRules).after.names)
    (emissions (execution duplicateRules).events)
    ((LoweringTraversal.declarations duplicateRules).map LoweringBranches.Emission.recipe) = true :=
  LoweringProtocol.generated_origin duplicateRules 0 (execution duplicateRules)

end ParserEngine.LoweringProgramControls
