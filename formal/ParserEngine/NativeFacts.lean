import ParserEngine.GrammarWitnessDecoder
import ParserEngine.NativeGrammarData

/-! Maintained proofs over macro-published native data. Scheme emits neither
Lean declarations nor proof programs. Whole-list decoding is checked explicitly. -/
namespace ParserEngine.NativeFacts
open Grammar Completion GrammarWitnessDecoder

-- The fallback is unreachable by the checked decode equations below.
def completions : List GrammarWitnessDecoder.Completion :=
  (array completion NativeGrammarData.completion).getD []
def origins : List Origin := (array origin NativeGrammarData.origins).getD []

set_option maxRecDepth 16384
set_option maxHeartbeats 1600000

theorem completion_decode_checked : array completion NativeGrammarData.completion = some completions := by decide
theorem origin_decode_checked : array origin NativeGrammarData.origins = some origins := by decide
theorem corpus_cardinality : completions.length = 4 ∧ origins.length = 18 := by decide

def completionCheck (value : GrammarWitnessDecoder.Completion) : Bool :=
  checkNullable value.grammar value.nullable value.fuel &&
    checkFirst value.grammar value.nullable value.first value.fuel

theorem completion_checked : completions.all completionCheck = true := by decide

theorem nullable_exact (value : GrammarWitnessDecoder.Completion) (member : value ∈ completions) :
    ∀ name, name ∈ value.nullable ↔ Nullable value.grammar [.nonterminal name] := by
  have checked := (List.all_eq_true.mp completion_checked) value member
  simp only [completionCheck, Bool.and_eq_true] at checked
  have parts := checked
  exact nullable_certificate_exact _ _ value.fuel parts.1

theorem first_exact (value : GrammarWitnessDecoder.Completion) (member : value ∈ completions) :
    ∀ name token, (name, token) ∈ value.first ↔ First value.grammar [.nonterminal name] token := by
  have checked := (List.all_eq_true.mp completion_checked) value member
  simp only [completionCheck, Bool.and_eq_true] at checked
  have parts := checked
  exact first_certificate_exact _ _ _ value.fuel value.fuel parts.1 parts.2

def originCheck (value : Origin) : Bool :=
  Names.admitNative value.rules value.decode value.native value.recipes &&
    decide (ConstructionGrammar.checks value.input = value.admitted ∧
      (value.projectable = true → ConstructionGrammar.grammar value.input =
        ⟨value.augmented, [.nonterminal 0]⟩ :: value.native) ∧ value.decode 0 = .named 0)

theorem origin_checked : origins.all originCheck = true := by decide

theorem origin_facts (value : Origin) (member : value ∈ origins) :
    Names.admitNative value.rules value.decode value.native value.recipes = true ∧
    ConstructionGrammar.checks value.input = value.admitted ∧
    (value.projectable = true → ConstructionGrammar.grammar value.input =
      ⟨value.augmented, [.nonterminal 0]⟩ :: value.native) ∧ value.decode 0 = .named 0 := by
  have checked := (List.all_eq_true.mp origin_checked) value member
  simpa only [originCheck, Bool.and_eq_true, decide_eq_true_eq] using checked

theorem origin_admitted (value : Origin) (member : value ∈ origins) :
    Names.admitNative value.rules value.decode value.native value.recipes = true :=
  (origin_facts value member).1

theorem origin_sound (value : Origin) (member : value ∈ origins) :
    ∀ production ∈ value.native, Names.OriginSupported value.rules value.decode production :=
  Names.admitted_native_sound (origin_admitted value member)

theorem origin_language_sound (value : Origin) (member : value ∈ origins) (word : List Nat)
    (derivation : Grammar.Derives value.native [.nonterminal 0] word) :
    Lowering.Eval value.rules (.reference 0) word :=
  Names.admitted_native_root_sound (origin_admitted value member) 0 0 word (origin_facts value member).2.2.2 derivation

end ParserEngine.NativeFacts
