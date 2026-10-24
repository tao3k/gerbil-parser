import ParserEngine.Capture
import ParserEngine.Reduction

/-! Shared token-flow semantics for deterministic LR requests.
Frames denote retained terminal yields, not the possibly lossy public AST.
Lexical admission, grammar/action/goto correctness, effects and native storage
remain external obligations. Conservation does not imply grammar membership.
-/
namespace ParserEngine.Request

structure Config (Token : Type) where
  states : List Nat
  frames : List (List Token)
  rest : List Token
  deriving Repr, DecidableEq

/-- Newest frame first in storage; source order in observations. -/
def terminalYield (frames : List (List Token)) : List Token := frames.reverse.flatten

def Valid (input : List Token) (c : Config Token) : Prop :=
  c.states.length = c.frames.length + 1 ∧ terminalYield c.frames ++ c.rest = input

def initial (state : Nat) (input : List Token) : Config Token := ⟨[state], [], input⟩

theorem initial_valid (state : Nat) (input : List Token) : Valid input (initial state input) := by
  simp [Valid, initial, terminalYield]

/-- Consume precisely the pending terminal without rescanning it. -/
def shift (c : Config Token) (target : Nat) (token : Token) (tail : List Token) : Config Token :=
  ⟨target :: c.states, [token] :: c.frames, tail⟩

/-- Capture the used prefix, preserve the suffix and retain the lookahead. -/
def reduce (c : Config Token) (width target : Nat) : Config Token :=
  let captured := Capture.splitPrefix width c.frames []
  ⟨target :: c.states.drop width, captured.1.flatten :: captured.2, c.rest⟩

theorem capture_conserves (frames : List (List Token)) (width : Nat) :
    terminalYield ((Capture.splitPrefix width frames []).1.flatten ::
      (Capture.splitPrefix width frames []).2) = terminalYield frames := by
  rw [Capture.split_prefix_exact]
  simp only [List.append_nil]
  have ordered : (frames.drop width).reverse ++ (frames.take width).reverse =
      frames.reverse := by
    rw [← List.reverse_append, List.take_append_drop]
  simpa [terminalYield, List.flatten_append] using congrArg List.flatten ordered

theorem shift_valid (input : List Token) (c : Config Token) (target : Nat)
    (token : Token) (tail : List Token) (valid : Valid input c)
    (pending : c.rest = token :: tail) : Valid input (shift c target token tail) := by
  rcases valid with ⟨depth, conservation⟩
  constructor
  · simpa [shift] using congrArg Nat.succ depth
  · simpa [shift, terminalYield, List.reverse_cons, List.flatten_append,
      List.append_assoc, pending] using conservation

theorem reduce_valid (input : List Token) (c : Config Token) (width target : Nat)
    (valid : Valid input c) (fits : width ≤ c.frames.length) :
    Valid input (reduce c width target) := by
  constructor
  · have suffix := Capture.suffix_preserved width c.frames
    simp only [reduce, List.length_cons, List.length_drop, suffix]
    have depth := valid.1
    omega
  · exact (congrArg (fun frames => frames ++ c.rest)
      (capture_conserves c.frames width)).trans valid.2

theorem reduction_retains_pending (c : Config Token) (width target : Nat) :
    (reduce c width target).rest = c.rest := rfl

/-- Targets and widths still require table/production admission in the
implementation. This relation only admits the token-flow kernel. -/
inductive Step : Config Token → Config Token → Prop where
  | shift (c : Config Token) (target : Nat) (token : Token) (tail : List Token)
      (pending : c.rest = token :: tail) : Step c (shift c target token tail)
  | reduce (c : Config Token) (width target : Nat) (fits : width ≤ c.frames.length) :
      Step c (reduce c width target)

inductive Trace : Config Token → Config Token → Prop where
  | refl (c : Config Token) : Trace c c
  | next {a b c : Config Token} (history : Trace a b) (step : Step b c) : Trace a c

theorem step_valid (input : List Token) {before after : Config Token}
    (valid : Valid input before) (step : Step before after) : Valid input after := by
  cases step with
  | shift target token tail pending => exact shift_valid input _ target token tail valid pending
  | reduce width target fits => exact reduce_valid input _ width target valid fits

/-- Arbitrary finite mixtures include nullable and repeated reductions.
Neither termination nor grammar-language soundness is asserted. -/
theorem trace_valid (input : List Token) {before after : Config Token}
    (valid : Valid input before) (trace : Trace before after) : Valid input after := by
  induction trace with
  | refl => exact valid
  | next history step ih => exact step_valid input ih step

/-- Low-level accept returns a root AND remainder; EOF is a separate policy. -/
theorem root_and_remainder (input root : List Token) (c : Config Token)
    (valid : Valid input c) (single : c.frames = [root]) : root ++ c.rest = input := by
  simpa [terminalYield, single] using valid.2

theorem complete_root (input root : List Token) (c : Config Token)
    (valid : Valid input c) (single : c.frames = [root]) (complete : c.rest = []) :
    root = input := by
  simpa [complete] using root_and_remainder input root c valid single

/-- Depth alone does not rule out reversed terminal order. -/
example : ¬ Valid [1, 2] (⟨[0, 1], [[2, 1]], []⟩ : Config Nat) := by simp [Valid, terminalYield]

/-- A valid prefix root need not cover the complete input. -/
example : Valid [1, 2] (⟨[1, 0], [[1]], [2]⟩ : Config Nat) ∧
    ([1] : List Nat) ≠ [1, 2] := by simp [Valid, terminalYield]

end ParserEngine.Request
