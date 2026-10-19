import ParserEngine.LoweringDispatch
import ParserEngine.ConstructionVocabulary

/-! Atomic lower-symbol branches execute actual extracted Scheme, with no
caller-supplied branch result, callback stream or final-heap correspondence. -/
namespace ParserEngine.LoweringSymbolEvaluation
open WalkSource SchemeTableEvaluation LoweringDispatchSyntax LoweringDispatch

/-- Every concrete terminal category, including token's canonical tag, returns
its exact encoded payload and leaves the mutable allocation heap unchanged. -/
theorem terminal (encoding : Encoding) (key : Nat)
    (read : resolve env heap "expression" = some (datumValue (encode encoding (.terminal key))))
    (carPrimitive : env "car" = none) (cadrPrimitive : env "cadr" = none) (listPrimitive : env "list" = none) :
    Eval env heap (caseSource LoweringNamesSource.symbolSource)
      (.list [.symbol "terminal", .symbol (encoding.terminalTag key).name, datumValue (encoding.terminalPayload key)]) heap := by
  have payload : Eval env heap (app "cadr" [s "expression"]) (datumValue (encoding.terminalPayload key)) heap :=
    .apply (.cons (.variable read) .nil) (by simp [encode, datumValue, datumValues, primitive, l, s]) cadrPrimitive
  have category := selector_eval encoding (.terminal key) read carPrimitive
  apply symbol_eval encoding (.terminal key) read carPrimitive
  cases kind : encoding.terminalTag key <;>
    simp only [symbolBranch, kind] <;> apply SchemeTableEvaluation.Sequence.single
  · exact .apply (.cons .quoteSymbol (.cons (by simpa only [tag, kind, NativeActions.Source.call, List.map_cons, List.map_nil] using category) (.cons payload .nil))) rfl listPrimitive
  · exact .apply (.cons .quoteSymbol (.cons .quoteSymbol (.cons payload .nil))) rfl listPrimitive
  · exact .apply (.cons .quoteSymbol (.cons (by simpa only [tag, kind, NativeActions.Source.call, List.map_cons, List.map_nil] using category) (.cons payload .nil))) rfl listPrimitive
  · exact .apply (.cons .quoteSymbol (.cons (by simpa only [tag, kind, NativeActions.Source.call, List.map_cons, List.map_nil] using category) (.cons payload .nil))) rfl listPrimitive

theorem reference (encoding : Encoding) (name : Nat)
    (read : resolve env heap "expression" = some (datumValue (encode encoding (.reference name))))
    (carPrimitive : env "car" = none) (cadrPrimitive : env "cadr" = none) (listPrimitive : env "list" = none) :
    Eval env heap (caseSource LoweringNamesSource.symbolSource)
      (.list [.symbol "nonterminal", .symbol (encoding.name name)]) heap := by
  apply symbol_eval encoding (.reference name) read carPrimitive
  apply SchemeTableEvaluation.Sequence.single
  apply Eval.apply (values := [.symbol "nonterminal", .symbol (encoding.name name)]) _ rfl listPrimitive
  exact .cons .quoteSymbol (.cons (.apply (.cons (.variable read) .nil) (by simp [encode, datumValue, datumValues, primitive, l, s]) cadrPrimitive) .nil)

/-- Any returning atomic terminal run agrees, including its untouched heap. -/
theorem terminal_unique (encoding : Encoding) (key : Nat)
    (read : resolve env heap "expression" = some (datumValue (encode encoding (.terminal key))))
    (carPrimitive : env "car" = none) (cadrPrimitive : env "cadr" = none) (listPrimitive : env "list" = none)
    (actual : Eval env heap (caseSource LoweringNamesSource.symbolSource) value after) :
    value = .list [.symbol "terminal", .symbol (encoding.terminalTag key).name, datumValue (encoding.terminalPayload key)] ∧
      after = heap :=
  actual.deterministic (terminal encoding key read carPrimitive cadrPrimitive listPrimitive)

def terminalDatum (encoding : Encoding) (key : Nat) : Datum :=
  l [s "terminal", s (encoding.terminalTag key).name, encoding.terminalPayload key]

/-- Compose actual source evaluation with the existing retained LeanPoo
construction product. Canonical raw admission and catalogue lookup remain
checked representation contracts; arbitrary payloads are not silently admitted.
No factory is rebuilt and no independent transition table is substituted. -/
theorem retained_terminal (encoding : Encoding) (key : Nat)
    (expressionRead : resolve env heap "expression" = some (datumValue (encode encoding (.terminal key))))
    (carPrimitive : env "car" = none) (cadrPrimitive : env "cadr" = none) (listPrimitive : env "list" = none)
    (actual : Eval env heap (caseSource LoweringNamesSource.symbolSource) value after)
    (sources : ConstructionConsumer.Sources) (input : ConstructionEncoding.Input)
    (traversal : CachedConstruction.Traversal) (output : ConstructionEncoding.Output sources input traversal)
    (rowIndex operandIndex : Nat) (record : ConstructionEncoding.Record)
    (rowRead : input.records[rowIndex]? = some record)
    (operandRead : record.rhs[operandIndex]? = some (terminalDatum encoding key))
    (raw : ConstructionEncoding.Terminal)
    (canonical : ConstructionVocabulary.rawTerminal (terminalDatum encoding key) = some raw)
    (ordinal : ConstructionEncoding.lookup (ConstructionEncoding.catalogue input) raw = some key)
    (nonEOF : raw ≠ .eof) :
    value = datumValue (terminalDatum encoding key) ∧ after = heap ∧
      ∃ production, (ConstructionGrammar.grammar output.normalized)[rowIndex]? = some production ∧
        production.rhs[operandIndex]? = some (.terminal key) := by
  have returned := terminal_unique encoding key expressionRead carPrimitive cadrPrimitive listPrimitive actual
  refine ⟨?_, returned.2, ?_⟩
  · simpa [terminalDatum, datumValue, datumValues, l, s] using returned.1
  · exact ConstructionVocabulary.factory_terminal sources input traversal output rowIndex operandIndex record
      (terminalDatum encoding key) rowRead operandRead raw key canonical ordinal nonEOF

end ParserEngine.LoweringSymbolEvaluation
