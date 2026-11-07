// Bounded reverse-scan histories against an independent forward specification.
// Name-level FIRST/nullable facts are inputs, not a native-solver certificate.
module SuffixPrediction {
  type Symbol = {terminal:bool,name:int}
  type Summary = {first:Set[int],empty:bool}
  type State = {word:List[Symbol],cursor:int,rows:List[Summary]}
  var s:State
  pure val end:Summary = {first:Set(),empty:true}
  pure val nullableName:Symbol = {terminal:false,name:0}
  pure val deadName:Symbol = {terminal:false,name:1}
  pure val a:Symbol = {terminal:true,name:2}
  pure val b:Symbol = {terminal:true,name:3}
  pure def headFirst(symbol:Symbol):Set[int] =
    if(symbol.terminal) Set(symbol.name) else if(symbol.name == 0) Set(2) else Set()
  pure def headEmpty(symbol:Symbol):bool = not(symbol.terminal) and symbol.name == 0
  pure def prepend(symbol:Symbol,suffixSummary:Summary):Summary = {
    first:if(headEmpty(symbol)) headFirst(symbol).union(suffixSummary.first) else headFirst(symbol),
    empty:headEmpty(symbol) and suffixSummary.empty}
  pure def forward(word:List[Symbol]):Summary = word.foldl(end,(known,symbol) => {
    first:if(known.empty) known.first.union(headFirst(symbol)) else known.first,
    empty:known.empty and headEmpty(symbol)})
  pure def evidence(summary:Summary,activated:bool,delta:Set[int]):Set[int] =
    (if(activated) Set() else summary.first).union(if(summary.empty) delta else Set())
  action setup(word:List[Symbol]):bool = s' = {word:word,cursor:word.length(),rows:List(end)}
  action init = {
    nondet word = Set(List(),List(nullableName),List(deadName,a),List(a,deadName),
      List(nullableName,b),List(nullableName,nullableName,a,deadName)).oneOf()
    setup(word)
  }
  action advance = all {
    s.cursor > 0,
    s' = {...s,cursor:s.cursor - 1,
      rows:List(prepend(s.word.nth(s.cursor - 1),s.rows.nth(0))).concat(s.rows)}
  }
  action step = any {advance,all {s.cursor == 0,s' = s}}
  val SuffixSafety = s.cursor >= 0 and s.cursor <= s.word.length()
    and s.rows.length() == s.word.length() - s.cursor + 1
    and s.rows.indices().forall(index =>
      s.rows.nth(index) == forward(s.word.slice(s.cursor + index,s.word.length())))

  run completedSlotTest = setup(List()).expect(s.rows == List(end) and SuffixSafety)
  run suffixSelectsAfterDotTest = setup(List(nullableName,b)).then(advance).then(advance).expect(
    s.rows.nth(0).first == Set(2,3) and s.rows.nth(1).first == Set(3) and
      evidence(s.rows.nth(1),false,Set(0)) == Set(3) and SuffixSafety)
  run nullableSuffixAllowsEOFTest = setup(List(nullableName)).then(advance).expect(
    evidence(s.rows.nth(0),false,Set(0)) == Set(0,2) and
      evidence(s.rows.nth(0),true,Set(0)) == Set(0) and SuffixSafety)
  run unproductiveTailDoesNotEraseReachablePrefixTest =
    setup(List(a,deadName)).then(advance).then(advance).expect(
      s.rows.nth(0).first == Set(2) and not(s.rows.nth(0).empty) and
        s.rows.nth(1) == {first:Set(),empty:false} and SuffixSafety)
  run emptyFirstDoesNotMeanNullableTest = setup(List(deadName)).then(advance).expect(
    evidence(s.rows.nth(0),false,Set(0)) == Set() and not(s.rows.nth(0).empty))
  run repeatedActivationDoesNotRepublishFirstTest = setup(List(b)).then(advance).expect(
    evidence(s.rows.nth(0),false,Set(0)) == Set(3) and
      evidence(s.rows.nth(0),true,Set(0)) == Set())
  run wrongSuffixOffsetIsDetectedTest = setup(List(nullableName,b)).then(advance).then(advance).expect(
    evidence(s.rows.nth(0),false,Set(0)) != evidence(s.rows.nth(1),false,Set(0)))
}
