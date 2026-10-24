// Same token-flow abstraction as ParserEngine/Request.lean. This bounded
// transition envelope is NOT a grammar oracle or a scanner/FFI refinement.
module RequestFlow {
  type State = {input:List[int],states:List[int],frames:List[List[int]],rest:List[int],phase:str}
  var q:State
  pure val input = List(1,2,3)
  pure val initial:State = {input:input,states:List(0),frames:List(),rest:input,phase:"running"}
  pure def joined(parts:List[List[int]]):List[int] =
    parts.foldl(List(), (acc,part) => acc.concat(part))
  pure def yieldOf(frames:List[List[int]]):List[int] =
    frames.foldl(List(), (acc,frame) => frame.concat(acc))
  pure def safe(s:State):bool = s.states.length() == s.frames.length() + 1
    and yieldOf(s.frames).concat(s.rest) == s.input
    and (s.phase == "accepted" implies s.frames.length() == 1)
    and (s.phase == "complete" implies (s.frames.length() == 1 and s.rest == List()))
  action init = q' = initial
  action initEmpty = q' = {...initial,input:List(),rest:List()}
  action Shift = all {q.phase == "running",q.rest.length() > 0,q.frames.length() < 6,
    q' = {...q,states:List(1).concat(q.states),
      frames:List(List(q.rest.nth(0))).concat(q.frames),rest:q.rest.slice(1,q.rest.length())}}
  action Reduce(width:int):bool = all {q.phase == "running",width >= 0,width <= q.frames.length(),
    q.frames.length() - width < 6,
    q' = {...q,states:List(2).concat(q.states.slice(width,q.states.length())),
      frames:List(yieldOf(q.frames.slice(0,width))).concat(q.frames.slice(width,q.frames.length()))}}
  action AcceptPrefix = all {q.phase == "running",q.frames.length() == 1,
    q' = {...q,phase:"accepted"}}
  action Complete = all {q.phase == "accepted",q.rest == List(),q' = {...q,phase:"complete"}}
  action Reject = all {q.phase == "running",q' = {...q,phase:"rejected"}}
  action step = any {Shift,Reduce(0),Reduce(1),Reduce(2),Reduce(3),AcceptPrefix,Complete,Reject}
  val FlowSafety = safe(q)

  // Deliberate faulty transitions are excluded from step. They demonstrate
  // why depth-only and accept-only specifications miss semantic failures.
  action WrongOrder = all {q.frames.length() == 2,
    q' = {...q,states:List(2,0),frames:List(joined(q.frames))}}
  action WrongAdvance = all {q.rest.length() > 0,
    q' = {...q,rest:q.rest.slice(1,q.rest.length())}}
  action WrongComplete = all {q.phase == "accepted",q' = {...q,phase:"complete"}}

  run emptyNullableCompletionTest = initEmpty.then(Reduce(0)).then(AcceptPrefix).then(Complete)
    .expect(FlowSafety and q.frames == List(List()) and q.phase == "complete")
  run completeRequestTest = init.then(Shift).then(Shift).then(Reduce(2))
    .then(Shift).then(Reduce(2)).then(AcceptPrefix).then(Complete)
    .expect(FlowSafety and q.frames == List(input) and q.phase == "complete")
  run nullableBeforeShiftTest = init.then(Reduce(0)).then(Shift).then(Reduce(2))
    .expect(FlowSafety and q.frames == List(List(1)) and q.rest == List(2,3))
  run repeatedReductionRetainsLookaheadTest = init.then(Shift).then(Reduce(1)).then(Reduce(1))
    .expect(FlowSafety and q.rest == List(2,3) and q.frames == List(List(1)))
  run suffixOwnershipTest = init.then(Shift).then(Shift).then(Shift).then(Reduce(2))
    .expect(FlowSafety and q.frames == List(List(2,3),List(1)))
  run prefixAcceptanceIsNotCompletionTest = init.then(Shift).then(AcceptPrefix)
    .expect(FlowSafety and q.rest == List(2,3) and q.phase != "complete")
  run wrongOrderWitnessTest = init.then(Shift).then(Shift).then(WrongOrder)
    .expect(q.states.length() == q.frames.length() + 1 and not(FlowSafety))
  run wrongAdvanceWitnessTest = init.then(Shift).then(Reduce(1)).then(WrongAdvance)
    .expect(not(FlowSafety))
  run wrongCompletionWitnessTest = init.then(Shift).then(AcceptPrefix).then(WrongComplete)
    .expect(not(FlowSafety))
  run rejectionPreservesPrivateFlowTest = init.then(Shift).then(Reduce(0)).then(Reject)
    .expect(FlowSafety and q.phase == "rejected" and q.rest == List(2,3))
}
