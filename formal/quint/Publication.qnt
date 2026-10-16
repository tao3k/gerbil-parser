// Verification-only publication protocol; not a parser implementation.
module Publication {
  type State = {source: int, basis: int, phase: str, cancelled: bool, output: int}
  var q: State
  pure val initial: State = {source: 0, basis: 0, phase: "idle", cancelled: false, output: -1}
  pure def prepare(s: State): State = {...s, basis: s.source, phase: "running", cancelled: false, output: -1}
  pure def edit(s: State): State = {...s, source: s.source + 1, output: -1,
    phase: if (s.phase == "published") "idle" else s.phase}
  pure def cancel(s: State): State = {...s, cancelled: true, phase: "cancelled", output: -1}
  pure def finish(s: State): State =
    if (s.phase == "running" and not(s.cancelled) and s.basis == s.source)
    {...s, phase: "published", output: s.basis}
    else {...s, phase: "rejected", output: -1}
  pure def valid(s: State): bool = s.output == -1 or
    (s.phase == "published" and not(s.cancelled) and s.output == s.source and s.basis == s.source)
  action init = q' = initial
  action Prepare = all {q.phase != "running", q' = prepare(q)}
  action Edit = all {q.source < 2, q' = edit(q)}
  action Cancel = all {q.phase == "running", q' = cancel(q)}
  action Finish = all {q.phase == "running", q' = finish(q)}
  action step = any {Prepare, Edit, Cancel, Finish}
  val CurrentPublication = valid(q)

  // Exhaustive one-step preservation over the explicit finite domain.
  pure val States = tuples(Set(0,1,2), Set(0,1,2),
    Set("idle","running","published","cancelled","rejected"), Set(false,true), Set(-1,0,1,2))
    .map(t => {source: t._1, basis: t._2, phase: t._3, cancelled: t._4, output: t._5})
  pure val Closed = States.forall(s => valid(s) implies
    (valid(prepare(s)) and valid(cancel(s)) and valid(finish(s)) and
      (s.source < 2 implies valid(edit(s)))))
  run finiteClosureTest = init.expect(Closed)
  run successfulPublicationTest = init.then(Prepare).then(Finish).expect(CurrentPublication and q.output == 0)
  run staleRequestTest = init.then(Prepare).then(Edit).then(Finish).expect(q.phase == "rejected" and q.output == -1)
  run cancelledRequestTest = init.then(Prepare).then(Cancel).expect(q.output == -1 and q.cancelled)
  run editWithdrawsPublicationTest = init.then(Prepare).then(Finish).then(Edit).expect(q.output == -1)
  // Mutation witnesses establish that the admission checks are consequential.
  pure def uncheckedFinish(s: State): State = {...s, phase: "published", output: s.basis}
  run staleMutationTest = init.then(Prepare).then(Edit).expect(not(valid(uncheckedFinish(q))))
  run cancelledMutationTest = init.then(Prepare).then(Cancel).expect(not(valid(uncheckedFinish(q))))
  // Directed source-queue/ID adapter controls; distinct from revision admission.
  type SourceProduct = {rest:List[int],tokens:List[List[int]],nextToken:int,nextNode:int,frames:List[int],ok:bool}
  pure def sourceProduct(tokens:List[int]):SourceProduct =
    {rest:tokens,tokens:List(),nextToken:0,nextNode:0,frames:List(),ok:true}
  pure def claimSource(s:SourceProduct,expected:int):SourceProduct =
    if(s.ok and s.rest.length() > 0 and s.rest.nth(0) == expected)
      {...s,rest:s.rest.slice(1,s.rest.length()),tokens:s.tokens.append(List(s.nextToken,s.rest.nth(0))),nextToken:s.nextToken+1}
    else {...s,ok:false}
  pure def openSourceNode(s:SourceProduct):SourceProduct =
    {...s,nextNode:s.nextNode+1,frames:List(s.nextNode).concat(s.frames)}
  pure def closeSourceNode(s:SourceProduct,id:int):SourceProduct =
    if(s.ok and s.frames.length() > 0 and s.frames.nth(0) == id)
      {...s,frames:s.frames.slice(1,s.frames.length())} else {...s,ok:false}
  pure def completeSource(s:SourceProduct):bool = s.ok and s.rest == List() and s.frames == List()
  run sourcePayloadOrderAndZeroIdTest = init.expect(
    val start = openSourceNode(sourceProduct(List(0,7,0)))
    val done = closeSourceNode(claimSource(claimSource(claimSource(start,0),7),0),0)
    completeSource(done) and done.tokens == List(List(0,0),List(1,7),List(2,0)) and done.nextNode == 1)
  run wrongSourcePayloadWitnessTest = init.expect(
    not(claimSource(sourceProduct(List(7)),8).ok))
  run mismatchedNodeIdWitnessTest = init.expect(
    not(closeSourceNode(openSourceNode(sourceProduct(List())),1).ok))
  run leftoverSourceWitnessTest = init.expect(
    not(completeSource(closeSourceNode(openSourceNode(sourceProduct(List(7))),0))))

  // Continuations own their view context; cancellation retains provenance.
  type ViewContext = {delta:int,moved:bool}
  pure def moveContext(c:ViewContext,d:int):ViewContext = {delta:c.delta+d,moved:true}
  pure val unmoved:ViewContext = {delta:0,moved:false}
  run cancelledViewRetainsProofTest = init.expect(
    moveContext(moveContext(unmoved,11),-11) == {delta:0,moved:true})
  run siblingContextWitnessTest = init.expect(
    val deferred = unmoved
    val visited = moveContext(unmoved,7)
    deferred != visited and deferred == {delta:0,moved:false})
  run appendBlockOrderTest = init.expect(
    val newestFirst = List(List(3,4),List(1,2))
    List(0).concat(newestFirst.foldl(List(5),(acc,block) => block.concat(acc))) == List(0,1,2,3,4,5))

  // Saved callback returns are not interchangeable with the later counter.
  run retainedOpeningIdTest = init.expect(
    val outer = openSourceNode(sourceProduct(List()))
    val saved = outer.nextNode - 1
    val inner = openSourceNode(outer)
    val closedInner = closeSourceNode(inner,inner.nextNode - 1)
    completeSource(closeSourceNode(closedInner,saved)) and saved == 0 and inner.nextNode == 2)
  run currentCounterIsNotSavedIdWitnessTest = init.expect(
    val opened = openSourceNode(sourceProduct(List()))
    not(closeSourceNode(opened,opened.nextNode).ok))
  run physicalChunkAdvanceTest = init.expect(
    val items = List(10,11,12,13)
    val index = 1
    items.slice(index,items.length()) == List(items.nth(index)).concat(items.slice(index+1,items.length())))

}
