// One connected bounded fixture: S -> A b; A -> a A | epsilon.
// Terminals a=1,b=2; nonterminals A=10,S=11. State IDs are reference
// automaton IDs, not native compiler IDs. Lexical offsets count characters.
module GrammarPipeline {
  type Candidate = {kind:int,end:int,rank:int,ordinal:int}
  type Frame = {symbol:int,tokens:List[int]}
  type State = {source:List[int],wide:bool,phase:str,nullableA:bool,
    firstA:Set[int],firstS:Set[int],cursor:int,pending:bool,kind:int,
    end:int,ordinal:int,lexed:List[int],emitted:List[int],
    states:List[int],frames:List[Frame]}
  var q:State
  pure val initial:State = {source:List(1,1,2),wide:true,phase:"facts",nullableA:false,
    firstA:Set(),firstS:Set(),cursor:0,pending:false,kind:0,end:0,ordinal:0,
    lexed:List(),emitted:List(),states:List(0),frames:List()}
  action init = q' = initial
  action initUnit = q' = {...initial,wide:false}
  action initInvalid = q' = {...initial,source:List(1)}
  action initNullable = q' = {...initial,source:List(2)}
  action Nullable = all {q.phase == "facts",not(q.nullableA),q' = {...q,nullableA:true}}
  action FirstA = all {q.phase == "facts",q' = {...q,firstA:q.firstA.union(Set(1))}}
  action FirstS = all {q.phase == "facts",
    q' = {...q,firstS:q.firstS.union(q.firstA).union(if(q.nullableA) Set(2) else Set())}}
  action Prepare = all {q.phase == "facts",q.nullableA,q.firstA == Set(1),q.firstS == Set(1,2),
    q' = {...q,phase:"running"}}

  pure def atLeast(a:Candidate,b:Candidate):bool = a.end > b.end or
    (a.end == b.end and (a.rank > b.rank or (a.rank == b.rank and a.ordinal <= b.ordinal)))
  pure def choose(a:Candidate,b:Candidate):Candidate = if(atLeast(b,a)) b else a
  pure def candidates(s:State):List[Candidate] = {
    val at = s.cursor
    val unit = if(at < s.source.length())
      List({kind:s.source.nth(at),end:at + 1,rank:99,ordinal:0}) else List()
    val wide = if(s.wide and at + 1 < s.source.length() and s.source.slice(at,at + 2) == List(1,1))
      List({kind:1,end:at + 2,rank:1,ordinal:1},{kind:1,end:at + 2,rank:1,ordinal:2}) else List()
    unit.concat(wide)
  }
  // The admitted fixture has two lexical kinds; complete candidate production
  // is explicit here. General regex/callback/mode completeness remains open.
  action Scan = all {q.phase == "running",not(q.pending),q.cursor < q.source.length(),
    val allCandidates = candidates(q)
    val winner = allCandidates.slice(1,allCandidates.length()).foldl(allCandidates.nth(0),choose)
    q' = {...q,pending:true,kind:winner.kind,end:winner.end,ordinal:winner.ordinal,
      lexed:q.lexed.append(winner.kind)}}
  action EOF = all {q.phase == "running",not(q.pending),q.cursor == q.source.length(),
    q' = {...q,pending:true,kind:0,end:q.cursor}}
  pure def edge(state:int,symbol:int):int =
    if(symbol == 1 and (state == 0 or state == 1)) 1
    else if(symbol == 10 and state == 0) 2
    else if(symbol == 10 and state == 1) 4
    else if(symbol == 2 and state == 2) 3
    else if(symbol == 11 and state == 0) 5 else -1
  pure def joined(frames:List[Frame]):List[int] =
    frames.foldl(List(),(acc,frame) => frame.tokens.concat(acc))
  action Shift = all {q.phase == "running",q.pending,q.kind != 0,
    edge(q.states.nth(0),q.kind) != -1,
    val target = edge(q.states.nth(0),q.kind)
    q' = {...q,pending:false,cursor:q.end,emitted:q.emitted.append(q.kind),
      states:List(target).concat(q.states),frames:List({symbol:q.kind,tokens:List(q.kind)}).concat(q.frames)}}
  action EmptyA = all {q.phase == "running",q.pending,q.kind == 2,
    q.states.nth(0) == 0 or q.states.nth(0) == 1,
    val target = edge(q.states.nth(0),10)
    q' = {...q,states:List(target).concat(q.states),frames:List({symbol:10,tokens:List()}).concat(q.frames)}}
  action ReduceA = all {q.phase == "running",q.pending,q.kind == 2,q.states.nth(0) == 4,
    q.frames.length() >= 2,q.frames.nth(0).symbol == 10,q.frames.nth(1).symbol == 1,
    val target = edge(q.states.nth(2),10)
    val value = {symbol:10,tokens:joined(q.frames.slice(0,2))}
    q' = {...q,states:List(target).concat(q.states.slice(2,q.states.length())),
      frames:List(value).concat(q.frames.slice(2,q.frames.length()))}}
  action ReduceS = all {q.phase == "running",q.pending,q.kind == 0,q.states.nth(0) == 3,
    q.frames.length() == 2,q.frames.nth(0).symbol == 2,q.frames.nth(1).symbol == 10,
    val value = {symbol:11,tokens:joined(q.frames)}
    q' = {...q,states:List(5,0),frames:List(value)}}
  action Accept = all {q.phase == "running",q.pending,q.kind == 0,q.states == List(5,0),
    q.frames.length() == 1,q.frames.nth(0).symbol == 11,q' = {...q,phase:"complete"}}
  pure def hasAction(s:State):bool = {
    val state = s.states.nth(0)
    ((state == 0 or state == 1) and (s.kind == 1 or s.kind == 2)) or
      ((state == 2 or state == 4) and s.kind == 2) or
      ((state == 3 or state == 5) and s.kind == 0)
  }
  action Reject = all {q.phase == "running",q.pending,not(hasAction(q)),q' = {...q,phase:"rejected"}}
  action step = any {Nullable,FirstA,FirstS,Prepare,Scan,EOF,Shift,EmptyA,ReduceA,ReduceS,Accept,Reject}

  pure def frameValid(frame:Frame):bool =
    if(frame.symbol == 1 or frame.symbol == 2) frame.tokens == List(frame.symbol)
    else if(frame.symbol == 10) frame.tokens.indices().forall(i => frame.tokens.nth(i) == 1)
    else if(frame.symbol == 11) frame.tokens.length() > 0 and
      frame.tokens.nth(frame.tokens.length() - 1) == 2 and
      frame.tokens.slice(0,frame.tokens.length() - 1).indices().forall(i => frame.tokens.nth(i) == 1) else false
  pure def safe(s:State):bool = s.states.length() == s.frames.length() + 1
    and s.states.nth(s.states.length() - 1) == 0
    and s.frames.indices().forall(i => edge(s.states.nth(i + 1),s.frames.nth(i).symbol) == s.states.nth(i))
    and s.frames.indices().forall(i => frameValid(s.frames.nth(i))) and joined(s.frames) == s.emitted
    and s.emitted == s.lexed.slice(0,s.emitted.length())
    and s.lexed.length() == s.emitted.length() + (if(s.pending and s.kind != 0) 1 else 0)
    and (s.phase != "facts" implies (s.nullableA and s.firstA == Set(1) and s.firstS == Set(1,2)))
    and (s.phase == "complete" implies (s.pending and s.kind == 0 and s.cursor == s.source.length()
      and s.frames.length() == 1 and s.frames.nth(0).symbol == 11 and s.lexed == s.emitted))
  val PipelineSafety = safe(q)

  action WrongEarlyAccept = q' = {...q,phase:"complete"}
  action WrongGoto = all {q.frames.length() > 0,q' = {...q,states:List(5).concat(q.states.slice(1,q.states.length()))}}
  action WrongAdvance = all {q.pending,q.kind == 2,q' = {...q,pending:false,cursor:q.end}}
  action WrongFirstPublication = q' = {...q,phase:"running",nullableA:true,firstA:Set(1),firstS:Set(1)}
  action WrongOrder = all {q.frames.length() == 2,
    val value = {symbol:11,tokens:q.frames.nth(0).tokens.concat(q.frames.nth(1).tokens)}
    q' = {...q,states:List(5,0),frames:List(value)}}

  run fullPipelineTest = init.then(Nullable).then(FirstA).then(FirstS).then(Prepare)
    .then(Scan).expect(q.ordinal == 1 and q.end == 2)
    .then(Shift).then(Scan).then(EmptyA).then(ReduceA).then(Shift)
    .then(EOF).then(ReduceS).then(Accept)
    .expect(PipelineSafety and q.emitted == List(1,2) and q.phase == "complete")
  run repeatedShiftPipelineTest = initUnit.then(FirstA).then(FirstS).then(Nullable).then(FirstS).then(Prepare)
    .then(Scan).then(Shift).then(Scan).then(Shift).then(Scan)
    .then(EmptyA).then(ReduceA).then(ReduceA).then(Shift).then(EOF).then(ReduceS).then(Accept)
    .expect(PipelineSafety and q.emitted == List(1,1,2))
  run nullablePipelineTest = initNullable.then(Nullable).then(FirstA).then(FirstS).then(Prepare)
    .then(Scan).then(EmptyA).then(Shift).then(EOF).then(ReduceS).then(Accept)
    .expect(PipelineSafety and q.emitted == List(2))
  run rejectionPipelineTest = initInvalid.then(Nullable).then(FirstA).then(FirstS).then(Prepare)
    .then(Scan).then(Shift).then(EOF).then(Reject)
    .expect(PipelineSafety and q.phase == "rejected")
  run earlyAcceptWitnessTest = initInvalid.then(Nullable).then(FirstA).then(FirstS).then(Prepare)
    .then(Scan).then(Shift).then(EOF).then(WrongEarlyAccept).expect(not(PipelineSafety))
  run incompleteFirstWitnessTest = init.then(WrongFirstPublication).expect(not(PipelineSafety))
  run wrongGotoWitnessTest = init.then(Nullable).then(FirstA).then(FirstS).then(Prepare)
    .then(Scan).then(Shift).then(WrongGoto).expect(not(PipelineSafety))
  run reductionMustRetainPendingTest = init.then(Nullable).then(FirstA).then(FirstS).then(Prepare)
    .then(Scan).then(Shift).then(Scan).then(EmptyA).then(WrongAdvance).expect(not(PipelineSafety))
  run wrongOrderWitnessTest = init.then(Nullable).then(FirstA).then(FirstS).then(Prepare)
    .then(Scan).then(Shift).then(Scan).then(EmptyA).then(ReduceA).then(Shift).then(WrongOrder).expect(not(PipelineSafety))
  // The source lowering defect: A -> A a | epsilon | b admits b a.
  // With a private loop R -> R a | epsilon and A -> R | b it does not.
  // Lean's lowering/unique-owner laws quantify over all finite derivations;
  // this bounded falsifier binds the former shared-owner interpretation.
  type LoopLanguages = {root:Set[List[int]],loop:Set[List[int]]}
  pure def loopRound(shared:bool,s:LoopLanguages):LoopLanguages = {
    val basis = if(shared) s.root else s.loop
    val expanded = s.loop.union(basis.map(word => word.append(1)))
    {root:s.root.union(expanded),loop:expanded}
  }
  pure def loopUnroll(shared:bool):LoopLanguages =
    0.to(3).fold({root:Set(List(),List(2)),loop:Set(List())},(s,index) => loopRound(shared,s))
  run privateRepetitionOwnerTest = init.expect(
    loopUnroll(false).root.contains(List(1,1,1)) and loopUnroll(false).root.contains(List(2))
      and not(loopUnroll(false).root.contains(List(2,1))))
  run sharedRepetitionOwnerWitnessTest = init.expect(loopUnroll(true).root.contains(List(2,1)))
  // Lifecycle falsifier: transition refinement can discard the member that
  // witnessed an earlier packed conflict. Re-admission uses surviving members.
  pure def conflictOrigin(members:List[Set[int]]):bool = {
    val combined = members.foldl(Set(),(acc,cell) => acc.union(cell))
    combined.size() <= 1 or members.foldl(false,(found,cell) => found or cell == combined)
  }
  run splitLosesConflictOriginTest = init.expect(
    conflictOrigin(List(Set(3,4,5),Set(3),Set(4))) and
      not(conflictOrigin(List(Set(3),Set(4)))))
  run splitReadmissionPreservesOriginTest = init.expect(
    List(List(Set(3)),List(Set(4))).foldl(true,(valid,group) => valid and conflictOrigin(group)) and
      Set(3).union(Set(4)) == Set(3,4))

  // Physical production indices survive equal semantic rows.
  pure def coreOffset(widths:List[int],production:int):int =
    widths.indices().filter(index => index < production)
      .fold(0,(offset,index) => offset + widths.nth(index) + 1)
  run duplicateProductionIdentityTest = init.expect(
    List({lhs:1,rhs:List(2)},{lhs:1,rhs:List(2)}).nth(0) ==
      List({lhs:1,rhs:List(2)},{lhs:1,rhs:List(2)}).nth(1) and
    coreOffset(List(1,1),0) == 0 and coreOffset(List(1,1),1) == 2 and
    coreOffset(List(0,0),1) == 1)

  pure def peers(ids:List[int],node:int):Set[int] =
    ids.indices().filter(peer => ids.nth(peer) == ids.nth(node))
  pure def splitSources(oldIds:List[int],newIds:List[int]):Set[int] =
    oldIds.indices().filter(source =>
      peers(oldIds,source).exists(peer => newIds.nth(peer) != newIds.nth(source)))
  type DependencyEdge = {source:int,target:int}
  pure def dirtyNodes(oldIds:List[int],newIds:List[int],edges:Set[DependencyEdge]):Set[int] =
    edges.filter(edge => splitSources(oldIds,newIds).contains(edge.source))
      .fold(Set(),(marked,edge) => marked.union(peers(newIds,edge.target)))
  run renumberWithoutSplitTest = init.expect(
    splitSources(List(0,0,1,1),List(1,1,0,0)) == Set() and
      List(0,0,1,1) != List(1,1,0,0))
  run missedDirtySuccessorWitnessTest = init.expect(
    dirtyNodes(List(0,0,0,0),List(0,1,1,1),
      Set({source:0,target:1},{source:1,target:2},{source:2,target:3})) == Set(1,2,3) and
    dirtyNodes(List(0,0,0,0),List(0,1,1,1),Set()) == Set())

  pure def clearTouched(slots:List[int],touched:Set[int]):List[int] =
    slots.indices().fold(slots,(result,key) =>
      if(touched.contains(key)) result.replaceAt(key,-1) else result)
  run publicationScratchIsolationTest = init.expect({
    val row = List(4,-1,7)
    clearTouched(row,Set(0,2)) == List(-1,-1,-1) and row == List(4,-1,7) and
      clearTouched(row,Set(0)) != List(-1,-1,-1)
  })

  // Native storage writes are bounded; zero is a live value, not absence.
  run finiteStoragePublicationTest = init.expect({
    val scratch = List(-1,-1,-1).replaceAt(0,0).replaceAt(2,9)
    val published = scratch
    clearTouched(scratch,Set(0,2)) == List(-1,-1,-1) and
      published == List(0,-1,9) and clearTouched(scratch,Set(0)) != List(-1,-1,-1)
  })
  pure def suffixTransfer(first:Set[int],nullable:bool,activated:bool,delta:Set[int]):Set[int] =
    (if(activated) Set() else first).union(if(nullable) delta else Set())
  run firstActivationAndNullableDeltaTest = init.expect(
    suffixTransfer(Set(4),true,false,Set(0,70)) == Set(0,4,70) and
      suffixTransfer(Set(4),true,true,Set(2)) == Set(2) and
      suffixTransfer(Set(4),false,true,Set(0,70)) == Set())
  type CoreLookahead = {core:int,looks:Set[int]}
  pure def quotientPosition(rows:List[List[CoreLookahead]],position:int):Set[int] =
    rows.foldl(Set(),(mask,row) => mask.union(row.nth(position).looks))
  run quotientCoreAlignmentWitnessTest = init.expect({
    val first = List({core:1,looks:Set(3)},{core:2,looks:Set(7)})
    val aligned = List({core:1,looks:Set(5)},{core:2,looks:Set(9)})
    val reversed = List(aligned.nth(1),aligned.nth(0))
    quotientPosition(List(first,aligned),0) == Set(3,5) and
      quotientPosition(List(first,reversed),0) == Set(3,9) and
      first.nth(0).core != reversed.nth(0).core
  })
  run layoutFallbackMustRetainRawReductionsTest = init.expect({
    val resolved = Set(100)
    val rawReductions = Set(1,2)
    val allowed = resolved
    val blocked = rawReductions
    allowed.union(blocked).subseteq(resolved.union(rawReductions)) and
      not(blocked.subseteq(resolved)) and blocked != Set()
  })

  type HeapObject = {tag:str,refs:List[int],atom:int}
  type HeapWrite = {cells:List[HeapObject],ok:bool}
  pure def heapVectorWrite(heap:List[HeapObject],owner:int,index:int,value:int):HeapWrite =
    if(not(heap.indices().contains(owner))) {cells:heap,ok:false}
    else if(heap.nth(owner).tag != "vector" or not(heap.nth(owner).refs.indices().contains(index)))
      {cells:heap,ok:false}
    else {cells:heap.replaceAt(owner,{...heap.nth(owner),refs:heap.nth(owner).refs.replaceAt(index,value)}),ok:true}
  run heapPublicationAndAtomicFailureTest = init.expect({
    val heap = List({tag:"pair",refs:List(1,1),atom:0},
      {tag:"atom",refs:List(),atom:7},{tag:"vector",refs:List(0),atom:0})
    val cleared = heapVectorWrite(heap,2,0,1)
    cleared.ok and cleared.cells.nth(0) == heap.nth(0) and cleared.cells.nth(1) == heap.nth(1) and
      cleared.cells.nth(2) != heap.nth(2) and
      heapVectorWrite(heap,0,0,2) == {cells:heap,ok:false} and
      heapVectorWrite(heap,2,9,0) == {cells:heap,ok:false}
  })
  type QueueBudget = {remaining:Set[int],queued:List[int]}
  pure def queueRank(s:QueueBudget):int = 2 * s.remaining.size() + s.queued.length()
  pure def queuePublication(s:QueueBudget,owner:int,incoming:Set[int]):QueueBudget =
    if(s.remaining.intersect(incoming) == Set()) s
    else {remaining:s.remaining.exclude(incoming),
      queued:if(s.queued.indices().exists(i => s.queued.nth(i) == owner)) s.queued else s.queued.append(owner)}
  run freshQueueBudgetProgressTest = init.expect({
    val before = {remaining:Set(0,1,70),queued:List()}
    val published = queuePublication(before,4,Set(0,70))
    val repeated = queuePublication(published,4,Set(0,70))
    queueRank(published) < queueRank(before) and repeated == published and
      queueRank({...published,queued:List()}) < queueRank(published)
  })
  run stalePublicationCannotBeProgressTest = init.expect({
    val stale = {remaining:Set(1),queued:List(4)}
    queuePublication(stale,4,Set(70)) == stale and
      queueRank({...stale,queued:stale.queued.append(4)}) > queueRank(stale)
  })
  type ScopeContext = {columns:int,frames:List[int]}
  type ScopeState = {current:ScopeContext,saved:List[ScopeContext]}
  pure def unwindScope(s:ScopeState,depth:int):ScopeState =
    0.to(s.saved.length()).fold(s,(active,index) =>
      if(active.saved.length() <= depth) active
      else {current:active.saved.nth(0),saved:active.saved.slice(1,active.saved.length())})
  run layoutEveryExitRestoresScopeTest = init.expect({
    val outer = {columns:1,frames:List(100)}
    val inner = {columns:2,frames:List(7)}
    val current = {current:{columns:3,frames:List(9)},saved:List(inner,outer)}
    Set("returned","failed","cancelled").forall(outcome =>
      {result:outcome,state:unwindScope(current,0)} ==
        {result:outcome,state:{current:outer,saved:List()}}) and
      current.current.columns != outer.columns
  })
  pure def layoutClose(frames:List[int],nextEligible:bool,boundary:bool,eof:bool):{ok:bool,frames:List[int]} =
    if(frames == List()) {ok:false,frames:frames}
    else if(eof or (not(nextEligible) and boundary))
      {ok:true,frames:frames.slice(1,frames.length())} else {ok:false,frames:frames}
  run layoutEmptySuccessAndNextPriorityTest = init.expect(
    layoutClose(List(1),false,false,true) == {ok:true,frames:List()} and
      layoutClose(List(),false,false,true) == {ok:false,frames:List()} and
      layoutClose(List(1),true,true,false) == {ok:false,frames:List(1)})

  pure def partitionContinues(nodes:int,current:int,nextGroups:int):bool =
    current < nextGroups and nextGroups < nodes
  run partitionRecursionMustProgressTest = init.expect(
    partitionContinues(4,2,3) and not(partitionContinues(4,2,2)) and
      not(partitionContinues(4,3,4)) and not(partitionContinues(4,3,2)))

  pure def renameHeapObject(object:HeapObject,rho:List[int]):HeapObject =
    {...object,refs:object.refs.foldl(List(),(refs,address) => refs.append(rho.nth(address)))}
  pure def copiedRoots(before:List[HeapObject],after:List[HeapObject],rho:List[int],roots:Set[int]):bool =
    roots.forall(root => if(after.indices().contains(rho.nth(root)))
      after.nth(rho.nth(root)) == renameHeapObject(before.nth(root),rho) else false)
  run suspendedRootsMustSurviveCollectionTest = init.expect({
    val before = List({tag:"pair",refs:List(1,1),atom:0},
      {tag:"atom",refs:List(),atom:7},{tag:"vector",refs:List(0),atom:0})
    val rho = List(1,0,2)
    val activeOnly = List(renameHeapObject(before.nth(1),rho),renameHeapObject(before.nth(0),rho))
    val retained = activeOnly.append(renameHeapObject(before.nth(2),rho))
    copiedRoots(before,activeOnly,rho,Set(0,1)) and
      not(copiedRoots(before,activeOnly,rho,Set(0,1,2))) and
      copiedRoots(before,retained,rho,Set(0,1,2)) and retained.nth(1).refs == List(0,0)
  })
  pure def moveOwner(owner:int):int = if(owner == 0) 7 else if(owner == 3) 0 else owner
  run relocationMustNotReuseNumericFrontierTest = init.expect(
    3 >= 2 and moveOwner(3) < 2 and
      not(Set(0).map(moveOwner).contains(moveOwner(3))) and moveOwner(0) != moveOwner(3))
  pure def sharedWindLength(current:List[int],target:List[int]):int = {
    val shortest = if(current.length() < target.length()) current.length() else target.length()
    current.indices().filter(i => if(i < target.length()) current.nth(i) != target.nth(i) else false)
      .fold(shortest,(shared,i) => if(i < shared) i else shared)
  }
  pure def reverseWind(path:List[int]):List[int] = path.foldl(List(),(rest,owner) => List(owner).concat(rest))
  run continuationCrossingUsesActivationIdentityTest = init.expect({
    val current = List(1,2,3)
    val target = List(1,4)
    val shared = sharedWindLength(current,target)
    reverseWind(current.slice(shared,current.length())) == List(3,2) and
      target.slice(shared,target.length()) == List(4) and
      sharedWindLength(current,current) == current.length()
  })
  pure def closureQueuePublication(s:QueueBudget,owner:int,incoming:Set[int]):QueueBudget =
    if(s.remaining.intersect(incoming) == Set()) s
    else {remaining:s.remaining.exclude(incoming),queued:s.queued.append(owner)}
  run canonicalClosureDuplicateQueueStillProgressesTest = init.expect({
    val before = {remaining:Set(1,70),queued:List(4)}
    val after = closureQueuePublication(before,4,Set(70))
    after.queued == List(4,4) and queueRank(after) < queueRank(before) and
      closureQueuePublication(after,4,Set(70)) == after and
      queuePublication(before,4,Set(70)).queued == List(4)
  })
  pure def backwardSignature(vertex:int):int = if(vertex == 1) 1 else 0
  pure def backwardBuckets(block:Set[int]):List[Set[int]] =
    block.map(backwardSignature).fold(List(),(groups,key) =>
      groups.append(block.filter(vertex => backwardSignature(vertex) == key)))
  pure def backwardMass(blocks:List[Set[int]]):int = blocks.foldl(0,(total,block) => total + block.size())
  run backwardChangedRequiresNonemptyBlocksTest = init.expect({
    val before = List(Set(0,1,2),Set(3,4))
    val after = before.foldl(List(),(groups,block) => groups.concat(backwardBuckets(block)))
    val invalid = List(Set(),Set(0,1))
    val invalidAfter = invalid.foldl(List(),(groups,block) => groups.concat(backwardBuckets(block)))
    after.length() > before.length() and backwardMass(after) == backwardMass(before) and
      backwardBuckets(Set(0,1)).length() > 1 and invalidAfter.length() == invalid.length()
  })
  type CanonicalRegistry = {keys:List[List[int]],pending:List[int],processed:int}
  pure def internMatrix(s:CanonicalRegistry,key:List[int]):CanonicalRegistry =
    if(s.keys.indices().exists(i => s.keys.nth(i) == key)) s
    else {...s,keys:s.keys.append(key),pending:s.pending.append(s.keys.length())}
  run canonicalInterningReusesZeroAndOnlyQueuesNewKeysTest = init.expect({
    val before = {keys:List(List(1,0)),pending:List(),processed:1}
    val after = internMatrix(before,List(0,1))
    internMatrix(before,List(1,0)) == before and after.pending == List(1) and
      after.pending.length() + after.processed == after.keys.length() and
      internMatrix(after,List(0,1)) == after
  })

  // GC maps select tagged pointer slots; numeric lookalikes are not roots.
  type RootWord = {pointer:bool,value:int}
  pure def scanFrame(words:List[RootWord],selected:Set[int]):Set[int] =
    words.indices().filter(i => selected.contains(i) and words.nth(i).pointer)
      .map(i => words.nth(i).value)
  run frameMapAndPointerTagBothRequiredTest = init.expect({
    val words = List({pointer:true,value:70},{pointer:false,value:70},
      {pointer:true,value:1024},{pointer:true,value:9})
    scanFrame(words,Set(0,1,2)) == Set(70,1024) and
      scanFrame(words,Set(1)) == Set() and
      scanFrame(words,Set(0)) != Set(70,1024)
  })
  type HashEntry = {key:List[int],value:int}
  pure def bucketLookup(bucket:List[HashEntry],key:List[int]):List[int] = {
    bucket.foldl(List(),(result,entry) =>
      if(result.length() == 0 and entry.key == key) List(entry.value) else result)
  }

  run collisionEqualityAndZeroSurviveUpdateTest = init.expect({
    val bucket = List({key:List(2),value:7},{key:List(1),value:0})
    val updated = List({key:List(2),value:8}).concat(bucket)
    bucketLookup(bucket,List(1)) == List(0) and
      bucketLookup(updated,List(2)) == List(8) and
      bucketLookup(updated,List(1)) == List(0) and
      bucketLookup(updated,List(3)) == List()
  })
  pure def notifyReverse(edges:Set[DependencyEdge],changed:Set[int]):Set[int] =
    edges.filter(edge => changed.contains(edge.target)).map(edge => edge.source)
  run reverseCoreNotificationMustIncludeEveryReaderTest = init.expect({
    val graph = Set({source:0,target:2},{source:1,target:2},
      {source:2,target:2},{source:3,target:4})
    notifyReverse(graph,Set(2)) == Set(0,1,2) and
      notifyReverse(graph.filter(edge => edge != {source:1,target:2}),Set(2)) == Set(0,2) and
      notifyReverse(graph,Set()) == Set()
  })
  pure def strongMark(edges:Set[DependencyEdge],known:Set[int]):Set[int] =
    known.union(edges.filter(edge => known.contains(edge.source)).map(edge => edge.target))
  run capturedClosureTableCycleNeedsTransitiveRootsTest = init.expect({
    val edges = Set({source:0,target:1},{source:1,target:2},
      {source:2,target:3},{source:3,target:2},{source:3,target:4})
    val reached = 0.to(4).fold(Set(0),(marked,round) => strongMark(edges,marked))
    reached == Set(0,1,2,3,4) and strongMark(edges,reached) == reached and
      0.to(4).fold(Set(),(marked,round) => strongMark(edges,marked)) == Set()
  })

  pure def power2(width:int):int = if(width == 0) 1 else 0.to(width - 1).fold(1,(power,i) => power * 2)
  pure def generatedMap(flags:List[bool]):int =
    flags.foldl({power:1,mask:0},(s,live) =>
      {power:s.power * 2,mask:s.mask + (if(live) s.power else 0)}).mask
  pure def signedChunk(value:int):int = if(value < 2147483648) value else value - 4294967296
  pure def unsignedChunk(value:int):int = ((value % 4294967296) + 4294967296) % 4294967296
  run targetMapChunksAndSignedLiteralTest = init.expect({
    val flags = 0.to(95).fold(List(),(rows,i) => rows.append(true))
    val mask = generatedMap(flags)
    val sparse = 1 + power2(31) + power2(32) + power2(65) + power2(95)
    generatedMap(List(true,false,true,false,true)) == 21 and
      sparse % power2(32) == 2147483649 and
      (sparse / power2(32)) % power2(32) == 1 and
      (sparse / power2(64)) % power2(32) == 2147483650 and
      mask == power2(96) - 1 and
      unsignedChunk(signedChunk(mask % power2(32))) == power2(32) - 1 and
      (mask / power2(32)) % power2(32) == power2(32) - 1 and
      (mask / power2(64)) % power2(32) == power2(32) - 1 and
      0.to(95).forall(i => (mask / power2(i)) % 2 == 1)
  })
  pure def internalMapLive(ordinary:bool,aligned:int,registers:int,index:int):bool =
    ordinary or (aligned <= index and index <= aligned + registers)
  run internalSavedRegisterBandIncludesReturnVariableTest = init.expect(
    internalMapLive(false,100,5,100) and internalMapLive(false,100,5,105) and
      not(internalMapLive(false,100,5,106)) and not(internalMapLive(false,100,5,99)) and
      internalMapLive(true,100,5,99))
  pure def probePosition(capacity:int,start:int,turn:int):int =
    if(turn <= start) start - turn else capacity + start - turn
  run physicalProbeCoversEverySlotExactlyOnceTest = init.expect(
    1.to(9).forall(capacity => 0.to(capacity - 1).forall(start => {
      val seen = 0.to(capacity - 1).map(turn => probePosition(capacity,start,turn))
      seen == 0.to(capacity - 1) and seen.size() == capacity and
        0.to(capacity - 2).forall(turn =>
          probePosition(capacity,start,turn + 1) ==
            (if(probePosition(capacity,start,turn) == 0) capacity - 1
             else probePosition(capacity,start,turn) - 1))
    })))
  type ProbeCell = {state:str,key:int,value:int}
  pure def observeProbe(rows:List[ProbeCell],key:int):{stopped:bool,found:List[int]} =
    rows.foldl({stopped:false,found:List()},(s,cell) =>
      if(s.stopped) s else if(cell.state == "unused") {stopped:true,found:List()}
      else if(cell.state == "entry" and cell.key == key) {stopped:true,found:List(cell.value)} else s)
  run deletedProbeSlotMustNotActLikeUnusedTest = init.expect({
    val prefix = List({state:"deleted",key:0,value:0},{state:"entry",key:3,value:0},
      {state:"unused",key:0,value:0})
    observeProbe(prefix,3) == {stopped:true,found:List(0)} and
      observeProbe(prefix,4) == {stopped:true,found:List()} and
      observeProbe(List({state:"deleted",key:0,value:0}),4) == {stopped:false,found:List()}
  })
  type ScanWorker = {active:bool,queued:Set[int],held:Set[int]}
  pure def workerOwnership(workers:List[ScanWorker],count:int):bool =
    count == workers.foldl(0,(n,w) => n + (if(w.active) 1 else 0)) and
      workers.indices().forall(i => workers.nth(i).active or
        (workers.nth(i).queued == Set() and workers.nth(i).held == Set()))
  run reservedObjectPreventsFalseGlobalTerminationTest = init.expect({
    val busy = {active:true,queued:Set(),held:Set(70)}
    workerOwnership(List(busy),1) and not(workerOwnership(List(busy),0)) and
      not(workerOwnership(List({...busy,active:false}),0)) and
      workerOwnership(List({active:false,queued:Set(),held:Set()}),0)
  })
  run chunkStealingRetainsInFlightOwnershipTest = init.expect({
    val before = List({active:true,queued:Set(1,2),held:Set()},
      {active:true,queued:Set(3),held:Set()})
    val after = List({active:true,queued:Set(),held:Set()},
      {active:true,queued:Set(3),held:Set(1,2)})
    workerOwnership(before,2) and workerOwnership(after,2) and
      before.foldl(Set(),(jobs,w) => jobs.union(w.queued).union(w.held)) ==
        after.foldl(Set(),(jobs,w) => jobs.union(w.queued).union(w.held)) and
      not(workerOwnership(after,1))
  })

  // Source branch controls for _front.scm and _gvm.scm; not a CFG approximation.
  pure def liveBefore(base:Set[int],force:Set[int],steps:List[Set[int]]):Set[int] =
    steps.foldl(base,(live,uses) => live.union(force).union(uses))
  run forcedLivenessIncludesPlaceholderButNotFinalBoundaryTest = init.expect(
    liveBefore(Set(1),Set(2),List(Set())) == Set(1,2) and
      liveBefore(Set(1),Set(2),List()) == Set(1))
  run actualEvaluationSuffixMustOwnItsBoundaryTest = init.expect({
    val original = List(Set(1),Set(2))
    val scheduled = List(Set(2),Set(1))
    liveBefore(Set(),Set(),List(scheduled.nth(1))) == Set(1) and
      liveBefore(Set(),Set(),List(original.nth(1))) != Set(1)
  })
  pure def closureEnvironmentLive(explicit:bool,closed:Set[int],live:Set[int]):bool =
    explicit or closed.exists(variable => live.contains(variable))
  pure def returnUses(tailReason:bool,ret:int):Set[int] = if(tailReason) Set(ret) else Set()
  run capturedUseRetainsEnvironmentAndTailReturnIsConditionalTest = init.expect(
    closureEnvironmentLive(false,Set(5,6),Set(6)) and
      closureEnvironmentLive(true,Set(),Set()) and
      not(closureEnvironmentLive(false,Set(5,6),Set(7))) and
      liveBefore(Set(),Set(),List(returnUses(true,99))) == Set(99) and
      liveBefore(Set(),Set(),List(returnUses(false,99))) == Set())
  type ChunkVisibility = {initialized:int,visible:int,tail:int}
  pure def chunkSafe(s:ChunkVisibility):bool = s.tail <= s.visible and s.visible <= s.initialized
  run atomicTailWithoutPayloadFenceIsUnsafeTest = init.expect(
    not(chunkSafe({initialized:1,visible:0,tail:1})) and
      chunkSafe({initialized:1,visible:1,tail:1}))
  pure def takeLocked(queue:List[int]):{picked:List[int],remaining:List[int]} =
    if(queue.length() == 0) {picked:List(),remaining:List()}
    else {picked:List(queue.head()),remaining:queue.tail()}
  run lockedRecheckMustDiscardOptimisticQueueHeadTest = init.expect({
    val observedBeforeLock = List(7)
    val first = takeLocked(observedBeforeLock)
    val second = takeLocked(first.remaining)
    first.picked == List(7) and second.picked == List() and
      // Reusing the optimistic head duplicates the first thief's reservation.
      takeLocked(observedBeforeLock).picked == first.picked
  })
  run ticketUniquenessRequiresNoWrapBoundTest = init.expect(
    0.to(7).forall(a => 0.to(7).forall(b => (a % 8 == b % 8) == (a == b))) and
      0 % 8 == 8 % 8)

}
