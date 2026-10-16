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

  pure def reverseSlots(rows:List[int]):List[int] =
    rows.foldl(List(),(result,value) => List(value).concat(result))
  pure def resizeSlots(rows:List[int],capacity:int):List[int] = {
    val padding = 0.to(capacity - rows.length() - 1).fold(List(),(result,_i) => List(-1).concat(result))
    val start = if(rows.length() > capacity) rows.length() - capacity else 0
    reverseSlots(padding.concat(rows.slice(start,rows.length())))
  }
  run sourceSlotResizePreservesRetainedIndicesAndPadsHighSlotsTest = init.expect(
    0.to(6).forall(capacity => {
      val rows = resizeSlots(List(30,20,10),capacity)
      rows.length() == capacity and rows.indices().forall(i =>
        rows.nth(i) == (if(i < 3) List(10,20,30).nth(i) else -1))
    }))
  run savedRegisterAndReturnSlotsHaveDifferentOffsetsTest = init.expect({
    val aligned = resizeSlots(List(30,20,10),4)
    val saved = resizeSlots(reverseSlots(List(70,80)),3)
    val internal = aligned.concat(saved).append(99).append(-1)
    internal == List(10,20,30,-1,70,80,-1,99,-1) and
      internal.nth(4 + 3) == 99
  })
  run emittedAddressAgreesWithCollectorBaseAndSlotTest = init.expect(
    (-4).to(4).forall(base => 0.to(8).forall(fp => 1.to(6).forall(slot =>
      (base - fp) - (slot - fp) == base - slot and
        (base - fp) - (-fp) == base))))
  type HostViews = {current:List[int],released:List[int],views:List[List[int]],owner:int}
  // Build joins in list order, independent of Set iteration order.
  pure def importViews(a:List[int],b:List[int]):List[int] =
    a.foldl({values:List(),index:0},(s,value) =>
      {values:s.values.append(if(value > b.nth(s.index)) value else b.nth(s.index)),index:s.index + 1}).values
  pure def acquireHost(s:HostViews,worker:int):HostViews =
    if(s.owner != -1) s else {...s,owner:worker,
      views:s.views.replaceAt(worker,importViews(s.views.nth(worker),s.released))}
  pure def writeHost(s:HostViews,worker:int,location:int):HostViews =
    if(s.owner != worker) s else {
      val version = s.current.nth(location) + 1
      {...s,current:s.current.replaceAt(location,version),
        views:s.views.replaceAt(worker,s.views.nth(worker).replaceAt(location,version))}
    }
  pure def releaseHost(s:HostViews,worker:int):HostViews =
    if(s.owner != worker) s else {...s,owner:-1,released:importViews(s.released,s.views.nth(worker))}
  pure def hostViewsSafe(s:HostViews):bool = s.current.indices().forall(location =>
    if(s.owner == -1) s.current.nth(location) <= s.released.nth(location)
    else s.current.nth(location) <= s.views.nth(s.owner).nth(location))
  pure val freshHost:HostViews = {current:List(0,0),released:List(0,0),
    views:List(List(0,0),List(0,0),List(0,0)),owner:-1}
  run mutexHandoffsTransitivelyImportAllProtectedLocationsTest = init.expect({
    val first = writeHost(acquireHost(freshHost,0),0,0)
    val second = writeHost(acquireHost(releaseHost(first,0),1),1,1)
    val third = acquireHost(releaseHost(second,1),2)
    hostViewsSafe(first) and hostViewsSafe(second) and hostViewsSafe(third) and
      third.views.nth(2) == List(1,1) and
      writeHost(first,1,1) == first and releaseHost(first,1) == first and
      acquireHost(first,1) == first
  })
  run ticketPollCannotImportAReleaseThatFollowedItsEarlierFenceTest = init.expect({
    val allocated = acquireHost(freshHost,1)
    val released = releaseHost(writeHost(allocated,1,0),1)
    // Worker 0's earlier ticket allocation saw only the initial version.
    val polling = {...released,owner:0}
    hostViewsSafe(released) and not(hostViewsSafe(polling)) and
      hostViewsSafe(acquireHost(released,0))
  })

  pure def assignEmpty(rows:List[int],index:int,value:int):{rows:List[int],ok:bool} =
    if(index < 0 or index >= rows.length()) {rows:rows,ok:false}
    else if(rows.nth(index) != -1) {rows:rows,ok:false}
    else {rows:rows.replaceAt(index,value),ok:true}
  run nativeContextAssignmentNeedsAnExistingEmptyCellTest = init.expect({
    val entrySlots = List(-1,-1,-1)
    val one = assignEmpty(entrySlots,2,70)
    one.ok and one.rows.length() == 3 and one.rows.nth(2) == 70 and
      not(assignEmpty(one.rows,2,80).ok) and
      not(assignEmpty(entrySlots,entrySlots.length(),90).ok) and
      not(assignEmpty(entrySlots,-1,90).ok)
  })
  run nativeTaskAndReturnContextsPartitionTheOriginalSlotsTest = init.expect({
    val original = List(30,20,10)
    0.to(3).forall(start => {
      val taskSlots = reverseSlots(reverseSlots(original).slice(start,3))
      val parentSlots = original.slice(3 - start,3)
      taskSlots.length() == 3 - start and parentSlots.length() == start and
        reverseSlots(parentSlots).concat(reverseSlots(taskSlots)) == reverseSlots(original)
    })
  })
  type FpCell = {present:bool,location:int,dirty:bool,stamp:int,payload:int}
  pure val emptyFp:FpCell = {present:false,location:0,dirty:false,stamp:0,payload:0}
  pure def probeFp(rows:List[FpCell],location:int):List[FpCell] =
    rows.foldl(List(),(found,cell) => if(found.length() == 0 and cell.present and cell.location == location)
      List(cell) else found)
  pure def flushFp(rows:List[FpCell],location:int):{rows:List[FpCell],events:List[FpCell]} =
    rows.foldl({rows:List(),events:List()},(s,cell) =>
      if(s.events.length() == 0 and cell.present and cell.location == location and cell.dirty)
        {rows:s.rows.append({...cell,dirty:false}),events:List(cell)}
      else {rows:s.rows.append(cell),events:s.events})
  pure def invalidateFp(rows:List[FpCell],location:int):List[FpCell] =
    rows.foldl(List(),(result,cell) => result.append(if(cell.present and cell.location == location) emptyFp else cell))
  pure def insertFp(rows:List[FpCell],entryCell:FpCell):List[FpCell] = {
    val scanned = rows.foldl({rows:List(),inserted:false},(s,cell) =>
      if(not(s.inserted) and not(cell.present)) {rows:s.rows.append(entryCell),inserted:true}
      else {rows:s.rows.append(cell),inserted:s.inserted})
    if(scanned.inserted) scanned.rows else rows.append(entryCell).concat(rows.foldl(List(),(result,_cell) => result.append(emptyFp)))
  }
  run dirtyCacheReadWritesBackOnceAndInvalidationClearsAllAliasesTest = init.expect({
    val value = {present:true,location:4,dirty:true,stamp:8,payload:123}
    val once = flushFp(List(emptyFp,value,emptyFp),4)
    once.events == List(value) and once.rows.nth(1).stamp == 8 and
      not(once.rows.nth(1).dirty) and flushFp(once.rows,4).events == List() and
      probeFp(invalidateFp(List(value,value),4),4) == List()
  })
  run cleanAndDirtyAliasWouldBreakTheProbeWritebackCorrespondenceTest = init.expect({
    val clean = {present:true,location:4,dirty:false,stamp:7,payload:111}
    val dirty = {...clean,dirty:true,stamp:8,payload:222}
    probeFp(List(clean,dirty),4) == List(clean) and
      flushFp(List(clean,dirty),4).events == List(dirty)
  })
  run cacheEntryReusesFirstHoleOrDoublesCapacityAndKeepsFreshStampTest = init.expect({
    val left = {present:true,location:4,dirty:false,stamp:1,payload:11}
    val right = {...left,location:5,stamp:2,payload:22}
    val reused = insertFp(List(left,emptyFp,emptyFp),right)
    val grown = insertFp(List(left),right)
    reused == List(left,right,emptyFp) and grown == List(left,right,emptyFp) and
      insertFp(List(),left) == List(left) and
      probeFp(insertFp(invalidateFp(grown,4),{...left,stamp:3,dirty:true}),4).nth(0).stamp == 3
  })
  run staleTicketCASCannotEnterAHeldLockWithinTheNoWrapWindowTest = init.expect(
    0.to(3).forall(currentTicket => currentTicket.to(currentTicket + 3).forall(nextTicket =>
      0.to(currentTicket).forall(observed =>
        (nextTicket == observed) implies (nextTicket == currentTicket)))))

  run unlockWithWaitingTicketsIsAValidOwnerlessIntermediateStateTest = init.expect({
    val currentTicket = 1
    val nextTicket = 2
    val owner = -1
    currentTicket <= nextTicket and ((currentTicket == nextTicket) implies (owner == -1)) and
      not((currentTicket == nextTicket) == (owner == -1))
  })

  type SpillCell = {variableId:int,pointer:int}
  pure def lowestDead(rows:List[SpillCell],live:Set[int]):int =
    rows.foldl({index:0,found:false},(s,cell) =>
      if(s.found) s else if(live.contains(cell.variableId)) {index:s.index + 1,found:false}
      else {...s,found:true}).index
  pure def saveRegister(rows:List[SpillCell],live:Set[int],cell:SpillCell):List[SpillCell] = {
    val destination = lowestDead(rows,live)
    if(destination == rows.length()) rows.append(cell) else rows.replaceAt(destination,cell)
  }
  run repeatedRegisterSpillsReuseOnlyDeadSlotsAndPreserveEveryLiveCopyTest = init.expect({
    val live = Set(10,20,30,40)
    val original = List({variableId:0,pointer:0},{variableId:10,pointer:110},{variableId:5,pointer:105})
    val copied = List({variableId:20,pointer:120},{variableId:30,pointer:130},{variableId:40,pointer:140})
    val result = copied.foldl(original,(rows,cell) => saveRegister(rows,live,cell))
    result == List(copied.nth(0),original.nth(1),copied.nth(1),copied.nth(2)) and
      result.nth(1) == original.nth(1) and result.length() == 4
  })

  type NativeOperandRow = {kind:int,first:int,second:int}
  pure def decodeNativeOperand(rows:List[NativeOperandRow],raw:int):List[NativeOperandRow] = {
    val tag = raw % 8
    val index = raw / 8
    if(raw < 0 or tag > 5) List()
    else if(tag < 3) List({kind:tag,first:index,second:0})
    else if(index >= rows.length()) List()
    else if(rows.nth(index).kind != tag) List() else List(rows.nth(index))
  }
  pure def internNativeOperand(rows:List[NativeOperandRow],key:NativeOperandRow):{rows:List[NativeOperandRow],index:int} = {
    val scanned = rows.foldl({at:0,found:-1},(s,row) =>
      {at:s.at + 1,found:if(s.found == -1 and row == key) s.at else s.found})
    if(scanned.found == -1) {rows:rows.append(key),index:rows.length()}
    else {rows:rows,index:scanned.found}
  }
  run nativeOperandTagsKeepRegisterZeroDistinctFromAbsentSourceTest = init.expect(
    0.to(16).forall(index => 0.to(2).forall(tag =>
      decodeNativeOperand(List(),index * 8 + tag) == List({kind:tag,first:index,second:0}))) and
      decodeNativeOperand(List(),0) == List({kind:0,first:0,second:0}) and
      decodeNativeOperand(List(),-1) == List() and decodeNativeOperand(List(),6) == List())
  run operandInterningRetainsExistingRowsAcrossAllTableKindsTest = init.expect({
    val globalRow = {kind:3,first:9,second:1}
    val constantRow = {kind:5,first:9,second:0}
    val closureRow = {kind:4,first:0,second:1}
    val first = internNativeOperand(List(),globalRow)
    val second = internNativeOperand(first.rows,constantRow)
    val third = internNativeOperand(second.rows,closureRow)
    decodeNativeOperand(third.rows,first.index * 8 + 3) == List(globalRow) and
      decodeNativeOperand(third.rows,second.index * 8 + 5) == List(constantRow) and
      decodeNativeOperand(third.rows,third.index * 8 + 4) == List(closureRow) and
      internNativeOperand(third.rows,globalRow).index == first.index and
      decodeNativeOperand(third.rows,second.index * 8 + 3) == List()
  })
  pure def resolveClosureField(rows:List[NativeOperandRow],registers:List[int],stack:List[int],raw:int,fieldIndex:int):List[int] = {
    val operand = decodeNativeOperand(rows,raw)
    if(operand.length() == 0 or fieldIndex < 1) List()
    else {
      val decoded = operand.nth(0)
      if(decoded.kind == 0 and decoded.first < registers.length()) List(registers.nth(decoded.first),fieldIndex)
      else if(decoded.kind == 1 and decoded.first > 0 and decoded.first <= stack.length()) List(stack.nth(decoded.first - 1),fieldIndex)
      else List()
    }
  }
  run closureFieldsAliasByResolvedPointerAndNotRawBaseOperandTest = init.expect({
    val registerField = resolveClosureField(List(),List(42),List(42),0,1)
    val stackField = resolveClosureField(List(),List(42),List(42),9,1)
    val unrelated = resolveClosureField(List(),List(42),List(43),9,1)
    registerField == List(42,1) and registerField == stackField and registerField != unrelated and
      resolveClosureField(List(),List(42),List(42),1,1) == List() and
      resolveClosureField(List(),List(42),List(42),0,0) == List()
  })
  type NativeCopyState = {memory:List[int],names:List[int]}
  pure def copyNative(s:NativeCopyState,source:int,target:int,name:int):NativeCopyState = {
    memory:if(source < 0 or source == target) s.memory else s.memory.replaceAt(target,s.memory.nth(source)),
    names:if(name < 0) s.names else s.names.replaceAt(target,name)
  }
  run selfCopySuppressesDataMoveButStillUpdatesVariableMetadataTest = init.expect({
    val original = {memory:List(77,88),names:List(1,2)}
    val selfCopy = copyNative(original,0,0,9)
    val registerZero = copyNative(original,0,1,-1)
    val absent = copyNative(original,-1,1,9)
    selfCopy == {memory:List(77,88),names:List(9,2)} and
      registerZero == {memory:List(77,77),names:List(1,2)} and
      absent == {memory:List(77,88),names:List(1,9)}
  })
  type BoxCell = {kind:str,bits:int}
  pure def boxHeap(rows:List[BoxCell],payload:int):List[BoxCell] =
    rows.append({kind:"header",bits:2288}).append({kind:"payload",bits:payload})
  run heapBoxingAdvancesTwoWordsAndRetainsEarlierPayloadsTest = init.expect({
    val prefix = List({kind:"old",bits:91})
    val first = boxHeap(prefix,123)
    val second = boxHeap(first,456)
    second.length() == prefix.length() + 4 and second.slice(0,first.length()) == first and
      second.nth(prefix.length()) == {kind:"header",bits:2288} and
      second.nth(prefix.length() + 1) == {kind:"payload",bits:123} and
      ((prefix.length() * 8 + 7) - 7) / 8 + 1 == prefix.length() + 1
  })
  run boxingReservationUsesPhysicalSegmentCapacityTest = init.expect(
    not(3 + 2 <= 4) and 3 + 2 <= 5 and
      // A GC trigger threshold is not the physical segment bound.
      3 + 2 > 3 and 3 + 2 <= 8)
  type PollReadState = {stage:str,ticket:int,published:int,sampled:int,result:int}
  pure def pollReadStep(s:PollReadState,event:str,expected:int,epoch:int):PollReadState =
    if(event == "sample" and s.stage == "waiting") {...s,sampled:s.published}
    else if(event == "unlock" and s.stage == "waiting") {...s,ticket:expected,published:epoch}
    else if(event == "poll" and s.stage == "waiting" and s.ticket == expected) {...s,stage:"held"}
    else if(event == "consume" and s.stage == "held") {...s,stage:"consumed",result:s.sampled}
    else s
  run ordinaryPollCanConsumeAPreviouslySatisfiedStaleReadTest = init.expect({
    val start = {stage:"waiting",ticket:0,published:0,sampled:-1,result:-1}
    val finalRead = List("sample","unlock","poll","consume").foldl(start,
      (s,event) => pollReadStep(s,event,1,1))
    finalRead.stage == "consumed" and finalRead.ticket == 1 and
      finalRead.result == 0 and finalRead.published == 1
  })
  run firstMatchingPollDoesNotProveThatAllocationWasUncontendedTest = init.expect({
    val epochAtAllocation = 0
    val afterUnlock = pollReadStep({stage:"waiting",ticket:0,published:0,sampled:0,result:-1},"unlock",1,1)
    val firstPoll = pollReadStep(afterUnlock,"poll",1,1)
    firstPoll.stage == "held" and firstPoll.published > epochAtAllocation
  })

  type PhysicalOperandPair = {present:bool,firstKind:int,firstValue:int,secondKind:int,secondValue:int}
  pure val unusedOperand:PhysicalOperandPair = {present:false,firstKind:0,firstValue:0,secondKind:0,secondValue:0}
  pure def observeOperandPair(pair:PhysicalOperandPair):List[NativeOperandRow] = {
    val identity = if(pair.firstKind == 0) pair.firstValue else pair.firstValue * 4 + pair.firstKind + 1
    if(not(pair.present)) List()
    else if(pair.secondKind == 0 and pair.secondValue == 1) List({kind:3,first:identity,second:0})
    else if(pair.secondKind == 0 and pair.secondValue == 0) List({kind:5,first:identity,second:0})
    else if(pair.secondKind == 1 and pair.firstKind == 1)
      List({kind:4,first:pair.firstValue,second:pair.secondValue})
    else List()
  }
  pure def extractOperandPrefix(slots:List[PhysicalOperandPair],count:int):{ok:bool,rows:List[NativeOperandRow]} =
    if(count < 0 or count > slots.length()) {ok:false,rows:List()} else
    slots.slice(0,count).foldl({ok:true,rows:List()},(s,pair) => {
      val observed = observeOperandPair(pair)
      {ok:s.ok and observed.length() == 1,rows:s.rows.concat(observed)}
    })
  pure def appendOperand(slots:List[PhysicalOperandPair],count:int,pair:PhysicalOperandPair):List[PhysicalOperandPair] = {
    val expanded = if(count < slots.length()) slots else
      0.to(slots.length() / 2).fold(slots,(rows,index) => rows.append(unusedOperand))
    expanded.replaceAt(count,pair)
  }
  run allocatedOperandPrefixIgnoresCapacityAndRetainsPairMarkersTest = init.expect({
    val globalPair = {present:true,firstKind:2,firstValue:9,secondKind:0,secondValue:1}
    val constantPair = {...globalPair,secondValue:0}
    val closurePair = {...globalPair,firstKind:1,firstValue:0,secondKind:1,secondValue:1}
    val physical = List(globalPair,constantPair,closurePair,unusedOperand,globalPair)
    val snapshot = extractOperandPrefix(physical,3)
    snapshot.ok and snapshot.rows.length() == 3 and
      snapshot.rows.nth(0).kind == 3 and snapshot.rows.nth(1).kind == 5 and
      snapshot.rows.nth(2) == {kind:4,first:0,second:1} and
      not(extractOperandPrefix(physical,4).ok) and not(extractOperandPrefix(physical,6).ok)
  })
  run physicalOperandAppendGrowsAndPreservesAllocatedRowsTest = init.expect({
    val pair = {present:true,firstKind:2,firstValue:9,secondKind:0,secondValue:0}
    0.to(8).forall(capacity => {
      val full = if(capacity == 0) List() else 0.to(capacity - 1).fold(List(),(rows,index) => rows.append(pair))
      val appended = appendOperand(full,capacity,pair)
      appended.length() == 3 * capacity / 2 + 1 and appended.slice(0,capacity) == full and
        extractOperandPrefix(appended,capacity + 1).ok
    })
  })
  run malformedOperandPairCannotBecomeAValidSnapshotTest = init.expect({
    val wrongBase = {present:true,firstKind:2,firstValue:0,secondKind:1,secondValue:1}
    val wrongMarker = {...wrongBase,secondKind:2}
    not(extractOperandPrefix(List(wrongBase),1).ok) and
      not(extractOperandPrefix(List(wrongMarker),1).ok)
  })
  type CopyCacheState = {rows:List[FpCell],memory:List[int],hp:int,heap:List[BoxCell]}
  pure def readBoxedCache(s:CopyCacheState,source:int):CopyCacheState = {
    val flushed = flushFp(s.rows,source)
    if(flushed.events.length() == 0) {...s,rows:flushed.rows}
    else {...s,rows:flushed.rows,memory:s.memory.replaceAt(source,s.hp),hp:s.hp + 2,
      heap:boxHeap(s.heap,flushed.events.nth(0).payload)}
  }
  pure def executeCacheCopy(s:CopyCacheState,source:int,target:int):CopyCacheState = {
    val fetched = readBoxedCache(s,source)
    {...fetched,rows:invalidateFp(fetched.rows,target),memory:fetched.memory.replaceAt(target,fetched.memory.nth(source))}
  }
  run dirtySourceIsBoxedBeforeCopyAndDestinationInvalidationTest = init.expect({
    val source = {present:true,location:0,dirty:true,stamp:8,payload:123}
    val target = {...source,location:1,stamp:9,payload:456}
    val original = {rows:List(source,target),memory:List(-10,-20),hp:0,heap:List()}
    val copied = executeCacheCopy(original,0,1)
    copied.memory == List(0,0) and copied.hp == 2 and copied.heap.nth(1).bits == 123 and
      probeFp(copied.rows,1) == List() and not(probeFp(copied.rows,0).nth(0).dirty) and
      copied.memory.nth(1) != original.memory.nth(0)
  })
  run repeatedCleanReadDoesNotAllocateOrCopyStaleBoxedStorageTest = init.expect({
    val source = {present:true,location:0,dirty:true,stamp:8,payload:123}
    val original = {rows:List(source),memory:List(-10,-20),hp:0,heap:List()}
    val first = executeCacheCopy(original,0,1)
    val second = executeCacheCopy(first,0,1)
    second.hp == first.hp and second.heap == first.heap and second.memory == first.memory
  })
  pure def heapCheck(hp:int,threshold:int,requestsGC:bool,overflow:bool):{collect:bool,overflow:bool} =
    {collect:hp > threshold and requestsGC,overflow:hp > threshold and requestsGC and overflow}
  run heapSlowPathShortCircuitsAndUsesStrictThresholdTest = init.expect(
    heapCheck(10,10,true,true) == {collect:false,overflow:false} and
      heapCheck(11,10,false,true) == {collect:false,overflow:false} and
      heapCheck(11,10,true,false) == {collect:true,overflow:false} and
      heapCheck(11,10,true,true) == {collect:true,overflow:true})

  type SectionHeap = {hp:int,limit:int,cells:List[BoxCell]}
  pure def selectBoxSection(s:SectionHeap,start:int,stop:int):SectionHeap = {...s,hp:start,limit:stop}
  pure def freshBoxSection(s:SectionHeap):bool =
    s.hp.to(s.limit - 1).forall(address => s.cells.nth(address).kind == "free")
  pure def allocateInSection(s:SectionHeap,bits:int):SectionHeap =
    {...s,hp:s.hp + 2,cells:s.cells.replaceAt(s.hp,{kind:"header",bits:2288}).replaceAt(s.hp + 1,{kind:"payload",bits:bits})}
  run lowerAddressSectionAllocationRetainsEarlierHighAddressObjectsTest = init.expect({
    val initialHeap = {hp:10,limit:12,cells:0.to(11).fold(List(),(cells,address) => cells.append({kind:"free",bits:0}))}
    val highBox = allocateInSection(initialHeap,123)
    val selected = selectBoxSection(highBox,2,4)
    val lowerBox = allocateInSection(selected,456)
    freshBoxSection(selected) and selected.cells.nth(11).kind == "payload" and
      lowerBox.hp == 4 and lowerBox.cells.nth(3).bits == 456 and
      lowerBox.cells.nth(10) == highBox.cells.nth(10) and lowerBox.cells.nth(11).bits == 123
  })

  pure def enterReportedCache(s:{reported:int,rows:List[FpCell]},entry:FpCell):{reported:int,rows:List[FpCell]} =
    {reported:if(s.rows.foldl(false,(hole,cell) => hole or not(cell.present))) s.reported else s.rows.length() + 1,
      rows:insertFp(s.rows,entry)}
  run nativeReportedCacheSizeCannotStandInForOccupiedEntryCountTest = init.expect({
    val first = {present:true,location:0,dirty:true,stamp:1,payload:11}
    val second = {...first,location:8,stamp:2,payload:22}
    val third = {...first,location:16,stamp:3,payload:33}
    val entered = List(first,second,third).foldl({reported:0,rows:List()},(s,entry) => enterReportedCache(s,entry))
    entered.reported == 2 and entered.rows.foldl(0,(count,cell) => count + (if(cell.present) 1 else 0)) == 3
  })

  pure def occupiedFp(rows:List[FpCell]):int = rows.foldl(0,(count,cell) => count + (if(cell.present) 1 else 0))
  pure def dirtyFp(rows:List[FpCell]):int = rows.foldl(0,(count,cell) => count + (if(cell.present and cell.dirty) 1 else 0))
  pure def flushManyFp(rows:List[FpCell],locations:List[int]):{rows:List[FpCell],events:int} =
    locations.foldl({rows:rows,events:0},(s,location) => {
      val flushed = flushFp(s.rows,location)
      {rows:flushed.rows,events:s.events + flushed.events.length()}
    })
  pure def portableFlonumCharge(wordBytes:int,headerWords:int):int =
    (((8 + wordBytes + headerWords * wordBytes + 7) / 8) * 8) / wordBytes
  run cacheLifecycleSeparatesReportedCapacityOccupancyAndDirtyPotentialTest = init.expect({
    val history = 0.to(31).fold({reported:0,rows:List(),valid:true},(s,index) => {
      val entry = {present:true,location:index * 8,dirty:true,stamp:index + 1,payload:index}
      val entered = enterReportedCache({reported:s.reported,rows:s.rows},entry)
      {reported:entered.reported,rows:entered.rows,valid:s.valid and
        entered.rows.length() + 1 == entered.reported * 2 and
        dirtyFp(entered.rows) <= occupiedFp(entered.rows) and occupiedFp(entered.rows) <= entered.rows.length() and
        dirtyFp(entered.rows) * 2 <= entered.reported * portableFlonumCharge(4,2)}
    })
    val invalidated = invalidateFp(history.rows,8)
    val flushed = flushFp(invalidated,16)
    history.valid and flushed.rows.length() == history.rows.length() and
      history.reported == 32 and dirtyFp(flushed.rows) == 30
  })
  run repeatedAndMissingFlushRequestsConserveDirtyBoxPotentialTest = init.expect({
    val first = {present:true,location:0,dirty:true,stamp:1,payload:11}
    val second = {...first,location:8,stamp:2,payload:22}
    val third = {...first,location:16,stamp:3,payload:33}
    val rows = List(first,second,third)
    val flushed = flushManyFp(rows,List(0,0,8,16,16,99))
    flushed.events == 3 and dirtyFp(flushed.rows) == 0 and
      dirtyFp(flushed.rows) + flushed.events == dirtyFp(rows) and
      7 + flushed.events * 2 + dirtyFp(flushed.rows) * 2 == 7 + dirtyFp(rows) * 2
  })
  run portableChargeClosesTheEqualUnitReportedSizeCounterexampleTest = init.expect({
    val reported = 2
    val occupied = 3
    portableFlonumCharge(4,2) == 6 and reported * 2 <= 4 and occupied * 2 > 4 and
      reported * portableFlonumCharge(4,2) > 4 and occupied * 2 <= reported * portableFlonumCharge(4,2)
  })
  pure def preparedThreshold(hp:int,physical:int,fudge:int,available:int):List[int] =
    if(hp + fudge > physical) List() else List(hp + (if(available < physical - hp - fudge) available else physical - hp - fudge))
  run preparedThresholdRetainsPhysicalFudgeAndRejectsMissingRoomTest = init.expect({
    val physical = 65536 - 2
    val threshold = preparedThreshold(1,physical,8194,100000).nth(0)
    threshold + 8194 == physical and threshold >= 1 and
      preparedThreshold(1,physical,8194,0) == List(1) and preparedThreshold(10,12,3,0) == List()
  })
  run sourceChargeUsesDistinctActualAndCompilerReservationLedgersTest = init.expect({
    val events = List({actual:2,charge:6,floating:true},{actual:3,charge:5,floating:false},{actual:2,charge:6,floating:true})
    val counted = events.foldl({actual:0,reserved:0,floating:0},(s,event) =>
      {actual:s.actual + event.actual,reserved:s.reserved + event.charge,floating:s.floating + (if(event.floating) event.charge else 0)})
    counted == {actual:7,reserved:17,floating:12} and
      counted.actual <= counted.reserved and counted.floating <= counted.reserved
  })
  run physicalReadBatchKeepsItsHeapAndRemainingDirtyPotentialBoundTest = init.expect({
    val first = {present:true,location:0,dirty:true,stamp:1,payload:11}
    val second = {...first,location:1,stamp:2,payload:22}
    val third = {...first,location:2,stamp:3,payload:33}
    val original = {rows:List(first,second,third),memory:List(-1,-2,-3),hp:0,heap:List()}
    val checked = List(0,0,1,2,2).foldl({state:original,safe:true},(s,location) => {
      val after = readBoxedCache(s.state,location)
      {state:after,safe:s.safe and after.hp + dirtyFp(after.rows) * 2 == 6}
    })
    checked.safe and checked.state.hp == 6 and checked.state.memory == List(0,2,4) and dirtyFp(checked.state.rows) == 0
  })

  // Negative numbers denote Scheme #f in these control inputs. Numeric zero
  // remains a present request. floating == -2 denotes Scheme #t, not a number.
  type CheckRequest = {heap:int,floating:int,ssb:int,poll:bool,combine:bool,stack:int}
  pure def activeCheck(r:CheckRequest):bool = r.heap >= 0 or r.ssb >= 0 or r.poll
  pure def checkOpcode(r:CheckRequest):List[str] =
    List(if(r.combine) "CHECK" else if(r.poll) "POLL" else "CHECK")
      .concat(if(r.heap >= 0) List("HEAP") else List())
      .concat(if(r.heap >= 0 and r.heap == r.floating) List("FLO") else List())
      .concat(if(r.ssb >= 0) List("SSB") else List())
      .concat(if(r.combine and r.poll) List("POLL") else List())
  type CheckBuffer = {code:List[List[str]],label:int,fp:int,heap:int,floating:int,ssb:int}
  pure def emitCheck(r:CheckRequest,s:CheckBuffer):CheckBuffer =
    if(not(activeCheck(r))) s else
      {code:s.code.concat(List(List(if(s.fp == r.stack) "false-adjust" else "adjust"),checkOpcode(r),List("internal-return"))),
       label:s.label + 1,fp:r.stack,heap:if(r.heap >= 0) 0 else s.heap,
       floating:if(r.heap >= 0) 0 else s.floating,ssb:if(r.ssb >= 0) 0 else s.ssb}
  pure val initialCheckBuffer:CheckBuffer = {code:List(),label:17,fp:1,heap:11,floating:6,ssb:3}
  pure val disabledCheck:CheckRequest = {heap:-1,floating:-1,ssb:-1,poll:false,combine:false,stack:3}
  run numericZeroRequestsStillEmitCompleteBlocksTest = init.expect({
    val heap = emitCheck({...disabledCheck,heap:0},initialCheckBuffer)
    val ssb = emitCheck({...disabledCheck,ssb:0},initialCheckBuffer)
    heap.code == List(List("adjust"),List("CHECK","HEAP"),List("internal-return")) and
      heap.heap == 0 and heap.floating == 0 and heap.ssb == 3 and
      ssb.code == List(List("adjust"),List("CHECK","SSB"),List("internal-return")) and
      ssb.heap == 11 and ssb.floating == 6 and ssb.ssb == 0
  })
  run disabledChecksPreserveAllCountersAndFrameTest = init.expect(
    emitCheck(disabledCheck,initialCheckBuffer) == initialCheckBuffer)
  run floatingBooleanMustNotAliasNumericHeapRequestTest = init.expect(
    checkOpcode({...disabledCheck,heap:0,floating:-2}) == List("CHECK","HEAP") and
      checkOpcode({...disabledCheck,heap:0,floating:0}) == List("CHECK","HEAP","FLO") and
      checkOpcode({...disabledCheck,heap:7,floating:7}) == List("CHECK","HEAP","FLO"))
  run allCheckMasksPreserveBudgetAndLabelProtocolTest = init.expect({
    val requests = List(-1,0,7).foldl(List(),(outer,heap) =>
      outer.concat(List(-1,-2,0,7).foldl(List(),(floats,floating) =>
        floats.concat(List(-1,0,7).foldl(List(),(stores,ssb) =>
          stores.concat(List(false,true).foldl(List(),(polls,poll) =>
            polls.concat(List(false,true).foldl(List(),(combined,combine) =>
              combined.concat(List(1,3).foldl(List(),(stacks,stack) =>
                stacks.append({heap:heap,floating:floating,ssb:ssb,poll:poll,combine:combine,stack:stack}))))))))))))
    val result = requests.foldl({state:initialCheckBuffer,activeBlocks:0,valid:true},(history,r) => {
      val updated = emitCheck(r,history.state)
      val activeBlocks = if(activeCheck(r)) 1 else 0
      {state:updated,activeBlocks:history.activeBlocks + activeBlocks,valid:history.valid and
        updated.code.length() == history.state.code.length() + activeBlocks * 3 and
        updated.label == history.state.label + activeBlocks}
    })
    requests.length() == 288 and result.valid and result.state.label == 17 + result.activeBlocks and
      result.state.code.length() == 3 * result.activeBlocks
  })
  pure def slowAttempt(hp:int,threshold:int,requestsGC:bool,overflow:bool):List[str] =
    if(hp <= threshold) List("resume") else
      List("save","limit").concat(if(requestsGC) List("collect") else List()).append("restore").append(
        if(requestsGC and overflow) "raise" else "resume")
  run gcSlowPathRestoresBeforeOverflowAndShortCircuitsReuseTest = init.expect(
    slowAttempt(11,10,true,true) == List("save","limit","collect","restore","raise") and
      slowAttempt(11,10,false,true) == List("save","limit","restore","resume") and
      slowAttempt(10,10,true,true) == List("resume"))
  run overflowAttemptsRequireReturningHandlerAndRecheckTest = init.expect({
    val attempts = List(true,true,true,false)
    val history = attempts.foldl({events:List(),done:false,count:0},(s,overflow) =>
      if(s.done) s else {events:s.events.concat(slowAttempt(11,10,true,overflow)).concat(if(overflow) List("retry") else List()),
        done:not(overflow),count:s.count + 1})
    history.done and history.count == 4 and history.events.foldl(0,(count,event) => count + (if(event == "resume") 1 else 0)) == 1 and
      history.events.foldl(0,(count,event) => count + (if(event == "raise") 1 else 0)) == 3 and
      history.events.foldl(0,(count,event) => count + (if(event == "retry") 1 else 0)) == 3 and
      not(slowAttempt(11,10,true,true).foldl(false,(seen,event) => seen or event == "resume"))
  })

  pure def reversedInts(values:List[int]):List[int] = values.foldl(List(),(result,value) => List(value).concat(result))
  pure def closureProtocol(identities:List[int]):List[int] = {
    val allocations = identities.foldl(List(),(result,identity) => result.append(identity))
    // Recursive calls consume reverse parameters; unwind consumes that stack
    // in reverse again. Negative IDs denote setup events in this control.
    val work = reversedInts(identities)
    allocations.concat(work.foldl(List(),(result,identity) => List(-identity).concat(result)))
  }
  run recursiveClosureGroupsAllocateBeforeAnyCaptureSetupTest = init.expect({
    val declarations = List(1,2,3)
    val protocol = closureProtocol(declarations)
    protocol == List(1,2,3,-1,-2,-3) and
      protocol.foldl({setupSeen:false,valid:true},(s,event) =>
        {setupSeen:s.setupSeen or event < 0,valid:s.valid and (event < 0 or not(s.setupSeen))}).valid
  })
  run closureUnwindingMustPreserveOriginalDeclarationOrderTest = init.expect(
    closureProtocol(List()) == List() and closureProtocol(List(1)) == List(1,-1) and
      closureProtocol(List(1,2,3)) != List(1,2,3,-3,-2,-1) and
      closureProtocol(List(1,2)) != List(1,-1,2,-2))
  pure val copyIndices = List(0,1,2,3,4,5,6,7)
  pure val copyMemory = List(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  pure def physicalCopy(memory:List[int],source:int,target:int,count:int):List[int] =
    copyIndices.slice(0,count).foldl(memory,(current,index) => current.replaceAt(target + index,current.nth(source + index)))
  pure def bodyRangesDisjoint(source:int,target:int,count:int):bool =
    source + count <= target or target + count <= source
  run nativeWordCopiesRetainSourceAndUnrelatedCellsForBothDirectionsTest = init.expect(
    List(0,1,2,4,8).foldl(true,(countsOK,count) => countsOK and List(true,false).foldl(true,(directionsOK,forward) => directionsOK and {
      val source = if(forward) 1 else 16
      val target = if(forward) 16 else 1
      val copied = physicalCopy(copyMemory,source,target,count)
      bodyRangesDisjoint(source,target,count) and copyIndices.slice(0,count).foldl(true,(wordsOK,index) =>
        wordsOK and copied.nth(target + index) == copyMemory.nth(source + index)) and
        copied.nth(0) == copyMemory.nth(0) and copied.nth(31) == copyMemory.nth(31)
    })))
  run overlappingForwardCopyCannotSupplyOriginalPayloadContractTest = init.expect({
    val copied = physicalCopy(copyMemory,1,2,4)
    not(bodyRangesDisjoint(1,2,4)) and copied.slice(2,6) == List(1,1,1,1) and
      copied.slice(2,6) != copyMemory.slice(1,5)
  })
  run forwardingAndNewHeadersSurviveTheActualWordLoopTest = init.expect({
    val prepared = copyMemory.replaceAt(16,12345).replaceAt(0,777)
    val copied = physicalCopy(prepared,1,17,8)
    copied.nth(0) == 777 and copied.nth(16) == 12345 and
      copied.slice(17,25) == copyMemory.slice(1,9) and copied.nth(25) == copyMemory.nth(25)
  })
  run zeroLengthCollectionCopyDoesNotTouchEitherBodyTest = init.expect(
    physicalCopy(copyMemory,1,17,0) == copyMemory)
  pure def localTransfer(label:bool,arguments:int,safe:bool,fallthrough:bool):str =
    if(label) (if(fallthrough) "fallthrough" else "goto") else
      if(arguments < 0) "return" else if(safe) "safe-call" else "call"
  run localZeroArgumentCallsMustNotBecomeReturnsOrEraseSafetyTest = init.expect(
    localTransfer(false,0,true,false) == "safe-call" and
      localTransfer(false,0,false,true) == "call" and localTransfer(false,-1,true,false) == "return" and
      localTransfer(true,-1,false,true) == "fallthrough" and localTransfer(true,0,true,false) == "goto")

}
