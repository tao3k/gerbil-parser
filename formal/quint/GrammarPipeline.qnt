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

}
