// Bounded protocol controls; arbitrary finite graphs, stores and lists are Lean obligations.
module CompilerExecution {
  type Queue = {pending:List[int],seen:Set[int]}
  var queue:Queue
  var graph:List[List[int]]
  var accepting:Set[int]
  var done:bool
  var accepted:bool
  pure def enqueue(q:Queue,target:int):Queue = if(q.seen.contains(target)) q else
    {pending:q.pending.append(target),seen:q.seen.union(Set(target))}
  pure def expand(q:Queue,targets:List[int]):Queue = targets.foldl(q,(out,target)=>enqueue(out,target))
  action setup(g:List[List[int]],a:Set[int]):bool = all {
    queue' = {pending:List(0),seen:Set(0)},graph'=g,accepting'=a,done'=false,accepted'=false}
  action init = {
    nondet g=Set(List(List(2,2,1),List(0),List(1)),List(List(1),List(0),List()),
      List(List(),List(),List()),List(List(0,1,2),List(0,1,2),List(0,1,2))).oneOf()
    nondet a=Set(Set(),Set(0),Set(1),Set(2),Set(0,1),Set(1,2)).oneOf()
    setup(g,a)
  }
  action advance = if(done) all {queue'=queue,graph'=graph,accepting'=accepting,done'=done,accepted'=accepted}
    else if(queue.pending.length() == 0) all {
      queue'=queue,graph'=graph,accepting'=accepting,done'=true,accepted'=false}
    else {val owner=queue.pending.nth(0); all {
      queue'=expand({pending:queue.pending.slice(1,queue.pending.length()),seen:queue.seen},graph.nth(owner)),
      graph'=graph,accepting'=accepting,done'=accepting.contains(owner),accepted'=accepting.contains(owner)}}
  action step = advance
  val QueueSafety = queue.pending.indices().forall(i=>queue.seen.contains(queue.pending.nth(i))) and
    queue.pending.indices().forall(i=>queue.pending.indices().forall(j=>i == j or queue.pending.nth(i) != queue.pending.nth(j))) and
    queue.seen.subseteq(Set(0,1,2)) and queue.pending.length() <= queue.seen.size()
  run physicalStateIDTest = setup(List(List(2,2,1),List(0),List(1)),Set(1)).then(advance).expect(queue.pending == List(2,1) and QueueSafety)
  run acceptingExpansionTest = setup(List(List(2,1),List(),List()),Set(0)).then(advance)
    .expect(accepted and done and queue.pending == List(2,1))
  run cycleExhaustionTest = setup(List(List(1),List(0),List()),Set(2))
    .then(advance).then(advance).then(advance).expect(done and not(accepted) and queue.pending == List())
  run reachableAcceptTest = setup(List(List(2,2,1),List(0),List(1)),Set(1)).then(advance).then(advance).then(advance).expect(accepted and QueueSafety)
  type Replay = {slot:int,raw:List[int],touches:List[int],failed:bool}
  pure def install(s:Replay,key:int,incoming:int):Replay = if(s.failed) s else {
    val raw=if(incoming == 3) List(incoming).concat(s.raw) else s.raw
    if(s.slot >= 0 and incoming == 3) {slot:s.slot,raw:raw,touches:s.touches,failed:true}
    else {slot:incoming,raw:raw,touches:if(s.slot < 0) s.touches.append(key) else s.touches,failed:false}}
  run failedReplayTest = init.expect({
    val before={slot:2,raw:List(),touches:List(7),failed:false}
    val after=install(install(before,7,3),7,4)
    after.slot == 2 and after.raw == List(3) and after.touches == List(7) and after.failed})
  run firstTouchTest = init.expect({
    val before={slot:-1,raw:List(),touches:List(),failed:false}
    val after=install(install(before,7,2),7,4)
    after.slot == 4 and after.touches == List(7) and not(after.failed)})
  pure def maskNode(base:int,cores:List[int],core:int):int =
    cores.foldl({node:base,found:-1},(out,coreID)=>
      {node:out.node+1,found:if(coreID==core) out.node else out.found}).found
  run ordinalMaskTest = init.expect(maskNode(5,List(0,3,8),8)==7 and maskNode(5,List(0,3,8),8)!=13)
}
