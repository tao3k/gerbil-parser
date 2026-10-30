// Finite independent set interpretation; unbounded masks/graphs are Lean obligations.
module LookaheadExecution {
  type Graph = {closure:List[List[int]],shift:List[int],first:List[Set[int]],nullable:Set[int]}
  type Store = {known:List[Set[int]],pending:List[Set[int]],queue:List[int],active:Set[int],visits:List[int]}
  var graph:Graph
  var state:Store
  pure def replaceRow(values:List[Set[int]],index:int,value:Set[int]):List[Set[int]] =
    values.foldl({index:0,out:List()},(out,old)=>
      {index:out.index+1,out:out.out.append(if(out.index==index) value else old)}).out
  pure def enqueue(s:Store,node:int,evidence:Set[int]):Store = {
    val novel=evidence.exclude(s.known.nth(node))
    if(novel.size()==0) s else {...s,known:replaceRow(s.known,node,s.known.nth(node).union(novel)),
      pending:replaceRow(s.pending,node,s.pending.nth(node).union(novel)),
      queue:if(s.pending.nth(node).size()==0) s.queue.append(node) else s.queue}
  }
  pure def initial:Store = enqueue({known:List(Set(),Set(),Set()),pending:List(Set(),Set(),Set()),
    queue:List(),active:Set(),visits:List()},0,Set(0))
  pure val self:Graph = {closure:List(List(0),List(),List()),shift:List(-1,-1,-1),first:List(Set(1),Set(),Set()),nullable:Set(0)}
  pure val fifo:Graph = {closure:List(List(2,1),List(),List(1)),shift:List(-1,-1,-1),first:List(Set(1),Set(),Set(2)),nullable:Set()}
  pure val coalesced:Graph = {closure:List(List(1),List(),List()),shift:List(1,-1,-1),first:List(Set(1),Set(),Set()),nullable:Set(0)}
  pure val disconnected:Graph = {closure:List(List(),List(2),List()),shift:List(-1,-1,-1),first:List(Set(),Set(2),Set()),nullable:Set()}
  pure val zero:Graph = {closure:List(List(1),List(),List()),shift:List(-1,-1,-1),first:List(Set(),Set(),Set()),nullable:Set()}
  action setup(g:Graph):bool = all {graph'=g,state'=initial}
  action init = {nondet g=Set(self,fifo,coalesced,disconnected,zero).oneOf(); setup(g)}
  pure def advance(g:Graph,s:Store):Store = if(s.queue.length()==0) s else {
    val node=s.queue.nth(0)
    val delta=s.pending.nth(node)
    val evidence=if(not(s.active.contains(node))) {
      if(g.nullable.contains(node)) g.first.nth(node).union(delta) else g.first.nth(node)
    } else if(g.nullable.contains(node)) delta else Set()
    val cleared={...s,queue:s.queue.slice(1,s.queue.length()),pending:replaceRow(s.pending,node,Set()),
      active:if(g.closure.nth(node).length()==0) s.active else s.active.union(Set(node)),visits:s.visits.append(node)}
    val closure=g.closure.nth(node).foldl(cleared,(out,target)=>enqueue(out,target,evidence))
    val target=g.shift.nth(node)
    if(target<0) closure else enqueue(closure,target,delta)
  }
  action step = all {graph'=graph,state'=advance(graph,state)}
  // Saturation scans every rule each round, independently of FIFO/delta/activation storage.
  pure def round(g:Graph,known:List[Set[int]]):List[Set[int]] = known.foldl({node:0,known:known},(out,source)=>{
    val evidence=if(source.size()==0) Set() else {
      if(g.nullable.contains(out.node)) g.first.nth(out.node).union(source) else g.first.nth(out.node)}
    val closure=g.closure.nth(out.node).foldl(out.known,(rows,target)=>replaceRow(rows,target,rows.nth(target).union(evidence)))
    val target=g.shift.nth(out.node)
    {node:out.node+1,known:if(target<0) closure else replaceRow(closure,target,closure.nth(target).union(source))}
  }).known
  pure def expected(g:Graph):List[Set[int]] = List(0,1,2,3,4,5,6,7,8).foldl(initial.known,(known,_)=>round(g,known))
  val Unique = state.queue.indices().forall(i=>state.queue.indices().forall(j=>i==j or state.queue.nth(i)!=state.queue.nth(j)))
  val Pending = state.pending.indices().forall(i=>
    state.pending.nth(i).subseteq(state.known.nth(i)) and
    (state.pending.nth(i).size()>0 iff state.queue.indices().exists(j=>state.queue.nth(j)==i)))
  val Complete = state.queue.length()!=0 or state.known==expected(graph)
  val PropagationSafety = Unique and Pending and Complete
  run fifoNativeIDsTest = setup(fifo).then(step).expect(state.queue==List(2,1) and PropagationSafety)
  run selfRequeueTest = setup(self).then(step).expect(state.queue==List(0) and state.pending.nth(0)==Set(1))
    .then(step).expect(PropagationSafety and state.visits==List(0,0) and state.known.nth(0)==Set(0,1))
  run coalescedEvidenceTest = setup(coalesced).then(step).expect(state.queue==List(1) and state.pending.nth(1)==Set(0,1))
    .then(step).expect(PropagationSafety)
  run disconnectedFirstTest = setup(disconnected).then(step).expect(PropagationSafety and state.known.nth(2)==Set())
  run zeroFirstActivationTest = setup(zero).then(step).expect(PropagationSafety and state.active==Set(0) and state.queue==List())
  run sourceScopedEmptyCacheTest = init.expect({val cache={present:true,state:0,targets:List()};
    cache.present and cache.targets==List() and cache.state!=1})
  run zeroNodeLookupTest = init.expect(0>=0 and not(-1>=0))
}
