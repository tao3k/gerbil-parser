// Finite protocol exploration; arbitrary catalogues and lists are Lean proofs.
module PredictionConstruction {
  type Queue = {known:List[Set[int]],pending:List[Set[int]],work:List[int]}
  var queue:Queue
  var parents:List[List[int]]
  var own:List[Set[int]]
  pure def publish(q:Queue,target:int,delta:Set[int]):Queue = {
    val novel=delta.exclude(q.known.nth(target))
    if(novel.size()==0) q else {
      known:q.known.replaceAt(target,q.known.nth(target).union(novel)),
      pending:q.pending.replaceAt(target,q.pending.nth(target).union(novel)),
      work:if(q.work.indices().exists(i=>q.work.nth(i)==target)) q.work else q.work.append(target)}}
  pure def expand(known:List[Set[int]],edges:List[List[int]]):List[Set[int]] =
    known.indices().fold(known,(out,child)=>edges.nth(child).foldl(out,(updated,owner)=>
      updated.replaceAt(owner,updated.nth(owner).union(known.nth(child)))))
  pure def closure(seeds:List[Set[int]],edges:List[List[int]]):List[Set[int]] =
    expand(expand(expand(seeds,edges),edges),edges)
  action setup(seeds:List[Set[int]],edges:List[List[int]]):bool = all {
    own'=seeds,parents'=edges,queue'={known:seeds,pending:seeds,
      work:List(2,1,0).foldl(List(),(out,node)=>if(seeds.nth(node).size()>0) out.append(node) else out)}}
  action init = {
    nondet edges=Set(List(List(1),List(0,2),List(1)),List(List(0),List(0),List(1)),
      List(List(),List(),List()),List(List(0,1,2),List(0,1,2),List(0,1,2))).oneOf()
    nondet seeds=Set(List(Set(0),Set(1),Set(2,3)),List(Set(),Set(),Set()),
      List(Set(0),Set(),Set(3))).oneOf()
    setup(seeds,edges)
  }
  action step = if(queue.work.length()==0) all {queue'=queue,parents'=parents,own'=own}
    else {val node=queue.work.nth(0); val delta=queue.pending.nth(node); all {
      queue'=parents.nth(node).foldl({...queue,pending:queue.pending.replaceAt(node,Set()),
        work:queue.work.slice(1,queue.work.length())},(out,target)=>publish(out,target,delta)),
      parents'=parents,own'=own}}
  val ConstructionSafety = queue.known.indices().forall(node=>
    queue.pending.nth(node).subseteq(queue.known.nth(node)) and
    own.nth(node).subseteq(queue.known.nth(node)) and
    queue.known.nth(node).subseteq(closure(own,parents).nth(node)) and
    (queue.pending.nth(node).size()>0 iff queue.work.indices().exists(i=>queue.work.nth(i)==node))) and
    queue.work.indices().forall(i=>queue.work.indices().forall(j=>i==j or queue.work.nth(i)!=queue.work.nth(j))) and
    (queue.work.length()>0 or queue.known==closure(own,parents))
  run reverseCycleTest = setup(List(Set(0),Set(1),Set(2,3)),List(List(1),List(0,2),List(1)))
    .then(step).then(step).then(step).then(step).then(step).then(step)
    .expect(queue.work==List() and queue.known==List(Set(0,1,2,3),Set(0,1,2,3),Set(0,1,2,3)) and ConstructionSafety)
  run emptyOwnTest = setup(List(Set(),Set(),Set()),List(List(0),List(0),List(1)))
    .expect(queue.work==List() and ConstructionSafety)
  // Owner 1 reaches owner 0 through owner 2 only after owner 0 drains.
  run zeroIDReentryTest = setup(List(Set(0),Set(1),Set()),List(List(0,1),List(2),List(0)))
    .then(step).then(step).then(step)
    .expect(ConstructionSafety and queue.work==List(1,0))

  type Registry = {keys:List[int],work:List[int],processed:int}
  pure def intern(r:Registry,key:int):Registry =
    if(r.keys.indices().exists(i=>r.keys.nth(i)==key)) r else
      {keys:r.keys.append(key),work:r.work.append(r.keys.length()),processed:r.processed}
  run repeatedKernelTest = init.expect({
    val initial={keys:List(7),work:List(0),processed:0}
    val final=intern(intern(intern(initial,7),65),7)
    final.keys==List(7,65) and final.work==List(0,1) and final.processed==0})
  pure def starts(rows:List[List[int]]):List[int] =
    rows.foldl(List(0),(out,row)=>out.append(out.nth(out.length()-1)+row.length()))
  run emptyCoordinateRowsTest = init.expect(starts(List(List(),List(0,65,2048),List(),List(0,130)))==List(0,0,3,3,5))
}
