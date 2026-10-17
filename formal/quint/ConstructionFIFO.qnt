// Source initialization, zero-valued existing IDs, reversed rows and deferred
// enqueue. Lean supplies arbitrary-grammar and termination proofs.
module ConstructionFIFO {
  type Edge={symbol:int,target:int}
  type State={phase:str,count:int,processed:int,keys:List[int],states:List[int],plans:List[int],
    work:List[int],owner:int,cursor:int,target:int,fresh:bool,acc:List[Edge],published:List[List[Edge]]}
  var q:State
  pure def lookup(values:List[int],key:int):int=values.indices().fold(-1,(id,node)=>if(values.nth(node)==key) node else id)
  pure def reverseRows(rows:List[Edge]):List[Edge]=rows.foldl(List(),(out,row)=>List(row).concat(out))
  pure def inputs(key:int):List[int]=if(key==0) List(0,2,2,1) else if(key==2) List(1,0) else List()
  pure def edges(values:List[int],key:int,used:int):List[Edge]=List(0,1,2,3).foldl(List(),(out,i)=>
    if(i<used) out.append({symbol:i,target:lookup(values,inputs(key).nth(i))}) else out)
  action init=q'={phase:"init-state",count:1,processed:0,keys:List(),states:List(),plans:List(),
    work:List(),owner:-1,cursor:0,target:-1,fresh:false,acc:List(),published:List()}
  action initialState=all {q.phase=="init-state",q'={...q,states:List(0),phase:"init-plan"}}
  action initialPlan=all {q.phase=="init-plan",q'={...q,plans:List(0),phase:"init-register"}}
  action initialRegister=all {q.phase=="init-register",q'={...q,keys:List(0),phase:"init-seed"}}
  action seed=all {q.phase=="init-seed",q'={...q,work:List(0),phase:"ready"}}
  action dequeue=all {q.phase=="ready",q.work.length()>0,
    q'={...q,owner:q.work.nth(0),work:q.work.slice(1,q.work.length()),cursor:0,acc:List(),phase:"process"}}
  action process=all {q.phase=="process",q'={...q,processed:q.processed+1,phase:"choose"}}
  action choose=all {q.phase=="choose",q.cursor<inputs(q.keys.nth(q.owner)).length(),
    val known=lookup(q.keys,inputs(q.keys.nth(q.owner)).nth(q.cursor))
    q'={...q,target:if(known>=0) known else q.count,fresh:known<0,phase:if(known>=0) "row" else "append"}}
  action appendTarget=all {q.phase=="append",
    val key=inputs(q.keys.nth(q.owner)).nth(q.cursor)
    q'={...q,count:q.count+1,keys:q.keys.append(key),states:q.states.append(key),plans:q.plans.append(key),phase:"row"}}
  action row=all {q.phase=="row",q'={...q,acc:List({symbol:q.cursor,target:q.target}).concat(q.acc),phase:"enqueue"}}
  action enqueue=all {q.phase=="enqueue",
    q'={...q,work:if(q.fresh) q.work.append(q.target) else q.work,cursor:q.cursor+1,phase:"choose"}}
  action publish=all {q.phase=="choose",q.cursor==inputs(q.keys.nth(q.owner)).length(),
    q'={...q,published:q.published.append(reverseRows(q.acc)),phase:"ready"}}
  action step=any {initialState,initialPlan,initialRegister,seed,dequeue,process,choose,appendTarget,row,enqueue,publish,q'=q}
  val SourceSafety=all {
    Set("init-state","init-plan","init-register","init-seed","ready","process","choose","append","row","enqueue").contains(q.phase),
    q.count>=1,q.processed>=0,q.processed<=q.count,
    if(Set("init-state","init-plan","init-register","init-seed").contains(q.phase)) all {
      q.count==1,q.processed==0,q.work==List(),q.published==List(),
      q.states==(if(q.phase=="init-state") List() else List(0)),
      q.plans==(if(q.phase=="init-state" or q.phase=="init-plan") List() else List(0)),
      q.keys==(if(q.phase=="init-seed") List(0) else List())}
    else all {
      q.states==q.keys,q.plans==q.keys,q.keys.length()==q.count,
      q.keys.indices().forall(i=>q.keys.indices().forall(j=>i==j or q.keys.nth(i)!=q.keys.nth(j))),
      val gap=if((q.phase=="row" or q.phase=="enqueue") and q.fresh) 1 else 0
      q.work==List(0,1,2,3).foldl(List(),(out,id)=>
        if(id>=q.processed+(if(q.phase=="process") 1 else 0) and id<q.count-gap) out.append(id) else out),
      q.published.length()==q.processed-(if(q.phase=="ready" or q.phase=="process") 0 else 1),
      q.published.indices().forall(node=>q.published.nth(node)==edges(q.keys,q.keys.nth(node),inputs(q.keys.nth(node)).length())),
      if(q.phase!="ready") all {
        q.owner==q.processed-(if(q.phase=="process") 0 else 1),q.cursor>=0,q.cursor<=inputs(q.keys.nth(q.owner)).length(),
        val used=q.cursor+(if(q.phase=="enqueue") 1 else 0)
        reverseRows(q.acc)==edges(q.keys,q.keys.nth(q.owner),used),
        (q.phase=="row" or q.phase=="enqueue") implies all {
          q.target==lookup(q.keys,inputs(q.keys.nth(q.owner)).nth(q.cursor)),
          q.fresh implies q.target==q.count-1}}
      else true}}
  run initializationTest=init.then(initialState).then(initialPlan).then(initialRegister).then(seed)
    .expect(q.count==1 and q.work==List(0) and SourceSafety)
  run existingZeroTest=init.then(initialState).then(initialPlan).then(initialRegister).then(seed)
    .then(dequeue).then(process).then(choose).expect(q.target==0 and not(q.fresh) and SourceSafety)
    .then(row).then(enqueue).expect(q.work==List() and q.count==1 and SourceSafety)
  run deferredEnqueueTest=init.then(initialState).then(initialPlan).then(initialRegister).then(seed)
    .then(dequeue).then(process).then(choose).then(row).then(enqueue).then(choose).then(appendTarget)
    .expect(q.count==2 and q.work==List() and SourceSafety)
    .then(row).then(enqueue).expect(q.work==List(1) and SourceSafety)
  run wrongInitialCounterRejectedTest=init.then(q'={...q,count:0}).expect(not(SourceSafety))
  run missingSeedRejectedTest=init.then(initialState).then(initialPlan).then(initialRegister)
    .then(q'={...q,phase:"ready"}).expect(not(SourceSafety))
  run duplicateExistingEnqueueRejectedTest=init.then(initialState).then(initialPlan).then(initialRegister).then(seed)
    .then(dequeue).then(process).then(choose).then(row).then(enqueue)
    .then(q'={...q,work:List(0)}).expect(not(SourceSafety))
  run missingFreshEnqueueRejectedTest=init.then(initialState).then(initialPlan).then(initialRegister).then(seed)
    .then(dequeue).then(process).then(choose).then(row).then(enqueue).then(choose).then(appendTarget).then(row)
    .then(q'={...q,cursor:q.cursor+1,phase:"choose"}).expect(not(SourceSafety))
  run completedRowTest=init.then(initialState).then(initialPlan).then(initialRegister).then(seed).then(dequeue).then(process)
    .then(choose).then(row).then(enqueue).then(choose).then(appendTarget).then(row).then(enqueue)
    .then(choose).then(row).then(enqueue).then(choose).then(appendTarget).then(row).then(enqueue)
    .then(publish).expect(q.published.nth(0)==List({symbol:0,target:0},{symbol:1,target:1},
      {symbol:2,target:1},{symbol:3,target:2}) and SourceSafety)
  run missingReverseRejectedTest=init.then(initialState).then(initialPlan).then(initialRegister).then(seed).then(dequeue).then(process)
    .then(choose).then(row).then(enqueue).then(choose).then(appendTarget).then(row).then(enqueue)
    .then(choose).then(row).then(enqueue).then(choose).then(appendTarget).then(row).then(enqueue)
    .then(q'={...q,published:q.published.append(q.acc),phase:"ready"}).expect(not(SourceSafety))
  run dequeueAccountingTest=init.then(initialState).then(initialPlan).then(initialRegister).then(seed).then(dequeue)
    .expect(q.owner==0 and q.processed==0 and q.work==List() and SourceSafety)
    .then(process).expect(q.processed==1 and SourceSafety)
}
