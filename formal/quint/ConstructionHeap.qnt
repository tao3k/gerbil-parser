// Microsteps of the joint owned-vector protocol; arbitrary grammar composition
// and all positive capacities are proved in Lean, not by this bounded model.
module ConstructionHeap {
  type Heap={states:List[int],rows:List[int],plans:List[int]}
  type State={origin:int,heap:Heap,stage:Heap,count:int,cursor:int,phase:str,
    states:List[int],rows:List[int],plans:List[int],ids:List[int],captured:int,result:int}
  var q:State
  pure def blanks(capacity:int):List[int]=0.to(capacity-1).fold(List(),(out,_)=>out.append(-1))
  pure def empty(capacity:int):Heap={states:blanks(capacity),rows:blanks(capacity),plans:blanks(capacity)}
  action setup(capacity:int):bool=q'={origin:capacity,heap:empty(capacity),stage:empty(0),count:0,cursor:0,
    phase:"ready",states:List(),rows:List(),plans:List(),ids:List(),captured:0,result:-1}
  action init={nondet capacity=Set(1,2,3).oneOf();setup(capacity)}
  action appendState=all {q.phase=="ready",q.count<q.heap.states.length(),q.count<9,
    q'={...q,count:q.count+1,captured:q.count,result:q.count,ids:q.ids.append(q.count),
      states:q.states.append(q.count+10),rows:q.rows.append(-1),plans:q.plans.append(q.count+20),
      heap:{states:q.heap.states.replaceAt(q.count,q.count+10),rows:q.heap.rows,
        plans:q.heap.plans.replaceAt(q.count,q.count+20)}}}
  // Operational steps, rather than the atomic helper above, drive simulation.
  action beginAppend=all {q.phase=="ready",q.count<q.heap.states.length(),q.count<9,
    q'={...q,captured:q.count,result:-1,phase:"append-state"}}
  action writeState=all {q.phase=="append-state",
    q'={...q,heap:{...q.heap,states:q.heap.states.replaceAt(q.captured,q.count+10)},phase:"append-plan"}}
  action writePlan=all {q.phase=="append-plan",
    q'={...q,heap:{...q.heap,plans:q.heap.plans.replaceAt(q.captured,q.count+20)},phase:"append-register"}}
  action registerIndex=all {q.phase=="append-register",
    q'={...q,ids:q.ids.append(q.captured),phase:"append-advance"}}
  action advanceCount=all {q.phase=="append-advance",
    q'={...q,count:q.count+1,states:q.states.append(q.count+10),rows:q.rows.append(-1),
      plans:q.plans.append(q.count+20),phase:"append-return"}}
  action returnIndex=all {q.phase=="append-return",q'={...q,result:q.captured,phase:"ready"}}
  action beginGrowth=all {q.phase=="ready",q.count==q.heap.states.length(),q.count<9,
    q'={...q,stage:empty(2*q.count),cursor:0,phase:"copy"}}
  action copyCell=all {q.phase=="copy",q.cursor<q.count,
    q'={...q,cursor:q.cursor+1,stage:{states:q.stage.states.replaceAt(q.cursor,q.heap.states.nth(q.cursor)),
      rows:q.stage.rows.replaceAt(q.cursor,q.heap.rows.nth(q.cursor)),
      plans:q.stage.plans.replaceAt(q.cursor,q.heap.plans.nth(q.cursor))}}}
  action publishStates=all {q.phase=="copy",q.cursor==q.count,
    q'={...q,heap:{...q.heap,states:q.stage.states},phase:"publish-rows"}}
  action publishRows=all {q.phase=="publish-rows",
    q'={...q,heap:{...q.heap,rows:q.stage.rows},phase:"publish-plans"}}
  action publishPlans=all {q.phase=="publish-plans",
    q'={...q,heap:{...q.heap,plans:q.stage.plans},phase:"ready"}}
  action writeRow(node:int):bool=all {
    q.phase=="ready",node<q.count,
    q'={...q,rows:q.rows.replaceAt(node,node+30),heap:{...q.heap,rows:q.heap.rows.replaceAt(node,node+30)}}}
  action publishRow={nondet node=Set(0,1,2,3,4,5,6,7,8).oneOf();writeRow(node)}
  action step=any {beginAppend,writeState,writePlan,registerIndex,advanceCount,returnIndex,
    beginGrowth,copyCell,publishStates,publishRows,publishPlans,publishRow,q'=q}
  val HeapSafety=all {
    q.origin>0,q.count>=0,q.count<=q.heap.states.length(),
    Set("ready","copy","publish-rows","publish-plans","append-state","append-plan",
      "append-register","append-advance","append-return").contains(q.phase),
    (q.phase!="publish-rows" and q.phase!="publish-plans") implies
      (q.heap.states.length()==q.heap.rows.length() and q.heap.states.length()==q.heap.plans.length()),
    q.ids.length()==q.count+(if(q.phase=="append-advance") 1 else 0),
    q.ids.indices().forall(node=>q.ids.nth(node)==node),
    (q.phase=="ready") implies q.result==q.count-1,
    Set("append-state","append-plan","append-register","append-advance").contains(q.phase) implies
      (q.captured==q.count and q.result== -1 and q.count<q.heap.states.length()),
    (q.phase=="append-return") implies (q.captured==q.count-1 and q.result== -1),
    q.states.length()==q.count,q.rows.length()==q.count,q.plans.length()==q.count,
    if(q.heap.rows.length()>=q.count and q.heap.plans.length()>=q.count)
      q.states.indices().forall(node=>q.heap.states.nth(node)==q.states.nth(node) and
        q.heap.rows.nth(node)==q.rows.nth(node) and q.heap.plans.nth(node)==q.plans.nth(node)) else false,
    q.heap.states.indices().forall(node=>node<q.count or
      (if(node==q.count and Set("append-plan","append-register","append-advance").contains(q.phase))
        q.heap.states.nth(node)==q.count+10 else q.heap.states.nth(node)== -1)),
    q.heap.rows.indices().forall(node=>node<q.count or q.heap.rows.nth(node)== -1),
    q.heap.plans.indices().forall(node=>node<q.count or
      (if(node==q.count and Set("append-register","append-advance").contains(q.phase))
        q.heap.plans.nth(node)==q.count+20 else q.heap.plans.nth(node)== -1)),
    (q.phase=="publish-rows" or q.phase=="publish-plans") implies all {
      q.cursor==q.count,q.heap.states==q.stage.states,q.heap.states.length()==2*q.count,
      q.stage.rows.length()==2*q.count,q.stage.plans.length()==2*q.count,q.heap.plans.length()==q.count,
      if(q.phase=="publish-rows") q.heap.rows.length()==q.count else q.heap.rows==q.stage.rows},
    (q.phase=="copy") implies all {
      q.count==q.heap.states.length(),q.cursor>=0,q.cursor<=q.count,
      q.stage.states.length()==2*q.count,q.stage.rows.length()==2*q.count,q.stage.plans.length()==2*q.count,
      q.stage.states.indices().forall(node=>
        if(node<q.cursor) q.stage.states.nth(node)==q.heap.states.nth(node) and
          q.stage.rows.nth(node)==q.heap.rows.nth(node) and q.stage.plans.nth(node)==q.heap.plans.nth(node)
        else q.stage.states.nth(node)== -1 and q.stage.rows.nth(node)== -1 and q.stage.plans.nth(node)== -1)}}
  run unitGrowthTest=setup(1).then(appendState).then(writeRow(0)).then(beginGrowth)
    .then(copyCell).then(publishStates).then(publishRows).then(publishPlans).then(appendState)
    .expect(q.count==2 and q.heap.rows.nth(0)==30 and HeapSafety)
  run nonPowerCapacityTest=setup(3).then(appendState).then(appendState).then(appendState)
    .then(beginGrowth).then(copyCell).then(copyCell).then(copyCell).then(publishStates).then(publishRows).then(publishPlans)
    .then(appendState).expect(q.heap.states.length()==6 and q.count==4 and HeapSafety)
  run zeroCapacityRejectedTest=setup(0).expect(not(HeapSafety))
  run unequalCapacityRejectedTest=setup(2).then(q'={...q,heap:{...q.heap,rows:List()}}).expect(not(HeapSafety))
  run lostRowCopyRejectedTest=setup(1).then(appendState).then(writeRow(0)).then(beginGrowth).then(copyCell)
    .then(q'={...q,stage:{...q.stage,rows:q.stage.rows.replaceAt(0,-1)}}).expect(not(HeapSafety))
  run earlyPublicationRejectedTest=setup(1).then(appendState).then(beginGrowth)
    .then(q'={...q,heap:q.stage,phase:"ready"}).expect(not(HeapSafety))
  run partialOwnersPreservedTest=setup(1).then(appendState).then(beginGrowth).then(copyCell).then(publishStates)
    .expect(q.phase=="publish-rows" and q.heap.states.length()==2 and q.heap.rows.length()==1 and q.heap.plans.length()==1 and HeapSafety)
  run wrongOwnerOrderRejectedTest=setup(1).then(appendState).then(beginGrowth).then(copyCell)
    .then(q'={...q,heap:{...q.heap,rows:q.stage.rows},phase:"publish-rows"}).expect(not(HeapSafety))
  run appendCounterStagesTest=setup(1).then(beginAppend).then(writeState).then(writePlan).then(registerIndex)
    .expect(q.count==0 and q.ids==List(0) and HeapSafety)
    .then(advanceCount).expect(q.count==1 and q.result== -1 and HeapSafety)
    .then(returnIndex).expect(q.result==0 and HeapSafety)
  run earlyCounterRejectedTest=setup(2).then(beginAppend).then(writeState)
    .then(q'={...q,count:q.count+1}).expect(not(HeapSafety))
  run wrongReturnRejectedTest=setup(2).then(beginAppend).then(writeState).then(writePlan)
    .then(registerIndex).then(advanceCount).then(q'={...q,result:q.count,phase:"ready"}).expect(not(HeapSafety))
}
