// Transition descriptors are immutable. Reconfiguration replaces a cache cell;
// it cannot rewrite a context already retained by a grammar fragment.
module CaptureContext {
  type State = {runtimeWidth:int,planWidth:int,usedWidth:int,kind:int,cachedKind:int,nodeKind:int,basisKind:int,oldKind:int,executor:int,target:int,cachedExecutor:int,cachedTarget:int,cached:bool,
                nodeExecutor:int,nodeTarget:int,basisExecutor:int,basisTarget:int,node:bool,
                oldExecutor:int,oldTarget:int,old:bool,allocations:int}
  var q:State
  pure val initial:State = {runtimeWidth:1,planWidth:1,usedWidth:0,kind:0,cachedKind:0,nodeKind:0,basisKind:0,oldKind:0,executor:0,target:1,cachedExecutor:0,cachedTarget:1,cached:false,
    nodeExecutor:0,nodeTarget:1,basisExecutor:0,basisTarget:1,node:false,
    oldExecutor:0,oldTarget:1,old:false,allocations:0}
  action init = q' = initial
  action Capture = all {q.executor != 0 or q.target == 1,
    q' = if(q.executor == 0) {...q,usedWidth:q.planWidth,
      cachedKind:q.kind,nodeKind:q.kind,basisKind:q.kind,cached:true,cachedExecutor:0,cachedTarget:1,
      node:true,nodeExecutor:0,nodeTarget:1,basisExecutor:0,basisTarget:1,
      allocations:q.allocations + (if(q.cached and q.cachedKind == q.kind) 0 else 1)}
    else {...q,usedWidth:q.planWidth,nodeKind:q.kind,basisKind:q.kind,
      node:true,nodeExecutor:q.executor,nodeTarget:q.target,basisExecutor:q.executor,basisTarget:q.target,
      allocations:q.allocations + 1}}
  action KeepOld = all {q.node,not(q.old),q' = {...q,oldKind:q.nodeKind,old:true,oldExecutor:q.nodeExecutor,oldTarget:q.nodeTarget}}
  action ChangeExecutor = all {q.executor == 0,q' = {...q,executor:1}}
  action ChangeTarget = all {q.target == 1,q' = {...q,target:2}}
  action ChangeKind = all {q.kind == 0,q' = {...q,kind:1}}
  action step = any {Capture,KeepOld,ChangeExecutor,ChangeTarget,ChangeKind}
  val ContextSafety = q.planWidth == q.runtimeWidth and q.allocations >= 0
    and (q.cached implies (q.cachedExecutor == 0 and q.cachedTarget == 1))
    and (q.node implies (q.usedWidth == q.runtimeWidth and q.nodeKind == q.basisKind and q.nodeExecutor == q.basisExecutor and q.nodeTarget == q.basisTarget))
  action WrongCapture = all {q.cached,q' = {...q,node:true,nodeKind:q.cachedKind,basisKind:q.kind,nodeExecutor:q.cachedExecutor,
    nodeTarget:q.cachedTarget,basisExecutor:q.executor,basisTarget:q.target}}
  action WrongPlan = all {q.node,q' = {...q,usedWidth:q.planWidth + 1}}
  run wrongWidthPlanRejectedTest = init.then(Capture).then(WrongPlan).expect(not(ContextSafety))
  run repeatedTransitionSharesTest = init.then(Capture).then(Capture)
    .expect(ContextSafety and q.allocations == 1)
  run executorChangeKeepsOldTest = init.then(Capture).then(KeepOld).then(ChangeExecutor).then(Capture)
    .expect(ContextSafety and q.oldExecutor == 0 and q.nodeExecutor == 1 and q.allocations == 2)
  run targetChangeKeepsOldTest = init.then(Capture).then(KeepOld).then(ChangeExecutor).then(ChangeTarget).then(Capture)
    .expect(ContextSafety and q.oldTarget == 1 and q.nodeTarget == 2 and q.allocations == 2)
  run kindChangeKeepsOldTest = init.then(Capture).then(KeepOld).then(ChangeKind).then(Capture)
    .expect(ContextSafety and q.oldKind == 0 and q.nodeKind == 1 and q.allocations == 2)
  run directExecutorCannotPolluteCanonicalCacheTest = init.then(Capture).then(ChangeExecutor).then(ChangeTarget).then(Capture)
    .expect(ContextSafety and q.cachedExecutor == 0 and q.cachedTarget == 1 and q.nodeExecutor == 1 and q.nodeTarget == 2)
  run changedCanonicalTargetRejectedTest = init.then(Capture).then(ChangeTarget).then(WrongCapture)
    .expect(not(ContextSafety))
  run staleDescriptorRejectedTest = init.then(Capture).then(ChangeExecutor).then(WrongCapture)
    .expect(not(ContextSafety))
}
