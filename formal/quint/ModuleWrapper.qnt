// Loader plans are closed effects. SDK evaluation and cache behavior are premises.
module ModuleWrapper {
  type State = {kind:str, stage:str, plan:List[str], initialized:List[str], failure:str}
  var q: State
  action start(kind:str):bool = q'={kind:kind,stage:"new",plan:List(),initialized:List(),failure:""}
  action init = {nondet kind=Set("closed","quoted","deferred","fail-first").oneOf(); start(kind)}
  action inspect = all {q.stage=="new",q'={...q,stage:"inspected",
    plan:if(q.kind=="closed") List("first","second") else List()}}
  action preload = all {q.stage=="inspected",q'={...q,stage:"preloaded",initialized:q.plan}}
  // SDK memoization is external: repeated requests preserve initialization count.
  action execute = all {q.stage=="preloaded",q'={...q,
    stage:if(q.kind=="fail-first") "failed" else "done",
    failure:if(q.kind=="fail-first") "parent" else ""}}
  action invoke = all {q.stage=="done",q.kind=="deferred",
    q'={...q,stage:"failed",failure:"missing"}}
  action step = any {inspect,preload,execute,invoke,q'=q}
  val WrapperSafety = all {
    q.kind!="closed" implies q.plan==List(),
    q.kind!="closed" implies q.initialized==List(),
    q.kind=="closed" implies (q.plan==List() or q.plan==List("first","second")),
    q.kind=="closed" implies (q.initialized==List() or q.initialized==List("first","second")),
    q.stage=="inspected" and q.kind=="closed" implies q.plan==List("first","second"),
    Set("preloaded","done","failed").contains(q.stage) and q.kind=="closed"
      implies q.initialized==List("first","second"),
    q.failure=="parent" implies (q.kind=="fail-first" and q.stage=="failed"),
    q.failure=="missing" implies (q.kind=="deferred" and q.stage=="failed")
  }
  run closedOrderTest=start("closed").then(inspect).then(preload).then(execute)
    .expect(WrapperSafety and q.initialized==List("first","second"))
  run quotedDataTest=start("quoted").then(inspect).then(preload).then(execute)
    .expect(WrapperSafety and q.initialized==List() and q.failure=="")
  run deferredCallTest=start("deferred").then(inspect).then(preload).then(execute)
    .expect(WrapperSafety and q.initialized==List() and q.failure=="").then(invoke)
    .expect(WrapperSafety and q.failure=="missing")
  run firstFailureTest=start("fail-first").then(inspect).then(preload).then(execute)
    .expect(WrapperSafety and q.failure=="parent")
  run quotedSpeculationRejectedTest=start("quoted").then(inspect)
    .then(q'={...q,plan:List("missing")}).expect(not(WrapperSafety))
  run opaquePrefixRejectedTest=start("fail-first").then(inspect)
    .then(q'={...q,initialized:List("second")}).expect(not(WrapperSafety))
  run reorderedPlanRejectedTest=start("closed").then(inspect)
    .then(q'={...q,plan:List("second","first")}).expect(not(WrapperSafety))
}
