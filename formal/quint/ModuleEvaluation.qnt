// SDK-selected force order and cache behavior are external premises.
module ModuleEvaluation {
  type State = {pending:List[str], effects:List[str], observed:List[str],
    installed:bool, failed:bool, kind:str, parent:str, child:str}
  var q: State
  action start(kind:str):bool = q'={pending:if(kind=="failure") List("fail","helper")
    else if(kind=="dormant") List() else List("left","right"),effects:List(),observed:List(),installed:false,failed:false,kind:kind,parent:"cold",child:"cold"}
  action init = {nondet kind=Set("nested","failure","dormant").oneOf(); start(kind)}
  // Installing wrappers is not forcing the promises or preloading dependencies.
  action install = all {not(q.installed),q'={...q,installed:true}}
  action force = all {q.installed,not(q.failed),q.pending.length()>0,
    val selected=q.pending.nth(0)
    q'={...q,pending:q.pending.slice(1,q.pending.length()),
      observed:q.observed.append(selected),failed:selected=="fail",
      parent:if(selected=="fail") "failed" else q.parent,
      child:if(selected=="fail") "failed" else q.child,
      effects:if(selected=="fail") q.effects else q.effects.append(selected)}}
  action cached = all {q.installed,q.pending.length()==0,q'=q}
  action recoverChild = all {q.failed,q'={...q,child:"ready"}}
  action retryFailedParent = all {q.failed,q.parent=="failed",q'=q}
  action reloadParent = all {q.failed,q.child=="ready",q.parent=="failed",
    q'={...q,failed:false,parent:"ready",pending:List()}}
  action step = any {install,force,cached,recoverChild,retryFailedParent,reloadParent,q'=q}
  val ForceSafety = all {
    not(q.installed) implies (q.effects==List() and q.observed==List()),
    Set("failure","dormant").contains(q.kind) implies q.effects==List(),
    q.failed implies (q.observed==List("fail") and q.parent=="failed"),
    q.kind=="nested" implies Set(List(),List("left"),List("left","right")).contains(q.effects),
    q.kind=="nested" implies q.effects==q.observed
  }
  run parentFailureCacheTest=start("failure").then(install).then(force).then(recoverChild)
    .then(retryFailedParent).expect(ForceSafety and q.failed and q.parent=="failed")
    .then(reloadParent).expect(ForceSafety and not(q.failed) and q.parent=="ready")
  run lostParentCacheRejectedTest=start("failure").then(install).then(force).then(recoverChild)
    .then(q'={...q,parent:"cold"}).expect(not(ForceSafety))
  run dormantEdgeTest=start("dormant").then(install).then(cached)
    .expect(ForceSafety and q.effects==List() and q.observed==List())
  run nestedForceTest=start("nested").then(install).expect(ForceSafety and q.effects==List())
    .then(force).then(force).expect(ForceSafety and q.effects==List("left","right"))
  run firstFailureTest=start("failure").then(install).then(force)
    .expect(ForceSafety and q.failed and q.pending==List("helper"))
  run cachedForceTest=start("nested").then(install).then(force).then(force).then(cached)
    .expect(ForceSafety and q.effects==List("left","right"))
  run eagerInstallationRejectedTest=start("nested").then(q'={...q,effects:List("left")})
    .expect(not(ForceSafety))
  run sourceHelperOmissionRejectedTest=start("nested").then(install)
    .then(q'={...q,effects:List("left"),pending:List("right")}).expect(not(ForceSafety))
  run failureReorderingRejectedTest=start("failure").then(install)
    .then(q'={...q,effects:List("helper")}).expect(not(ForceSafety))
}
