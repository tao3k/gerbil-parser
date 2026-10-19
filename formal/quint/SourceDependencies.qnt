// Bounded lifecycle controls complement Lean's arbitrary-tree completeness.
// SDK expansion, hygienic binding classification and dispatch are premises.
module SourceDependencies {
  type State = {kind:str, stage:str, supplied:Set[str], missing:Set[str],
                expanded:int, core:int, checked:int, executed:int}
  var q:State
  pure def requires(kind:str):Set[str] =
    if(kind=="quoted") Set() else if(kind=="assignment") Set("target","rhs")
    else Set("callback")
  action start(kind:str,supplied:Set[str]):bool =
    q'={kind:kind,stage:"raw",supplied:supplied,missing:Set(),
        expanded:0,core:0,checked:0,executed:0}
  action init = {
    nondet kind=Set("reference","deferred","assignment","quoted").oneOf()
    nondet supplied=Set(Set(),Set("callback"),Set("target"),Set("target","rhs")).oneOf()
    start(kind,supplied)
  }
  action expand = all {q.stage=="raw",q'={...q,stage:"expanded",expanded:q.expanded+1,
    core:1,missing:requires(q.kind).exclude(q.supplied)}}
  action inspect = all {q.stage=="expanded",q'={...q,checked:q.core,
    stage:if(q.missing==Set()) "admitted" else "rejected"}}
  action execute = all {q.stage=="admitted",q'={...q,stage:"done",executed:q.core}}
  action step = any {expand,inspect,execute,q'=q}
  val DependencySafety = all {
    q.expanded<=1,
    q.stage!="raw" implies q.expanded==1,
    Set("admitted","done").contains(q.stage) implies q.missing==Set(),
    Set("admitted","done","rejected").contains(q.stage) implies q.checked==q.core,
    q.stage=="done" implies q.executed==q.checked,
    q.stage=="done" implies requires(q.kind).exclude(q.supplied)==Set()
  }
  run completeParametersTest=start("assignment",Set("target","rhs"))
    .then(expand).then(inspect).then(execute).expect(DependencySafety and q.stage=="done")
  run deferredMissingTest=start("deferred",Set()).then(expand).then(inspect)
    .expect(DependencySafety and q.stage=="rejected")
  run assignmentRhsTest=start("assignment",Set("target")).then(expand).then(inspect)
    .expect(DependencySafety and q.stage=="rejected" and q.missing==Set("rhs"))
  run quotedDataTest=start("quoted",Set()).then(expand).then(inspect).then(execute)
    .expect(DependencySafety and q.stage=="done")
  run duplicateExpansionRejectedTest=start("reference",Set("callback")).then(expand)
    .then(q'={...q,expanded:2}).expect(not(DependencySafety))
  run uncheckedExecutionRejectedTest=start("assignment",Set("target")).then(expand)
    .then(q'={...q,stage:"done",checked:1,executed:1}).expect(not(DependencySafety))
  run replacedCoreRejectedTest=start("quoted",Set()).then(expand).then(inspect)
    .then(q'={...q,stage:"done",executed:2}).expect(not(DependencySafety))
}
