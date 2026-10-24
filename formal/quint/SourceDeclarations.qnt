// Read, invoke, detach, guard and inspect form one project-owned protocol.
// Reader/core semantics and fresh object-graph copying are host premises.
module SourceDeclarations {
  type State = {stage:str, seen:int, calls:int, eof:bool, original:int,
    callback:int, published:int, detached:bool, callbackMissing:bool, missing:bool}
  var q:State
  action init = q'={stage:"reading",seen:0,calls:0,eof:false,original:0,
    callback:0,published:0,detached:false,callbackMissing:false,missing:false}
  action readHit = all {q.stage=="reading",q'={...q,seen:q.seen+1}}
  action readSkip = all {q.stage=="reading",q'=q}
  action end = all {q.stage=="reading",q'={...q,eof:true,
    stage:if(q.seen==1) "selected" else "rejected"}}
  action readError = all {q.stage=="reading",q'={...q,stage:"rejected"}}
  action transform(preserved:bool,missing:bool):bool = all {
    q.stage=="selected",q'={...q,calls:q.calls+1,stage:"returned",
      callback:if(preserved) q.original else q.original+1,callbackMissing:missing}}
  action publish = all {q.stage=="returned",q'={...q,stage:"published",detached:true,
    published:q.callback,missing:q.callbackMissing}}
  action guard = all {q.stage=="published",q'={...q,
    stage:if(q.published==q.original) "transformed" else "rejected"}}
  action mutateRetained = all {Set("published","transformed","admitted","done").contains(q.stage),
    q'={...q,callback:q.callback+1,callbackMissing:true,
      published:if(q.detached) q.published else q.callback+1,
      missing:if(q.detached) q.missing else true}}
  action inspect = all {q.stage=="transformed",q'={...q,
    stage:if(q.missing) "rejected" else "admitted"}}
  action execute = all {q.stage=="admitted",q'={...q,stage:"done"}}
  action step = any {readHit,readSkip,end,readError,transform(true,false),
    transform(true,true),transform(false,false),publish,guard,mutateRetained,inspect,execute,q'=q}
  val DeclarationSafety = all {
    q.calls<=1,
    q.calls>0 implies (q.eof and q.seen==1),
    Set("published","transformed","admitted","done").contains(q.stage) implies (q.calls==1 and q.detached),
    Set("transformed","admitted","done").contains(q.stage) implies q.published==q.original,
    Set("admitted","done").contains(q.stage) implies not(q.missing)
  }
  run completeTest=init.then(readSkip).then(readHit).then(end)
    .then(transform(true,false)).then(publish).then(guard).then(inspect).then(execute)
    .expect(DeclarationSafety and q.stage=="done")
  run duplicateTest=init.then(readHit).then(readHit).then(end).expect(q.stage=="rejected" and q.calls==0)
  run absentTest=init.then(end).expect(q.stage=="rejected" and q.calls==0)
  run readErrorTest=init.then(readHit).then(readError).expect(q.stage=="rejected" and q.calls==0)
  run changedInterfaceTest=init.then(readHit).then(end).then(transform(false,false)).then(publish).then(guard)
    .expect(DeclarationSafety and q.stage=="rejected")
  run trailingDependencyTest=init.then(readHit).then(end).then(transform(true,true)).then(publish).then(guard).then(inspect)
    .expect(DeclarationSafety and q.stage=="rejected")
  run retainedOutputTest=init.then(readHit).then(end).then(transform(true,false)).then(publish)
    .then(guard).then(mutateRetained).then(inspect).then(execute).then(mutateRetained)
    .expect(DeclarationSafety and q.stage=="done" and q.callback!=q.published)
  run mutationBeforeGuardTest=init.then(readHit).then(end).then(transform(true,false)).then(publish)
    .then(mutateRetained).then(guard).then(inspect).then(execute).expect(DeclarationSafety and q.stage=="done")
  run aliasedPublicationMutationTest=init.then(readHit).then(end).then(transform(true,false))
    .then(q'={...q,stage:"transformed",published:q.callback,detached:false}).then(mutateRetained)
    .expect(not(DeclarationSafety))
  run earlyTransformMutationTest=init.then(q'={...q,stage:"returned",calls:1}).expect(not(DeclarationSafety))
  run duplicateCallMutationTest=init.then(readHit).then(end).then(transform(true,false))
    .then(q'={...q,calls:2}).expect(not(DeclarationSafety))
}
