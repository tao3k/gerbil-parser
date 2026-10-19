// Project-owned read-to-transform protocol. Reader and expansion are host calls;
// duplicate declarations, read errors, interface mutation and omitted callbacks
// are independently rejectable before execution.
module SourceDeclarations {
  type State = {stage:str, seen:int, calls:int, preserved:bool, missing:bool, eof:bool}
  var q:State
  action init = q'={stage:"reading",seen:0,calls:0,preserved:true,missing:false,eof:false}
  action readHit = all {q.stage=="reading",q'={...q,seen:q.seen+1}}
  action readSkip = all {q.stage=="reading",q'=q}
  action end = all {q.stage=="reading",q'={...q,eof:true,
    stage:if(q.seen==1) "selected" else "rejected"}}
  action readError = all {q.stage=="reading",q'={...q,stage:"rejected"}}
  action transform(preserved:bool,missing:bool):bool = all {
    q.stage=="selected",q'={...q,calls:q.calls+1,preserved:preserved,missing:missing,
      stage:if(preserved) "transformed" else "rejected"}}
  action inspect = all {q.stage=="transformed",q'={...q,
    stage:if(q.missing) "rejected" else "admitted"}}
  action execute = all {q.stage=="admitted",q'={...q,stage:"done"}}
  action step = any {readHit,readSkip,end,readError,transform(true,false),
    transform(true,true),transform(false,false),inspect,execute,q'=q}
  val DeclarationSafety = all {
    q.calls<=1,
    q.calls>0 implies (q.eof and q.seen==1),
    Set("transformed","admitted","done").contains(q.stage) implies (q.calls==1 and q.preserved),
    Set("admitted","done").contains(q.stage) implies not(q.missing)
  }
  run completeTest=init.then(readSkip).then(readHit).then(end)
    .then(transform(true,false)).then(inspect).then(execute).expect(DeclarationSafety and q.stage=="done")
  run duplicateTest=init.then(readHit).then(readHit).then(end).expect(q.stage=="rejected" and q.calls==0)
  run absentTest=init.then(end).expect(q.stage=="rejected" and q.calls==0)
  run readErrorTest=init.then(readHit).then(readError).expect(q.stage=="rejected" and q.calls==0)
  run changedInterfaceTest=init.then(readHit).then(end).then(transform(false,false))
    .expect(DeclarationSafety and q.stage=="rejected")
  run trailingDependencyTest=init.then(readHit).then(end).then(transform(true,true)).then(inspect)
    .expect(DeclarationSafety and q.stage=="rejected")
  run earlyTransformMutationTest=init.then(q'={...q,stage:"transformed",calls:1})
    .expect(not(DeclarationSafety))
  run duplicateCallMutationTest=init.then(readHit).then(end).then(transform(true,false))
    .then(q'={...q,calls:2}).expect(not(DeclarationSafety))
}
