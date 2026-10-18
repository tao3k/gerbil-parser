// Protocol-owned compiler requests; unrelated callers and SDK internals are external.
module AuthorPhase {
  type State = {stage:str, available:bool, valid:bool, input:int,
    requests:List[str], expanded:bool, output:int}
  var q: State
  action start(available:bool, valid:bool, input:int):bool =
    q'={stage:"new",available:available,valid:valid,input:input,
      requests:List(),expanded:false,output:0}
  action init = {
    nondet available=Set(false,true).oneOf()
    nondet valid=Set(false,true).oneOf()
    nondet input=Set(1,2,3).oneOf()
    start(available,valid,input)
  }
  action interface = all {q.stage=="new",q'={...q,stage:"interface"}}
  action request = all {q.stage=="interface",
    q'={...q,stage:"requested",requests:q.requests.append("compiler")}}
  action resolve = all {q.stage=="requested",
    q'={...q,stage:if(q.available) "ready" else "missing"}}
  action expand = all {q.stage=="ready",q'={...q,
    stage:if(q.valid) "done" else "invalid",expanded:true,
    output:if(q.valid) q.input else 0}}
  action step = any {interface,request,resolve,expand,q'=q}
  val PhaseSafety = all {
    Set("new","interface").contains(q.stage) implies q.requests==List(),
    not(Set("new","interface").contains(q.stage)) implies q.requests==List("compiler"),
    q.expanded iff Set("done","invalid").contains(q.stage),
    q.stage=="missing" implies (not(q.available) and not(q.expanded)),
    q.stage=="done" implies (q.available and q.valid and q.output==q.input),
    q.stage=="invalid" implies (q.available and not(q.valid) and q.output==0)
  }
  run interfaceWithoutCompilerTest=start(false,true,1).then(interface)
    .expect(PhaseSafety and q.requests==List() and not(q.expanded))
  run interfaceWithCompilerTest=start(true,true,2).then(interface)
    .expect(PhaseSafety and q.requests==List() and not(q.expanded))
  run missingServiceTest=start(false,true,1).then(interface).then(request).then(resolve)
    .expect(PhaseSafety and q.stage=="missing")
  run invalidDeclarationTest=start(true,false,1).then(interface).then(request).then(resolve).then(expand)
    .expect(PhaseSafety and q.stage=="invalid")
  run retainedOperandsTest=start(true,true,3).then(interface).then(request).then(resolve).then(expand)
    .expect(PhaseSafety and q.output==3)
  run eagerAdmissionRejectedTest=start(true,true,1).then(interface)
    .then(q'={...q,requests:List("compiler")}).expect(not(PhaseSafety))
  run missingCannotExpandTest=start(false,true,1).then(interface).then(request).then(resolve)
    .then(q'={...q,expanded:true}).expect(not(PhaseSafety))
}
