// The selector is evaluated once; later heap changes cannot select another
// branch. These finite controls complement source-evaluation proofs in Lean.
module LoweringDispatch {
  pure val Tags=Set("empty","layout-end","literal","token","layout-start","layout-next",
    "reference","sequence","choice","optional","repeat","repeat1","field","alias","precedence","unsupported")
  pure def branch(kind:str,tag:str):str=
    if(kind=="symbol")
      if(Set("literal","layout-start","layout-next").contains(tag)) "terminal"
      else if(tag=="token") "token" else if(tag=="reference") "reference" else "helper"
    else if(kind=="operand")
      if(Set("field","alias").contains(tag)) tag else "symbol"
    else if(Set("literal","token","layout-start","layout-next","reference").contains(tag)) "atomic"
      else if(Set("repeat","repeat1").contains(tag)) "repetition"
      else if(tag=="unsupported") "error" else tag
  type State={kind:str,live:str,captured:str,selected:str,reads:int,stage:str,effects:int,published:bool}
  var q:State
  action start(kind:str,tag:str):bool=q'={kind:kind,live:tag,captured:"",selected:"",reads:0,
    stage:"ready",effects:0,published:false}
  action init={nondet kind=Set("lower","operand","symbol").oneOf();nondet tag=Tags.oneOf();start(kind,tag)}
  action read=all {q.stage=="ready",q'={...q,captured:q.live,reads:q.reads+1,stage:"read"}}
  action mutate={nondet tag=Tags.oneOf();all {q.stage=="read",q'={...q,live:tag}}}
  action choose=all {q.stage=="read",q'={...q,selected:branch(q.kind,q.captured),stage:"chosen"}}
  action body=all {q.stage=="chosen",q'={...q,
    effects:if(q.selected=="error") 0 else 1,stage:if(q.selected=="error") "failed" else "done",
    published:q.selected!="error"}}
  action step=any {read,mutate,choose,body,q'=q}
  val DispatchSafety=all {
    q.reads==(if(q.stage=="ready") 0 else 1),
    Set("chosen","done","failed").contains(q.stage) implies q.selected==branch(q.kind,q.captured),
    q.effects==(if(q.stage=="done") 1 else 0),
    q.published iff q.stage=="done",
    q.stage=="failed" implies (q.selected=="error" and not(q.published))}
  run selectorHeapChangeTest=start("lower","literal").then(read)
    .then(q'={...q,live:"repeat"}).then(choose).then(body)
    .expect(DispatchSafety and q.selected=="atomic" and q.captured=="literal")
  run tokenCategoryTest=start("symbol","token").then(read).then(choose).then(body)
    .expect(DispatchSafety and q.selected=="token")
  run aliasOperandTest=start("operand","alias").then(read).then(choose).then(body)
    .expect(DispatchSafety and q.selected=="alias")
  run unsupportedDoesNotRunChildTest=start("lower","unsupported").then(read).then(choose).then(body)
    .expect(DispatchSafety and q.effects==0 and not(q.published))
  run secondSelectorRejectedTest=start("lower","literal").then(read)
    .then(q'={...q,reads:2}).expect(not(DispatchSafety))
  run liveTagReselectionRejectedTest=start("lower","literal").then(read)
    .then(q'={...q,live:"repeat",selected:"repetition",stage:"chosen"}).expect(not(DispatchSafety))
}
