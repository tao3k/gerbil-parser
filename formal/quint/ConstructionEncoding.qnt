// Finite encoding protocol controls; generic source/CFG correspondence is Lean.
module ConstructionEncoding {
  type Key={kind:str,text:str}
  type State={directory:List[Key],raw:List[Key],codes:List[int],cursor:int,phase:str}
  var q:State
  pure val values:List[Key]=List({kind:"nonterminal",text:"名称"},{kind:"token",text:"名称"},
    {kind:"literal",text:"名称"},{kind:"layout-start",text:"名称"},
    {kind:"literal",text:"é"},{kind:"literal",text:"é"},{kind:"literal",text:"😀"})
  pure def lookup(directory:List[Key],key:Key):int=directory.indices().fold(-1,(found,i)=>
    if(found<0 and directory.nth(i)==key) i else found)
  pure def valid(key:Key):bool=key.kind=="nonterminal" or key.kind=="token" or
    (Set("literal","layout-start","layout-next").contains(key.kind) and key.text!="")
  action init=q'={directory:values,raw:values,codes:List(),cursor:0,phase:"input"}
  action scan=all {q.phase=="input",q.cursor<q.raw.length(),
    val key=q.raw.nth(q.cursor)
    val code=lookup(q.directory,key)
    q'=if(valid(key) and code>=0) {...q,codes:q.codes.append(code),cursor:q.cursor+1}
      else {...q,phase:"rejected"}}
  action finish=all {q.phase=="input",q.cursor==q.raw.length(),q'={...q,phase:"ready"}}
  action step=any {scan,finish,q'=q}
  val EncodingSafety=all {q.codes.length()==q.cursor,q.cursor<=q.raw.length(),
    q.codes.indices().forall(i=>q.codes.nth(i)>=0 and q.codes.nth(i)<q.directory.length() and
      q.directory.nth(q.codes.nth(i))==q.raw.nth(i)),
    q.phase=="ready" implies q.cursor==q.raw.length(),
    q.phase=="rejected" implies (q.cursor<q.raw.length() and
      (not(valid(q.raw.nth(q.cursor))) or lookup(q.directory,q.raw.nth(q.cursor))<0))}
  run taggedIdentityTest=init.then(scan).then(scan).then(scan).then(scan)
    .expect(q.codes==List(0,1,2,3) and EncodingSafety)
  run normalizationFormsRemainDistinctTest=init.then(q'={...q,raw:List(values.nth(4),values.nth(5))})
    .then(scan).then(scan).then(finish).expect(q.codes==List(4,5) and EncodingSafety)
  run supplementaryTextTest=init.then(q'={...q,raw:List(values.nth(6))}).then(scan).then(finish)
    .expect(q.codes==List(6) and EncodingSafety)
  run duplicateKeepsPositionsTest=init.then(q'={...q,raw:List(values.nth(0),values.nth(0))})
    .then(scan).then(scan).then(finish).expect(q.codes==List(0,0) and EncodingSafety)
  run missingNameRejectedTest=init.then(q'={...q,raw:List({kind:"nonterminal",text:"missing"})})
    .then(scan).expect(q.phase=="rejected" and q.codes==List() and EncodingSafety)
  run emptyLiteralRejectedTest=init.then(q'={...q,directory:List({kind:"literal",text:""}),raw:List({kind:"literal",text:""})})
    .then(scan).expect(q.phase=="rejected" and EncodingSafety)
  run malformedTextTagRejectedTest=init.then(q'={...q,directory:List({kind:"symbol-literal",text:"text"}),raw:List({kind:"symbol-literal",text:"text"})})
    .then(scan).expect(q.phase=="rejected" and EncodingSafety)
}
