// Finite encoding protocol controls; generic source/CFG correspondence is Lean.
module ConstructionEncoding {
  type Key={kind:str,text:str}
  type State={directory:List[Key],raw:List[Key],codes:List[int],cursor:int,phase:str,declarations:List[Key],declarationCodes:List[int],declarationCursor:int}
  var q:State
  pure val values:List[Key]=List({kind:"nonterminal",text:"名称"},{kind:"token",text:"名称"},
    {kind:"literal",text:"名称"},{kind:"layout-start",text:"名称"},
    {kind:"literal",text:"é"},{kind:"literal",text:"é"},{kind:"literal",text:"😀"})
  pure def lookup(directory:List[Key],key:Key):int=directory.indices().fold(-1,(found,i)=>
    if(found<0 and directory.nth(i)==key) i else found)
  pure def valid(key:Key):bool=key.kind=="nonterminal" or key.kind=="token" or
    (Set("literal","layout-start","layout-next").contains(key.kind) and key.text!="")
  action setup(declarations:List[Key]):bool=q'={directory:values,raw:values,codes:List(),cursor:0,phase:"input",
    declarations:declarations,declarationCodes:List(),declarationCursor:0}
  action init={
    nondet declarations=Set(List(),List(values.nth(0)),List(values.nth(0),values.nth(0)),
      List({kind:"nonterminal",text:"missing"})).oneOf()
    setup(declarations)
  }
  action scan=all {q.phase=="input",q.declarationCursor==q.declarations.length(),q.cursor<q.raw.length(),
    val key=q.raw.nth(q.cursor)
    val code=lookup(q.directory,key)
    q'=if(valid(key) and code>=0) {...q,codes:q.codes.append(code),cursor:q.cursor+1}
      else {...q,phase:"rejected"}}
  action scanDeclaration=all {q.phase=="input",q.cursor==0,
    q.declarationCursor<q.declarations.length(),
    val key=q.declarations.nth(q.declarationCursor)
    val code=lookup(q.directory,key)
    q'=if(key.kind=="nonterminal" and code>=0) {...q,
      declarationCodes:q.declarationCodes.append(code),declarationCursor:q.declarationCursor+1}
      else {...q,phase:"rejected"}}
  action finish=all {q.phase=="input",q.cursor==q.raw.length(),q.declarationCursor==q.declarations.length(),q'={...q,phase:"ready"}}
  action step=any {scan,scanDeclaration,finish,q'=q}
  val EncodingSafety=all {
    q.declarationCodes.length()==q.declarationCursor,q.declarationCursor<=q.declarations.length(),
    q.cursor>0 implies q.declarationCursor==q.declarations.length(),
    q.declarationCodes.indices().forall(i=>q.declarationCodes.nth(i)>=0 and
      q.declarationCodes.nth(i)<q.directory.length() and
      q.directory.nth(q.declarationCodes.nth(i))==q.declarations.nth(i)),
    q.phase=="ready" implies q.declarationCursor==q.declarations.length(),q.codes.length()==q.cursor,q.cursor<=q.raw.length(),
    q.codes.indices().forall(i=>q.codes.nth(i)>=0 and q.codes.nth(i)<q.directory.length() and
      q.directory.nth(q.codes.nth(i))==q.raw.nth(i)),
    q.phase=="ready" implies q.cursor==q.raw.length(),
    q.phase=="rejected" implies (
      (q.declarationCursor==q.declarations.length() and q.cursor<q.raw.length() and
        (not(valid(q.raw.nth(q.cursor))) or lookup(q.directory,q.raw.nth(q.cursor))<0)) or
      (q.cursor==0 and q.declarationCursor<q.declarations.length() and
        (q.declarations.nth(q.declarationCursor).kind!="nonterminal" or
          lookup(q.directory,q.declarations.nth(q.declarationCursor))<0)))}
  run taggedIdentityTest=setup(List()).then(scan).then(scan).then(scan).then(scan)
    .expect(q.codes==List(0,1,2,3) and EncodingSafety)
  run normalizationFormsRemainDistinctTest=setup(List()).then(q'={...q,raw:List(values.nth(4),values.nth(5))})
    .then(scan).then(scan).then(finish).expect(q.codes==List(4,5) and EncodingSafety)
  run supplementaryTextTest=setup(List()).then(q'={...q,raw:List(values.nth(6))}).then(scan).then(finish)
    .expect(q.codes==List(6) and EncodingSafety)
  run duplicateKeepsPositionsTest=setup(List()).then(q'={...q,raw:List(values.nth(0),values.nth(0))})
    .then(scan).then(scan).then(finish).expect(q.codes==List(0,0) and EncodingSafety)
  run missingNameRejectedTest=setup(List()).then(q'={...q,raw:List({kind:"nonterminal",text:"missing"})})
    .then(scan).expect(q.phase=="rejected" and q.codes==List() and EncodingSafety)
  run emptyLiteralRejectedTest=setup(List()).then(q'={...q,directory:List({kind:"literal",text:""}),raw:List({kind:"literal",text:""})})
    .then(scan).expect(q.phase=="rejected" and EncodingSafety)
  run malformedTextTagRejectedTest=setup(List()).then(q'={...q,directory:List({kind:"symbol-literal",text:"text"}),raw:List({kind:"symbol-literal",text:"text"})})
    .then(scan).expect(q.phase=="rejected" and EncodingSafety)
  run reservedNameIsNotDeclarationTest=setup(List()).then(q'={...q,raw:List()}).then(finish)
    .expect(EncodingSafety and q.phase=="ready" and q.declarationCodes==List())
  run zeroRowDeclarationTest=setup(List(values.nth(0))).then(q'={...q,raw:List()})
    .then(scanDeclaration).then(finish).expect(EncodingSafety and q.declarationCodes==List(0))
  run duplicateDeclarationsRetainPositionsTest=setup(List(values.nth(0),values.nth(0)))
    .then(q'={...q,raw:List()}).then(scanDeclaration).then(scanDeclaration).then(finish)
    .expect(EncodingSafety and q.declarationCodes==List(0,0))
  run missingDeclarationRejectedTest=setup(List({kind:"nonterminal",text:"missing"}))
    .then(q'={...q,raw:List()}).then(scanDeclaration)
    .expect(EncodingSafety and q.phase=="rejected")
  run undeclaredMetadataInjectionTest=setup(List()).then(q'={...q,raw:List()}).then(finish)
    .then(q'={...q,declarationCodes:List(0)}).expect(not(EncodingSafety))
}
