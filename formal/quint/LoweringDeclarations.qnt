// Helper allocation is distinct from augmented-start allocation. Callbacks
// precede entry, including zero-row bodies; failed callbacks cannot publish.
module LoweringDeclarations {
  type State = {claims:List[int], callbacks:List[int], bindings:List[int],
    stack:List[int], stage:str, rejectCallbacks:bool, published:bool}
  var q:State
  action start(rejectCallbacks:bool):bool = q'={claims:List(),callbacks:List(),bindings:List(),
    stack:List(),stage:"ready",rejectCallbacks:rejectCallbacks,published:false}
  action init={nondet rejectCallbacks=Set(false,true).oneOf();start(rejectCallbacks)}
  action allocate=all {q.stage=="ready",q.claims.length()<4,
    q'={...q,claims:q.claims.append(q.claims.length()),
      bindings:List(q.claims.length()).concat(q.bindings),stage:"allocated"}}
  action declare=all {q.stage=="allocated",q'={...q,
    callbacks:if(q.rejectCallbacks) q.callbacks else q.callbacks.append(q.claims.length()-1),
    stage:if(q.rejectCallbacks) "failed" else "declared"}}
  action enter=all {q.stage=="declared",q'={...q,
    stack:q.stack.append(q.claims.length()-1),stage:"ready"}}
  action leave=all {q.stage=="ready",q.stack.length()>0,
    q'={...q,stack:q.stack.slice(0,q.stack.length()-1)}}
  action emit=all {q.stage=="ready",q'=q}
  action publish=all {q.stage=="ready",q.stack==List(),q'={...q,stage:"done",published:true}}
  action step=any {allocate,declare,enter,leave,emit,publish,q'=q}
  val DeclarationSafety=all {
    q.claims==range(0,q.claims.length()),
    q.bindings.length()==q.claims.length(),
    q.bindings.indices().forall(index=>q.bindings.nth(index)==q.claims.nth(q.claims.length()-index-1)),
    if(q.callbacks.length()>q.claims.length()) false else q.callbacks==q.claims.slice(0,q.callbacks.length()),
    q.claims.length()==q.callbacks.length()+(if(q.stage=="allocated" or q.stage=="failed") 1 else 0),
    q.stack.indices().forall(index=>q.callbacks.indices().exists(other=>q.callbacks.nth(other)==q.stack.nth(index))),
    q.published implies (q.stage=="done" and q.stack==List() and q.callbacks==q.claims),
    q.stage=="failed" implies not(q.published)}
  run nestedZeroRowTest=start(false).then(allocate).then(declare).then(enter)
    .then(allocate).then(declare).then(enter).then(leave).then(leave).then(publish)
    .expect(DeclarationSafety and q.callbacks==List(0,1))
  run failedCallbackTest=start(true).then(allocate).then(declare)
    .expect(DeclarationSafety and q.stage=="failed" and q.callbacks==List())
  run missingCallbackRejectedTest=start(false).then(allocate)
    .then(q'={...q,stage:"ready"}).expect(not(DeclarationSafety))
  run reversedCallbacksRejectedTest=start(false).then(allocate).then(declare).then(enter)
    .then(allocate).then(declare).then(q'={...q,callbacks:List(1,0)}).expect(not(DeclarationSafety))
  run duplicateCallbackRejectedTest=start(false).then(allocate).then(declare)
    .then(q'={...q,callbacks:List(0,0)}).expect(not(DeclarationSafety))
  run failedPublicationRejectedTest=start(true).then(allocate).then(declare)
    .then(q'={...q,published:true}).expect(not(DeclarationSafety))
}
