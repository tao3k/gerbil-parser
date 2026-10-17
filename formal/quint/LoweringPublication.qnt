// Common lowering emitter: callbacks return before increment/store; failure aborts publication.
module LoweringPublication {
  type State={next:int,rows:List[int],origins:List[int],proofs:List[int],stage:str,
    origin:bool,proof:bool,failAt:int,failStage:str,published:bool}
  var q:State
  action start(origin:bool,proof:bool,failAt:int,failStage:str):bool =
    q'={next:1,rows:List(),origins:List(),proofs:List(),stage:"ready",
      origin:origin,proof:proof,failAt:failAt,failStage:failStage,published:false}
  action init={nondet origin=Set(false,true).oneOf(); nondet proof=Set(false,true).oneOf();
    nondet failAt=Set(1,2,3,4).oneOf(); nondet failStage=Set("origin","proof").oneOf();
    start(origin,proof,failAt,failStage)}
  action allocate=all {q.stage=="ready",q.rows.length()<3,q'={...q,stage:"origin"}}
  action observeOrigin=all {q.stage=="origin",q'={...q,
    origins:if(q.origin) q.origins.append(q.next) else q.origins,
    stage:if(q.origin and q.failStage=="origin" and q.next==q.failAt) "failed" else "proof"}}
  action observeProof=all {q.stage=="proof",q'={...q,
    proofs:if(q.proof) q.proofs.append(q.next) else q.proofs,
    stage:if(q.proof and q.failStage=="proof" and q.next==q.failAt) "failed" else "increment"}}
  action increment=all {q.stage=="increment",q'={...q,next:q.next+1,stage:"store"}}
  action store=all {q.stage=="store",q'={...q,rows:q.rows.append(q.next-1),stage:"ready"}}
  action finish=all {q.stage=="ready",q.rows.length()==3,q'={...q,stage:"done",published:true}}
  action step=any {allocate,observeOrigin,observeProof,increment,store,finish,q'=q}
  val PublicationSafety=all {
    q.rows==range(1,q.rows.length()+1),
    q.next==q.rows.length()+(if(q.stage=="store") 2 else 1),
    q.origin or q.origins==List(),q.proof or q.proofs==List(),
    (q.origin and q.proof) implies q.proofs.length()<=q.origins.length(),
    q.stage=="failed" implies (not(q.published) and q.next==q.failAt),
    q.published implies (q.stage=="done" and q.rows==List(1,2,3)) }
  run originFailureBeforeProofTest=start(true,true,1,"origin").then(allocate).then(observeOrigin)
    .expect(PublicationSafety and q.stage=="failed" and q.rows==List() and q.proofs==List())
  run proofFailureBeforeCommitTest=start(true,true,1,"proof").then(allocate).then(observeOrigin).then(observeProof)
    .expect(PublicationSafety and q.stage=="failed" and q.next==1 and q.rows==List())
  run lateFailurePreservesPrefixTest=start(true,true,2,"origin").then(allocate).then(observeOrigin).then(observeProof)
    .then(increment).then(store).then(allocate).then(observeOrigin)
    .expect(PublicationSafety and q.rows==List(1) and q.next==2)
  run disabledCallbacksDoNotRunTest=start(false,false,1,"origin").then(allocate).then(observeOrigin).then(observeProof)
    .then(increment).then(store).expect(PublicationSafety and q.rows==List(1) and q.origins==List() and q.proofs==List())
  run incrementIsNotPublicationTest=start(true,true,4,"origin").then(allocate).then(observeOrigin).then(observeProof)
    .then(increment).expect(PublicationSafety and q.rows==List() and q.next==2 and not(q.published))
  run completePublicationTest=start(false,false,4,"origin")
    .then(allocate).then(observeOrigin).then(observeProof).then(increment).then(store)
    .then(allocate).then(observeOrigin).then(observeProof).then(increment).then(store)
    .then(allocate).then(observeOrigin).then(observeProof).then(increment).then(store).then(finish)
    .expect(PublicationSafety and q.published and q.rows==List(1,2,3) and q.next==4)
  run earlyIncrementRejectedTest=start(true,true,1,"origin").then(allocate)
    .then(q'={...q,next:2}).expect(not(PublicationSafety))
  run proofBeforeOriginRejectedTest=start(true,true,4,"origin").then(allocate)
    .then(q'={...q,proofs:List(1)}).expect(not(PublicationSafety))
  run failedPrefixCannotPublishTest=start(true,true,1,"proof").then(allocate).then(observeOrigin).then(observeProof)
    .then(q'={...q,published:true}).expect(not(PublicationSafety))
}
