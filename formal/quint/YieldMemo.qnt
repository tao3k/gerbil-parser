// A derived terminal index over engine-owned immutable grammar topology.
// No allocation/CPU claim: Lean owns cost obligations; Scheme measures them.
module YieldMemo {
  type Memo = {epoch:int,basis:int,phase:str,work:int,visible:bool,cacheEpoch:int}
  var q:Memo
  pure val initial:Memo = {epoch:0,basis:0,phase:"pending",work:0,visible:false,cacheEpoch:0}
  action init = q' = initial
  action Begin = all {q.phase == "pending",q' = {...q,phase:"building",basis:q.epoch}}
  action Build = all {q.phase == "building",q.work < 2,q' = {...q,work:q.work + 1}}
  action Publish = all {q.phase == "building",q.work == 2,q.basis == q.epoch,
    q' = {...q,phase:"ready",visible:true,cacheEpoch:q.epoch}}
  action QueryWarm = all {q.phase == "ready",q' = q}
  // Invalidating a topology is outside the producer premise. Do not expose
  // partial output or admit a result from a different generation afterwards.
  action Invalidate = all {q.epoch == 0,q' = {...q,epoch:1,phase:"invalid",visible:false}}
  action step = any {Begin,Build,Publish,QueryWarm,Invalidate}
  val MemoSafety = (q.work >= 0 and q.work <= 2)
    and (q.visible implies (q.phase == "ready" and q.cacheEpoch == q.epoch and q.work == 2))
  action WrongPublish = all {q.phase == "building",q.work < 2,q' = {...q,visible:true,phase:"ready"}}
  run freshCaptureDoesNotBuildTest = init.expect(MemoSafety and q.work == 0 and not(q.visible))
  run coldThenWarmTest = init.then(Begin).then(Build).then(Build).then(Publish).then(QueryWarm)
    .expect(MemoSafety and q.visible and q.work == 2)
  run failedProducerCannotPublishTest = init.then(Begin).then(Build).then(Invalidate)
    .expect(MemoSafety and not(q.visible))
  run partialPublicationRejectedTest = init.then(Begin).then(Build).then(WrongPublish)
    .expect(not(MemoSafety))
}
