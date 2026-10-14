// Lifecycle of an immutable capture extent. Lean owns list ordering and suffix
// equivalence; Scheme owns actual LR state, token counts and terminal yields.
module CapturePlan {
  type State = {width:int, cursor:int, phase:str, epoch:int, basis:int,
                work:int, output:bool}
  var q: State
  pure val initial:State = {width:3,cursor:0,phase:"idle",epoch:0,basis:0,work:0,output:false}
  action init = q' = initial
  action initUnary = q' = {...initial,width:1}
  action initEmpty = q' = {...initial,width:0}
  action Begin = all {q.phase == "idle",q' = {...q,phase:"prefix",basis:q.epoch}}
  action Prefix = all {q.phase == "prefix",q.cursor < q.width,
    q' = {...q,cursor:q.cursor + 1,work:q.work + 1}}
  action BindYield = all {q.phase == "prefix",q.cursor == q.width,q.width != 1,
    q' = {...q,phase:"yield",cursor:0}}
  action Yield = all {q.phase == "yield",q.cursor < q.width,
    q' = {...q,cursor:q.cursor + 1,work:q.work + 1}}
  action Publish = all {q.phase == "yield",q.cursor == q.width,q.basis == q.epoch,
    q' = {...q,phase:"published",output:true}}
  action PublishUnary = all {q.phase == "prefix",q.width == 1,q.cursor == 1,q.basis == q.epoch,
    q' = {...q,phase:"published",output:true}}
  action Invalidate = all {q.epoch == 0,
    q' = {...q,epoch:1,output:false,phase:"rejected"}}
  action step = any {Begin,Prefix,BindYield,Yield,Publish,PublishUnary,Invalidate}
  val CaptureSafety = q.cursor >= 0 and q.cursor <= q.width and q.work <= 2 * q.width
    and (q.output implies (q.phase == "published" and q.basis == q.epoch
      and q.cursor == q.width and q.work == (if (q.width == 1) 1 else 2 * q.width)))
  run completeExtentTest = init.then(Begin).then(Prefix).then(Prefix).then(Prefix)
    .then(BindYield).then(Yield).then(Yield).then(Yield).then(Publish).expect(CaptureSafety and q.output)
  run unaryExtentTest = initUnary.then(Begin).then(Prefix).then(PublishUnary)
    .expect(CaptureSafety and q.work == 1 and q.output)
  run nullableExtentTest = initEmpty.then(Begin).then(BindYield).then(Publish)
    .expect(CaptureSafety and q.work == 0 and q.output)
  run mutationInvalidatesTest = init.then(Begin).then(Prefix).then(Invalidate)
    .expect(CaptureSafety and not(q.output))
  run incompleteMutationTest = init.then(Begin).then(Prefix)
    .expect(not({...q,output:true,phase:"published"}.work == 2 * q.width))
}
