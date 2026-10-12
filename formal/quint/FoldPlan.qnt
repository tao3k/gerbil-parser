// Shared-scan lifecycle histories only. Lean owns denotational refinement.
module FoldPlan {
  type State = {epoch: int, basis: int, ready: bool, scans: int,
                next: int, events: List[int]}
  var q: State
  pure val initial: State = {epoch:0,basis:-1,ready:false,scans:0,next:0,events:[]}
  pure def invalidate(s: State): State = {...s,epoch:s.epoch + 1,ready:false}
  pure def scan(s: State): State = {...s,basis:s.epoch,ready:true,scans:s.scans + 1}
  pure def consume(s: State, consumer: int): State =
    if (s.ready and s.basis == s.epoch and consumer == s.next)
      {...s,next:s.next + 1,events:s.events.append(consumer)} else s
  action init = q' = initial
  action Scan = all {not(q.ready), q' = scan(q)}
  action SourceEdit = q' = invalidate(q)
  action BoundsChange = q' = invalidate(q)
  action EffectBarrier = q' = invalidate(q)
  action HelperBarrier = q' = invalidate(q)
  action First = q' = consume(q,0)
  action Second = q' = consume(q,1)
  action step = any {Scan, SourceEdit, BoundsChange, EffectBarrier, HelperBarrier, First, Second}
  val CurrentScan = not(q.ready) or q.basis == q.epoch
  val OrderedConsumers = q.events == [] or q.events == [0] or q.events == [0,1]
  val ExecutionSafety = CurrentScan and OrderedConsumers
  // Epochs abstract frame identity. Detecting every implementation write,
  // alias and helper effect remains a compiler-to-AOT refinement obligation.
  run sharedScanLifecycleTest = init.then(Scan).then(First).then(Second)
    .expect(ExecutionSafety and q.scans == 1 and q.events == [0,1])
  run sourceInvalidationTest = init.then(Scan).then(SourceEdit).then(First)
    .expect(ExecutionSafety and q.events == [])
  run boundsInvalidationTest = init.then(Scan).then(BoundsChange).then(First)
    .expect(ExecutionSafety and q.events == [])
  run effectInvalidationTest = init.then(Scan).then(EffectBarrier).then(First)
    .expect(ExecutionSafety and q.events == [])
  run helperInvalidationTest = init.then(Scan).then(HelperBarrier).then(First)
    .expect(ExecutionSafety and q.events == [])
  run rescanAfterChangeTest = init.then(Scan).then(SourceEdit).then(Scan).then(First)
    .expect(ExecutionSafety and q.scans == 2 and q.events == [0])
  run consumerInterleavingTest = init.then(Scan).then(Second).then(First).then(Second)
    .expect(ExecutionSafety and q.events == [0,1])
  // Mutation witness: retaining ready=true while advancing the epoch is unsafe.
  run staleReuseMutationTest = init.then(Scan).expect(
    not(not(q.ready) or q.basis == q.epoch + 1))
}
