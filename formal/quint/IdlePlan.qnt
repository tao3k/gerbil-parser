// Guard re-entry and invalidation histories. Lean owns transition equivalence.
// Abstract epochs are not an implementation alias/effect-analysis proof.
module IdlePlan {
  type State = {epoch: int, basis: int, idle: bool, checked: bool, unsafe: bool}
  var q: State
  action init = q' = {epoch:0,basis:-1,idle:true,checked:false,unsafe:false}
  action Check = q' = {...q,basis:q.epoch,checked:true}
  action Activate = q' = {...q,epoch:q.epoch + 1,idle:false,checked:false}
  action Deactivate = q' = {...q,epoch:q.epoch + 1,idle:true,checked:false}
  action Barrier = q' = {...q,epoch:q.epoch + 1,checked:false}
  action Fast = all {q.checked, q.basis == q.epoch, q.idle,
                    q' = {...q,checked:false}}
  action Slow = q' = {...q,checked:false}
  action step = any {Check, Activate, Deactivate, Barrier, Fast, Slow}
  val GuardSafety = not(q.unsafe) and (not(q.checked) or q.basis == q.epoch)
  run idleExecutionTest = init.then(Check).then(Fast).expect(GuardSafety and not(q.checked))
  run activationInvalidatesTest = init.then(Check).then(Activate)
    .expect(GuardSafety and not(q.checked) and not(q.idle))
  run reentryChecksCurrentEpochTest = init.then(Activate).then(Deactivate).then(Check).then(Fast)
    .expect(GuardSafety and q.epoch == 2 and not(q.checked))
  run barrierInvalidatesTest = init.then(Check).then(Barrier)
    .expect(GuardSafety and not(q.checked))
}
