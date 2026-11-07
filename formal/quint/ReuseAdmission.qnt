// Lifecycle safety only. Lean owns unbounded identity admission and cost laws.
// Integers abstract collision-free identities, not actual digests or parser data.
module ReuseAdmission {
  type Key = {source:int, grammar:int, plan:int}
  type State = {current:Key, basis:Key, ticket:int, owner:int,
                phase:str, cancelled:bool, failed:bool, output:bool}
  var q: State
  pure val zero: Key = {source:0,grammar:0,plan:0}
  pure val initial: State = {current:zero,basis:zero,ticket:0,owner:0,
                            phase:"idle",cancelled:false,failed:false,output:false}
  pure def valid(s:State):bool = s.output implies
    (s.phase == "published" and s.current == s.basis and s.ticket == s.owner
      and not(s.cancelled) and not(s.failed))
  pure def begin(s:State):State = {...s,basis:s.current,owner:s.ticket,
    phase:"running",cancelled:false,failed:false,output:false}
  pure def finish(s:State):State =
    if (s.phase == "running" and s.current == s.basis and s.ticket == s.owner
        and not(s.cancelled) and not(s.failed))
      {...s,phase:"published",output:true}
    else {...s,phase:"rejected",output:false}
  pure def invalidate(s:State,k:Key):State = {...s,current:k,output:false}
  pure def cancel(s:State):State = {...s,cancelled:true,output:false}
  pure def failure(s:State):State = {...s,failed:true,output:false}
  pure def supersede(s:State):State = {...s,ticket:s.ticket + 1,output:false}
  action init = q' = initial
  action Begin = all {q.phase != "running", q' = begin(q)}
  action Finish = q' = finish(q)
  action Edit = all {q.current.source < 1,
    q' = invalidate(q,{...q.current,source:q.current.source + 1})}
  action Grammar = all {q.current.grammar < 1,
    q' = invalidate(q,{...q.current,grammar:q.current.grammar + 1})}
  action Plan = all {q.current.plan < 1,
    q' = invalidate(q,{...q.current,plan:q.current.plan + 1})}
  action Cancel = q' = cancel(q)
  action Fail = q' = failure(q)
  action Supersede = all {q.ticket < 1, q' = supersede(q)}
  action step = any {Begin,Finish,Edit,Grammar,Plan,Cancel,Fail,Supersede}
  val ReuseSafety = valid(q)

  pure val Keys = tuples(Set(0,1),Set(0,1),Set(0,1))
    .map(t => {source:t._1,grammar:t._2,plan:t._3})
  pure val States = tuples(Keys,Keys,Set(0,1),Set(0,1),
    Set("idle","running","published","rejected"),Set(false,true),Set(false,true),Set(false,true))
    .map(t => {current:t._1,basis:t._2,ticket:t._3,owner:t._4,
      phase:t._5,cancelled:t._6,failed:t._7,output:t._8})
  // Exhaustive one-step invariant preservation, not a temporal liveness proof.
  pure val Closed = States.forall(s => valid(s) implies
    (valid(begin(s)) and valid(finish(s)) and valid(cancel(s)) and valid(failure(s))
      and (s.ticket < 1 implies valid(supersede(s))) and Keys.forall(k => valid(invalidate(s,k)))))
  run finiteAdmissionClosureTest = init.expect(Closed)
  run currentPublicationTest = init.then(Begin).then(Finish).expect(ReuseSafety and q.output)
  run sourceInvalidationTest = init.then(Begin).then(Edit).then(Finish).expect(ReuseSafety and not(q.output))
  run grammarInvalidationTest = init.then(Begin).then(Grammar).then(Finish).expect(ReuseSafety and not(q.output))
  run planInvalidationTest = init.then(Begin).then(Plan).then(Finish).expect(ReuseSafety and not(q.output))
  run cancelledCompletionTest = init.then(Begin).then(Cancel).then(Finish).expect(ReuseSafety and not(q.output))
  run failedCompletionTest = init.then(Begin).then(Fail).then(Finish).expect(ReuseSafety and not(q.output))
  run olderOwnerCompletionTest = init.then(Begin).then(Supersede).then(Finish).expect(ReuseSafety and not(q.output))
  run recoveryTest = init.then(Begin).then(Fail).then(Finish).then(Begin).then(Finish).expect(ReuseSafety and q.output)
  pure def unchecked(s:State):State = {...s,phase:"published",output:true}
  run staleMutationTest = init.then(Begin).then(Edit).expect(not(valid(unchecked(q))))
  run grammarMutationTest = init.then(Begin).then(Grammar).expect(not(valid(unchecked(q))))
  run ownerMutationTest = init.then(Begin).then(Supersede).expect(not(valid(unchecked(q))))
  run planMutationTest = init.then(Begin).then(Plan).expect(not(valid(unchecked(q))))
  run cancellationMutationTest = init.then(Begin).then(Cancel).expect(not(valid(unchecked(q))))
  run editWithdrawsOutputTest = init.then(Begin).then(Finish).then(Edit).expect(ReuseSafety and not(q.output))
  run failureMutationTest = init.then(Begin).then(Fail).expect(not(valid(unchecked(q))))
}
