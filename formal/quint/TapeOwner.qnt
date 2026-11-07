// Lifecycle only: Lean owns exact-prefix semantics; AOT owns real bytes/GC.
module TapeOwner {
  type State = {sealed: bool, writer: bool, output: bool}
  var q: State
  pure val initial: State = {sealed:false, writer:true, output:false}
  pure def publish(s: State): State =
    if (not(s.sealed) and s.writer)
      {sealed:true, writer:false, output:true} else s
  pure def write(s: State): State = s
  pure def safe(s: State): bool =
    (s.output implies (s.sealed and not(s.writer))) and
    (s.sealed implies not(s.writer))
  action init = q' = initial
  action Publish = q' = publish(q)
  // Payload is outside this lifecycle model. A write is admitted only while
  // the exclusive owner is open; rejected writes cannot reacquire ownership.
  action Write = all {not(q.sealed), q.writer, q' = write(q)}
  action RejectedWrite = all {q.sealed, q' = q}
  action Collect = q' = q
  action step = any {Publish, Write, RejectedWrite, Collect}
  val OwnerSafety = safe(q)
  pure val States = tuples(Set(false,true), Set(false,true), Set(false,true))
    .map(t => {sealed:t._1, writer:t._2, output:t._3})
  pure val Closed = States.forall(s => safe(s) implies
    (safe(publish(s)) and safe(write(s))))
  run ownerFiniteClosureTest = init.expect(Closed)
  run transferLifecycleTest = init.then(Write).then(Publish).then(Collect)
    .expect(OwnerSafety and q.output and not(q.writer))
  run sealedReentryTest = init.then(Publish).then(RejectedWrite).then(Publish)
    .expect(OwnerSafety and q == {sealed:true, writer:false, output:true})
  run aliasMutationWitnessTest = init.then(Publish)
    .expect(not(safe({...q,writer:true})))
}
