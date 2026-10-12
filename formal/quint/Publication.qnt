// Verification-only publication protocol; not a parser implementation.
module Publication {
  type State = {source: int, basis: int, phase: str, cancelled: bool, output: int}
  var q: State
  pure val initial: State = {source: 0, basis: 0, phase: "idle", cancelled: false, output: -1}
  pure def prepare(s: State): State = {...s, basis: s.source, phase: "running", cancelled: false, output: -1}
  pure def edit(s: State): State = {...s, source: s.source + 1, output: -1,
    phase: if (s.phase == "published") "idle" else s.phase}
  pure def cancel(s: State): State = {...s, cancelled: true, phase: "cancelled", output: -1}
  pure def finish(s: State): State =
    if (s.phase == "running" and not(s.cancelled) and s.basis == s.source)
    {...s, phase: "published", output: s.basis}
    else {...s, phase: "rejected", output: -1}
  pure def valid(s: State): bool = s.output == -1 or
    (s.phase == "published" and not(s.cancelled) and s.output == s.source and s.basis == s.source)
  action init = q' = initial
  action Prepare = all {q.phase != "running", q' = prepare(q)}
  action Edit = all {q.source < 2, q' = edit(q)}
  action Cancel = all {q.phase == "running", q' = cancel(q)}
  action Finish = all {q.phase == "running", q' = finish(q)}
  action step = any {Prepare, Edit, Cancel, Finish}
  val CurrentPublication = valid(q)

  // Exhaustive one-step preservation over the explicit finite domain.
  pure val States = tuples(Set(0,1,2), Set(0,1,2),
    Set("idle","running","published","cancelled","rejected"), Set(false,true), Set(-1,0,1,2))
    .map(t => {source: t._1, basis: t._2, phase: t._3, cancelled: t._4, output: t._5})
  pure val Closed = States.forall(s => valid(s) implies
    (valid(prepare(s)) and valid(cancel(s)) and valid(finish(s)) and
      (s.source < 2 implies valid(edit(s)))))
  run finiteClosureTest = init.expect(Closed)
  run successfulPublicationTest = init.then(Prepare).then(Finish).expect(CurrentPublication and q.output == 0)
  run staleRequestTest = init.then(Prepare).then(Edit).then(Finish).expect(q.phase == "rejected" and q.output == -1)
  run cancelledRequestTest = init.then(Prepare).then(Cancel).expect(q.output == -1 and q.cancelled)
  run editWithdrawsPublicationTest = init.then(Prepare).then(Finish).then(Edit).expect(q.output == -1)
  // Mutation witnesses establish that the admission checks are consequential.
  pure def uncheckedFinish(s: State): State = {...s, phase: "published", output: s.basis}
  run staleMutationTest = init.then(Prepare).then(Edit).expect(not(valid(uncheckedFinish(q))))
  run cancelledMutationTest = init.then(Prepare).then(Cancel).expect(not(valid(uncheckedFinish(q))))
}
