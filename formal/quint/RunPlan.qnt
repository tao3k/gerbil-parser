// Lifecycle only; Lean owns overwrite/residual semantic refinement.
// Source immutability, compiler eligibility and FFI checks need AOT controls.
module RunPlan {
  type State = {epoch: int, basis: int, phase: str, stop: int,
                cursor: int, writes: int, residuals: int, activeA: bool, activeB: bool, membership: str}
  var q: State
  pure val initial: State = {epoch:0,basis:-1,phase:"idle",stop:0,cursor:0,writes:0,residuals:0,
                             activeA:false,activeB:false,membership:"unselected"}
  pure def valid(s: State): bool =
    (not(Set("borrowed", "scanned").contains(s.phase)) or (s.basis == s.epoch and
      s.membership == (if (s.activeA or s.activeB) "wide" else "narrow"))) and
    s.writes <= s.residuals + 1 and s.residuals <= s.writes
  action init = q' = initial
  action initActive = q' = {...initial,activeA:true,activeB:true}
  action Borrow = all {q.phase == "idle", q' = {...q,basis:q.epoch,phase:"borrowed",
    membership:if (q.activeA or q.activeB) "wide" else "narrow"}}
  // The synchronous leaf releases every pointer before publishing summaries.
  // No state write, callback or source mutation can interleave with its borrow.
  action Scan = all {q.phase == "borrowed", q.basis == q.epoch,
    q' = {...q,phase:"scanned",stop:q.cursor + 3}}
  action Invalidate = all {q.phase != "borrowed",q.phase != "residual",
    q' = {...q,epoch:q.epoch + 1,phase:"idle"}}
  action Commit = all {q.phase == "scanned",q.basis == q.epoch,
    q' = {...q,phase:"residual",cursor:q.stop,writes:q.writes + 1}}
  action Residual = all {q.phase == "residual",
    q' = {...q,phase:"idle",residuals:q.residuals + 1}}
  action step = any {Borrow, Scan, Commit, Residual,
    all {q.phase != "residual",Invalidate}}
  val RunSafety = valid(q)
  run activeConsumersCanCommitTest = initActive.then(Borrow).then(Scan).then(Commit).then(Residual)
    .expect(RunSafety and q.activeA and q.activeB and q.membership == "wide" and q.writes == 1 and q.residuals == 1)
  run orderedRunTest = init.then(Borrow).then(Scan).then(Commit).then(Residual)
    .expect(RunSafety and q.membership == "narrow" and q.cursor == 3 and q.writes == 1 and q.residuals == 1)
  run mutationInvalidatesTest = init.then(Borrow).then(Scan).then(Invalidate)
    .expect(RunSafety and q.phase == "idle")
  run residualCannotBeSkippedTest = init.then(Borrow).then(Scan).then(Commit)
    .expect(RunSafety and q.phase == "residual" and q.residuals == 0)
  run staleCommitMutationTest = init.then(Borrow).then(Scan).then(Invalidate)
    .expect(not(valid({...q,phase:"scanned"})))
  run borrowHasNoPublishedEffectsTest = init.then(Borrow)
    .expect(RunSafety and q.phase == "borrowed" and q.writes == 0 and q.residuals == 0)
}
