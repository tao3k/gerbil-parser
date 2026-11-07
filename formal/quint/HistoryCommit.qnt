// Callback histories only; Lean owns value-preserving commit admission.
// No independent parser, OS-thread scheduler or GC semantics are modeled.
module HistoryCommit {
  type State = {generation:int, outer:int, inner:int, history:int, latest:int,
                live:bool, outerOpen:bool, innerOpen:bool}
  var q: State
  pure val initial:State = {generation:0,outer:0,inner:0,history:0,latest:0,
                           live:true,outerOpen:false,innerOpen:false}
  pure def commit(s:State,attempt:int,published:bool):State =
    if (s.live and published and s.generation == attempt)
      {...s,history:attempt,latest:attempt} else s
  pure def valid(s:State):bool = s.history == s.latest and s.latest <= s.generation
  action init = q' = initial
  action Outer = all {q.live, q.generation == 0,
    q' = {...q,generation:1,outer:1,outerOpen:true}}
  action Inner = all {q.live,q.outerOpen,q.generation == 1,
    q' = {...q,generation:2,inner:2,innerOpen:true}}
  action CommitInner = all {q.innerOpen,
    q' = {...commit(q,q.inner,true),innerOpen:false}}
  action FailInner = all {q.innerOpen,q' = {...q,innerOpen:false}}
  action CommitOuter = all {q.outerOpen,not(q.innerOpen),
    q' = {...commit(q,q.outer,true),outerOpen:false}}
  action FailOuter = all {q.outerOpen,not(q.innerOpen),q' = {...q,outerOpen:false}}
  action Release = q' = {...q,live:false}
  action step = any {Outer,Inner,CommitInner,FailInner,CommitOuter,FailOuter,Release}
  val HistorySafety = valid(q)
  pure val States = tuples(Set(0,1,2),Set(0,1,2),Set(0,1,2),Set(0,1,2),Set(0,1,2),
      Set(false,true),Set(false,true),Set(false,true))
    .map(t => {generation:t._1,outer:t._2,inner:t._3,history:t._4,latest:t._5,
      live:t._6,outerOpen:t._7,innerOpen:t._8})
  pure val Closed = States.forall(s => valid(s) implies
    Set(false,true).forall(ok =>
      (valid(commit(s,s.outer,ok)) and valid(commit(s,s.inner,ok)))))
  run finiteCommitPreservationTest = init.expect(Closed)
  run ordinaryCommitTest = init.then(Outer).then(CommitOuter).expect(HistorySafety and q.history == 1)
  run innerCommitSurvivesOuterTest = init.then(Outer).then(Inner).then(CommitInner).then(CommitOuter)
    .expect(HistorySafety and q.history == 2)
  run failedInnerSupersedesOuterTest = init.then(Outer).then(Inner).then(FailInner).then(CommitOuter)
    .expect(HistorySafety and q.history == 0)
  run failedOuterRetainsInnerTest = init.then(Outer).then(Inner).then(CommitInner).then(FailOuter)
    .expect(HistorySafety and q.history == 2)
  run releaseRejectsCommitTest = init.then(Outer).then(Release).then(CommitOuter)
    .expect(HistorySafety and q.history == 0)
  run missingGenerationMutationTest = init.then(Outer).then(Inner).then(CommitInner)
    .expect(not(valid({...q,history:q.outer})))
}
