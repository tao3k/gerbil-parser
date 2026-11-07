// Scope safety only: Lean owns projection equality and complete-request costs.
// Key transitions abstract real admission/revision events, not pointer equality.
module LookaheadScope {
  type Key = {owner:int,source:int,contract:int,extent:int,revision:int}
  type State = {current:Key,basis:Key,active:bool,ready:bool,output:bool,
                value:int,cached:int,eof:bool,cachedEof:bool,pending:bool,
                observed:Key,observedValue:int,observedEof:bool}
  var q:State
  pure val key:Key = {owner:0,source:0,contract:0,extent:0,revision:0}
  pure val initial:State = {current:key,basis:key,active:false,ready:false,
    output:false,value:0,cached:0,eof:false,cachedEof:false,pending:false,
    observed:key,observedValue:0,observedEof:false}
  action init = q' = initial
  action Begin = all {not(q.active),q.current.owner == 0,
    q' = {...q,active:true,ready:false,pending:false,output:false}}
  action Bind = all {q.active,q' = {...q,basis:q.current,ready:true,
    cached:q.value,cachedEof:q.eof,output:false}}
  // Atomic Bind remains the specification. These actions expose a staged
  // snapshot's producer obligation instead of assuming construction atomic.
  action Observe = all {q.active,not(q.pending),
    q' = {...q,pending:true,ready:false,output:false,observed:q.current,
      observedValue:q.value,observedEof:q.eof}}
  action CommitObserved = all {q.active,q.pending,q.observed == q.current,
    q' = {...q,pending:false,ready:true,basis:q.observed,
      cached:q.observedValue,cachedEof:q.observedEof,output:false}}
  action DiscardObserved = all {q.pending,q' = {...q,pending:false,output:false}}
  action Read = all {q.active,q.ready,q.basis == q.current,
    q' = {...q,output:true}}
  // A reduction may change LR state without consuming this admitted token.
  // No parser acceptance, shiftability or scan purity is inferred here.
  action Reduce = all {q.active,q' = {...q,output:false}}
  action Shift = all {q.active,q.current.extent == 0,
    q' = {...q,current:{...q.current,extent:1},value:1,output:false}}
  action AdmitEOF = all {q.active,q.current.extent == 0,
    q' = {...q,current:{...q.current,extent:1},eof:true,output:false}}
  action Mutate = all {q.active,q.current.revision == 0,
    q' = {...q,current:{...q.current,revision:1},value:1 - q.value,output:false}}
  action Edit = all {q.current.source == 0,
    q' = {...q,current:{...q.current,source:1},active:false,output:false}}
  action Reconfigure = all {q.current.contract == 0,
    q' = {...q,current:{...q.current,contract:1},active:false,output:false}}
  action Supersede = all {q.active,q.current.owner == 0,
    q' = {...q,current:{...q.current,owner:1},ready:false,output:false}}
  action Cancel = all {q.active,q' = {...q,active:false,output:false}}
  action Pause = all {q.active,q' = {...q,active:false,ready:false,output:false}}
  action Fallback = all {q.active,q' = {...q,active:false,ready:false,output:false}}
  action Fail = all {q.active,q' = {...q,active:false,ready:false,output:false}}
  action Finish = all {q.active,q' = {...q,active:false,ready:false,output:false}}
  action step = any {Begin,Bind,Read,Reduce,Shift,AdmitEOF,Mutate,Edit,
    Reconfigure,Supersede,Cancel,Pause,Fallback,Fail,Finish,Observe,CommitObserved,DiscardObserved}
  pure def safe(s:State):bool = s.output implies (s.active and s.ready
    and s.basis == s.current and s.cached == s.value and s.cachedEof == s.eof)
  val ScopeSafety = safe(q) and (q.pending and q.observed == q.current implies
    (q.observedValue == q.value and q.observedEof == q.eof))
  run stagedSnapshotTest = init.then(Begin).then(Observe).then(CommitObserved).then(Read)
    .expect(ScopeSafety and q.output)
  run reductionDuringSnapshotTest = init.then(Begin).then(Observe).then(Reduce)
    .then(CommitObserved).then(Read).expect(ScopeSafety and q.output)
  run mutationDuringSnapshotTest = init.then(Begin).then(Observe).then(Mutate)
    .expect(ScopeSafety and q.observed != q.current and not(q.ready))
  run uncheckedCommitMutationWitnessTest = init.then(Begin).then(Observe).then(Mutate)
    .expect(not(safe({...q,pending:false,ready:true,output:true,basis:q.current,
      cached:q.observedValue,cachedEof:q.observedEof})))
  run snapshotRetryTest = init.then(Begin).then(Observe).then(Mutate).then(DiscardObserved)
    .then(Observe).then(CommitObserved).then(Read).expect(ScopeSafety and q.output and q.cached == 1)
  run eofDuringSnapshotTest = init.then(Begin).then(Observe).then(AdmitEOF)
    .expect(ScopeSafety and q.observed != q.current and not(q.ready))
  run supersededSnapshotTest = init.then(Begin).then(Observe).then(Supersede)
    .expect(ScopeSafety and q.observed.owner != q.current.owner and not(q.ready))
  run cancelledSnapshotTest = init.then(Begin).then(Observe).then(Cancel)
    .expect(ScopeSafety and not(q.active) and not(q.output))
  run repeatedReductionTest = init.then(Begin).then(Bind).then(Read)
    .then(Reduce).then(Read).expect(ScopeSafety and q.output)
  run shiftedExtentTest = init.then(Begin).then(Bind).then(Shift)
    .expect(ScopeSafety and q.basis != q.current and not(q.output))
  run rebindAfterShiftTest = init.then(Begin).then(Bind).then(Shift).then(Bind).then(Read)
    .expect(ScopeSafety and q.output and q.cached == 1)
  run eofAdmissionTest = init.then(Begin).then(Bind).then(AdmitEOF).then(Bind).then(Read)
    .expect(ScopeSafety and q.output and q.cachedEof)
  run mutationRequiresRebindTest = init.then(Begin).then(Bind).then(Mutate)
    .expect(ScopeSafety and q.basis != q.current and q.cached != q.value)
  run pointerOnlyMutationWitnessTest = init.then(Begin).then(Bind).then(Mutate)
    .expect(not(safe({...q,output:true})))
  run eofSentinelMutationWitnessTest = init.then(Begin).then(Bind).then(AdmitEOF)
    .expect(not(safe({...q,output:true})))
  run supersessionTest = init.then(Begin).then(Bind).then(Read).then(Supersede)
    .expect(ScopeSafety and not(q.ready) and not(q.output))
  run cancelledScopeTest = init.then(Begin).then(Bind).then(Read).then(Cancel)
    .expect(ScopeSafety and not(q.active) and not(q.output))
  run changedSourceTest = init.then(Begin).then(Bind).then(Read).then(Edit)
    .expect(ScopeSafety and q.basis.source != q.current.source and not(q.output))
  run changedContractTest = init.then(Begin).then(Bind).then(Read).then(Reconfigure)
    .expect(ScopeSafety and q.basis.contract != q.current.contract and not(q.output))
  run pauseRequiresFreshAdmissionTest = init.then(Begin).then(Bind).then(Read).then(Pause).then(Begin)
    .expect(ScopeSafety and q.active and not(q.ready) and not(q.output))
  run fallbackWithdrawsScopeTest = init.then(Begin).then(Bind).then(Read).then(Fallback)
    .expect(ScopeSafety and not(q.active) and not(q.ready) and not(q.output))
  run failedScopeTest = init.then(Begin).then(Bind).then(Read).then(Fail)
    .expect(ScopeSafety and not(q.ready) and not(q.output))
  run finishedScopeTest = init.then(Begin).then(Bind).then(Read).then(Finish)
    .expect(ScopeSafety and not(q.ready) and not(q.output))
}
