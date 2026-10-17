// Source identity is about the currently installed SDK context, not history.
// SDK runtime failure caching is owned by Gerbil; no loader is reimplemented.
module ModuleAdmission {
  type State = {source:int,admitted:int,context:int,pending:bool,failed:bool,output:bool}
  var q:State
  pure val initial:State = {source:0,admitted:-1,context:-1,pending:false,failed:false,output:false}
  pure def needsReload(s:State):bool = s.admitted != s.source
  pure def safe(s:State):bool =
    (s.admitted != -1 implies (s.admitted == s.context and not(s.failed) and not(s.pending)))
    and (s.output implies (s.source == s.admitted and not(s.failed) and not(s.pending)))
  action init = q' = initial
  action BeginReload = all {not(q.pending),
    q' = {...q,admitted:-1,context:q.source,pending:true,failed:false,output:false}}
  action Accept = all {q.pending,q.context == q.source,
    q' = {...q,admitted:q.source,pending:false,output:false}}
  action Fail = all {q.pending,q' = {...q,pending:false,failed:true,output:false}}
  action RejectSelection = all {q.pending,q' = {...q,pending:false,output:false}}
  action Edit = all {not(q.pending),q.source == 0,q' = {...q,source:1,output:false}}
  action Restore = all {not(q.pending),q.source == 1,q' = {...q,source:0,output:false}}
  action Read = all {not(q.pending),not(q.failed),not(needsReload(q)),q' = {...q,output:true}}
  action step = any {BeginReload,Accept,Fail,RejectSelection,Edit,Restore,Read}
  val ModuleSafety = safe(q)
  run unchangedReuseTest = init.then(BeginReload).then(Accept).then(Read)
    .expect(ModuleSafety and q.output and not(needsReload(q)))
  run changedSourceTest = init.then(BeginReload).then(Accept).then(Edit)
    .expect(ModuleSafety and needsReload(q) and not(q.output))
  run failedReplacementRestoreTest = init.then(BeginReload).then(Accept).then(Edit)
    .then(BeginReload).then(Fail).then(Restore)
    .expect(ModuleSafety and needsReload(q) and not(q.output))
  run restoredSourceRecoveryTest = init.then(BeginReload).then(Accept).then(Edit)
    .then(BeginReload).then(Fail).then(Restore).then(BeginReload).then(Accept).then(Read)
    .expect(ModuleSafety and q.output and q.context == 0)
  run explicitReloadFailureTest = init.then(BeginReload).then(Accept).then(BeginReload).then(Fail)
    .expect(ModuleSafety and needsReload(q) and not(q.output))
  run rejectedSelectionTest = init.then(BeginReload).then(RejectSelection)
    .expect(ModuleSafety and needsReload(q) and not(q.failed))
  run repeatedFailureTest = init.then(BeginReload).then(Fail).then(BeginReload).then(Fail)
    .expect(ModuleSafety and needsReload(q) and not(q.output))
  run historicalMarkerMutationTest = init.then(BeginReload).then(Accept).then(Edit)
    .then(BeginReload).then(Fail).then(Restore)
    .expect(not(safe({...q,admitted:0})) and not(needsReload({...q,admitted:0})))
}
