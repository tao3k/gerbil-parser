// Lifecycle and exact sparse observation only; this is not an LR parser oracle.
module SparseCapture {
  type Frame = {shift:int,version:int,cursor:int,tokens:int}
  type State = {spacing:int,shift:int,version:int,cursor:int,tokens:int,
    pending:bool,pendingMode:int,mode:int,admitted:List[int],emitted:List[int],active:bool,output:bool,dense:List[Frame],sparse:List[Frame]}
  var q:State
  pure val first:Frame = {shift:0,version:0,cursor:0,tokens:0}
  pure val initial:State = {spacing:2,shift:0,version:0,cursor:0,tokens:0,
    pending:false,pendingMode:0,mode:0,admitted:List(),emitted:List(),active:true,output:false,dense:List(first),sparse:List(first)}
  action init = q' = initial
  action initEveryShift = q' = {...initial,spacing:1}
  val canAdmit = q.active and not(q.pending) and q.cursor < 5
  action Trivia = all {canAdmit,
    q' = {...q,cursor:q.cursor + 1,tokens:q.tokens + 1,admitted:q.admitted.append(q.mode),emitted:q.emitted.append(q.mode)}}
  action Scan = all {canAdmit,
    q' = {...q,cursor:q.cursor + 1,pending:true,pendingMode:q.mode,admitted:q.admitted.append(q.mode)}}
  action Reduce = all {q.active,q.pending,q.version < 10,
    q' = {...q,version:q.version + 1,mode:(q.mode + 1) % 3}}
  action Shift = all {q.active,q.pending,
    val frame = {shift:q.shift + 1,version:q.version + 1,cursor:q.cursor,tokens:q.tokens + 1}
    q' = {...q,shift:q.shift + 1,version:q.version + 1,tokens:q.tokens + 1,pending:false,emitted:q.emitted.append(q.pendingMode),
      dense:q.dense.append(frame),
      sparse:if ((q.shift + 1) % q.spacing == 0) q.sparse.append(frame) else q.sparse}}
  action Finish = all {q.active,not(q.pending),q' = {...q,active:false,output:true}}
  action Fork = all {q.active,q.pending,q' = {...q,active:false,output:false}}
  action Fail = all {q.active,q' = {...q,active:false,output:false}}
  action step = any {Trivia,Scan,Reduce,Shift,Finish,Fork,Fail}
  val SparseSafety = q.sparse == q.dense.select(frame => frame.shift % q.spacing == 0)
    and (q.output implies not(q.pending))
    and q.admitted.length() == q.cursor and q.emitted.length() == q.tokens
    and q.emitted == q.admitted.slice(0,q.tokens)
  // Counterfactual implementation: exporting an aliased buffer rewrites a
  // retained frame when a later reduction updates that private storage.
  action AliasedSnapshotWrite = all {q.active,q.pending,q.sparse.length() > 1,
    q' = {...q,sparse:q.sparse.foldl(List(), (frames,frame) => frames.append(if (frame.shift == 0) frame else {...frame,version:q.version}))}}
  run aliasedBufferMustBeRejectedTest = init.then(Scan).then(Shift).then(Scan).then(Shift)
    .then(Scan).then(Reduce).then(AliasedSnapshotWrite).expect(not(SparseSafety))
  run reducedModeDoesNotRewriteAdmissionTest = init.then(Scan).then(Reduce).then(Reduce).then(Shift)
    .expect(SparseSafety and q.mode == 2 and q.emitted == List(0))
  run triviaWaitsForPendingShiftTest = init.then(Scan)
    .expect(not(canAdmit) and SparseSafety)
  run forkRetainsUnconsumedModeTest = init.then(Scan).then(Reduce).then(Fork)
    .expect(SparseSafety and q.pending and q.pendingMode == 0 and q.mode == 1 and q.emitted == List())
  run sparseTwoShiftsTest = init.then(Scan).then(Shift).then(Scan).then(Shift).then(Finish)
    .expect(SparseSafety and q.output and q.sparse.length() == 2 and q.dense.length() == 3)
  run reductionBeforeShiftTest = init.then(Scan).then(Reduce).then(Shift).then(Scan)
    .then(Reduce).then(Shift).expect(SparseSafety and q.sparse == List(first,{shift:2,version:4,cursor:2,tokens:2}))
  run triviaDoesNotCaptureTest = init.then(Trivia).then(Trivia)
    .expect(SparseSafety and q.sparse.length() == 1 and q.shift == 0)
  run everyShiftTest = initEveryShift.then(Scan).then(Shift).then(Scan).then(Shift)
    .expect(SparseSafety and q.sparse == q.dense)
  run forkRetainsPrefixTest = init.then(Scan).then(Shift).then(Scan).then(Fork)
    .expect(SparseSafety and not(q.output) and q.sparse.length() == 1)
  run failureRetainsPrefixTest = init.then(Scan).then(Shift).then(Fail)
    .expect(SparseSafety and not(q.output))
  run wrongShiftBoundaryMutationTest = init.then(Scan).then(Shift)
    .expect(q.dense != q.sparse)
}
