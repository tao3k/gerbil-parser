// Lexical eligibility histories only. Lean owns unsigned-selector refinement.
// No runtime fact cache is implemented; epochs abstract the compiler's effects.
module ConsumerScope {
  type State = {a: int, b: int, basisA: int, basisB: int, knownA: bool, knownB: bool}
  var q: State
  action init = q' = {a:0,b:0,basisA:-1,basisB:-1,knownA:false,knownB:false}
  action SelectA = q' = {...q,basisA:q.a,knownA:true}
  action SelectB = q' = {...q,basisB:q.b,knownB:true}
  action WriteA = q' = {...q,a:q.a + 1,knownA:false}
  action WriteB = q' = {...q,b:q.b + 1,knownB:false}
  action Barrier = q' = {...q,a:q.a + 1,b:q.b + 1,knownA:false,knownB:false}
  action NextByte = q' = {...q,knownA:false,knownB:false}
  action step = any {SelectA,SelectB,WriteA,WriteB,Barrier,NextByte}
  val ScopeSafety = (not(q.knownA) or q.basisA == q.a) and
                    (not(q.knownB) or q.basisB == q.b)
  run independentWriteTest = init.then(SelectA).then(WriteB)
    .expect(ScopeSafety and q.knownA and not(q.knownB))
  run dependentWriteTest = init.then(SelectA).then(SelectB).then(WriteA)
    .expect(ScopeSafety and not(q.knownA) and q.knownB)
  run barrierDropsBothTest = init.then(SelectA).then(SelectB).then(Barrier)
    .expect(ScopeSafety and not(q.knownA) and not(q.knownB))
  run nextByteDropsBothTest = init.then(SelectA).then(SelectB).then(NextByte)
    .expect(ScopeSafety and not(q.knownA) and not(q.knownB))
  run staleReuseRejectedTest = init.then(SelectA).then(WriteA)
    .expect(not((q.basisA == q.a)))
}
