// Bounded source-order traversal controls; Lean covers arbitrary trees.
module SourceSites {
  type State = {kind:str, targets:Set[int], pending:List[int], output:List[int]}
  var q:State
  pure def descendants(node:int):List[int] =
    if(node==0) List(1,4) else if(node==1) List(2)
    else if(node==2) List(3) else if(node==4) List(5,6) else List()
  pure def literal(kind:str):bool = Set("quote","quote-syntax","syntax").contains(kind)
  pure def expected(kind:str,targets:Set[int]):List[int] =
    List(0,1,2,3,4,5,6).foldl(List(), (out,node) =>
      if(targets.contains(node) and (not(literal(kind)) or not(Set(1,2,3).contains(node))))
        out.append(node) else out)
  action start(kind:str,targets:Set[int]):bool =
    q'={kind:kind,targets:targets,pending:List(0),output:List()}
  action init = {
    nondet kind=Set("ordinary","quote","quote-syntax","syntax").oneOf()
    nondet targets=Set(Set(3,5,6),Set(1),Set(0,1,2,3,4,5,6)).oneOf()
    start(kind,targets)
  }
  action visit = {
    val node=if(q.pending.length()>0) q.pending.head() else 0
    val rest=if(q.pending.length()>0) q.pending.tail() else List()
    val blocked=node==1 and literal(q.kind)
    all {q.pending.length()>0,
      q'={...q,pending:if(blocked) rest else descendants(node).concat(rest),
        output:if(not(blocked) and q.targets.contains(node)) q.output.append(node) else q.output}}
  }
  action step = any {visit,q'=q}
  val SiteSafety = all {
    q.output==expected(q.kind,q.targets).slice(0,q.output.length()),
    q.pending.length()==0 implies q.output==expected(q.kind,q.targets)
  }
  run quotedTest=start("quote",Set(3,5,6)).then(visit).then(visit).then(visit).then(visit).then(visit)
    .expect(SiteSafety and q.output==List(5,6) and q.pending.length()==0)
  run syntaxTest=start("quote-syntax",Set(3,5,6)).then(visit).then(visit).then(visit).then(visit).then(visit)
    .expect(SiteSafety and q.output==List(5,6))
  run syntaxAliasTest=start("syntax",Set(1,3,5,6)).then(visit).then(visit).then(visit).then(visit).then(visit)
    .expect(SiteSafety and q.output==List(5,6))
  run ordinaryTest=start("ordinary",Set(3,5,6)).then(visit).then(visit).then(visit).then(visit)
    .then(visit).then(visit).then(visit).expect(SiteSafety and q.output==List(3,5,6))
  run missingBarrierMutationTest=start("quote-syntax",Set(3,5,6)).then(visit)
    .then(q'={...q,pending:List(2,4)}).then(visit).then(visit).expect(not(SiteSafety))
  run reversedOrderMutationTest=start("ordinary",Set(3,5,6)).then(visit)
    .then(q'={...q,pending:List(4,1)}).then(visit).then(visit).expect(not(SiteSafety))
}
