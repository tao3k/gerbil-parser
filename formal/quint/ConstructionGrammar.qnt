// Admission protocol only. Lean checks arbitrary production-origin recipes;
// this model does not replace those proofs with a finite language oracle.
module ConstructionGrammar {
  type Symbol={nonterminal:bool,key:int}
  type Row={id:int,lhs:int,rhs:List[Symbol]}
  type State={rows:List[Row],declarations:Set[int],recipes:int,phase:str,projected:List[Row]}
  var q:State
  pure val root:Symbol={nonterminal:true,key:10}
  pure val token:Symbol={nonterminal:false,key:1}
  pure val fixture:List[Row]=List({id:0,lhs:99,rhs:List(root)},
    {id:1,lhs:10,rhs:List(token,root)},{id:2,lhs:10,rhs:List()})
  pure def valid(rows:List[Row],declarations:Set[int],recipes:int):bool=if(rows.length()==0) false else all {
    rows.indices().forall(i=>rows.nth(i).id==i),
    rows.nth(0).lhs==99,rows.nth(0).rhs==List(root),
    rows.indices().forall(i=>declarations.contains(rows.nth(i).lhs)),
    rows.indices().forall(i=>i==0 or rows.nth(i).lhs!=99),
    rows.indices().forall(i=>rows.nth(i).rhs.indices().forall(j=>{
      val symbol=rows.nth(i).rhs.nth(j)
      if(symbol.nonterminal) declarations.contains(symbol.key) and symbol.key!=99
      else symbol.key>0})),
    recipes==rows.length()-1}
  // Erasure retains the exact physical positions, even for equal CFG rows.
  pure def project(rows:List[Row]):List[Row]=rows.foldl(List(),(out,row)=>out.append({...row,id:out.length()}))
  action init=q'={rows:fixture,declarations:Set(10,99),recipes:2,phase:"input",projected:List()}
  action admit=all {q.phase=="input",q'={...q,phase:if(valid(q.rows,q.declarations,q.recipes)) "ready" else "rejected",
    projected:if(valid(q.rows,q.declarations,q.recipes)) project(q.rows) else List()}}
  action damage=all {q.phase=="input",nondet kind=0.to(6).oneOf()
    q'=if(kind==0) {...q,rows:List()}
      else if(kind==1) {...q,rows:List(q.rows.nth(0),{...fixture.nth(1),id:2},fixture.nth(2))}
      else if(kind==2) {...q,rows:List(fixture.nth(0),{...fixture.nth(1),rhs:List({nonterminal:true,key:77})},fixture.nth(2))}
      else if(kind==3) {...q,rows:List(fixture.nth(0),{...fixture.nth(1),rhs:List({nonterminal:false,key:0})},fixture.nth(2))}
      else if(kind==4) {...q,recipes:1}
      else if(kind==5) {...q,rows:List({...fixture.nth(0),rhs:List({nonterminal:true,key:99})},fixture.nth(1),fixture.nth(2))}
      else {...q,rows:List(fixture.nth(0),{...fixture.nth(1),lhs:99},fixture.nth(2))}}
  action step=any {admit,all {q.phase=="input",q.rows.length()>0,damage},q'=q}
  val InputSafety=all {
    q.phase=="ready" implies (valid(q.rows,q.declarations,q.recipes) and q.projected==q.rows),
    q.phase=="rejected" implies (not(valid(q.rows,q.declarations,q.recipes)) and q.projected==List()),
    q.phase=="input" implies q.projected==List()}
  run admittedInputTest=init.then(admit).expect(q.phase=="ready" and InputSafety)
  run emptyRejectedTest=init.then(q'={...q,rows:List()}).then(admit).expect(q.phase=="rejected" and InputSafety)
  run physicalIdRejectedTest=init.then(q'={...q,rows:List(fixture.nth(0),{...fixture.nth(1),id:2},fixture.nth(2))})
    .then(admit).expect(q.phase=="rejected" and InputSafety)
  run undeclaredReferenceRejectedTest=init.then(q'={...q,rows:List(fixture.nth(0),{...fixture.nth(1),rhs:List({nonterminal:true,key:77})},fixture.nth(2))})
    .then(admit).expect(q.phase=="rejected" and InputSafety)
  run failedTerminalDecodeRejectedTest=init.then(q'={...q,rows:List(fixture.nth(0),{...fixture.nth(1),rhs:List({nonterminal:false,key:0})},fixture.nth(2))})
    .then(admit).expect(q.phase=="rejected" and InputSafety)
  run incompleteRecipesRejectedTest=init.then(q'={...q,recipes:1}).then(admit).expect(q.phase=="rejected" and InputSafety)
  run augmentedOwnerRejectedTest=init.then(q'={...q,rows:List(fixture.nth(0),{...fixture.nth(1),lhs:99},fixture.nth(2))})
    .then(admit).expect(q.phase=="rejected" and InputSafety)
  run rootCannotResolveThroughAugmentationTest=init.then(q'={...q,rows:List({...fixture.nth(0),rhs:List({nonterminal:true,key:99})},fixture.nth(1),fixture.nth(2))})
    .then(admit).expect(q.phase=="rejected" and InputSafety)
  run duplicateRowsRetainPositionsTest=init.then(q'={...q,rows:List(fixture.nth(0),{...fixture.nth(2),id:1},fixture.nth(2))})
    .then(admit).expect(q.phase=="ready" and q.projected.length()==3 and
      q.projected.nth(1).rhs==q.projected.nth(2).rhs and InputSafety)
  run zeroRowDeclarationAdmittedTest=init.then(q'={...q,declarations:Set(10,77,99),
    rows:List(fixture.nth(0),{...fixture.nth(1),rhs:List({nonterminal:true,key:77})},fixture.nth(2))})
    .then(admit).expect(q.phase=="ready" and InputSafety and
      not(q.rows.indices().exists(i=>q.rows.nth(i).lhs==77)))
  run reservedReferenceIsNotDeclarationTest=init.then(q'={...q,
    rows:List(fixture.nth(0),{...fixture.nth(1),rhs:List({nonterminal:true,key:77})},fixture.nth(2))})
    .then(admit).expect(q.phase=="rejected" and not(q.declarations.contains(77)) and InputSafety)

  run undeclaredLhsRejectedTest=init.then(q'={...q,
    rows:List(fixture.nth(0),{...fixture.nth(1),lhs:77},fixture.nth(2))})
    .then(admit).expect(q.phase=="rejected" and InputSafety)

  run augmentationReferenceRejectedTest=init.then(q'={...q,
    rows:List(fixture.nth(0),{...fixture.nth(1),rhs:List({nonterminal:true,key:99})},fixture.nth(2))})
    .then(admit).expect(q.phase=="rejected" and InputSafety)

}
