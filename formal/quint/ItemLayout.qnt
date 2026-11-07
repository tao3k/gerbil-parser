// Bounded scheduling/mutation controls. Universal widths are proved in Lean.
module ItemLayout {
  type State = {widths:List[int],offsets:List[int],owners:List[int],
    id:int,offset:int,cursor:int,phase:str}
  var q:State
  pure def initial(widths:List[int]):State = {widths:widths,offsets:List(),owners:List(),
    id:0,offset:0,cursor:0,phase:"index"}
  action init = {
    nondet widths = Set(List(),List(0),List(1,0,3),List(3,0,1)).oneOf()
    q' = initial(widths)
  }
  action initEmpty = q' = initial(List())
  action initEpsilon = q' = initial(List(0))
  pure def prefix(widths:List[int],count:int):int =
    widths.slice(0,count).foldl(0,(total,width) => total + width + 1)
  action IndexWith(padding:int):bool = all {q.phase == "index",
    q' = if(q.id == q.widths.length())
      {...q,offsets:q.offsets.append(q.offset),id:0,cursor:0,phase:"fill"}
    else {...q,offsets:q.offsets.append(q.offset),id:q.id + 1,
      offset:q.offset + padding + q.widths.nth(q.id)}}
  action Index = IndexWith(1)
  action FillWith(owner:int):bool = all {q.phase == "fill",q.id < q.widths.length(),
    q.cursor < q.offsets.nth(q.id + 1),
    q' = {...q,owners:q.owners.append(owner),cursor:q.cursor + 1}}
  action Fill = FillWith(q.id)
  action Next = all {q.phase == "fill",q.id < q.widths.length(),
    q.cursor == q.offsets.nth(q.id + 1),q' = {...q,id:q.id + 1}}
  action Publish = all {q.phase == "fill",q.id == q.widths.length(),q' = {...q,phase:"done"}}
  action Finished = all {q.phase == "done",q' = q}
  action step = any {Index,Fill,Next,Publish,Finished}
  val OffsetsCorrect = 0.to(q.offsets.length() - 1).forall(i => q.offsets.nth(i) == prefix(q.widths,i))
  val OwnersCorrect = 0.to(q.owners.length() - 1).forall(i => {
    val owner = q.owners.nth(i)
    if(owner < 0 or owner >= q.widths.length()) false else
      prefix(q.widths,owner) <= i and i < prefix(q.widths,owner + 1)
  })
  val Complete = q.phase != "done" or
    (q.offsets.length() == q.widths.length() + 1 and
     q.owners.length() == prefix(q.widths,q.widths.length()))
  val LayoutSafety = OffsetsCorrect and OwnersCorrect and Complete
  run emptyTableTest = initEmpty.then(Index).then(Publish).expect(LayoutSafety and q.phase == "done")
  run epsilonCompletedDotTest = initEpsilon.then(Index).then(Index).then(Fill).then(Next).then(Publish)
    .expect(LayoutSafety and q.owners == List(0))
  run missingCompletedDotTest = initEpsilon.then(IndexWith(0)).then(IndexWith(0)).expect(not(OffsetsCorrect))
  run wrongOwnerTest = initEpsilon.then(Index).then(Index).then(FillWith(1)).expect(not(OwnersCorrect))
}
