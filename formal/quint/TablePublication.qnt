// Scheduling controls complement the unbounded Lean construction proof.
module TablePublication {
  type State = {states:int,rows:List[int],count:int,phase:str,validated:bool,published:bool}
  var q:State
  pure def initial(states:int):State = {states:states,rows:List(),count:0,
    phase:"actions",validated:false,published:false}
  action init = {nondet count = Set(1,2,7).oneOf(); q' = initial(count)}
  action initOne = q' = initial(1)
  action Row = all {q.phase == "actions",q.rows.length() < q.states,
    q' = {...q,rows:q.rows.append(q.rows.length()),count:q.count + 1}}
  action Finish = all {q.phase == "actions",q.rows.length() == q.states,
    q' = {...q,phase:if(q.count == q.states) "gotos" else "blocked"}}
  action Gotos = all {q.phase == "gotos",q' = {...q,phase:"validation"}}
  action Validate = all {q.phase == "validation",q' = {...q,validated:true,phase:"publish"}}
  action Reject = all {q.phase == "validation",q' = {...q,phase:"blocked"}}
  action Publish = all {q.phase == "publish",q.validated,q' = {...q,published:true,phase:"done"}}
  action Done = all {Set("done","blocked").contains(q.phase),q' = q}
  action step = any {Row,Finish,Gotos,Validate,Reject,Publish,Done}
  val CountExact = q.count == q.rows.length()
  val RowsExact = 0.to(q.rows.length() - 1).forall(index => q.rows.nth(index) == index)
  val Published = not(q.published) or (q.validated and q.count == q.states and q.rows.length() == q.states)
  val PublicationSafety = CountExact and RowsExact and Published
  action CorruptCount = q' = {...q,count:q.count + 1}
  action PublishEarly = q' = {...q,published:true}
  run completePublicationTest = initOne.then(Row).then(Finish).then(Gotos).then(Validate).then(Publish)
    .expect(PublicationSafety and q.published)
  run rejectedValidationTest = initOne.then(Row).then(Finish).then(Gotos).then(Reject)
    .expect(PublicationSafety and not(q.published))
  run countMismatchTest = initOne.then(Row).then(CorruptCount).then(Finish)
    .expect(q.phase == "blocked" and not(q.published))
  run earlyPublicationTest = initOne.then(Row).then(Finish).then(Gotos).then(PublishEarly)
    .expect(not(Published))
}
