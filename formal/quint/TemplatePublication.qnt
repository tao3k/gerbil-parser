// Bounded template and transition publication controls; Lean proves the
// arbitrary-list composition and the generated FIFO witness protocol.
module TemplatePublication {
  type Group={symbol:int,kernel:int}
  type Row={symbol:int,target:int}
  var groups:List[Group]
  var cursor:int
  var catalog:List[int]
  var work:List[int]
  var rows:List[Row]
  pure def lookup(catalog:List[int],key:int):int =
    catalog.indices().fold(-1,(found,id)=>if(catalog.nth(id)==key) id else found)
  action setup(input:List[Group]):bool = all {
    groups'=input,cursor'=0,catalog'=List(7),work'=List(0),rows'=List()}
  action init = {
    nondet input=Set(List(),List({symbol:0,kernel:7}),
      List({symbol:0,kernel:65},{symbol:1,kernel:7},{symbol:2,kernel:65}),
      List({symbol:0,kernel:1},{symbol:1,kernel:2},{symbol:2,kernel:3})).oneOf()
    setup(input)
  }
  action step = if(cursor==groups.length()) all {
    groups'=groups,cursor'=cursor,catalog'=catalog,work'=work,rows'=rows}
    else {val group=groups.nth(cursor);val existing=lookup(catalog,group.kernel);
      val target=if(existing<0) catalog.length() else existing; all {
        groups'=groups,cursor'=cursor+1,
        catalog'=if(existing<0) catalog.append(group.kernel) else catalog,
        work'=if(existing<0) work.append(target) else work,
        rows'=rows.append({symbol:group.symbol,target:target})}}
  val PublicationSafety = cursor==rows.length() and cursor<=groups.length() and
    catalog.indices().forall(i=>catalog.indices().forall(j=>i==j or catalog.nth(i)!=catalog.nth(j))) and
    rows.indices().forall(i=>rows.nth(i).symbol==groups.nth(i).symbol and
      lookup(catalog,groups.nth(i).kernel)==rows.nth(i).target) and
    work.length()==catalog.length() and work.indices().forall(i=>work.nth(i)==i)
  run zeroTargetRetainedTest = setup(List({symbol:0,kernel:7})).then(step)
    .expect(rows==List({symbol:0,target:0}) and work==List(0) and PublicationSafety)
  run sharedTargetStableTest = setup(List({symbol:0,kernel:65},{symbol:1,kernel:7},{symbol:2,kernel:65}))
    .then(step).then(step).then(step).expect(
      rows==List({symbol:0,target:1},{symbol:1,target:0},{symbol:2,target:1}) and work==List(0,1) and PublicationSafety)
  pure val widths=List(1,1,1,0,0)
  pure def offset(id:int):int = widths.slice(0,id).foldl(0,(total,width)=>total+width+1)
  pure def materialize(mask:Set[int]):List[int] = List(0,1,2,3,4).foldl(List(),(out,id)=>
    if(mask.contains(id)) out.append(offset(id)) else out)
  run duplicateRulesRetainIDsTest = init.expect(materialize(Set(3,4))==List(6,7))
  run emptyTemplateTest = init.expect(materialize(Set())==List())
  run completedSlotsExcludedTest = init.expect(materialize(Set(1,2,3,4))==List(2,4,6,7))
}
