// One bounded lifecycle: dequeue, ordered sparse grouping, cleanup, publication.
// Lean quantifies over arbitrary grammars and proves termination without fuel.
module ConstructionLifecycle {
  type Group={first:int,symbol:int,kernel:Set[int]}
  type Edge={symbol:int,target:int}
  var planSlots:List[int]
  var sharedTables:List[List[Group]]
  var objectMasks:List[Set[int]]
  var planRefs:List[int]
  var templates:Set[Set[int]]
  var materializations:int
  var phase:str
  var catalog:List[Set[int]]
  var work:List[int]
  var processed:int
  var owner:int
  var groups:List[Group]
  var masks:List[Set[int]]
  var firsts:List[int]
  var published:List[List[Edge]]
  pure def symbol(core:int):int = if(core==3) -1 else if(core==1) 1 else 0
  pure def predicted(kernel:Set[int]):Set[int] = if(kernel.contains(0)) Set(1,2) else Set()
  pure def closure(kernel:Set[int]):Set[int] = kernel.union(predicted(kernel))
  pure def earliest(items:Set[int],key:int):int = List(0,1,2,3).foldl(999,(found,core)=>
    if(items.contains(core) and symbol(core)==key and core<found) core else found)
  pure def advanced(items:Set[int],key:int):Set[int] = items.fold(Set(),(out,core)=>
    if(symbol(core)==key) out.union(Set(core+1)) else out)
  pure def row(items:Set[int],key:int):Group =
    {first:earliest(items,key),symbol:key,kernel:advanced(items,key)}
  pure def sparse(items:Set[int]):List[Group] = {
    val left=row(items,0);val right=row(items,1)
    if(left.first==999) (if(right.first==999) List() else List(right))
    else if(right.first==999) List(left)
    else if(left.first<right.first) List(left,right) else List(right,left)
  }
  pure def buildTable(items:Set[int]):List[Group] = List(0,1,2,3).foldl(
    List({first:999,symbol:0,kernel:Set()},{first:999,symbol:1,kernel:Set()}),(table,core)=>
      if(items.contains(core) and symbol(core)>=0) {
        val key=symbol(core);val prior=table.nth(key)
        table.replaceAt(key,{first:if(prior.kernel==Set()) core else prior.first,
          symbol:key,kernel:prior.kernel.union(Set(core+1))})
      } else table)
  pure def indexedRow(kernel:Set[int],table:List[Group],key:int):Group = {
    val shared=table.nth(key);val own=earliest(kernel,key)
    {first:if(own<shared.first) own else shared.first,symbol:key,kernel:advanced(kernel,key).union(shared.kernel)}
  }
  pure def indexed(kernel:Set[int],table:List[Group]):List[Group] = {
    val left=indexedRow(kernel,table,0);val right=indexedRow(kernel,table,1)
    if(left.first==999) (if(right.first==999) List() else List(right))
    else if(right.first==999) List(left)
    else if(left.first<right.first) List(left,right) else List(right,left)
  }
  pure def appendSlot(slots:List[int],id:int,reference:int):List[int] = {
    val fresh=List(0,1,2,3,4,5,6,7).slice(0,2*slots.length()).foldl(List(),(out,_)=>out.append(-1))
    val prepared=if(id==slots.length()) slots.indices().fold(fresh,(out,index)=>out.replaceAt(index,slots.nth(index))) else slots
    prepared.replaceAt(id,reference)
  }
  pure def lookup(values:List[Set[int]],key:Set[int]):int =
    values.indices().fold(-1,(found,id)=>if(values.nth(id)==key) id else found)
  pure def install(values:List[Set[int]],rows:List[Group]):List[Set[int]] =
    rows.foldl(values,(out,group)=>if(lookup(out,group.kernel)<0) out.append(group.kernel) else out)
  pure def requested(values:List[Set[int]]):Set[Set[int]] =
    values.foldl(Set(),(out,kernel)=>out.union(Set(predicted(kernel))))
  pure def cached(kernel:Set[int],available:Set[Set[int]]):Set[int] =
    if(available.contains(predicted(kernel))) predicted(kernel) else Set()
  action setup(stale:int):bool = all {
    planSlots'=List(0,-1),sharedTables'=List(buildTable(predicted(Set(0)))),
    objectMasks'=List(predicted(Set(0))),planRefs'=List(0),
    templates'=Set(predicted(Set(0))),materializations'=1,
    phase'="ready",catalog'=List(Set(0)),work'=List(0),processed'=0,owner'=-1,
    groups'=List(),masks'=List(Set(),Set()),firsts'=List(stale,stale),published'=List()}
  action init = {nondet stale=Set(0,7,999).oneOf();setup(stale)}
  action step = if(phase=="ready" and work.length()>0) all {
    planSlots'=planSlots,sharedTables'=sharedTables,objectMasks'=objectMasks,planRefs'=planRefs,templates'=templates,materializations'=materializations,
    phase'="group",catalog'=catalog,work'=work.slice(1,work.length()),processed'=processed+1,
    owner'=work.nth(0),groups'=groups,masks'=masks,firsts'=firsts,published'=published}
    else if(phase=="group") {val kernel=catalog.nth(owner);all {
      planSlots'=planSlots,sharedTables'=sharedTables,objectMasks'=objectMasks,planRefs'=planRefs,templates'=templates,materializations'=materializations,
      phase'="borrowed",catalog'=catalog,work'=work,processed'=processed,owner'=owner,
      groups'=indexed(kernel,sharedTables.nth(planSlots.nth(owner))),masks'=List(advanced(kernel,0),advanced(kernel,1)),
      firsts'=List(if(earliest(kernel,0)<999) earliest(kernel,0) else firsts.nth(0),
        if(earliest(kernel,1)<999) earliest(kernel,1) else firsts.nth(1)),published'=published}}
    else if(phase=="borrowed") all {
      planSlots'=planSlots,sharedTables'=sharedTables,objectMasks'=objectMasks,planRefs'=planRefs,templates'=templates,materializations'=materializations,
      phase'="publish",catalog'=catalog,work'=work,processed'=processed,owner'=owner,
      groups'=groups,masks'=List(Set(),Set()),firsts'=firsts,published'=published}
    else if(phase=="publish") {val grown=install(catalog,groups)
      val allocated=grown.foldl(objectMasks,(out,kernel)=>
        if(lookup(out,predicted(kernel))<0) out.append(predicted(kernel)) else out)
      all {
      planSlots'=grown.indices().fold(planSlots,(out,id)=>
        if(id>=catalog.length()) appendSlot(out,id,lookup(allocated,predicted(grown.nth(id)))) else out),
      sharedTables'=allocated.indices().fold(sharedTables,(out,id)=>
        if(id>=objectMasks.length()) out.append(buildTable(allocated.nth(id))) else out),
      objectMasks'=allocated,
      planRefs'=grown.indices().fold(planRefs,(out,id)=>
        if(id>=catalog.length()) out.append(lookup(allocated,predicted(grown.nth(id)))) else out),
      templates'=templates.union(requested(grown)),
      materializations'=materializations+requested(grown).exclude(templates).size(),
      phase'="ready",catalog'=grown,
      work'=work.concat(List(0,1,2,3,4,5,6,7).foldl(List(),(out,id)=>if(id>=catalog.length() and id<grown.length()) out.append(id) else out)),
      processed'=processed,owner'=owner,groups'=groups,masks'=masks,firsts'=firsts,
      published'=published.append(groups.foldl(List(),(out,group)=>out.append({symbol:group.symbol,target:lookup(grown,group.kernel)})))}}
    else all {planSlots'=planSlots,sharedTables'=sharedTables,objectMasks'=objectMasks,planRefs'=planRefs,templates'=templates,materializations'=materializations,phase'=phase,catalog'=catalog,work'=work,processed'=processed,owner'=owner,
      groups'=groups,masks'=masks,firsts'=firsts,published'=published}
  val LifecycleSafety = all {
    templates==requested(catalog),materializations==templates.size(),
    materializations==objectMasks.length(),planRefs.length()==catalog.length(),
    planSlots.length()>0,catalog.length()<=planSlots.length(),
    catalog.indices().forall(id=>if(id<planSlots.length()) planSlots.nth(id)==planRefs.nth(id) else false),
    planSlots.indices().forall(id=>id<catalog.length() or planSlots.nth(id)== -1),
    sharedTables.length()==objectMasks.length(),
    objectMasks.indices().forall(id=>if(id<sharedTables.length()) Set(0,1).forall(key=>
      sharedTables.nth(id).nth(key)=={first:earliest(objectMasks.nth(id),key),symbol:key,kernel:advanced(objectMasks.nth(id),key)}) else false),
    templates==objectMasks.foldl(Set(),(out,mask)=>out.union(Set(mask))),
    objectMasks.indices().forall(i=>objectMasks.indices().forall(j=>i==j or objectMasks.nth(i)!=objectMasks.nth(j))),
    catalog.indices().forall(id=>planRefs.nth(id)==lookup(objectMasks,predicted(catalog.nth(id)))),
    catalog.indices().forall(i=>catalog.indices().forall(j=>i==j or catalog.nth(i)!=catalog.nth(j))),
    processed<=catalog.length(),work.length()+processed==catalog.length(),
    work.indices().forall(i=>work.nth(i)==processed+i),
    published.length()==(if(phase=="ready") processed else processed-1),
    (phase=="borrowed") or masks==List(Set(),Set()),
    (phase=="borrowed" or phase=="publish") implies all {
      groups.indices().forall(i=>groups.indices().forall(j=>i>=j or groups.nth(i).first<groups.nth(j).first)),
      groups.indices().forall(i=>groups.nth(i).kernel==advanced(closure(catalog.nth(owner)),groups.nth(i).symbol) and
        groups.nth(i).first==earliest(closure(catalog.nth(owner)),groups.nth(i).symbol)),
      Set(0,1).forall(key=>advanced(closure(catalog.nth(owner)),key).size()>0 implies
        groups.indices().exists(i=>groups.nth(i).symbol==key))},
    (phase=="borrowed") implies Set(0,1).forall(key=>masks.nth(key)==advanced(catalog.nth(owner),key) and
      (masks.nth(key).size()>0 implies firsts.nth(key)==earliest(catalog.nth(owner),key))),
    published.indices().forall(id=>Set(0,1).forall(key=>
      advanced(closure(catalog.nth(id)),key).size()>0 implies published.nth(id).indices().exists(i=>published.nth(id).nth(i).symbol==key))),
    published.indices().forall(id=>published.nth(id).indices().forall(i=>{
      val edge=published.nth(id).nth(i)
      edge.target>=0 and edge.target<catalog.length() and
        catalog.nth(edge.target)==advanced(closure(catalog.nth(id)),edge.symbol)}))
  }
  run missingTemplateRejectedTest = setup(999)
    .then(all {planSlots'=planSlots,sharedTables'=sharedTables,objectMasks'=objectMasks,planRefs'=planRefs,templates'=Set(),materializations'=0,phase'=phase,catalog'=catalog,
      work'=work,processed'=processed,owner'=owner,groups'=groups,masks'=masks,
      firsts'=firsts,published'=published})
    .expect(not(LifecycleSafety))
  run kernelKeyRejectedTest = setup(999)
    .then(all {planSlots'=planSlots,sharedTables'=sharedTables,objectMasks'=objectMasks,planRefs'=planRefs,templates'=Set(Set(0)),materializations'=1,phase'=phase,catalog'=catalog,
      work'=work,processed'=processed,owner'=owner,groups'=groups,masks'=masks,
      firsts'=firsts,published'=published})
    .expect(not(LifecycleSafety))

  run wrongPlanReferenceRejectedTest = setup(999)
    .then(all {planSlots'=planSlots,sharedTables'=sharedTables,objectMasks'=objectMasks,planRefs'=List(-1),templates'=templates,
      materializations'=materializations,phase'=phase,catalog'=catalog,work'=work,
      processed'=processed,owner'=owner,groups'=groups,masks'=masks,firsts'=firsts,published'=published})
    .expect(not(LifecycleSafety))
  run overwrittenTemplateRejectedTest = setup(999)
    .then(all {planSlots'=planSlots,sharedTables'=sharedTables,objectMasks'=List(Set()),planRefs'=planRefs,templates'=templates,
      materializations'=materializations,phase'=phase,catalog'=catalog,work'=work,
      processed'=processed,owner'=owner,groups'=groups,masks'=masks,firsts'=firsts,published'=published})
    .expect(not(LifecycleSafety))

  run lostPlanDuringGrowthRejectedTest = setup(999).then(step).then(step).then(step).then(step)
    .then(all {planSlots'=planSlots.replaceAt(0,-1),sharedTables'=sharedTables,objectMasks'=objectMasks,
      planRefs'=planRefs,templates'=templates,materializations'=materializations,phase'=phase,catalog'=catalog,
      work'=work,processed'=processed,owner'=owner,groups'=groups,masks'=masks,firsts'=firsts,published'=published})
    .expect(not(LifecycleSafety))
  run missingSharedIndexRejectedTest = setup(999)
    .then(all {planSlots'=planSlots,sharedTables'=sharedTables.replaceAt(0,List(
      {first:2,symbol:0,kernel:Set()},{first:1,symbol:1,kernel:Set(2)})),objectMasks'=objectMasks,
      planRefs'=planRefs,templates'=templates,materializations'=materializations,phase'=phase,catalog'=catalog,
      work'=work,processed'=processed,owner'=owner,groups'=groups,masks'=masks,firsts'=firsts,published'=published})
    .expect(not(LifecycleSafety))

  run staleFirstsTest = setup(999).then(step).then(step).expect(
    firsts==List(0,999) and groups==List({first:0,symbol:0,kernel:Set(1,3)},
      {first:1,symbol:1,kernel:Set(2)}) and LifecycleSafety)
  run cleanupBeforePublicationTest = setup(7).then(step).then(step).then(step)
    .expect(masks==List(Set(),Set()) and phase=="publish" and LifecycleSafety)
  run publicationEnqueuesOnlyNewIDsTest = setup(0).then(step).then(step).then(step).then(step)
    .expect(work==List(1,2) and catalog.length()==3 and published.length()==1 and LifecycleSafety)
  run missingResetRejectedTest = setup(0).then(step).then(step).then(step)
    .then(all {planSlots'=planSlots,sharedTables'=sharedTables,objectMasks'=objectMasks,planRefs'=planRefs,templates'=templates,materializations'=materializations,phase'=phase,catalog'=catalog,work'=work,processed'=processed,owner'=owner,
      groups'=groups,masks'=List(Set(1),Set()),firsts'=firsts,published'=published}).expect(not(LifecycleSafety))
  run duplicateQueueRejectedTest = setup(0)
    .then(all {planSlots'=planSlots,sharedTables'=sharedTables,objectMasks'=objectMasks,planRefs'=planRefs,templates'=templates,materializations'=materializations,phase'=phase,catalog'=catalog,work'=List(0,0),processed'=processed,owner'=owner,
      groups'=groups,masks'=masks,firsts'=firsts,published'=published}).expect(not(LifecycleSafety))
  run reversedGroupsRejectedTest = setup(0).then(step).then(step)
    .then(all {planSlots'=planSlots,sharedTables'=sharedTables,objectMasks'=objectMasks,planRefs'=planRefs,templates'=templates,materializations'=materializations,phase'=phase,catalog'=catalog,work'=work,processed'=processed,owner'=owner,
      groups'=List(groups.nth(1),groups.nth(0)),masks'=masks,firsts'=firsts,published'=published})
      .expect(not(LifecycleSafety))
  run completeDrainTest = setup(999)
    .then(step)
    .then(step)
    .then(step)
    .then(step)
    .then(step)
    .then(step)
    .then(step)
    .then(step)
    .then(step)
    .then(step)
    .then(step)
    .then(step)
    .then(step)
    .then(step)
    .then(step)
    .then(step)
    .expect(phase=="ready" and work==List() and processed==catalog.length() and published.length()==catalog.length() and LifecycleSafety)

}
