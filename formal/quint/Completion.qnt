// Occurrence counters and coalesced nullable/productive deltas in compute-completion.
// 0=S -> A A b; 1=A -> B B | a; 2=B -> A | epsilon;
// 3=dead -> dead; 4=blocked -> missing(5). Every edge exists before drain.
module Completion {
  type State = {n:Set[int],p:Set[int],pn:Set[int],pp:Set[int],queue:List[int],
    nl:List[int],pl:List[int],seeded:Set[int],phase:str}
  pure val deps = List(List(1,1),List(2,2),List(1),List(3),List(5),List(),List())
  pure val owners = List(0,1,2,3,4,2,1)
  pure val initial:State = {n:Set(),p:Set(),pn:Set(),pp:Set(),queue:List(),
    nl:List(-1,2,1,1,1,0,-1),pl:List(2,2,1,1,1,0,0),seeded:Set(),phase:"register"}
  var q:State
  pure def queued(s:State,name:int):bool = s.queue.indices().exists(i => s.queue.nth(i) == name)
  pure def publish(s:State,name:int,n:bool,p:bool):State = {
    val dn = n and not(s.n.contains(name))
    val dp = p and not(s.p.contains(name))
    {...s,n:if(dn) s.n.union(Set(name)) else s.n,p:if(dp) s.p.union(Set(name)) else s.p,
      pn:if(dn) s.pn.union(Set(name)) else s.pn,pp:if(dp) s.pp.union(Set(name)) else s.pp,
      queue:if((dn or dp) and not(queued(s,name))) s.queue.append(name) else s.queue}
  }
  pure def drain(s:State):State = {
    val source = s.queue.nth(0)
    val dn = s.pn.contains(source)
    val dp = s.pp.contains(source)
    val removed = {...s,pn:s.pn.exclude(Set(source)),pp:s.pp.exclude(Set(source)),queue:s.queue.slice(1,s.queue.length())}
    0.to(6).fold(removed,(acc,index) => {
      val multiplicity = deps.nth(index).select(name => name == source).length()
      val nr = acc.nl.nth(index) - (if(dn and acc.nl.nth(index) >= 0) multiplicity else 0)
      val pr = acc.pl.nth(index) - (if(dp) multiplicity else 0)
      publish({...acc,nl:acc.nl.replaceAt(index,nr),pl:acc.pl.replaceAt(index,pr)},owners.nth(index),nr == 0,pr == 0)
    })
  }
  action init = q' = initial
  action SeedEmpty = all {q.phase == "register",not(q.seeded.contains(5)),
    q' = publish({...q,seeded:q.seeded.union(Set(5))},2,true,true)}
  action SeedTerminal = all {q.phase == "register",not(q.seeded.contains(6)),
    q' = publish({...q,seeded:q.seeded.union(Set(6))},1,false,true)}
  action Start = all {q.phase == "register",q.seeded == Set(5,6),q' = {...q,phase:"propagate"}}
  action Drain = all {q.phase == "propagate",q.queue.length() > 0,q' = drain(q)}
  action Publish = all {q.phase == "propagate",q.queue.length() == 0,q' = {...q,phase:"published"}}
  action step = any {SeedEmpty,SeedTerminal,Start,Drain,Publish}
  pure def safe(s:State):bool = s.n.subseteq(Set(1,2)) and s.p.subseteq(Set(0,1,2))
    and s.pn.subseteq(s.n) and s.pp.subseteq(s.p)
    and 0.to(5).forall(name => queued(s,name) == (s.pn.contains(name) or s.pp.contains(name)))
    and s.queue.indices().forall(i => s.queue.indices().forall(j => i == j or s.queue.nth(i) != s.queue.nth(j)))
    and 0.to(6).forall(index =>
      s.nl.nth(index) == (if(index == 0 or index == 6) -1 else
        deps.nth(index).select(name => not(s.n.exclude(s.pn).contains(name))).length())
      and s.pl.nth(index) == deps.nth(index).select(name => not(s.p.exclude(s.pp).contains(name))).length())
    and (s.phase == "published" implies (s.queue == List() and s.n == Set(1,2) and s.p == Set(0,1,2)))
  val CompletionSafety = safe(q)
  action WrongDistinctCount = q' = {...q,nl:q.nl.replaceAt(1,1)}
  action WrongMissingPublisher = q' = {...q,pl:q.pl.replaceAt(4,0)}
  action WrongEarlyPublication = q' = {...q,phase:"published"}
  action WrongQueueDuplicate = all {q.queue.length() > 0,q' = {...q,queue:q.queue.append(q.queue.nth(0))}}
  run coalescedDomainsTest = init.then(SeedEmpty).then(SeedTerminal).then(Start).then(Drain)
    .expect(CompletionSafety and q.pn.contains(1) and q.pp.contains(1) and q.queue == List(1))
    .then(Drain).then(Drain).then(Publish).expect(CompletionSafety and q.phase == "published")
  run reversedSeedOrderTest = init.then(SeedTerminal).then(SeedEmpty).then(Start)
    .then(Drain).then(Drain).then(Drain).then(Drain).then(Publish).expect(CompletionSafety)
  run dependencyMultiplicityWitnessTest = init.then(WrongDistinctCount).expect(not(CompletionSafety))
  run missingReferenceWitnessTest = init.then(WrongMissingPublisher).expect(not(CompletionSafety))
  run pendingPublicationWitnessTest = init.then(SeedEmpty).then(WrongEarlyPublication).expect(not(CompletionSafety))
  run duplicateQueueWitnessTest = init.then(SeedEmpty).then(WrongQueueDuplicate).expect(not(CompletionSafety))
}
