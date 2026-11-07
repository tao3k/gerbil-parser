// Bounded controls; arbitrary grammars/ranks/list lengths are proved in Lean.
module NativeActions {
  type Candidate = {key:int,kind:str,id:int}
  type Observation = {production:int,kind:str,terminal:int,target:int,lookaheads:List[int]}
  var observation:Observation
  var candidates:List[Candidate]
  var failed:bool
  pure def extract(o:Observation):List[Candidate] =
    if(o.kind == "shift") {
      if(o.target < 0) List() else List({key:o.terminal,kind:"shift",id:o.target})
    } else if(o.kind == "nonterminal") List() else
      o.lookaheads.foldl(List(),(out,key) => out.append({key:key,kind:if(o.production == 0) "accept" else "reduce",id:o.production}))
  action choose(o:Observation):bool = all {observation' = o,candidates' = extract(o),
    failed' = (o.kind == "shift" and o.target < 0)}
  action init = choose({production:0,kind:"complete",terminal:0,target:0,lookaheads:List(0)})
  action step = {
    nondet production = Set(0,1,2).oneOf()
    nondet kind = Set("shift","complete","nonterminal").oneOf()
    nondet terminal = Set(0,3).oneOf()
    nondet target = Set(-1,0,2).oneOf()
    nondet lookaheads = Set(List(),List(0),List(0,3,65,129)).oneOf()
    choose({production:production,kind:kind,terminal:terminal,target:target,lookaheads:lookaheads})
  }
  val CandidateOrigins = candidates.indices().forall(index => {val c = candidates.nth(index);
    (c.kind == "accept" implies observation.production == 0) and
    (c.kind == "reduce" implies observation.production > 0 and c.id == observation.production) and
    (c.kind == "shift" implies observation.kind == "shift" and c.id == observation.target and c.id >= 0)})
  val MissingBlocks = not(failed) or candidates == List()
  val CandidateSafety = CandidateOrigins and MissingBlocks
  pure def bounded(value:int,count:int):bool = value >= 0 and value < count
  pure def decision(association:str,shift:int,reduce:int,allowFork:bool):str =
    if(association == "dynamic") {if(allowFork) "fork" else "fail"}
    else if(reduce > shift) "reduce" else if(reduce < shift) "shift"
    else if(association == "left") "reduce" else if(association == "right") "shift" else "reject"
  run terminalZeroTest = init.then(choose({production:2,kind:"shift",terminal:0,target:0,lookaheads:List()}))
    .expect(CandidateSafety and candidates == List({key:0,kind:"shift",id:0}))
  run missingTransitionTest = init.then(choose({production:2,kind:"shift",terminal:0,target:-1,lookaheads:List(0)}))
    .expect(CandidateSafety and failed)
  run authoredReductionTest = init.then(choose({production:2,kind:"complete",terminal:0,target:-1,lookaheads:List(0,65,129)}))
    .expect(CandidateSafety and candidates.length() == 3 and candidates.indices().forall(index => candidates.nth(index).kind == "reduce"))
  run augmentationAcceptTest = init.expect(CandidateSafety and candidates == List({key:0,kind:"accept",id:0}))
  run signedPriorityTest = init.expect(decision("left",-3,-2,false) == "reduce" and
    decision("none",0,0,false) == "reject" and decision("dynamic",100,-100,true) == "fork")
  run negativeIndexTest = init.expect(not(bounded(-1,3)) and bounded(0,3) and not(bounded(3,3)))
}
