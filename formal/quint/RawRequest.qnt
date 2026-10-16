// State-dependent terminal commitment for S -> X literal-if | Y token-word;
// X -> a, Y -> b. Raw value 3 is word/"if"; terminals 3 and 4 both match it.
// State IDs and rows are an independent finite fixture, not native encoding.
module RawRequest {
  type Frame = {symbol:int,terminals:List[int],raw:List[int]}
  type State = {input:List[int],rest:List[int],states:List[int],frames:List[Frame],phase:str}
  var q:State
  pure def initial(prefix:int):State = {input:List(prefix,3),rest:List(prefix,3),
    states:List(0),frames:List(),phase:"running"}
  action init = q' = initial(1)
  action initWord = q' = initial(2)
  action initInvalid = q' = {...initial(1),input:List(1,2),rest:List(1,2)}
  // 10=X, 11=Y, 12=S. Top-first physical stacks, source-order yields.
  pure def edge(state:int,symbol:int):int =
    if(state == 0 and symbol == 1) 1 else if(state == 0 and symbol == 2) 2
    else if(state == 0 and symbol == 10) 3 else if(state == 0 and symbol == 11) 4
    else if(state == 3 and symbol == 3) 5 else if(state == 4 and symbol == 4) 6
    else if(state == 0 and symbol == 12) 7 else -1
  // [key,action,target/production]; 1=shift,2=reduce,3=accept,0=reject.
  pure def selectRow(state:int,raw:int):List[int] =
    if(state == 0 and raw == 1) List(1,1,1)
    else if(state == 0 and raw == 2) List(2,1,2)
    else if(state == 1 and raw == 3) List(3,2,1)
    else if(state == 2 and raw == 3) List(4,2,2)
    else if(state == 3 and raw == 3) List(3,1,5)
    else if(state == 4 and raw == 3) List(4,1,6)
    else if(state == 5 and raw == 0) List(0,2,3)
    else if(state == 6 and raw == 0) List(0,2,4)
    else if(state == 7 and raw == 0) List(0,3,0) else List(0,0,0)
  pure def pending(s:State):int = if(s.rest.length() == 0) 0 else s.rest.nth(0)
  pure def lhs(production:int):int = if(production == 1) 10 else if(production == 2) 11 else 12
  pure def rhs(production:int):List[int] = if(production == 1) List(1)
    else if(production == 2) List(2) else if(production == 3) List(10,3) else List(11,4)
  pure def terminalYield(frames:List[Frame]):List[int] = frames.foldl(List(),(acc,f) => f.terminals.concat(acc))
  pure def rawYield(frames:List[Frame]):List[int] = frames.foldl(List(),(acc,f) => f.raw.concat(acc))
  pure def symbols(frames:List[Frame]):List[int] = frames.foldl(List(),(acc,f) => List(f.symbol).concat(acc))
  pure def selected(s:State):List[int] = selectRow(s.states.nth(0),pending(s))
  action Shift = all {q.phase == "running",selected(q).nth(1) == 1,
    val row = selected(q)
    q' = {...q,rest:q.rest.slice(1,q.rest.length()),states:List(row.nth(2)).concat(q.states),
      frames:List({symbol:row.nth(0),terminals:List(row.nth(0)),raw:List(pending(q))}).concat(q.frames)}}
  action Reduce = all {q.phase == "running",selected(q).nth(1) == 2,
    val production = selected(q).nth(2)
    val width = rhs(production).length()
    // These checks are derived generically from graph provenance in Lean.
    // The finite protocol also checks them independently in every visited state.
    val target = edge(q.states.nth(width),lhs(production))
    val used = q.frames.slice(0,width)
    all {q.frames.length() >= width,symbols(q.frames.slice(0,width)) == rhs(production),
    target != -1,
    q' = {...q,states:List(target).concat(q.states.slice(width,q.states.length())),
      frames:List({symbol:lhs(production),terminals:terminalYield(used),raw:rawYield(used)})
        .concat(q.frames.slice(width,q.frames.length()))}}}
  action Accept = all {q.phase == "running",selected(q).nth(1) == 3,q' = {...q,phase:"complete"}}
  action Reject = all {q.phase == "running",selected(q).nth(1) == 0,q' = {...q,phase:"rejected"}}
  action step = any {Shift,Reduce,Accept,Reject}
  pure def matches(terminal:int,raw:int):bool = terminal == raw or (terminal == 4 and raw == 3)
  pure def derived(f:Frame):bool =
    if(f.symbol < 10) f.terminals == List(f.symbol)
    else if(f.symbol == 10) f.terminals == List(1)
    else if(f.symbol == 11) f.terminals == List(2)
    else f.symbol == 12 and (f.terminals == List(1,3) or f.terminals == List(2,4))
  pure def safe(s:State):bool = s.states.length() == s.frames.length() + 1
    and s.states.nth(s.states.length() - 1) == 0
    and s.frames.indices().forall(i => edge(s.states.nth(i + 1),s.frames.nth(i).symbol) == s.states.nth(i))
    and s.frames.indices().forall(i => derived(s.frames.nth(i)))
    and rawYield(s.frames).concat(s.rest) == s.input
    and terminalYield(s.frames).length() == rawYield(s.frames).length()
    and terminalYield(s.frames).indices().forall(i => matches(terminalYield(s.frames).nth(i),rawYield(s.frames).nth(i)))
    and (s.phase == "complete" implies s.rest == List() and s.frames.length() == 1 and s.frames.nth(0).symbol == 12)
    and (s.phase != "running" or selected(s).nth(1) != 2 or
      (s.frames.length() >= rhs(selected(s).nth(2)).length() and
       symbols(s.frames.slice(0,rhs(selected(s).nth(2)).length())) == rhs(selected(s).nth(2))))
  val RawSafety = safe(q)
  action WrongConsumeOnReduce = all {q.rest.length() > 0,
    q' = {...q,rest:q.rest.slice(1,q.rest.length())}}
  action WrongStateIndependentTerminal = all {q.states.nth(0) == 4,
    q' = {...q,rest:List(),states:List(6).concat(q.states),
      frames:List({symbol:3,terminals:List(3),raw:List(3)}).concat(q.frames)}}
  action WrongAccept = q' = {...q,phase:"complete"}
  action WrongReductionOrder = all {q.frames.length() == 2,
    q' = {...q,states:List(7,0),frames:List({symbol:12,
      terminals:q.frames.nth(0).terminals.concat(q.frames.nth(1).terminals),raw:rawYield(q.frames)})}}
  run literalRequestTest = init.then(Shift).expect(selected(q) == List(3,2,1))
    .then(Reduce).expect(q.rest == List(3) and selected(q) == List(3,1,5))
    .then(Shift).then(Reduce).then(Accept).expect(RawSafety and terminalYield(q.frames) == List(1,3))
  run tokenRequestTest = initWord.then(Shift).expect(selected(q) == List(4,2,2))
    .then(Reduce).expect(q.rest == List(3) and selected(q) == List(4,1,6))
    .then(Shift).then(Reduce).then(Accept).expect(RawSafety and terminalYield(q.frames) == List(2,4))
  run rejectionTest = initInvalid.then(Shift).then(Reject).expect(RawSafety and q.phase == "rejected")
  run consumeOnReductionWitnessTest = init.then(Shift).then(WrongConsumeOnReduce).expect(not(RawSafety))
  run globalTerminalWitnessTest = initWord.then(Shift).then(Reduce).then(WrongStateIndependentTerminal).expect(not(RawSafety))
  run acceptanceNeedsStartWitnessTest = init.then(Shift).then(WrongAccept).expect(not(RawSafety))
  run reductionOrderWitnessTest = init.then(Shift).then(Reduce).then(Shift).then(WrongReductionOrder).expect(not(RawSafety))
}
