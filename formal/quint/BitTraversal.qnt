// Bounded callback schedule. Integer arithmetic and unbounded correctness
// belong to Lean; this model checks callback-before-clear and field frames.
module BitTraversal {
  var support:Set[int]
  var pending:Set[int]
  var rest:Set[int]
  var width:int
  var offset:int
  var origin:int
  var limit:int
  var direct:bool
  var phase:str
  var callbacks:List[int]
  var offsets:List[int]
  pure def least(bits:Set[int]):int = bits.fold(99,(a,b)=>if(b<a) b else a)
  pure def bitLength(bits:Set[int]):int = bits.fold(0,(a,b)=>if(b+1>a) b+1 else a)
  pure def ordered(bits:Set[int]):List[int] = List(0,1,2,3,4,5,6,7).foldl(List(),(out,bit)=>if(bits.contains(bit)) out.append(bit) else out)
  pure def localField(bits:Set[int],base:int,span:int):Set[int] = bits.fold(Set(),(out,bit)=>
    if(base<=bit and bit<base+span) out.union(Set(bit-base)) else out)
  action setup(bits:Set[int],span:int,small:bool):bool = {
    val base=if(bits.size()==0 or small) 0 else (least(bits)/span)*span
    all {support'=bits,pending'=bits,rest'=if(small) bits else Set(),width'=span,
      offset'=base,origin'=base,limit'=bitLength(bits),direct'=small,
      phase'=if(small) "visit" else "field",callbacks'=List(),offsets'=List()}}
  action init = {
    nondet bits=Set(Set(),Set(0),Set(7),Set(0,7),Set(2,3,6,7),Set(0,1,2,3,4,5,6,7)).oneOf()
    nondet span=Set(1,2,3).oneOf()
    nondet small=Set(true,false).oneOf()
    setup(bits,span,small)
  }
  action step = if(phase=="field") all {
    support'=support,pending'=pending,rest'=if(offset<limit) localField(support,offset,width) else Set(),
    width'=width,offset'=offset,origin'=origin,limit'=limit,direct'=direct,
    phase'=if(offset<limit) "visit" else "done",callbacks'=callbacks,
    offsets'=if(offset<limit) offsets.append(offset) else offsets}
    else if(phase=="visit" and rest.size()>0) all {
      support'=support,pending'=pending,rest'=rest,width'=width,offset'=offset,origin'=origin,limit'=limit,direct'=direct,
      phase'="clear",callbacks'=callbacks.append(offset+least(rest)),offsets'=offsets}
    else if(phase=="clear") all {
      support'=support,pending'=pending.exclude(Set(offset+least(rest))),rest'=rest.exclude(Set(least(rest))),
      width'=width,offset'=offset,origin'=origin,limit'=limit,direct'=direct,phase'="visit",callbacks'=callbacks,offsets'=offsets}
    else if(phase=="visit") all {
      support'=support,pending'=pending,rest'=rest,width'=width,offset'=if(direct) offset else offset+width,
      origin'=origin,limit'=limit,direct'=direct,phase'=if(direct) "done" else "field",callbacks'=callbacks,offsets'=offsets}
    else all {support'=support,pending'=pending,rest'=rest,width'=width,offset'=offset,origin'=origin,
      limit'=limit,direct'=direct,phase'=phase,callbacks'=callbacks,offsets'=offsets}
  val CallbackSafety = {
    val expected=ordered(support)
    val delivered=callbacks.slice(0,if(phase=="clear") callbacks.length()-1 else callbacks.length()).foldl(Set(),(out,bit)=>out.union(Set(bit)))
    all {callbacks.length()<=expected.length(),callbacks==expected.slice(0,callbacks.length()),
      pending==support.exclude(delivered),width>0,
      offsets.indices().forall(i=>offsets.nth(i)==origin+i*width and offsets.nth(i)<limit),
      (phase=="clear") implies rest.size()>0,
      (phase=="done") implies (pending==Set() and callbacks==expected)}
  }
  run directCallbackBeforeClearTest = setup(Set(1,3),2,true).then(step)
    .expect(callbacks==List(1) and pending==Set(1,3) and phase=="clear" and CallbackSafety)
  run fieldOffsetTest = setup(Set(7),3,false).then(step).then(step)
    .expect(origin==6 and callbacks==List(7) and offsets==List(6) and CallbackSafety)
  run emptyFieldPreservesOrderTest = setup(Set(0,7),2,false)
    .then(step).then(step).then(step).then(step).then(step).then(step)
    .expect(callbacks==List(0) and offsets==List(0,2) and CallbackSafety)
  run missingClearRejectedTest = setup(Set(1,3),2,true).then(step)
    .then(all {support'=support,pending'=pending,rest'=rest,width'=width,offset'=offset,origin'=origin,
      limit'=limit,direct'=direct,phase'="visit",callbacks'=callbacks,offsets'=offsets}).expect(not(CallbackSafety))
  run missingOffsetRejectedTest = setup(Set(7),3,false).then(step)
    .then(all {support'=support,pending'=pending,rest'=rest,width'=width,offset'=offset,origin'=origin,
      limit'=limit,direct'=direct,phase'="clear",callbacks'=List(1),offsets'=offsets}).expect(not(CallbackSafety))
}
