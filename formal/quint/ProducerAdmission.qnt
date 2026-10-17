// Finite controls for declaration-derived admission and producer storage.
// Universal storage/queue composition is proved in ProducerAdmission.lean.
module ProducerAdmission {
  type Base = {tag:str,name:int}
  type Operand = {base:Base,annotation:str}
  var inputs:List[Operand]
  var catalog:List[Base]
  var cursor:int
  var capability:bool
  pure val eof:Base = {tag:"eof",name:0}
  pure def layoutKey(key:Base):bool = key.tag == "layout-start" or key.tag == "layout-next"
  pure def terminal(key:Base):bool = key.tag != "nonterminal"
  action setup(values:List[Operand],layoutEnd:bool):bool = all {
    inputs'=values,catalog'=List(eof),cursor'=0,capability'=layoutEnd}
  action init = {
    nondet values=Set(List(),List({base:{tag:"token",name:1},annotation:"layout-end"}),
      List({base:{tag:"token",name:1},annotation:"field"},{base:{tag:"layout-next",name:2},annotation:""},
        {base:{tag:"token",name:1},annotation:"other"}),
      List({base:{tag:"nonterminal",name:2},annotation:""},{base:{tag:"literal",name:3},annotation:""})).oneOf()
    setup(values,false)
  }
  action step = if(cursor == inputs.length()) all {
    inputs'=inputs,catalog'=catalog,cursor'=cursor,capability'=capability}
    else {val key=inputs.nth(cursor).base; all {
      inputs'=inputs,cursor'=cursor+1,capability'=(capability or layoutKey(key)),
      catalog'=(if(not(terminal(key)) or catalog.indices().exists(i=>catalog.nth(i)==key)) catalog else catalog.append(key))}}
  val ProducerSafety = catalog.nth(0) == eof and catalog.indices().forall(i=>
    catalog.indices().forall(j=>i == j or catalog.nth(i) != catalog.nth(j))) and
    catalog.indices().forall(i=>catalog.nth(i) == eof or inputs.slice(0,cursor).indices().exists(j=>
      inputs.nth(j).base == catalog.nth(i) and terminal(inputs.nth(j).base)))
  run orderedBaseKeysTest = setup(List({base:{tag:"token",name:1},annotation:"field"},
      {base:{tag:"token",name:1},annotation:"other"},{base:{tag:"literal",name:2},annotation:""}),false)
    .then(step).then(step).then(step).expect(ProducerSafety and
      catalog == List(eof,{tag:"token",name:1},{tag:"literal",name:2}))
  run annotationNoCapabilityTest = setup(List({base:{tag:"token",name:1},annotation:"layout-end"}),false)
    .then(step).expect(not(capability) and ProducerSafety)
  run layoutOperandCapabilityTest = setup(List({base:{tag:"layout-start",name:1},annotation:"field"}),false)
    .then(step).expect(capability and ProducerSafety)
  run layoutActionCapabilityTest = setup(List(),true).expect(capability)
  pure def gotoKey(lhs:List[int],name:int):bool = lhs.indices().exists(i=>lhs.nth(i)==name)
  run lhsOnlyVocabularyTest = init.expect(gotoKey(List(0,2),2) and not(gotoKey(List(0,2),1)))
  pure def starts(rows:List[List[int]]):List[int] = rows.foldl(List(0),(out,row)=>out.append(out.nth(out.length()-1)+row.length()))
  run packedEmptyRowsTest = init.expect(starts(List(List(0,3),List(),List(8))) == List(0,2,2,3))
  run zeroMaskPreservedTest = init.expect(List(List(1,0),List(),List(65)).foldl(List(),(out,row)=>out.concat(row)) == List(1,0,65))
  run completedSlotFalseTest = init.expect(List(List(),List(7,9)).foldl(List(),(out,row)=>out.concat(row.append(-1))) == List(-1,7,9,-1))
}
