// Reader vocabulary -> encoded production stream -> installed ordinal catalogue.
// Generic arbitrary-list correspondence and paper publication are Lean proofs.
module ConstructionVocabulary {
  type Key = { kind: str, text: str }
  type Encoded = { kind: str, code: int }
  type State = { raw: List[Key], directory: List[Key], stream: List[Encoded],
    installed: List[Encoded], cursor: int, phase: str }
  var q: State
  pure val eof: Key = { kind: "eof", text: "" }
  pure val encodedEOF: Encoded = { kind: "eof", code: 0 }
  pure val controls: List[List[Key]] = List(
    List(),
    List({kind:"token",text:"same"},{kind:"literal",text:"same"},{kind:"token",text:"same"}),
    List({kind:"layout-start",text:"same"},{kind:"layout-next",text:"same"},{kind:"literal",text:"same"}),
    List({kind:"literal",text:"é"},{kind:"literal",text:"é"},{kind:"token",text:"名称"}))
  pure def collect(entries: List[Key]): List[Key] = entries.foldl(List(eof), (found, key) =>
    if (found.indices().exists(i=>found.nth(i)==key)) found else found.append(key))
  pure def ordinal(entries: List[Key], key: Key): int = entries.indices().fold(-1, (at, index) =>
    if (at < 0 and entries.nth(index) == key) index else at)
  pure def encode(entries: List[Key], key: Key): Encoded = {kind:key.kind,code:ordinal(entries,key)}
  action start(raw: List[Key]): bool = q' = {raw:raw,directory:collect(raw),stream:List(),
    installed:List(encodedEOF),cursor:0,phase:"input"}
  action init = nondet choice = controls.indices().oneOf(); start(controls.nth(choice))
  action scan = all { q.phase == "input", q.cursor < q.raw.length(),
    val key = encode(q.directory,q.raw.nth(q.cursor))
    q' = {...q,stream:q.stream.append(key),cursor:q.cursor+1,
      installed:if(q.installed.indices().exists(i=>q.installed.nth(i)==key)) q.installed else q.installed.append(key)} }
  action finish = all {q.phase=="input",q.cursor==q.raw.length(),q'={...q,phase:"ready"}}
  action step = any {scan,finish,q'=q}
  val VocabularySafety = all {
    q.directory == collect(q.raw), q.stream.length() == q.cursor, q.cursor <= q.raw.length(),
    q.installed == q.directory.select(key =>
      key==eof or q.raw.indices().exists(i=>i<q.cursor and q.raw.nth(i)==key)).foldl(List(),(out,key)=>out.append(encode(q.directory,key))),
    q.stream.indices().forall(i=>q.stream.nth(i)==encode(q.directory,q.raw.nth(i))),
    q.installed.nth(0)==encodedEOF,
    q.installed.indices().forall(i=>q.installed.nth(i).code==i),
    q.phase=="ready" implies q.installed==q.directory.foldl(List(),(out,key)=>out.append(encode(q.directory,key))) }
  run emptyCatalogueTest = start(controls.nth(0)).then(finish).expect(VocabularySafety)
  run repeatedTaggedKeysTest = start(controls.nth(1)).then(scan).then(scan).then(scan).then(finish)
    .expect(VocabularySafety and q.installed.length()==3 and q.stream.nth(0)==q.stream.nth(2))
  run layoutTagSeparationTest = start(controls.nth(2)).then(scan).then(scan).then(scan).then(finish)
    .expect(VocabularySafety and q.installed.length()==4)
  run unicodeContentsTest = start(controls.nth(3)).then(scan).then(scan).then(scan).then(finish)
    .expect(VocabularySafety and q.installed.length()==4)
  run missingEOFRejectedTest = start(controls.nth(1)).then(scan).then(q'={...q,installed:q.installed.slice(1,q.installed.length())})
    .expect(not(VocabularySafety))
  run recatalogueOrderRejectedTest = start(controls.nth(1)).then(scan).then(scan).then(scan).then(finish)
    .then(q'={...q,installed:List(encodedEOF,q.installed.nth(2),q.installed.nth(1))}).expect(not(VocabularySafety))
  run recodedOrdinalRejectedTest = start(controls.nth(1)).then(scan)
    .then(q'={...q,stream:List({kind:"token",code:2})}).expect(not(VocabularySafety))
  run droppedOccurrenceRejectedTest = start(controls.nth(1)).then(scan).then(scan).then(scan)
    .then(q'={...q,stream:q.stream.slice(0,2)}).expect(not(VocabularySafety))
}
