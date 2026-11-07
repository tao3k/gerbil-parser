// Stable host lookup is an external premise. Empty entries denote cwd.
module BuildEnvironment {
  pure def normalize(paths:List[str]):List[str] = paths.foldl(
    {first:"", prefix:true, out:List()}, (s,p) =>
      if(s.out.length()==0) {first:p,prefix:true,out:List(p)}
      else if(s.prefix and p==s.first) s
      else {...s,prefix:false,out:s.out.append(p)}).out
  pure def resolve(paths:List[str]):str = paths.foldl("none",(selected,p) =>
    if(selected!="none" or p=="missing") selected else if(p=="") "cwd" else p)
  type State = {oldPath:List[str],newPath:List[str],oldFlags:str,newFlags:str,admitted:bool}
  var q:State
  action start(paths:List[str]):bool = q'={oldPath:paths,newPath:paths,
    oldFlags:"gcc",newFlags:"gcc",admitted:false}
  action init = {nondet paths=Set(List(),List(""),List("a","b"),
    List("missing","a",""),List("","","a"),List("a","a","b","")).oneOf(); start(paths)}
  action duplicate = all {q.newPath.length()>0,
    q'={...q,newPath:List(q.newPath.nth(0)).concat(q.newPath),admitted:false}}
  action changeFlags = q'={...q,newFlags:"different",admitted:false}
  action check = q'={...q,admitted:normalize(q.oldPath)==normalize(q.newPath) and q.oldFlags==q.newFlags}
  action step = any {duplicate,changeFlags,check,q'=q}
  val SearchSafety = all {
    resolve(normalize(q.newPath))==resolve(q.newPath),
    q.admitted implies (resolve(q.oldPath)==resolve(q.newPath) and q.oldFlags==q.newFlags)
  }
  run duplicatePrefixTest=start(List("a","b")).then(duplicate).then(check)
    .expect(q.admitted and SearchSafety)
  run currentDirectoryTest=start(List("","","missing","a")).then(duplicate).then(check)
    .expect(q.admitted and resolve(normalize(q.newPath))=="cwd" and SearchSafety)
  run absentToolTest=start(List("missing","missing")).then(duplicate).then(check)
    .expect(q.admitted and resolve(q.newPath)=="none" and SearchSafety)
  run changedFlagsTest=start(List("a","b")).then(changeFlags).then(check)
    .expect(not(q.admitted) and SearchSafety)
  run reorderedPathTest=start(List("a","b"))
    .then(q'={...q,newPath:List("b","a")}).then(check).expect(not(q.admitted) and SearchSafety)
  run reorderedAdmissionRejectedTest=start(List("a","b"))
    .then(q'={...q,newPath:List("b","a"),admitted:true}).expect(not(SearchSafety))
}
