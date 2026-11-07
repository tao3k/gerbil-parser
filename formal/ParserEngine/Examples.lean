import ParserEngine.Reduction

namespace ParserEngine.Examples

private def token : Tree := .token "identifier" ⟨3, 7⟩

example : interpretActions [.field "name", .alias "expression"] ⟨[(none, token)], 9⟩ =
    [(none, .node "expression" ⟨3, 7⟩ [(some "name", token)])] := rfl

example : interpretActions [.alias "expression", .field "name"] ⟨[(none, token)], 9⟩ =
    [(some "name", .node "expression" ⟨3, 7⟩ [(none, token)])] := rfl

example : field "outer" [(some "inner", token)] 9 =
    [(some "outer", .fragment ⟨3, 7⟩ [(some "inner", token)])] := rfl

example : alias "empty" [] 17 = [(none, .node "empty" ⟨17, 17⟩ [])] := rfl

example : reduceStack [⟨[.alias "later"], [], 11⟩, ⟨[.alias "earlier"], [], 5⟩] =
    [(none, .node "earlier" ⟨5, 5⟩ []), (none, .node "later" ⟨11, 11⟩ [])] := rfl

example : (Sequence.branch (.leaf [1, 2]) (.branch (.leaf []) (.leaf [3]))).materializeInto [4] = ([1, 2, 3, 4], 8) := rfl

example : copiedPrefix 100 = 4950 := by decide

example : ¬ (4 ≤ ([[] , []] : List Children).length) := by decide

end ParserEngine.Examples
