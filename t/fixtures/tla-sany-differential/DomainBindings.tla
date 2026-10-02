---- MODULE DomainBindings ----
CONSTANTS S, T
F[x,y \in S] == <<x,y>>
G[<<x,y>> \in S] == <<x,y>>
Q == \A x,y \in S, z \in T: <<x,y,z>> = <<z,y,x>>
R == [x,y \in S |-> <<x,y>>]
U == {<<x,y>> : x \in S, y \in T}
V == {<<x,y>> \in S : x = y}
W == {x \in S : \E y \in T : <<x,y>> \in S}
X == \E x \in S : x \in T
Y == \A x \in S : \E y \in T : x = y
Z == {x \in S : <<x,x>> \in T}
Map == {(x \in S) : x \in T}
====
