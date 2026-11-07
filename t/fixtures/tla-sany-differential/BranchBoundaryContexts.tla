---- MODULE BranchBoundaryContexts ----
CONSTANTS D, A, B, Op(_, _)
I == IF /\ A
        /\ B THEN A ELSE B
J == IF A THEN /\ A
               /\ B ELSE A
K == LET F == /\ A
              /\ B IN F
L == [ /\ A
       /\ B -> D]
M == [x \in /\ A
            /\ B |-> x]
N == \A x \in /\ A
              /\ B : x = x
O == Op(/\ A
        /\ B, D)
====
