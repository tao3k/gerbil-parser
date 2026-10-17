---- MODULE TemporalPrecedence ----
CONSTANTS A, B, C
P == A ~> B
Q == (A /\ B) ~> C
R == A /\ B ~> C
S == A => B ~> C
T == A ~> (B \/ C)
====
