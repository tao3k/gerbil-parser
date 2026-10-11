---- MODULE SequenceFunctionOperators ----
EXTENDS Sequences, TLC
CONSTANTS A, B, C
P == A \o B \o C
Q == A \circ B
R == A @@ B @@ C
S == A :> B @@ C
T == A @@ B :> C
U == A :> (B @@ C)
V == (A @@ B) :> C
====
