---- MODULE InvalidFunctionPairChain ----
EXTENDS TLC
CONSTANTS A, B, C
P == A :> B :> C
====
