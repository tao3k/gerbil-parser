---- MODULE CaseScope ----
EXTENDS Naturals
CONSTANTS A, B, C, F
P == CASE A -> B [] OTHER -> F[C]
Q == CASE A -> B [] OTHER -> A + C
R == CASE A -> B [] OTHER -> F.x
S == (CASE A -> B [] OTHER -> F)[C]
T == CASE A -> B [] C -> A
====
