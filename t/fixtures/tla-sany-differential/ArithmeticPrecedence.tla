---- MODULE ArithmeticPrecedence ----
EXTENDS Integers
CONSTANTS A, B, C
P == A + B * C
Q == A - B + C
R == A + B - C
S == A /= B
T == A =< B
U == A + B /= C
====
