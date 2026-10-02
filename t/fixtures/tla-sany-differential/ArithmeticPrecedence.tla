---- MODULE ArithmeticPrecedence ----
EXTENDS Integers
CONSTANTS A, B, C
P == A + B * C
Q == A - B + C
R == A + B - C
====
