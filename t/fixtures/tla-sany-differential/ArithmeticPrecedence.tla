---- MODULE ArithmeticPrecedence ----
EXTENDS Reals
CONSTANTS A, B, C
P == A + B * C
Q == A - B + C
R == A + B - C
S == A /= B
T == A =< B
U == A + B /= C
V == A / (B / C)
W == (A / B) / C
X == A ^ (B ^ C)
Y == (A ^ B) ^ C
Z == A \div B
====
