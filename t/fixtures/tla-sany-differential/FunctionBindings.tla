---- MODULE FunctionBindings ----
CONSTANTS S, T
F[x \in S] == x
G[x \in S, y \in T] == <<x, y>>
L == LET H[x \in S] == x IN H
M == LET H[x \in S] == x K[y \in T] == y IN <<H, K>>
====
