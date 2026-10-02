---- MODULE TemporalTuple ----
CONSTANTS A, x, y
P == [A]_<<x, y>>
Q == [][A]_<<x, y>>
R == [A]_x
S == [A]_(<<x, y>>)
====
