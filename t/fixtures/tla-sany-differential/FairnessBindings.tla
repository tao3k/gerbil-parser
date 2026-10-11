---- MODULE FairnessBindings ----
CONSTANTS A, x, y
P == WF_<<x, y>>(A)
Q == SF_<<x, y>>(A)
R == WF_x(A)
S == SF_x(A)
====
