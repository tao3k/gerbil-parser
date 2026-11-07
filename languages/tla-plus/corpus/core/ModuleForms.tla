---- MODULE ModuleForms ----
EXTENDS Naturals
CONSTANT N
RECURSIVE Fact(_)
LOCAL Double(x) == x + x
INSTANCE Naturals
ASSUME Positive == N > 0
PROPOSITION Bound == N >= 0
====
