---- MODULE StructuredExpressions ----
EXTENDS Naturals, Sequences
CONSTANT S
VARIABLE x
Arithmetic == 1 + 2 * 3
Conditional == IF x \in S THEN x + 1 ELSE 0
TupleSet == <<x, 2>> \in {1, 2, 3}
Choice == CHOOSE y \in S : y > x
Quantified == \A y \in S : y >= x
Function == [y \in S |-> y + 1]
Applied == Function[x]
LetValue == LET Inc(y) == y + 1 IN Inc(x)
Cases == CASE x = 0 -> "zero" [] x > 0 -> "positive" [] OTHER -> "negative"
Record == [value |-> x, next |-> x + 1]
Filtered == {y \in S : y > x}
====
