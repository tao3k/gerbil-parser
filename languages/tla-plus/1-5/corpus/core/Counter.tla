---- MODULE Counter ----
EXTENDS Naturals
CONSTANT Max
VARIABLE count
Init == count = 0
Next == count' = IF count < Max THEN count + 1 ELSE 0
Spec == Init /\ [][Next]_count
THEOREM Spec => [] (count \in 0 .. Max)
====
