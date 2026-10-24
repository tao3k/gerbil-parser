---- MODULE AlignedQuantifiers ----
CONSTANT D
VARIABLE v
P == (/\ (\A i \in D : i = i)
      /\ (\A j \in D : j = j))
Q == [][ /\ (\A i \in D : i = i)
         /\ (\A j \in D : j = j) ]_<<v>>
R == [x \in D |-> /\ x = x
                  /\ x = x]
S == {x \in D : /\ x = x
                /\ x = x}
T == << /\ (\A i \in D : i = i)
        /\ (\A j \in D : j = j) >>
U == << /\ (\A i \in D : i = i)
        /\ (\A j \in D : j = j) >>_v
V == (\/ (\E i \in D : i = i)
      \/ (\E j \in D : j = j))
W == /\ (/\ (\A i \in D : i = i)
         /\ (\A j \in D : j = j))
     /\ TRUE
====
