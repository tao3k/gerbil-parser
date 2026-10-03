---- MODULE MalformedIf ----
VARIABLE x
Broken == IF x = 0 THEN ELSE x
====
