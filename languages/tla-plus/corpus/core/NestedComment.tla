---- MODULE NestedComment ----
(* outer comment (* nested comment *) remains one trivia token *)
VARIABLE state
Init == state = "ready"
====
