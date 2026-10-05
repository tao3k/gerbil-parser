;;; Scheme character endpoints for the three migrated TLA+ lexical rules.
(export structured-lexical-cases)
(def structured-lexical-cases
 '((identifier ("123" #f) ("1_name" 6) ("α٣_字!" 4) ("²" #f) ("α²" 1)
               ("_" 1) ("４2" #f) ("４a" 2) ("" #f))
   (proof-step ("<1>1. QED" 5) ("<+>*." 5) ("<*>-." 5) ("<1>" 3)
               ("<1>name" #f) ("<1>α٣." 6) ("<١>字..." 7)
               ("<+1>x." #f) ("<1" #f) ("<>." #f))
   (proof-reference ("<1>name" 7) ("<1>α٣" 5) ("<١>字" 4)
                    ("<1>1." #f) ("<1>" #f) ("<*>*" #f) ("<+>_" 4))))
